#!/usr/bin/env python3
"""
Code Improvement Agent
======================
Scans source files and suggests improvements for readability,
performance, and best practices.

Usage:
    python code_improver.py <file_or_directory> [--ext .R .py .js] [--output report.md]

Examples:
    python code_improver.py ../master_scripts/functions.R
    python code_improver.py ../case_template/scripts/ --ext .R
    python code_improver.py .. --ext .R --output improvements.md
"""

import argparse
import os
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional


# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------

@dataclass
class Issue:
    """A single code improvement suggestion."""
    file: str
    line: int
    category: str          # readability | performance | best-practice
    severity: str          # info | warning | error
    rule: str              # short rule id
    message: str           # human-readable explanation
    current_code: str      # the problematic snippet
    improved_code: str     # the suggested replacement


@dataclass
class ScanResult:
    """Aggregated scan output for one file."""
    file: str
    issues: list = field(default_factory=list)


# ---------------------------------------------------------------------------
# Rule registry
# ---------------------------------------------------------------------------

class Rule:
    """Base class for improvement rules."""

    id: str = "base"
    category: str = "best-practice"
    severity: str = "warning"

    def check(self, lines: list[str], filepath: str) -> list[Issue]:
        raise NotImplementedError


# -- R-specific rules -------------------------------------------------------

class RAssignmentArrow(Rule):
    """Prefer <- over = for assignment in R (tidyverse style guide)."""
    id = "r-assign-arrow"
    category = "readability"
    severity = "info"

    _PAT = re.compile(
        r"^([A-Za-z_.][A-Za-z0-9_.]*)\s+=\s+(?!.*[=!<>]=)"  # simple top-level =
    )
    # Exclude lines inside function calls, default args, named args
    _EXCLUDE = re.compile(r"^\s|.*\(.*=|function\s*\(")

    def check(self, lines, filepath):
        if not filepath.endswith(".R"):
            return []
        issues = []
        for i, line in enumerate(lines, 1):
            stripped = line.rstrip()
            if self._EXCLUDE.match(stripped):
                continue
            m = self._PAT.match(stripped)
            if m:
                var = m.group(1)
                issues.append(Issue(
                    file=filepath, line=i,
                    category=self.category, severity=self.severity,
                    rule=self.id,
                    message=f"Use `<-` instead of `=` for top-level assignment of `{var}`.",
                    current_code=stripped,
                    improved_code=stripped.replace(" = ", " <- ", 1),
                ))
        return issues


class RNestedIfElse(Rule):
    """Suggest dplyr::case_when() for deeply nested if/else chains."""
    id = "r-nested-ifelse"
    category = "readability"
    severity = "warning"

    _PAT = re.compile(r"\bifelse\s*\(.*\bifelse\s*\(")

    def check(self, lines, filepath):
        if not filepath.endswith(".R"):
            return []
        issues = []
        for i, line in enumerate(lines, 1):
            if self._PAT.search(line):
                issues.append(Issue(
                    file=filepath, line=i,
                    category=self.category, severity=self.severity,
                    rule=self.id,
                    message="Nested `ifelse()` calls are hard to read. Consider `dplyr::case_when()`.",
                    current_code=line.rstrip(),
                    improved_code="# Replace nested ifelse with:\n# dplyr::case_when(\n#   condition1 ~ value1,\n#   condition2 ~ value2,\n#   TRUE ~ default_value\n# )",
                ))
        return issues


class RSuppressWarnings(Rule):
    """Flag blanket suppressWarnings() which can hide real problems."""
    id = "r-suppress-warnings"
    category = "best-practice"
    severity = "warning"

    _PAT = re.compile(r"\bsuppressWarnings\s*\(")

    def check(self, lines, filepath):
        if not filepath.endswith(".R"):
            return []
        issues = []
        for i, line in enumerate(lines, 1):
            if self._PAT.search(line):
                issues.append(Issue(
                    file=filepath, line=i,
                    category=self.category, severity=self.severity,
                    rule=self.id,
                    message="Blanket `suppressWarnings()` may hide legitimate issues. "
                            "Use `tryCatch()` or handle specific warning classes instead.",
                    current_code=line.rstrip(),
                    improved_code=line.rstrip().replace(
                        "suppressWarnings(",
                        "withCallingHandlers(  # handle specific warnings\n  ",
                    ),
                ))
        return issues


class RHardcodedPath(Rule):
    """Detect hardcoded Windows-style absolute paths."""
    id = "r-hardcoded-path"
    category = "best-practice"
    severity = "warning"

    _PAT = re.compile(r'["\'][A-Z]:[/\\]')

    def check(self, lines, filepath):
        if not filepath.endswith(".R"):
            return []
        issues = []
        for i, line in enumerate(lines, 1):
            stripped = line.strip()
            if stripped.startswith("#"):
                continue
            if self._PAT.search(line):
                issues.append(Issue(
                    file=filepath, line=i,
                    category=self.category, severity=self.severity,
                    rule=self.id,
                    message="Hardcoded absolute path reduces portability. "
                            "Use `file.path()`, config files, or environment variables.",
                    current_code=line.rstrip(),
                    improved_code="# Use a config variable or environment variable instead:\n"
                                  "# path <- Sys.getenv(\"ADS_SHARED\", \"D:/Shared/Master_Scripts\")",
                ))
        return issues


class RToPaste0(Rule):
    """Suggest paste0() when paste(..., sep='') is used."""
    id = "r-paste0"
    category = "readability"
    severity = "info"

    _PAT = re.compile(r'\bpaste\s*\(.*sep\s*=\s*""')

    def check(self, lines, filepath):
        if not filepath.endswith(".R"):
            return []
        issues = []
        for i, line in enumerate(lines, 1):
            if self._PAT.search(line):
                issues.append(Issue(
                    file=filepath, line=i,
                    category=self.category, severity=self.severity,
                    rule=self.id,
                    message='Use `paste0()` instead of `paste(..., sep="")`.',
                    current_code=line.rstrip(),
                    improved_code=re.sub(
                        r'\bpaste\s*\((.*),\s*sep\s*=\s*""\s*\)',
                        r"paste0(\1)",
                        line.rstrip(),
                    ),
                ))
        return issues


class RDuplicateLibrary(Rule):
    """Flag duplicate library() calls within the same file."""
    id = "r-duplicate-library"
    category = "readability"
    severity = "warning"

    _PAT = re.compile(r"^\s*library\s*\(\s*([A-Za-z0-9_.]+)\s*\)")

    def check(self, lines, filepath):
        if not filepath.endswith(".R"):
            return []
        seen: dict[str, int] = {}
        issues = []
        for i, line in enumerate(lines, 1):
            m = self._PAT.match(line)
            if m:
                pkg = m.group(1)
                if pkg in seen:
                    issues.append(Issue(
                        file=filepath, line=i,
                        category=self.category, severity=self.severity,
                        rule=self.id,
                        message=f"`library({pkg})` already loaded on line {seen[pkg]}.",
                        current_code=line.rstrip(),
                        improved_code=f"# library({pkg})  # already loaded on line {seen[pkg]}",
                    ))
                else:
                    seen[pkg] = i
        return issues


class RLongLine(Rule):
    """Flag lines exceeding 120 characters."""
    id = "r-long-line"
    category = "readability"
    severity = "info"

    MAX_LEN = 120

    def check(self, lines, filepath):
        if not filepath.endswith(".R"):
            return []
        issues = []
        for i, line in enumerate(lines, 1):
            if len(line.rstrip()) > self.MAX_LEN:
                issues.append(Issue(
                    file=filepath, line=i,
                    category=self.category, severity=self.severity,
                    rule=self.id,
                    message=f"Line is {len(line.rstrip())} chars (max {self.MAX_LEN}). "
                            "Break into multiple lines for readability.",
                    current_code=line.rstrip()[:80] + "...",
                    improved_code="# Break long lines using:\n#   result <- some_function(\n#     arg1, arg2,\n#     arg3\n#   )",
                ))
        return issues


class RTEqualsTrue(Rule):
    """Flag explicit == TRUE / == FALSE comparisons."""
    id = "r-eq-true"
    category = "readability"
    severity = "info"

    _PAT = re.compile(r"==\s*(TRUE|FALSE)\b|\b(TRUE|FALSE)\s*==")

    def check(self, lines, filepath):
        if not filepath.endswith(".R"):
            return []
        issues = []
        for i, line in enumerate(lines, 1):
            stripped = line.strip()
            if stripped.startswith("#"):
                continue
            m = self._PAT.search(line)
            if m:
                val = m.group(1) or m.group(2)
                issues.append(Issue(
                    file=filepath, line=i,
                    category=self.category, severity=self.severity,
                    rule=self.id,
                    message=f"Explicit `== {val}` is redundant. "
                            f"Use `{'!' if val == 'FALSE' else ''}variable` directly.",
                    current_code=line.rstrip(),
                    improved_code="# Instead of: x == TRUE  -> use: x\n"
                                  "# Instead of: x == FALSE -> use: !x",
                ))
        return issues


# -- General rules (language-agnostic) --------------------------------------

class TodoFixme(Rule):
    """Surface TODO/FIXME/HACK/XXX comments for visibility."""
    id = "todo-fixme"
    category = "best-practice"
    severity = "info"

    _PAT = re.compile(r"#\s*(TODO|FIXME|HACK|XXX|BUG)\b", re.IGNORECASE)

    def check(self, lines, filepath):
        issues = []
        for i, line in enumerate(lines, 1):
            m = self._PAT.search(line)
            if m:
                tag = m.group(1).upper()
                issues.append(Issue(
                    file=filepath, line=i,
                    category=self.category, severity=self.severity,
                    rule=self.id,
                    message=f"Unresolved `{tag}` comment found. Address or track in an issue.",
                    current_code=line.rstrip(),
                    improved_code="# (resolve and remove the comment)",
                ))
        return issues


class LargeFunction(Rule):
    """Flag functions longer than 50 lines (R-style definitions)."""
    id = "large-function"
    category = "readability"
    severity = "warning"

    MAX_LINES = 50

    def check(self, lines, filepath):
        if not filepath.endswith(".R"):
            return []
        issues = []
        func_pat = re.compile(r"^(\w[\w.]*)\s*<-\s*function\s*\(")
        i = 0
        while i < len(lines):
            m = func_pat.match(lines[i])
            if m:
                func_name = m.group(1)
                start = i
                brace_count = lines[i].count("{") - lines[i].count("}")
                i += 1
                while i < len(lines) and brace_count > 0:
                    brace_count += lines[i].count("{") - lines[i].count("}")
                    i += 1
                length = i - start
                if length > self.MAX_LINES:
                    issues.append(Issue(
                        file=filepath, line=start + 1,
                        category=self.category, severity=self.severity,
                        rule=self.id,
                        message=f"Function `{func_name}` is {length} lines "
                                f"(threshold {self.MAX_LINES}). Consider extracting helpers.",
                        current_code=f"{func_name} <- function(...) {{ # {length} lines }}",
                        improved_code=f"# Split into smaller focused functions:\n"
                                      f"# {func_name}_step1 <- function(...) {{ ... }}\n"
                                      f"# {func_name}_step2 <- function(...) {{ ... }}",
                    ))
            else:
                i += 1
        return issues


# ---------------------------------------------------------------------------
# Rule registry (instantiate all rules)
# ---------------------------------------------------------------------------

ALL_RULES: list[Rule] = [
    RAssignmentArrow(),
    RNestedIfElse(),
    RSuppressWarnings(),
    RHardcodedPath(),
    RToPaste0(),
    RDuplicateLibrary(),
    RLongLine(),
    RTEqualsTrue(),
    TodoFixme(),
    LargeFunction(),
]


# ---------------------------------------------------------------------------
# Scanner
# ---------------------------------------------------------------------------

def scan_file(filepath: str, rules: list[Rule] | None = None) -> ScanResult:
    """Scan a single file and return all issues found."""
    rules = rules or ALL_RULES
    result = ScanResult(file=filepath)
    try:
        with open(filepath, encoding="utf-8", errors="replace") as f:
            lines = f.readlines()
    except (OSError, UnicodeDecodeError) as exc:
        result.issues.append(Issue(
            file=filepath, line=0,
            category="best-practice", severity="error",
            rule="file-read-error",
            message=f"Could not read file: {exc}",
            current_code="", improved_code="",
        ))
        return result

    for rule in rules:
        result.issues.extend(rule.check(lines, filepath))

    result.issues.sort(key=lambda x: x.line)
    return result


def scan_directory(
    dirpath: str,
    extensions: list[str] | None = None,
    rules: list[Rule] | None = None,
) -> list[ScanResult]:
    """Recursively scan a directory, returning results per file."""
    extensions = extensions or [".R"]
    results = []
    for root, _dirs, files in os.walk(dirpath):
        for fname in sorted(files):
            if any(fname.endswith(ext) for ext in extensions):
                fpath = os.path.join(root, fname)
                res = scan_file(fpath, rules)
                if res.issues:
                    results.append(res)
    return results


# ---------------------------------------------------------------------------
# Formatters
# ---------------------------------------------------------------------------

SEVERITY_SYMBOL = {"error": "[!]", "warning": "[~]", "info": "[.]"}
CATEGORY_LABEL = {
    "readability": "Readability",
    "performance": "Performance",
    "best-practice": "Best Practice",
}


def format_issue_text(issue: Issue) -> str:
    """Format a single issue for terminal output."""
    sym = SEVERITY_SYMBOL.get(issue.severity, "[?]")
    cat = CATEGORY_LABEL.get(issue.category, issue.category)
    parts = [
        f"  {sym} Line {issue.line} [{cat}] ({issue.rule})",
        f"      {issue.message}",
    ]
    if issue.current_code:
        parts.append(f"      Current:  {issue.current_code}")
    if issue.improved_code:
        for j, imp_line in enumerate(issue.improved_code.split("\n")):
            prefix = "      Improved: " if j == 0 else "                "
            parts.append(f"{prefix}{imp_line}")
    return "\n".join(parts)


def format_results_text(results: list[ScanResult]) -> str:
    """Format all results for terminal output."""
    if not results:
        return "No issues found."

    total = sum(len(r.issues) for r in results)
    parts = [
        "=" * 72,
        "  CODE IMPROVEMENT REPORT",
        "=" * 72,
        f"  Files scanned with issues: {len(results)}",
        f"  Total suggestions: {total}",
        "=" * 72,
        "",
    ]

    for res in results:
        parts.append(f"--- {res.file} ({len(res.issues)} issue(s)) ---")
        for issue in res.issues:
            parts.append(format_issue_text(issue))
            parts.append("")
        parts.append("")

    # Summary by category
    cats: dict[str, int] = {}
    sevs: dict[str, int] = {}
    for res in results:
        for iss in res.issues:
            cats[iss.category] = cats.get(iss.category, 0) + 1
            sevs[iss.severity] = sevs.get(iss.severity, 0) + 1

    parts.append("-" * 72)
    parts.append("  SUMMARY")
    parts.append("-" * 72)
    parts.append("  By category:")
    for cat, count in sorted(cats.items()):
        parts.append(f"    {CATEGORY_LABEL.get(cat, cat):20s} {count}")
    parts.append("  By severity:")
    for sev, count in sorted(sevs.items()):
        parts.append(f"    {sev:20s} {count}")
    parts.append("-" * 72)

    return "\n".join(parts)


def format_results_markdown(results: list[ScanResult]) -> str:
    """Format all results as Markdown."""
    if not results:
        return "# Code Improvement Report\n\nNo issues found.\n"

    total = sum(len(r.issues) for r in results)
    parts = [
        "# Code Improvement Report\n",
        f"**Files with issues:** {len(results)}  ",
        f"**Total suggestions:** {total}\n",
    ]

    for res in results:
        parts.append(f"## `{res.file}`\n")
        for issue in res.issues:
            sev_badge = {"error": "**ERROR**", "warning": "WARNING", "info": "info"}.get(
                issue.severity, issue.severity
            )
            cat = CATEGORY_LABEL.get(issue.category, issue.category)
            parts.append(f"### Line {issue.line} — {cat} (`{issue.rule}`) [{sev_badge}]\n")
            parts.append(f"{issue.message}\n")
            if issue.current_code:
                parts.append("**Current code:**")
                parts.append(f"```r\n{issue.current_code}\n```\n")
            if issue.improved_code:
                parts.append("**Suggested improvement:**")
                parts.append(f"```r\n{issue.improved_code}\n```\n")
            parts.append("---\n")

    # Summary
    cats: dict[str, int] = {}
    sevs: dict[str, int] = {}
    for res in results:
        for iss in res.issues:
            cats[iss.category] = cats.get(iss.category, 0) + 1
            sevs[iss.severity] = sevs.get(iss.severity, 0) + 1

    parts.append("## Summary\n")
    parts.append("| Category | Count |")
    parts.append("|----------|-------|")
    for cat, count in sorted(cats.items()):
        parts.append(f"| {CATEGORY_LABEL.get(cat, cat)} | {count} |")
    parts.append("")
    parts.append("| Severity | Count |")
    parts.append("|----------|-------|")
    for sev, count in sorted(sevs.items()):
        parts.append(f"| {sev} | {count} |")

    return "\n".join(parts)


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Code Improvement Agent — scans files for readability, "
                    "performance, and best-practice issues.",
    )
    parser.add_argument(
        "target",
        help="File or directory to scan.",
    )
    parser.add_argument(
        "--ext",
        nargs="+",
        default=[".R"],
        help="File extensions to include (default: .R).",
    )
    parser.add_argument(
        "--output", "-o",
        default=None,
        help="Write Markdown report to this file.",
    )
    parser.add_argument(
        "--format", "-f",
        choices=["text", "markdown"],
        default=None,
        help="Output format (default: text for terminal, markdown for --output).",
    )
    args = parser.parse_args()

    target = os.path.abspath(args.target)

    if os.path.isfile(target):
        results = [scan_file(target)]
        results = [r for r in results if r.issues]
    elif os.path.isdir(target):
        results = scan_directory(target, extensions=args.ext)
    else:
        print(f"Error: '{args.target}' is not a valid file or directory.", file=sys.stderr)
        sys.exit(1)

    fmt = args.format
    if fmt is None:
        fmt = "markdown" if args.output else "text"

    if fmt == "markdown":
        report = format_results_markdown(results)
    else:
        report = format_results_text(results)

    if args.output:
        out_path = os.path.abspath(args.output)
        os.makedirs(os.path.dirname(out_path), exist_ok=True)
        with open(out_path, "w", encoding="utf-8") as f:
            f.write(report)
        total = sum(len(r.issues) for r in results)
        print(f"Report written to {out_path} ({total} suggestion(s))")
    else:
        print(report)

    sys.exit(0)


if __name__ == "__main__":
    main()
