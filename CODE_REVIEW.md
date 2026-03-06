# Code Review: ADS Dev Scripts

**Date:** 2026-03-06
**Reviewer:** Claude (automated)
**Scope:** All R scripts in `case_template/scripts/` and `master_scripts/`

---

## Executive Summary

The codebase is a well-structured California wage & hour litigation analysis pipeline covering time data cleaning, payroll analysis, meal/rest period violations, overtime calculations, damages modeling, a Shiny dashboard, and PDF report generation. The domain logic is thorough and clearly reflects deep subject-matter expertise.

However, there are several **critical bugs**, significant **code duplication**, **security concerns**, and **maintainability issues** that should be addressed. Below are findings organized by file, with severity ratings.

---

## 1. `master_scripts/functions.R` (~3,100 lines)

### Critical Bugs

| # | Issue | Location | Impact |
|---|-------|----------|--------|
| 1 | **Undefined variable `class1_population`** -- should be `class_population` | ~line 2326 in `generate_random_sample()` | Runtime crash when `use_class1 = TRUE` |
| 2 | **`export_metrics()` ignores its `out_dir` parameter** -- immediately overwrites with `out_dir <- OUT_DIR` | ~line 3109 | User-supplied argument silently discarded; errors if `OUT_DIR` not in global env |
| 3 | **`generate_random_sample()` uses exact string match `Source == "Time Data; Pay Data"`** -- misses employees in all three sources (`"Time Data; Pay Data; Class Data"`) | ~line 2261 | Employees in all 3 data sources excluded from `both_time_pay` count |

### Moderate Bugs

| # | Issue | Location |
|---|-------|----------|
| 4 | `sink()` leak in `finalize_logging()` -- no `on.exit(sink())` safety | lines 101-104 |
| 5 | `transpose_pay_data()` assumes `cols_to_keep` has 2+ elements -- will index OOB with 1 | ~line 396 |
| 6 | `safe_left_join()` returns `NULL` on error -- downstream code expects a data.table | ~line 296 |
| 7 | Redundant identity: `if(is.character(x)) as.numeric(x) else as.numeric(x)` -- both branches identical | line 331 |

### Code Quality

- **Massive duplication**: `fmt_pct_display()` is defined 5 separate times across `shift_hrs_tbl()`, `non_wrk_hrs_tbl()`, `meal_period_tbl()`, `meal_start_time_tbl()`, `meal_quarter_hour_tbl()`. The output directory resolution pattern (~8 lines) is repeated 8+ times.
- **Implicit global dependencies**: `aggregate_data()` falls back to global variables like `first_fields_default`, `sum_fields_default` when args are NULL.
- **Hardcoded path**: `source("D:/Shared/Master_Scripts/path_guard.R")` on line 8 -- fails on non-Windows systems and is a security risk if the shared path is writable by others.
- **`setDT()` side effects**: `categorize_pay_codes()` and `employee_period_comparison()` mutate caller's data in-place via `setDT()` on function arguments.
- **`library()` inside functions**: `all_time_pay_class_ids()` calls `library(data.table)` inside the function body -- bad practice, modifies global namespace.

### Security

- **Arbitrary code execution**: `eval_expr_safe()` / `eval_metric()` uses `eval(parse(text = expr_str))` on strings from metrics spec CSV. If the CSV is tampered with, arbitrary R code runs.
- **No file path sanitization**: Functions like `write_csv_and_rds()` construct paths from `case_name` -- `../` in name could write to unexpected locations.

---

## 2. `master_scripts/app.R` (~5,250 lines)

### Critical Bugs

| # | Issue | Impact |
|---|-------|--------|
| 1 | **Notes & Assumptions log section duplicated 8 times** (~lines 3047-4444). Only the last definition wins in Shiny. The last copy reads from a **different file path** (`analysis_log_summary.rds`) than the first (`Case_Log_summary.rds`). | ~1,400 lines of dead code; potentially reading wrong log file |
| 2 | **Second `output$table_example_damages`** (lines 4843-4897) references `matrix_data` from the overlap matrix table context -- will crash with "object 'matrix_data' not found" | Runtime error on the Example Damages tab |
| 3 | **Duplicate data aggregation inside `employee_coverage_plot`** (lines 2447-2490) -- `time_emp` and `pay_emp` computed twice, producing doubled data points in the plot | Incorrect visualization |

### Moderate Bugs

| # | Issue |
|---|-------|
| 4 | Duplicate `original_date_max` computation (lines 1585-1599) |
| 5 | Duplicate `all_employee_ids` initialization and `updateSelectizeInput` call (lines 1601-1615) |
| 6 | Department and Location filters applied twice in `observeEvent(input$apply_filters)` (lines 1685-1725) |
| 7 | `debounce`d filter reactives created (lines 1636-1638) but never used -- server reads raw inputs directly |
| 8 | CSS `font-size: !important` applied to h1-h5, making all headers same size as body text (line 1622) |

### Dead Code

- `under_construction_card()` -- defined but never called
- `combine_damages_with_headers()` -- replaced by pipeline system
- `filter_metrics_by_label()` -- unused
- `transpose_data_for_display()` -- defined but never called
- `extrap_factor` reactive -- always returns 1.0, never used
- First `employee_coverage_plot` definition (returns a string, not a plotly object)

### Code Quality

- At 5,254 lines, the file should be split into Shiny modules (`ui.R`, `server.R`, `global.R`, `utils.R`).
- `pay_date_col` is re-derived from scratch in 5+ places instead of being computed once.
- Filtering logic for 5 data sources across 5 filter categories (~300 lines) could be a single helper function.
- `list2env(metric_group_categories, envir = environment())` dumps variables into scope invisibly.

### Security

- `parse(text = expr_str)` / `eval()` on CSV-sourced expressions (same as functions.R).
- Unsanitized HTML output via `HTML()` with string interpolation of `case_name`, penalty amounts, etc. -- potential XSS if values contain HTML/JS.

---

## 3. `master_scripts/generate_pdf.R` (~818 lines)

### Issues

| # | Severity | Issue |
|---|----------|-------|
| 1 | **High** | Relies on undeclared global variables (`OUT_DIR`, `CASE_DIR`, `case_name`, `case_no`, etc.) without validation -- opaque "object not found" errors |
| 2 | **High** | HTML/CSS injection: `case_name` containing quotes breaks CSS `@page` content strings; no `htmlEscape()` on values injected into `<td>` elements |
| 3 | **Medium** | `safe_load_rds` duplicates the date-fix logic from `fix_date_columns` (lines 259-286 vs 138-155) -- should call `fix_date_columns` instead |
| 4 | **Medium** | `add_tbl` silently drops rows where all values are "0" or "$0.00" -- zero-value rows are meaningful in legal/financial contexts |
| 5 | **Medium** | No `on.exit()` for temp file cleanup -- if `chrome_print` fails, temp files leak |
| 6 | **Medium** | `chrome_print` requires Chrome but there's no pre-check or user-friendly error message |
| 7 | **Low** | `fread()` followed by redundant `setDT()` |
| 8 | **Low** | Entire HTML document built via `paste0` concatenation -- fragile, should use `htmltools` or templates |

---

## 4. `case_template/scripts/clean_data.R` (~1,060 lines)

### Issues

| # | Severity | Issue |
|---|----------|-------|
| 1 | **High** | Hardcoded paths: `CASE_DIR <- "D:/Cases/..."` and `ADS_SHARED <- "D:/Shared/Master_Scripts"` -- must be manually edited per case, breaks on non-Windows |
| 2 | **Medium** | `read_excel("")` on lines 150 and 331 -- empty string will error at runtime; these are placeholder templates but easy to miss |
| 3 | **Medium** | `standard_names` variable name reused 3 times (time, pay, class sections) -- fragile, could cause confusion if sections are reordered |
| 4 | **Medium** | Typo in pay data mapping: `"Pay Peirod End"` (line 389) -- will only match if the source data has the same typo |
| 5 | **Low** | `min_time_date` computation at line 854 has misplaced parenthesis: `min(as.Date(time1$Date, na.rm = TRUE))` -- `na.rm` is being passed to `as.Date()`, not `min()` |
| 6 | **Low** | `end.time` computed and `end.time - start.time` printed twice (lines 1053, 1060-1061) |
| 7 | **Low** | Duplicate check uses `REMOVE_DUPLICATES <- FALSE` as a flag but it's defined separately for time and pay sections -- should be a single config |

### Strengths

- Excellent data quality checks with clear console output
- Good defensive coding with column existence checks
- Pay calendar construction is clever and robust
- Well-structured commented sections make the workflow clear

---

## 5. `case_template/scripts/analysis.R` (~2,100 lines)

### Issues

| # | Severity | Issue |
|---|----------|-------|
| 1 | **High** | CA minimum wage table hardcoded through 2026 only (line 116-119) -- will silently fail to join for 2027+ data without error |
| 2 | **High** | `RROP_MIN`/`RROP_MAX`/`RROP_WARN` redefined at lines 747-749, overwriting values from `clean_data.R` config (lines 107-109) -- user's config is silently discarded |
| 3 | **Medium** | `Rate_Mult` computed twice identically (lines 446 and 452) |
| 4 | **Medium** | Relies on many undefined globals: `mode_days_btwn_pay_period_ends`, `most_common_period_end_weekday`, `separate_key_gps`, `rrop_buffer`, `rounding_hrs_cutoff`, `all_ids`, `OUT_DIR`, `PROCESSED_DIR` -- all must exist from `clean_data.R` |
| 5 | **Medium** | `Straight_Time_Amt` calculation (line 636) has operator precedence issue: `Reg_Pay_Code == 1 & Hrs_Wkd_Pay_Code == 1 | (Diff...)` -- `&` binds tighter than `|`, so the logic may not match intent without explicit parentheses |
| 6 | **Medium** | AWW detection flags `is_3_12_candidate` checks for `shifts_near_8hrs` (line 1478) instead of `shifts_near_12hrs` -- copy-paste error |
| 7 | **Low** | `var_half_time_OT_multiplier` and `var_half_time1_multiplier` re-checked with `exists()` at lines 697-698 after being defined at lines 671-672 |
| 8 | **Low** | Large amounts of commented-out code (~40% of file) -- should be removed or moved to separate feature-branch scripts |

### Strengths

- Sophisticated multi-layer overtime analysis (CA daily + FLSA weekly + 7th consecutive day)
- Thorough RROP (Regular Rate of Pay) analysis with distribution tracking
- Well-designed meal period violation detection with waiver scenarios
- Good use of data.table for performance

---

## 6. `master_scripts/path_guard.R` (5 lines)

### Issues

- Uses `grepl("Orig", root, ignore.case = TRUE)` which is very broad -- any path containing "orig" (e.g., "original_analysis", "origin") would trigger a false positive. Consider using a more specific pattern or checking for an exact directory name.

---

## Top Priority Fixes

1. **`functions.R` line ~2326**: Rename `class1_population` to `class_population` (crash bug)
2. **`app.R` lines 3047-4444**: Remove 7 duplicate copies of the Notes & Assumptions log section
3. **`app.R` lines 4843-4897**: Remove the second `output$table_example_damages` definition that references `matrix_data`
4. **`app.R` lines 2447-2490**: Fix duplicate data aggregation in `employee_coverage_plot`
5. **`functions.R` line ~3109**: Fix `export_metrics()` to use its `out_dir` parameter instead of overwriting with `OUT_DIR`
6. **`analysis.R` line 1478**: Fix `is_3_12_candidate` to use `shifts_near_12hrs` instead of `shifts_near_8hrs`
7. **`analysis.R` lines 747-749**: Remove duplicate `RROP_MIN`/`RROP_MAX`/`RROP_WARN` definitions that override user config
8. **`clean_data.R` line 854**: Fix `na.rm` parenthesis placement in `min_time_date`

## Recommendations

1. **Modularize `app.R`**: Split into Shiny modules (at minimum `ui.R` / `server.R` / `global.R`)
2. **Extract shared utilities in `functions.R`**: Create `resolve_output_dir()`, a single `fmt_pct_display()`, and a common `build_summary_table()` helper
3. **Replace `eval(parse(text=...))` with safer alternatives**: Use named function lists for metrics evaluation
4. **Make paths configurable**: Use environment variables or config files instead of hardcoded `D:/` paths
5. **Remove commented-out code**: ~40% of `analysis.R` is commented out -- move to feature branches or a separate reference file
6. **Add `on.exit()` handlers**: For `sink()` in logging and temp file cleanup in PDF generation
7. **HTML-escape all user data**: Use `htmltools::htmlEscape()` before injecting into HTML/CSS strings
