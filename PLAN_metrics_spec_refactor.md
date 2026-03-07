# Plan: Replace metrics_spec.csv with R-native Definition

## Problem

The 615-row `metrics_spec.csv` is painful to manage because:

1. **R expressions in CSV cells** -- You're writing `expr` and `extrap_expr` as quoted strings inside CSV cells. CSV escaping rules fight you (commas in expressions need quoting, nested quotes break, etc.)
2. **No syntax checking** -- A typo in an `expr` cell silently passes until runtime, when `eval_metric()` fails mid-pipeline
3. **Hard to add/remove rows** -- Inserting a new metric group or reordering rows in a spreadsheet is error-prone with 11 columns
4. **Repetitive patterns** -- The "Late Detail" and "Short Detail" rows repeat the same pattern 10+ times with only a bin range changing (1-2, 2-3, 3-4, ..., 20+)
5. **No code completion** -- Editing R expressions in a CSV editor gives no autocomplete or linting

## Proposed Solution

Replace `metrics_spec.csv` with an R script (`metrics_spec.R`) that **builds** the same `data.table` programmatically using helper functions. The script is `source()`d instead of `fread()`d.

### What changes

| Component | Before | After |
|-----------|--------|-------|
| Definition file | `scripts/metrics_spec.csv` | `scripts/metrics_spec.R` |
| Load mechanism | `fread("metrics_spec.csv")` | `source("metrics_spec.R")` → returns `metrics_spec` |
| Editing | Spreadsheet / text editor | R script with syntax highlighting & linting |
| Adding rows | Edit CSV manually | Call `add_metric()` or `add_bin_range()` |
| Testing | Run full pipeline, hope it works | `source("metrics_spec.R")` in console to validate |

### What stays the same

- The resulting `metrics_spec` data.table has the **exact same columns** (`metric_group`, `scenario`, `meal_rest_prems_credit`, `other_credit`, `metric_label`, `source`, `expr`, `extrap_expr`, `digits`, `denom`, `no_year_breakdown`)
- `calculate_metrics()`, `run_metrics_pipeline()`, `format_metrics_table()`, `export_metrics()` -- **zero changes**
- `analysis.R` loading section changes from 2 lines (`fread` + `setDT`) to 1 line (`source`)

## Design

### Helper functions (defined at top of `metrics_spec.R`)

```r
# Core row builder -- returns a 1-row data.table
m <- function(group, label, src, expr, extrap = NA_character_,
              digits = 0L, denom = NA_character_,
              scenario = "all",
              prems_credit = FALSE, other_credit = FALSE,
              no_year = FALSE) {
  data.table(
    metric_group            = group,
    scenario                = scenario,
    meal_rest_prems_credit  = prems_credit,
    other_credit            = other_credit,
    metric_label            = label,
    source                  = src,
    expr                    = expr,
    extrap_expr             = extrap,
    digits                  = digits,
    denom                   = denom,
    no_year_breakdown       = no_year
  )
}

# Bin range generator -- produces N rows for minute-range breakdowns
# e.g., bin_range_metrics("Late Detail", "mp1_mins_late", c(1:10, 15, 20), ...)
bin_range_metrics <- function(group, field, breaks, label_template,
                              filter_col, src = "shift_data1",
                              scenario = "all", denom = NA_character_,
                              denom_shift_expr = NULL, extrap_base = NULL, ...) {
  # Generates rows like "Meal periods 1-2 mins late", "2-3 mins late", etc.
  # plus a final "20+ mins" row
  # Returns rbindlist of m() calls
}
```

### Example: current CSV vs proposed R

**Current CSV (10 repetitive rows for late meal detail bins):**
```
Meal Period Violations - Late Detail,no waivers,...,Meal periods 1-2 mins late,...,"sum(LateMP1==1 & mp1_mins_late>=1 & mp1_mins_late<2, na.rm=TRUE)","sum(LateMP1==1 ...long extrap...",0,shifts_gt_5_late_meals,FALSE
Meal Period Violations - Late Detail,no waivers,...,Meal periods 2-3 mins late,...,"sum(LateMP1==1 & mp1_mins_late>=2 & mp1_mins_late<3, na.rm=TRUE)","sum(LateMP1==1 ...long extrap...",0,shifts_gt_5_late_meals,FALSE
... (8 more nearly identical rows)
```

**Proposed R (1 call):**
```r
bin_range_metrics(
  group     = "Meal Period Violations - Late Detail",
  scenario  = "no waivers",
  flag_col  = "LateMP1",
  field     = "mp1_mins_late",
  breaks    = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20),
  label_fmt = "Meal periods %s mins late",
  src       = "shift_data1",
  denom     = "shifts_gt_5_late_meals",
  shift_threshold = 5
)
```

10 CSV rows → 1 function call. And the expressions are generated correctly every time.

### Example: simple summary metrics

**Current CSV:**
```
Summary - Time Data,all,FALSE,FALSE,Begin date,shift_data1,"min(Date, na.rm=TRUE)",class_dmgs_start_date,,,FALSE
Summary - Time Data,all,FALSE,FALSE,End date,shift_data1,"max(Date, na.rm=TRUE)",mediation_date,,,FALSE
Summary - Time Data,all,FALSE,FALSE,Employees,shift_data1,"uniqueN(ID, na.rm=TRUE)",extrap_class_ees,0,,FALSE
```

**Proposed R:**
```r
grp <- "Summary - Time Data"

rbindlist(list(
  m(grp, "Begin date",  "shift_data1", "min(Date, na.rm=TRUE)",      extrap = "class_dmgs_start_date"),
  m(grp, "End date",    "shift_data1", "max(Date, na.rm=TRUE)",      extrap = "mediation_date"),
  m(grp, "Employees",   "shift_data1", "uniqueN(ID, na.rm=TRUE)",    extrap = "extrap_class_ees")
))
```

Same thing, but now you get: syntax highlighting, autocomplete on column names, and an immediate error if you have a missing comma or unmatched paren.

## Implementation Steps

### Step 1: Create `metrics_spec.R` with helpers + all 615 rows converted

- Define `m()` and `bin_range_metrics()` helper functions at top
- Convert each section of the CSV into R calls, collapsing repetitive bin ranges
- The script ends with `metrics_spec <- rbindlist(list(...all sections...))`
- Estimated reduction: 615 CSV rows → ~150-200 lines of R

### Step 2: Add a validation check at end of `metrics_spec.R`

```r
# Self-test: catch typos before they reach the pipeline
stopifnot(
  is.data.table(metrics_spec),
  nrow(metrics_spec) > 0,
  all(c("metric_group","scenario","metric_label","source","expr") %in% names(metrics_spec)),
  !anyNA(metrics_spec$expr),
  all(metrics_spec$source %in% c("shift_data1", "pay1", "pp_data1", "ee_data1"))
)
cat(sprintf("metrics_spec: %d rows, %d groups loaded OK\n",
            nrow(metrics_spec), uniqueN(metrics_spec$metric_group)))
```

### Step 3: Update `analysis.R` loading section (~3 lines change)

Replace:
```r
metrics_spec_path <- file.path(CASE_DIR, "scripts", "metrics_spec.csv")
if (!file.exists(metrics_spec_path)) stop("Missing metrics_spec.csv at: ", metrics_spec_path)
metrics_spec <- fread(metrics_spec_path)
setDT(metrics_spec)
```

With:
```r
metrics_spec_path <- file.path(CASE_DIR, "scripts", "metrics_spec.R")
if (!file.exists(metrics_spec_path)) stop("Missing metrics_spec.R at: ", metrics_spec_path)
source(metrics_spec_path, local = TRUE)
```

### Step 4: Keep CSV as frozen backup

- Rename `metrics_spec.csv` → `metrics_spec_ARCHIVED.csv`
- Keep it in the repo so you can cross-reference during transition
- Remove after you've validated a few cases

## Trainability Benefits

| Task | CSV workflow | R workflow |
|------|-------------|------------|
| Add a new metric | Open CSV, carefully type expr in a cell, hope quoting is right | Add one `m()` call, get syntax errors immediately if wrong |
| Add a new bin range (e.g., "10-11 mins") | Copy a row, change 2 numbers in 2 long expressions | Add one number to the `breaks` vector |
| Remove a metric group | Find and delete rows, hope you got them all | Delete or comment out one `rbindlist` block |
| Test syntax | Run entire pipeline | `source("metrics_spec.R")` in R console -- instant validation |
| Review changes | Diff a CSV (unreadable) | Diff an R script (clean, line-by-line) |
| Train new analyst | "Edit the CSV but don't break the quoting" | "Add a line like the ones above it" |

## Risk & Rollback

- **Zero risk to downstream functions** -- the output data.table is identical
- **Rollback** -- just point back to the CSV: change `source()` back to `fread()`
- **Validation** -- after building `metrics_spec.R`, compare `nrow`, column names, and a sample of `expr` values against the original CSV to confirm parity
