# ==============================================================================
# PROPRIETARY AND CONFIDENTIAL
# Anello Data Solutions LLC
#
# This file contains proprietary information and trade secrets.
# Unauthorized copying, distribution, or use is strictly prohibited.
# For authorized use by ANELLO DATA SOLUTIONS LLC contracted analysts only.
# ==============================================================================

# ---- Wage & Hour Compliance Dashboard ----

library(shiny)
library(bslib)
library(data.table)
library(lubridate)
library(DT)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(magrittr)  # for %>% pipe operator
library(digest)    # for cache key hashing
library(openxlsx)  # for Excel export

# ---- CHROME PATH FOR PDF EXPORT ----
# Set Chrome path for pagedown PDF export (required for PDF generation)

# Helper for path concatenation (handles empty strings)
path_join <- function(a, b) if (nzchar(a)) file.path(a, b) else ""

# Common Chrome paths - adjust if Chrome is installed elsewhere
local_app_data <- Sys.getenv("LOCALAPPDATA", "")
chrome_paths <- c(
  "C:/Program Files/Google/Chrome/Application/chrome.exe",
  "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
  path_join(local_app_data, "Google/Chrome/Application/chrome.exe"),
  "/usr/bin/google-chrome",
  "/usr/bin/chromium-browser",
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
)

# Find and set Chrome path
for (path in chrome_paths) {
  if (nzchar(path) && file.exists(path)) {
    Sys.setenv(CHROME = path)
    message("[OK] Chrome found at: ", path)
    break
  }
}

# If CHROME env var not set, try to find it
if (!nzchar(Sys.getenv("CHROME", ""))) {
  # Try pagedown's find_chrome as fallback
  if (requireNamespace("pagedown", quietly = TRUE)) {
    chrome_found <- tryCatch(pagedown::find_chrome(), error = function(e) "")
    if (nzchar(chrome_found)) {
      Sys.setenv(CHROME = chrome_found)
      message("[OK] Chrome found via pagedown: ", chrome_found)
    } else {
      message("[WARN] Chrome not found - PDF export may not work. Set CHROME env var manually.")
    }
  }
}


# ---- DASHBOARD INPUT FILE NAMES ----
# These must match what analysis.R actually writes to OUT_DIR

SHIFT_DATA_FILE <- "Time Shift Data.rds"
PAY_DATA_FILE   <- "Pay Data.rds"
TIME_DATA_FILE  <- "Time Punch Data.rds"
PP_DATA_FILE    <- "Pay Period Level Data.rds"
EE_DATA_FILE    <- "Employee Level Data.rds"

# class1 is in PROCESSED_DIR (from clean_data.R), not OUT_DIR
CLASS_DATA_FILE <- "class_processed.rds"

# Tables produced by analysis.R (RDS preferred)
SHIFT_HRS_FILE          <- "Shift_Hrs_Table.rds"
NON_WRK_HRS_FILE        <- "Non_Work_Hrs_Table.rds"
MEAL_PERIOD_FILE        <- "Meal_Period_Table.rds"
MEAL_START_TIME_FILE    <- "Meal_Start_Time_Table.rds"
MEAL_QUARTER_HR_FILE    <- "Meal_Quarter_Hour_Table.rds"
PAY_CODE_SUMMARY_FILE   <- "Pay_Code_Summary.rds"
RATE_TYPE_ANALYSIS_FILE <- "Rate_Type_Analysis.rds"
EMPLOYEE_COMPARISON_FILE <- "Employee Pay Period Comparison.rds"
CASE_LOG_FILE <- "Case_Log.txt"
CASE_LOG_SUMMARY_FILE <- "Case_Log_summary.rds"

# ---- CASE METADATA (optional - safe defaults) ----

if (!exists("case_name"))       case_name <- "Wage & Hour Analysis"
if (!exists("case_no"))         case_no <- "Not specified"
if (!exists("sample_size"))     sample_size <- "Not specified"
if (!exists("date_filed"))      date_filed <- Sys.Date()
if (!exists("complaint_date"))  complaint_date <- Sys.Date()
if (!exists("mediation_date"))  mediation_date <- Sys.Date()
if (!exists("class_dmgs_start_date")) class_dmgs_start_date <- Sys.Date() %m-% years(4)

get_case_version <- function(default = NULL) {
  pick_version <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    v <- trimws(as.character(x[[1]]))
    if (!nzchar(v)) return(NA_character_)
    v
  }

  if (exists("version", inherits = TRUE)) {
    v <- pick_version(get("version", inherits = TRUE))
    if (!is.na(v)) return(v)
  }

  if (exists("case_version", inherits = TRUE)) {
    v <- pick_version(get("case_version", inherits = TRUE))
    if (!is.na(v)) return(v)
  }

  if (exists("OUT_DIR", inherits = TRUE)) {
    out_dir_val <- tryCatch(get("OUT_DIR", inherits = TRUE), error = function(e) NA_character_)
    if (!is.na(out_dir_val) && nzchar(out_dir_val)) {
      version_file <- file.path(out_dir_val, "version.txt")
      if (file.exists(version_file)) {
        v <- tryCatch(readLines(version_file, n = 1, warn = FALSE), error = function(e) "")
        v <- pick_version(v)
        if (!is.na(v)) return(v)
      }
    }
  }

  if (!is.null(default) && nzchar(trimws(as.character(default)))) {
    trimws(as.character(default))
  } else {
    "N/A"
  }
}

get_case_version_label <- function(default = NULL) {
  v <- get_case_version(default = default)
  if (is.na(v) || !nzchar(v) || toupper(v) == "N/A") {
    "N/A"
  } else if (grepl("^v", v, ignore.case = TRUE)) {
    v
  } else {
    paste0("v", v)
  }
}

# ---- UTILITY FUNCTIONS ----

# Format column names: underscore -> space; proper case
format_col_name <- function(name) {
  name <- gsub("_", " ", name, fixed = TRUE)
  tools::toTitleCase(name)
}

format_all_cols <- function(dt) {
  if (!is.null(dt) && ncol(dt) > 0) {
    setnames(dt, names(dt), vapply(names(dt), format_col_name, character(1)))
  }
  dt
}

# ---- DATA LOADING ----

load_data <- function() {

  read_if_exists <- function(dir, file) {
    path <- file.path(dir, file)
    if (file.exists(path)) {
      cat(sprintf("[APP] Loading %s ... ", file))
      flush.console()
      obj <- readRDS(path)
      if (is.data.frame(obj) || data.table::is.data.table(obj)) {
        cat(sprintf("done (%s rows)\n", format(nrow(obj), big.mark = ",")))
      } else {
        cat("done\n")
      }
      flush.console()
      obj
    } else {
      cat(sprintf("[APP] Not found (skipping): %s\n", file))
      flush.console()
      NULL
    }
  }

  shift_data1 <- read_if_exists(OUT_DIR, SHIFT_DATA_FILE)
  pay1 <- read_if_exists(OUT_DIR, PAY_DATA_FILE)
  time1 <- read_if_exists(OUT_DIR, TIME_DATA_FILE)
  class1 <- read_if_exists(PROCESSED_DIR, CLASS_DATA_FILE)
  pp_data1 <- read_if_exists(OUT_DIR, PP_DATA_FILE)
  ee_data1 <- read_if_exists(OUT_DIR, EE_DATA_FILE)

  # Set primary keys for fast filtering
  if (!is.null(shift_data1) && all(c("Date", "ID") %in% names(shift_data1))) {
    setkeyv(shift_data1, c("Date", "ID"))

    # Pre-compute year column for faster year-based filtering
    if ("Date" %in% names(shift_data1) && !"Year" %in% names(shift_data1)) {
      shift_data1[, Year := data.table::year(Date)]
    }

    # Add secondary indices for commonly filtered columns
    if ("Sample" %in% names(shift_data1)) setindex(shift_data1, Sample)
    if ("Key_Gps" %in% names(shift_data1)) setindex(shift_data1, Key_Gps)
    if ("Subclass" %in% names(shift_data1)) setindex(shift_data1, Subclass)
    if ("ID_Period_End" %in% names(shift_data1)) setindex(shift_data1, ID_Period_End)
    if ("ID_Week_End" %in% names(shift_data1)) setindex(shift_data1, ID_Week_End)
    if ("Year" %in% names(shift_data1)) setindex(shift_data1, Year)
  }

  if (!is.null(pay1)) {
    pay_date_col <- if ("Pay_Period_End" %in% names(pay1)) {
      "Pay_Period_End"
    } else if ("Pay_Date" %in% names(pay1)) {
      "Pay_Date"
    } else {
      NULL
    }
    if (!is.null(pay_date_col) && "Pay_ID" %in% names(pay1)) {
      setkeyv(pay1, c(pay_date_col, "Pay_ID"))

      # Pre-compute year column for faster year-based filtering
      if (!is.null(pay_date_col) && !"Pay_Year" %in% names(pay1)) {
        pay1[, Pay_Year := data.table::year(get(pay_date_col))]
      }

      # Add secondary indices
      if ("Pay_Sample" %in% names(pay1)) setindex(pay1, Pay_Sample)
      if ("Pay_Key_Gps" %in% names(pay1)) setindex(pay1, Pay_Key_Gps)
      if ("Pay_Subclass" %in% names(pay1)) setindex(pay1, Pay_Subclass)
      if ("Subclass" %in% names(pay1)) setindex(pay1, Subclass)
      if ("Pay_ID_Period_End" %in% names(pay1)) setindex(pay1, Pay_ID_Period_End)
      if ("Pay_Year" %in% names(pay1)) setindex(pay1, Pay_Year)
    }
  }

  if (!is.null(pp_data1) && all(c("Period_End", "ID") %in% names(pp_data1))) {
    setkeyv(pp_data1, c("Period_End", "ID"))
    if ("ID_Period_End" %in% names(pp_data1)) setindex(pp_data1, ID_Period_End)
  }

  if (!is.null(ee_data1) && "ID" %in% names(ee_data1)) {
    setkeyv(ee_data1, "ID")
  }

  if (!is.null(class1) && "Class_ID" %in% names(class1)) {
    setkeyv(class1, "Class_ID")

    # Add secondary indices
    if ("Sample" %in% names(class1)) setindex(class1, Sample)
    if ("Class_Key_Gps" %in% names(class1)) setindex(class1, Class_Key_Gps)
    if ("Subclass" %in% names(class1)) setindex(class1, Subclass)
  }

  list(
    shift_data1 = shift_data1,   # required
    pay1        = pay1,          # required
    time1       = time1,
    class1      = class1,
    pp_data1    = pp_data1,
    ee_data1    = ee_data1
  )
}

load_metric_spec <- function() {
  metrics_spec_path <- file.path(CASE_DIR, "scripts", "metrics_spec.csv")
  if (!file.exists(metrics_spec_path)) stop("Missing metrics_spec.csv at: ", metrics_spec_path)

  cat("[APP] Loading metrics_spec.csv ... ")
  flush.console()
  spec <- fread(metrics_spec_path)
  cat(sprintf("done (%s rows)\n", format(nrow(spec), big.mark = ",")))
  flush.console()
  setDT(spec)

  if (!"metric_order" %in% names(spec)) {
    spec[, metric_order := .I]
  }

  spec[, metric_type := fcase(
    grepl("date", metric_label, ignore.case = TRUE), "date",
    grepl("percent", metric_label, ignore.case = TRUE), "percent",
    default = "value"
  )]

  spec
}
load_analysis_table <- function(filename) {

  path <- file.path(OUT_DIR, filename)
  if (!file.exists(path)) {
    cat(sprintf("[APP] Analysis table missing (skipping): %s\n", filename))
    flush.console()
    return(NULL)
  }

  cat(sprintf("[APP] Loading analysis table %s ... ", filename))
  flush.console()

  # Use readRDS for .rds files, fread for .csv
  dt <- if (grepl("\\.rds$", filename, ignore.case = TRUE)) {
    readRDS(path)
  } else {
    fread(path)
  }

  if (!is.data.table(dt)) setDT(dt)

  if (nrow(dt) > 0) {
    first_col <- names(dt)[1]
    dt <- dt[!(is.na(get(first_col)) |
                 get(first_col) == "" |
                 get(first_col) == "0")]
  }

  cat(sprintf("done (%s rows)\n", format(nrow(dt), big.mark = ",")))
  flush.console()
  dt
}

# ---- METRIC CALCULATION (OPTIMIZED) ----

get_denominator_value <- function(data, denom_name, source_type) {
  if (is.na(denom_name) || denom_name == "") return(NA_real_)

  result <- tryCatch({
    switch(denom_name,
           "shifts_all" = data[, .N],
           "shifts_gt_3_5" = data[shift_hrs > 3.5, .N],
           "shifts_gt_5" = data[shift_hrs > 5, .N],
           "shifts_gt_6" = data[shift_hrs > 6, .N],
           "shifts_gt_10" = data[shift_hrs > 10, .N],
           "shifts_gt_12" = data[shift_hrs > 12, .N],
           "shifts_gt_5_late_meals" = data[shift_hrs > 5 & LateMP1 == 1, .N],
           "shifts_gt_6_late_meals" = data[shift_hrs > 6 & LateMP1_w == 1, .N],
           "shifts_gt_5_short_meals" = data[shift_hrs > 5 & ShortMP1 == 1, .N],
           "shifts_gt_6_short_meals" = data[shift_hrs > 6 & ShortMP1_w == 1, .N],
           "meal_periods" = data[, sum(shift_mps, na.rm = TRUE)],
           "auto_meal_periods" = data[, sum(!is.na(auto_mp), na.rm = TRUE)],
           "rest_periods" = data[, sum(shift_rps, na.rm = TRUE)],
           "employees" = data[, uniqueN(ID)],
           "weeks" = data[, uniqueN(ID_Week_End)],
           "pay_periods" = data[, uniqueN(ID_Period_End)],
           "analyzed_shifts_round" = data[, sum(shifts_analyzed, na.rm = TRUE)],
           "r_analyzed_shifts_round" = data[, sum(r_shifts_analyzed, na.rm = TRUE)],
           "employees_pay" = data[, uniqueN(Pay_ID)],
           "pay_periods_pay" = data[, uniqueN(Pay_ID_Period_End)],
           NA_real_
    )
  }, error = function(e) NA_real_)

  return(result)
}

transform_expr <- function(expr_str) {
  if (is.na(expr_str) || expr_str == "") return(expr_str)
  expr_str <- gsub("n_distinct\\(", "uniqueN(", expr_str)
  expr_str <- gsub("\\bn\\(\\)", ".N", expr_str)
  expr_str
}

evaluate_metric <- function(data, expr_str, digits = NA) {
  if (is.na(expr_str) || expr_str == "") return(NA)
  if (nrow(data) == 0) return(NA)

  expr_str <- transform_expr(expr_str)

  result <- tryCatch({
    expr <- parse(text = expr_str)
    val <- data[, eval(expr)]
    if (!is.na(digits) && is.numeric(val)) {
      val <- round(val, digits)
    }
    val
  }, error = function(e) {
    NA
  })

  return(result)
}

calculate_single_metric <- function(data, expr_str, digits, denom_name) {
  value <- evaluate_metric(data, expr_str, digits)
  denom_value <- get_denominator_value(data, denom_name, "")

  pct <- if (!is.na(denom_value) && denom_value > 0 && is.numeric(value) && !is.na(value)) {
    round(value / denom_value * 100, 2)
  } else {
    NA_real_
  }

  list(value = value, pct = pct)
}

calculate_group_metrics <- function(data_list, spec, group_names, filters = list(), extrapolation_factor = 1.0) {
  # Allow multiple groups to be calculated at once
  if (length(group_names) == 0) return(data.table())

  all_results <- lapply(group_names, function(group_name) {
    group_spec <- spec[metric_group == group_name]
    if (nrow(group_spec) == 0) return(NULL)

    first_source <- group_spec$source[1]
    is_pay_group <- grepl("pay", first_source, ignore.case = TRUE)

    if (is_pay_group) {
      years <- data_list$pay_years
      key_groups <- data_list$pay_key_groups
    } else {
      years <- data_list$shift_years
      key_groups <- data_list$shift_key_groups
    }

    results <- lapply(seq_len(nrow(group_spec)), function(i) {
      row <- group_spec[i]
      source <- row$source

      # Dynamically determine source data based on source name
      src_data <- data_list[[source]]

      # If source doesn't exist, return placeholder
      if (is.null(src_data) || nrow(src_data) == 0) {
        result_row <- data.table(Metric = row$metric_label, All_Data = "-")
        return(result_row)
      }

      # Determine date and key columns based on source type
      if (source == "pay1") {
        src_key_col <- "Pay_Key_Gps"
        src_date_col <- "Pay_Period_End"
        src_years <- data_list$pay_years
        src_key_groups <- data_list$pay_key_groups
      } else if (source == "shift_data1") {
        src_key_col <- "Key_Gps"
        src_date_col <- "Date"
        src_years <- data_list$shift_years
        src_key_groups <- data_list$shift_key_groups
      } else if (source == "pp_data1") {
        src_key_col <- "Key_Gps"  # Adjust based on actual column names
        src_date_col <- "Period_End"  # Adjust based on actual column names
        src_years <- data_list$shift_years  # Can use either shift or pay years
        src_key_groups <- data_list$shift_key_groups
      } else if (source == "ee_data1") {
        src_key_col <- "Key_Gps"  # Adjust based on actual column names
        src_date_col <- NULL  # Employee level may not have dates
        src_years <- NULL
        src_key_groups <- data_list$shift_key_groups
      } else {
        # Default fallback
        src_key_col <- NULL
        src_date_col <- NULL
        src_years <- NULL
        src_key_groups <- NULL
      }

      all_data_result <- calculate_single_metric(src_data, row$expr, row$digits, row$denom)

      result_row <- data.table(
        Metric = row$metric_label,
        All_Data = format_metric_value(all_data_result$value, all_data_result$pct)
      )

      # Add year columns
      if (!is.null(src_years) && length(src_years) > 0 && !is.null(src_date_col) && src_date_col %in% names(src_data)) {
        for (yr in src_years) {
          yr_data <- src_data[year(get(src_date_col)) == yr]
          yr_result <- calculate_single_metric(yr_data, row$expr, row$digits, row$denom)
          result_row[, (as.character(yr)) := format_metric_value(yr_result$value, yr_result$pct)]
        }
      }

      # Add key group columns
      if (!is.null(src_key_groups) && length(src_key_groups) > 0 && !is.null(src_key_col) && src_key_col %in% names(src_data)) {
        for (gp in src_key_groups) {
          gp_data <- src_data[get(src_key_col) == gp]
          gp_result <- calculate_single_metric(gp_data, row$expr, row$digits, row$denom)
          result_row[, (gp) := format_metric_value(gp_result$value, gp_result$pct)]
        }
      }

      # Add extrapolated column if factor > 1
      if (extrapolation_factor > 1.0) {
        extrap_value <- if (is.numeric(all_data_result$value) && !is.na(all_data_result$value)) {
          all_data_result$value * extrapolation_factor
        } else {
          NA
        }
        result_row[, Extrapolated := format_metric_value(extrap_value, NA)]
      }

      result_row
    })

    rbindlist(results, fill = TRUE)
  })

  # Combine all groups
  rbindlist(Filter(Negate(is.null), all_results), fill = TRUE)
}

# NEW PIPELINE-BASED CALCULATION SYSTEM
# Uses run_metrics_pipeline() from functions.R with caching and filtering

#' Convert pipeline results to display format with metric groups
#' @param pipeline_results Raw output from run_metrics_pipeline()
#' @param group_names Vector of metric_group names to include
#' @param include_years Logical - whether to include year columns (TRUE for most, FALSE for Damages/PAGA)
#' @return data.table formatted for display with Metric column
pipeline_to_display_format <- function(pipeline_results, group_names = NULL, include_years = TRUE, scenario_filter = NULL) {
  if (is.null(pipeline_results) || nrow(pipeline_results) == 0) return(data.table())

  # Filter by metric groups if specified
  dt <- if (!is.null(group_names) && length(group_names) > 0) {
    pipeline_results[metric_group %in% group_names]
  } else {
    copy(pipeline_results)
  }

  if (nrow(dt) == 0) return(data.table())

  # Filter by scenario if specified and scenario column exists.
  # Include "all" rows in every filtered view.
  if (!is.null(scenario_filter) && "scenario" %in% names(dt)) {
    sc <- normalize_scenario_value(dt$scenario)
    sf <- unique(normalize_scenario_value(scenario_filter))
    sf <- sf[!is.na(sf) & nzchar(sf)]
    if (length(sf) > 0) {
      dt <- dt[sc %in% c(sf, "all") | is.na(dt$scenario) | is.na(sc)]
    }
  }

  if (nrow(dt) == 0) return(data.table())

  # Format the results table
  formatted <- format_metrics_table(dt)

  # Rename metric_label to Metric for display
  if ("metric_label" %in% names(formatted)) {
    setnames(formatted, "metric_label", "Metric")
  }

  # Remove metric_group and scenario columns (used for filtering, not display)
  if ("metric_group" %in% names(formatted)) {
    formatted[, metric_group := NULL]
  }
  if ("scenario" %in% names(formatted)) {
    formatted[, scenario := NULL]
  }

  # Remove credit flag columns (metadata, not for display)
  credit_cols <- intersect(c("meal_rest_prems_credit", "other_credit"), names(formatted))
  if (length(credit_cols) > 0) formatted[, (credit_cols) := NULL]

  # If not including years, remove year columns
  if (!include_years) {
    year_cols <- names(formatted)[grepl("^\\d{4}$", names(formatted))]
    if (length(year_cols) > 0) {
      formatted[, (year_cols) := NULL]
    }
  }

  formatted
}

#' Format damages tables with section headers
#' @param pipeline_results Raw output from run_metrics_pipeline()
#' @param section_definitions List of lists with section_name and groups
#' @param scenario_filter Optional: scenario values to include (uses spec column if available)
#' @return data.table formatted for display with section headers
pipeline_to_damages_format <- function(pipeline_results, section_definitions, scenario_filter = NULL) {
  if (is.null(pipeline_results) || nrow(pipeline_results) == 0) return(data.table())

  all_sections <- lapply(section_definitions, function(def) {
    section_name <- def$section_name
    groups <- def$groups

    if (length(groups) == 0) return(NULL)

    # Filter by metric groups
    dt <- pipeline_results[metric_group %in% groups]

    # If scenario column exists in spec, filter by it.
    # Include "all" scenario rows in every filtered view.
    if (!is.null(scenario_filter) && "scenario" %in% names(dt)) {
      sc <- normalize_scenario_value(dt$scenario)
      sf <- unique(normalize_scenario_value(scenario_filter))
      sf <- sf[!is.na(sf) & nzchar(sf)]
      if (length(sf) > 0) {
        dt <- dt[sc %in% c(sf, "all") | is.na(dt$scenario) | is.na(sc)]
      }
    }

    if (nrow(dt) == 0) return(NULL)

    # Format the results (no years for damages)
    formatted <- format_metrics_table(dt)

    # Rename metric_label to Metric
    if ("metric_label" %in% names(formatted)) {
      setnames(formatted, "metric_label", "Metric")
    }

    # Remove year columns (damages don't have year breakdown)
    year_cols <- names(formatted)[grepl("^\\d{4}$", names(formatted))]
    if (length(year_cols) > 0) {
      formatted[, (year_cols) := NULL]
    }

    # Remove metric_group and scenario columns
    if ("metric_group" %in% names(formatted)) {
      formatted[, metric_group := NULL]
    }
    if ("scenario" %in% names(formatted)) {
      formatted[, scenario := NULL]
    }

    # Remove credit flag columns (metadata, not for display)
    credit_cols <- intersect(c("meal_rest_prems_credit", "other_credit"), names(formatted))
    if (length(credit_cols) > 0) formatted[, (credit_cols) := NULL]

    # Create section header
    header_row <- data.table(Metric = paste0("### ", section_name))
    for (col in setdiff(names(formatted), "Metric")) {
      header_row[, (col) := ""]
    }

    # Combine header and metrics
    rbind(header_row, formatted, fill = TRUE)
  })

  # Combine all sections
  result <- rbindlist(Filter(Negate(is.null), all_sections), fill = TRUE)
  result
}


# Damages-specific calculation (no year columns, only All Data and Key Groups)
calculate_damages_metrics <- function(data_list, spec, group_names, filters = list(), extrapolation_factor = 1.0) {
  if (length(group_names) == 0) return(data.table())

  all_results <- lapply(group_names, function(group_name) {
    group_spec <- spec[metric_group == group_name]
    if (nrow(group_spec) == 0) return(NULL)

    first_source <- group_spec$source[1]

    # Determine key groups based on source
    if (grepl("pay", first_source, ignore.case = TRUE)) {
      key_groups <- data_list$pay_key_groups
    } else {
      key_groups <- data_list$shift_key_groups
    }

    results <- lapply(seq_len(nrow(group_spec)), function(i) {
      row <- group_spec[i]
      source <- row$source

      # Get source data
      src_data <- data_list[[source]]

      if (is.null(src_data) || nrow(src_data) == 0) {
        result_row <- data.table(Metric = row$metric_label, All_Data = "-")
        return(result_row)
      }

      # Determine key column based on source
      if (source == "pay1") {
        src_key_col <- "Pay_Key_Gps"
      } else if (source %in% c("shift_data1", "pp_data1", "ee_data1")) {
        src_key_col <- "Key_Gps"
      } else {
        src_key_col <- NULL
      }

      # Calculate All Data value
      all_data_result <- calculate_single_metric(src_data, row$expr, row$digits, row$denom)

      result_row <- data.table(
        Metric = row$metric_label,
        All_Data = format_metric_value(all_data_result$value, all_data_result$pct)
      )

      # Add key group columns only (no years for damages)
      if (!is.null(key_groups) && length(key_groups) > 0 && !is.null(src_key_col) && src_key_col %in% names(src_data)) {
        for (gp in key_groups) {
          gp_data <- src_data[get(src_key_col) == gp]
          gp_result <- calculate_single_metric(gp_data, row$expr, row$digits, row$denom)
          result_row[, (gp) := format_metric_value(gp_result$value, gp_result$pct)]
        }
      }

      # Add extrapolated column if factor > 1
      if (extrapolation_factor > 1.0) {
        extrap_value <- if (is.numeric(all_data_result$value) && !is.na(all_data_result$value)) {
          all_data_result$value * extrapolation_factor
        } else {
          NA
        }
        result_row[, Extrapolated := format_metric_value(extrap_value, NA)]
      }

      result_row
    })

    rbindlist(results, fill = TRUE)
  })

  rbindlist(Filter(Negate(is.null), all_results), fill = TRUE)
}


# Combine multiple metric groups into a single table with section headers
combine_damages_with_headers <- function(data, spec, group_definitions, filters = list(), factor = 1.0) {
  # group_definitions should be a list of lists with structure:
  # list(section_name = "MEAL PERIOD DAMAGES", groups = damages_meal_groups)

  all_sections <- lapply(group_definitions, function(def) {
    section_name <- def$section_name
    groups <- def$groups

    if (length(groups) == 0) return(NULL)

    # Calculate metrics for this section
    metrics <- calculate_damages_metrics(data, spec, groups, filters, factor)

    if (nrow(metrics) == 0) return(NULL)

    # Create a header row
    header_row <- data.table(Metric = paste0("### ", section_name))

    # Get all column names from metrics and fill header with empty strings
    for (col in setdiff(names(metrics), "Metric")) {
      header_row[, (col) := ""]
    }

    # Combine header and metrics
    rbind(header_row, metrics, fill = TRUE)
  })

  # Combine all sections
  result <- rbindlist(Filter(Negate(is.null), all_sections), fill = TRUE)

  return(result)
}

# Filter damage metrics by label content (for penalties that don't have waiver in group name)
filter_metrics_by_label <- function(metrics_table, include_waivers = FALSE) {
  if (is.null(metrics_table) || nrow(metrics_table) == 0) return(metrics_table)
  if (!"Metric" %in% names(metrics_table)) return(metrics_table)

  # Don't filter header rows
  is_header <- grepl("^###", metrics_table$Metric)

  # Check for waiver designations
  has_waivers <- grepl("\\(waivers\\)", metrics_table$Metric, ignore.case = TRUE)
  has_no_waivers <- grepl("\\(no\\s+waivers\\)", metrics_table$Metric, ignore.case = TRUE)
  has_no_designation <- !has_waivers & !has_no_waivers  # "all" scenarios

  # Filter data rows based on label content
  if (include_waivers) {
    # WAIVERS TAB: Include "(waivers)" metrics AND "all" scenarios (no designation)
    # Exclude only "(no waivers)" metrics
    keep <- is_header | has_waivers | has_no_designation
  } else {
    # NO WAIVERS TAB: Include "(no waivers)" metrics AND "all" scenarios (no designation)
    # Exclude only "(waivers)" metrics
    keep <- is_header | has_no_waivers | has_no_designation
  }

  return(metrics_table[keep])
}

# Determine if a metric group name indicates waiver-only variant
is_waiver_only_group <- function(group_name) {
  # Waiver-only groups have:
  # 1. ">6 hrs" or ">6hrs" (meal period waivers)
  # 2. "(waivers)" or "(waiver)" in the name
  # Pattern matches: (waiver), (waivers), (Waiver), etc.
  grepl(">6\\s*hrs|\\(\\s*waivers?\\s*\\)", group_name, ignore.case = TRUE)
}

# Determine if a metric group name indicates no-waiver-only variant
is_no_waiver_only_group <- function(group_name) {
  # No-waiver-only groups have:
  # 1. ">5 hrs" or ">5hrs" (meal periods without waivers)
  # 2. "(no waivers)" in the name (with or without 's')
  # Pattern matches: (no waiver), (no waivers), (No Waiver), etc.
  grepl(">5\\s*hrs|\\(\\s*no\\s+waivers?\\s*\\)", group_name, ignore.case = TRUE)
}

# Split metric groups into waiver and no-waiver categories
split_by_waiver <- function(all_groups) {
  waiver_only <- all_groups[sapply(all_groups, is_waiver_only_group)]
  no_waiver_only <- all_groups[sapply(all_groups, is_no_waiver_only_group)]
  neither <- all_groups[!sapply(all_groups, is_waiver_only_group) & !sapply(all_groups, is_no_waiver_only_group)]

  # Check if there are BOTH waiver and no-waiver versions in this set
  # If there are paired versions, "neither" groups should only go to no-waiver tab
  # If there are NO paired versions, "neither" groups should go to BOTH tabs
  has_paired_versions <- length(waiver_only) > 0 && length(no_waiver_only) > 0

  if (has_paired_versions) {
    # There are paired versions, so unclassified metrics belong only in no-waiver
    list(
      no_waiver = c(no_waiver_only, neither),
      waiver = waiver_only
    )
  } else {
    # No paired versions exist, so unclassified metrics (like unreimbursed expenses) go in BOTH
    list(
      no_waiver = c(no_waiver_only, neither),
      waiver = c(waiver_only, neither)
    )
  }
}


is_truthy_flag <- function(x) {
  if (is.null(x)) return(logical(0))
  if (is.logical(x)) return(!is.na(x) & x)
  if (is.numeric(x)) return(!is.na(x) & x != 0)
  lx <- tolower(trimws(as.character(x)))
  !is.na(lx) & lx %in% c("true", "t", "1", "yes", "y")
}

normalize_scenario_value <- function(x) {
  if (is.null(x)) return(character(0))

  sx <- tolower(trimws(as.character(x)))
  sx[sx == ""] <- NA_character_
  sx <- gsub("_", " ", sx, fixed = TRUE)
  sx <- gsub("\\s+", " ", sx)

  out <- sx
  is_hybrid <- !is.na(sx) & grepl("hybrid", sx)
  is_no_waivers <- !is.na(sx) & grepl("no\\s*waiver", sx)
  is_waivers <- !is.na(sx) & !is_no_waivers & grepl("\\bwaiver", sx)
  is_all <- !is.na(sx) & sx == "all"

  out[is_hybrid] <- "hybrid"
  out[is_no_waivers] <- "no waivers"
  out[is_waivers] <- "waivers"
  out[is_all] <- "all"

  out
}

get_flagged_metric_row_mask <- function(dt, spec = NULL, flag_col) {
  if (is.null(dt) || nrow(dt) == 0 || is.null(flag_col) || !nzchar(flag_col)) return(logical(0))

  mask <- rep(FALSE, nrow(dt))

  if (flag_col %in% names(dt)) {
    mask <- mask | is_truthy_flag(dt[[flag_col]])
  }

  if (!is.null(spec) &&
      all(c("metric_group", "metric_label", flag_col) %in% names(spec)) &&
      all(c("metric_group", "metric_label") %in% names(dt))) {
    spec_dt <- as.data.table(spec)
    spec_dt <- spec_dt[is_truthy_flag(get(flag_col)), .(metric_group, metric_label)]

    if (nrow(spec_dt) > 0) {
      spec_keys <- unique(paste(spec_dt$metric_group, spec_dt$metric_label, sep = "\r"))
      dt_keys <- paste(dt$metric_group, dt$metric_label, sep = "\r")
      mask <- mask | (dt_keys %in% spec_keys)
    }
  }

  mask
}

get_credit_row_mask <- function(dt, spec = NULL) {
  get_flagged_metric_row_mask(dt, spec = spec, flag_col = "other_credit")
}

get_less_prems_row_mask <- function(dt, spec = NULL) {
  get_flagged_metric_row_mask(dt, spec = spec, flag_col = "meal_rest_prems_credit")
}

format_metric_value <- function(val, pct = NA) {
  if (is.null(val) || length(val) == 0 || (is.atomic(val) && all(is.na(val)))) {
    return("-")
  }

  if (inherits(val, "Date")) {
    return(as.character(val))
  }

  if (is.numeric(val)) {
    # Round appropriately
    rounded <- if (abs(val) >= 100) round(val, 0) else round(val, 2)
    # Format with commas using prettyNum (more reliable than format)
    formatted <- prettyNum(rounded, big.mark = ",")

    if (!is.na(pct)) {
      formatted <- paste0(formatted, " (", pct, "%)")
    }
    return(formatted)
  }

  as.character(val)
}

# ---- UI HELPER FUNCTIONS ----

under_construction_card <- function(title, description = NULL) {
  card(
    card_header(
      class = "bg-warning text-dark",
      icon("hammer"),
      " ", title, " - Under Construction"
    ),
    card_body(
      p(class = "text-muted",
        description %||% "This section is being developed and will be available soon.")
    )
  )
}

# Create DT output with proper formatting (NO PAGINATION)
create_dt_table <- function(dt, metric_col = "Metric") {
  if (is.null(dt) || nrow(dt) == 0) {
    return(datatable(data.table(Message = "No data available"), rownames = FALSE, options = list(dom = 't')))
  }

  # Replace underscores with spaces in column names
  formatted_names <- gsub("_", " ", names(dt))

  # Determine which columns should be left-aligned
  # Always left-align the metric_col, plus "Key Group"/"Key Gp"/"Pay Code"/"Pay Code Category" if present
  left_align_cols <- c(metric_col, "Key Group", "Key Gp", "Pay Code", "Pay Code Category")
  left_cols_idx <- which(names(dt) %in% left_align_cols) - 1  # 0-indexed for JS

  # All other columns are center-aligned
  value_cols_idx <- setdiff(seq_along(names(dt)) - 1, left_cols_idx)

  # Find Extrapolated column index (0-indexed for JS)
  extrap_col_idx <- which(names(dt) == "Extrapolated") - 1

  # Build columnDefs list
  col_defs <- list(
    list(className = 'dt-left dt-head-left', targets = left_cols_idx),
    list(className = 'dt-center dt-head-center', targets = value_cols_idx)
  )

  # Add extrap-col class to Extrapolated column if it exists
  if (length(extrap_col_idx) > 0) {
    col_defs[[length(col_defs) + 1]] <- list(className = 'extrap-col dt-center dt-head-center', targets = extrap_col_idx)
  }

  datatable(
    dt,
    colnames = formatted_names,
    options = list(
      paging = FALSE,  # Remove pagination entirely
      scrollX = TRUE,
      scrollY = "calc(100vh - 300px)",  # Dynamic height based on viewport
      dom = 'frti',  # Removed 'p' for pagination
      order = list(),  # Preserve metric_order from pipeline (no auto-sort by column)
      columnDefs = col_defs,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#2c3e50', 'color': '#fff'});",
        "}"
      ),
      rowCallback = JS(
        "function(row, data) {",
        "  // Handle section headers",
        "  if (data[0] && data[0].toString().startsWith('### ')) {",
        "    $(row).find('td:first').html(data[0].replace('### ', ''));",
        "    $(row).css({",
        "      'background': 'linear-gradient(135deg, #90EE90 0%, #3CB371 50%, #2E8B57 100%)',",
        "      'color': '#fff',",
        "      'font-weight': 'bold',",
        "      'border-bottom': '3px solid #228B22',",
        "      'font-size': '14px'",
        "    });",
        "    $(row).find('td').css({",
        "      'background': 'linear-gradient(135deg, #90EE90 0%, #3CB371 50%, #2E8B57 100%)',",
        "      'color': '#fff'",
        "    });",
        "  }",
        "  // Bold dollar amounts (metrics with '$' in name)",
        "  else if (data[0] && data[0].toString().includes('$')) {",
        "    // Bold the entire row",
        "    $(row).find('td').css({'font-weight': 'bold'});",
        "    // Format numeric values with $ prefix, commas, and .00 cents",
        "    for (var i = 1; i < data.length; i++) {",
        "      var val = data[i];",
        "      if (val !== null && val !== undefined && val !== '' && val !== '-') {",
        "        // Remove existing commas and $ signs, then parse as number",
        "        var cleanVal = val.toString().replace(/[$,]/g, '');",
        "        var num = parseFloat(cleanVal);",
        "        if (!isNaN(num)) {",
        "          // Format with $, thousand separators, and .00 cents",
        "          var parts = num.toFixed(2).split('.');",
        "          parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
        "          var formatted = '$' + parts.join('.');",
        "          $(row).find('td:eq(' + i + ')').html(formatted);",
        "        }",
        "      }",
        "    }",
        "  }",
        "}"
      )
    ),
    rownames = FALSE,
    class = 'cell-border stripe hover',
    style = 'bootstrap4'
  )
}

# ---- FILTER SIDEBAR ----

filter_sidebar <- function(data_list) {
  shift_data <- data_list$shift_data1
  pay_data <- data_list$pay1

  pay_dates <- if (!is.null(pay_data)) {
    if ("Pay_Period_End" %in% names(pay_data)) {
      pay_data$Pay_Period_End
    } else if ("Pay_Date" %in% names(pay_data)) {
      pay_data$Pay_Date
    } else {
      as.Date(NA)
    }
  } else {
    as.Date(NA)
  }

  date_min <- min(
    min(shift_data$Date, na.rm = TRUE),
    min(pay_dates, na.rm = TRUE),
    na.rm = TRUE
  )
  date_max <- max(
    max(shift_data$Date, na.rm = TRUE),
    max(pay_dates, na.rm = TRUE),
    na.rm = TRUE
  )

  sidebar(
    title = "Filters & Settings",
    width = 300,

    h5("Date Range"),
    dateRangeInput(
      "date_range",
      NULL,
      start = date_min,
      end = date_max,
      min = date_min,
      max = date_max
    ),
    actionButton("reset_dates", "Reset to Original Dates", class = "btn-sm btn-outline-secondary w-100"),

    hr(),

    h5("Filters"),
    selectizeInput(
      "employee_filter",
      "Employee ID(s)",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "All employees")
    ),
    selectizeInput(
      "department_filter",
      "Department",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "Loading")
    ),
    selectizeInput(
      "location_filter",
      "Location",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "Loading")
    ),
    selectizeInput(
      "sample_filter",
      "Sample",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "Loading")
    ),
    selectizeInput(
      "subclass_filter",
      "Subclass",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "Loading")
    ),
    selectizeInput(
      "key_groups_filter",
      "Key Groups (Named Plaintiff(s), etc)",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "All key groups")
    ),

    hr(),

    actionButton("apply_filters", "Apply Filters", class = "btn-primary w-100"),
    actionButton("reset_filters", "Reset All Filters", class = "btn-outline-secondary w-100 mt-2"),

    hr(),

    # Employee-Period Selection (for Example tab)
    h5("Select Employee-Period"),
    selectizeInput(
      "example_period_select",
      NULL,
      choices = NULL,
      options = list(
        placeholder = "Type to search employee-period...",
        maxOptions = 100,
        closeAfterSelect = TRUE,
        openOnFocus = TRUE
      )
    ),

    hr(),

    # Display Settings
    h5("Display Settings"),
    selectInput("font_size", "Font Size",
                choices = c("Small" = "12px", "Medium" = "14px", "Large" = "16px", "X-Large" = "18px"),
                selected = "14px"),

    hr(),

    checkboxInput("toggle_extrap_cols", "Show Extrapolated Values", value = TRUE),
    checkboxInput("show_credits", "Show Credit-Adjusted Metrics", value = TRUE),
    checkboxInput("show_less_prems", "Show Less-Premiums-Adjusted Metrics", value = TRUE),

    tags$hr(style = "margin-top: 10px; margin-bottom: 10px;"),
    tags$small(style = "font-size: 11px; color: #6c757d; font-weight: 600;", "Scenario Visibility"),
    checkboxInput("show_scenario_no_waivers", "Show No-Waivers Scenario", value = TRUE),
    checkboxInput("show_scenario_waivers", "Show Waivers Scenario", value = TRUE),
    checkboxInput("show_scenario_hybrid", "Show Hybrid Scenario", value = TRUE),

    actionButton("open_pdf_modal", "Generate PDF Report",
                 class = "w-100 mt-2 btn-primary",
                 icon = icon("file-pdf"),
                 style = "background: linear-gradient(135deg, #90EE90 0%, #3CB371 50%, #2E8B57 100%);
                          border: none;
                          font-weight: bold;
                          padding: 10px;
                          box-shadow: 0 4px 6px rgba(0,0,0,0.15), inset 0 1px 0 rgba(255,255,255,0.4);
                          color: white;"),
    downloadButton("download_excel", "Download Excel",
                   class = "w-100 mt-2",
                   style = "background: linear-gradient(135deg, #90EE90 0%, #3CB371 50%, #2E8B57 100%);
                            border: none;
                            font-weight: bold;
                            padding: 10px;
                            box-shadow: 0 4px 6px rgba(0,0,0,0.15), inset 0 1px 0 rgba(255,255,255,0.4);
                            color: white;")
  )
}

# ---- UI ----

ui <- function(data_list, metric_spec) {

  # Get current year for watermark
  current_year <- format(Sys.Date(), "%Y")

  # Version information (from clean_data version variable when available)
  app_version <- get_case_version_label()

  metric_scenarios <- if ("scenario" %in% names(metric_spec)) normalize_scenario_value(metric_spec$scenario) else character(0)
  if (!("scenario" %in% names(metric_spec))) {
    show_no_waivers <- TRUE
    show_waivers <- TRUE
    show_hybrid <- FALSE
  } else {
    show_no_waivers <- any(metric_scenarios == "no waivers", na.rm = TRUE)
    show_waivers <- any(metric_scenarios == "waivers", na.rm = TRUE)
    show_hybrid <- any(metric_scenarios == "hybrid", na.rm = TRUE)

    # If scenario column has only "all" (or blank), keep legacy no/waiver tabs visible.
    if (!show_no_waivers && !show_waivers && !show_hybrid) {
      show_no_waivers <- TRUE
      show_waivers <- TRUE
    }
  }


  page_navbar(
    title = div(
      "Wage & Hour Compliance Dashboard",
      br(),
      tags$small(style = "font-size: 11px; color: #95a5a6; font-weight: normal;", "Confidential")
    ),
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2c3e50",
      "navbar-bg" = "#2c3e50"
    ),
    sidebar = filter_sidebar(data_list),
    header = tagList(
      # Initialize shinyjs
      useShinyjs(),

      # Custom CSS
      tags$head(
        tags$style(HTML("
          #filter_banner {
            display: none;
            background-color: #e74c3c;
            color: white;
            padding: 10px;
            text-align: center;
            font-weight: bold;
            position: sticky;
            top: 0;
            z-index: 500;
          }
          .dt-center { text-align: center !important; }
          .dt-left { text-align: left !important; }
          .dt-head-center { text-align: center !important; }
          .dt-head-left { text-align: left !important; }

          /* Time Data - Dark Blue */
          .value-box.time-data {
            border-left: 4px solid #2c3e50 !important;
          }
          .value-box.time-data .value-box-showcase {
            background-color: #2c3e50 !important;
          }

          /* Pay Data - Green */
          .value-box.pay-data {
            border-left: 4px solid #27ae60 !important;
          }
          .value-box.pay-data .value-box-showcase {
            background-color: #27ae60 !important;
          }

          /* Watermark and version - bottom center */
          .footer-info {
            position: fixed;
            bottom: 2px;
            left: 50%;
            transform: translateX(-50%);
            text-align: center;
            font-size: 10px;
            color: #bdc3c7;
            opacity: 0.5;
            z-index: 1;
            pointer-events: none;
          }

          /* Hide extrapolated columns when toggle is off */
          .hide-extrap-cols .extrap-col {
            display: none !important;
          }

          /* Make selectize input text smaller to fit more */
          .selectize-control .selectize-input {
            font-size: 13px !important;
          }
          .selectize-control .selectize-input input {
            font-size: 13px !important;
          }
          .selectize-control .selectize-input input::placeholder {
            font-size: 13px !important;
          }
          .selectize-control .selectize-input .item {
            font-size: 13px !important;
          }
        ")),
        tags$script(HTML("
          // Toggle extrapolated columns visibility
          $(document).on('shiny:connected', function() {
            Shiny.addCustomMessageHandler('toggleExtrapCols', function(show) {
              if (show) {
                $('body').removeClass('hide-extrap-cols');
              } else {
                $('body').addClass('hide-extrap-cols');
              }
            });

            // Show/hide scenario subtabs (Time Meal Violations, Class, PAGA)
            Shiny.addCustomMessageHandler('toggleScenarioTabs', function(cfg) {
              if (!cfg || !cfg.tabs) return;
              var fallbacks = cfg.fallbacks || {};

              var triggerSelector = function(val) {
                return '.nav-link[data-value=\"' + val + '\"], .dropdown-item[data-value=\"' + val + '\"], a[data-value=\"' + val + '\"], button[data-value=\"' + val + '\"]';
              };

              var paneSelector = function(val) {
                return '.tab-pane[data-value=\"' + val + '\"]';
              };

              var ensureFallbackIfActive = function(val) {
                var $triggers = $(triggerSelector(val));
                var $panes = $(paneSelector(val));
                var isActive = $triggers.filter('.active').length > 0 || $panes.filter('.active, .show').length > 0;
                if (!isActive) return;

                var fb = fallbacks[val];
                if (!fb) return;

                var $fb = $(triggerSelector(fb)).first();
                if ($fb.length) {
                  $fb.trigger('click');
                }
              };

              var setTabVisible = function(val, visible) {
                var show = !!visible;
                var $triggers = $(triggerSelector(val));
                var $panes = $(paneSelector(val));

                if (!show) {
                  ensureFallbackIfActive(val);
                }

                $triggers.each(function() {
                  var $t = $(this);
                  var $li = $t.closest('li');
                  if ($li.length) {
                    $li.toggle(show);
                  } else {
                    $t.toggle(show);
                  }

                  if (!show) {
                    $t.removeClass('active');
                    $t.attr('aria-selected', 'false');
                  }
                });

                $panes.each(function() {
                  var $p = $(this);
                  if (show) {
                    // Let Bootstrap classes control visibility; don't force display:block.
                    $p.removeClass('d-none');
                  } else {
                    $p.addClass('d-none');
                    $p.removeClass('show active');
                  }
                });
              };

              Object.keys(cfg.tabs).forEach(function(val) {
                setTabVisible(val, cfg.tabs[val]);
              });
            });
          });
        "))
      ),

      div(id = "filter_banner", style = "display: none;", uiOutput("filter_banner_text")),
      div(class = "footer-info", HTML(paste0(
        if (exists("contract_footer") && !is.na(contract_footer) && nzchar(contract_footer)) paste0(contract_footer, " &middot; ") else "",
        "Anello Data Solutions LLC (", current_year, ") &middot; ", app_version
      )))
    ),

    # =======================================================================
    # CASE ANALYSIS DETAIL TAB
    # =======================================================================
    nav_panel(
      title = "Case Detail",
      icon = icon("file-contract"),

      card(
        card_header("Case Configuration"),
        card_body(
          style = "min-height: 280px;",
          div(
            style = "line-height: 1.6;",
            p(strong("Case Name: "), textOutput("case_name", inline = TRUE)),
            p(strong("Case Number: "), textOutput("case_number", inline = TRUE)),
            p(strong("Date Filed: "), textOutput("date_filed", inline = TRUE)),
            p(strong("Relevant Period: "), textOutput("relevant_period", inline = TRUE)),
            p(strong("Mediation Date: "), textOutput("mediation_date", inline = TRUE)),
            p(strong("Sample Size: "), textOutput("sample_size", inline = TRUE))
          )
        )
      )
    ),


    # =======================================================================
    # DATA COMPARISON TAB (with subtabs)
    # =======================================================================
    nav_panel(
      title = "Data Comparison",
      icon = icon("project-diagram"),

      navset_card_underline(
        # Overview subtab (formerly main Overview tab)
        nav_panel(
          "Overview",

          layout_columns(
            col_widths = c(4, 4, 4),

            value_box(
              title = "Employees (Time)",
              value = textOutput("total_employees_time"),
              showcase = icon("users"),
              theme = "primary",
              class = "time-data"
            ),
            value_box(
              title = "Employees (Pay)",
              value = textOutput("total_employees_pay"),
              showcase = icon("users"),
              theme = "success",
              class = "pay-data"
            ),
            value_box(
              title = "Employees (Class)",
              value = textOutput("total_employees_class"),
              showcase = icon("users"),
              theme = "success"
            )
          ),

          layout_columns(
            col_widths = c(4, 4, 4),

            value_box(
              title = "Pay Periods (Time)",
              value = textOutput("time_pay_periods"),
              showcase = icon("calendar"),
              theme = "secondary",
              class = "time-data"
            ),
            value_box(
              title = "Pay Periods (Pay)",
              value = textOutput("pay_pay_periods"),
              showcase = icon("calendar"),
              theme = "secondary",
              class = "pay-data"
            ),
            value_box(
              title = "Weeks (Time)",
              value = textOutput("total_weeks"),
              showcase = icon("calendar-week"),
              theme = "secondary",
              class = "time-data"
            )
          ),

          layout_columns(
            col_widths = c(12),

            card(
              card_header("Employee Coverage Over Time"),
              card_body(
                withSpinner(plotlyOutput("employee_coverage_plot", height = "400px"), type = 6, color = "#2c3e50")
              )
            )
          )
        ),

        # Employee Comparison Table subtab (all_ids)
        nav_panel(
          "Employee Comparison",

          layout_columns(
            col_widths = c(12),

            card(
              card_header("Employee Comparison (all_ids)"),
              card_body(
                withSpinner(DTOutput("employee_comparison_all_ids"), type = 6, color = "#2c3e50")
              )
            )
          )
        ),

        # Employee Pay Period Comparison subtab
        nav_panel(
          "Employee Pay Period Comparison",

          layout_columns(
            col_widths = c(12),

            card(
              card_header("Employee Pay Period Comparison"),
              card_body(
                withSpinner(DTOutput("employee_comparison_table"), type = 6, color = "#2c3e50")
              )
            )
          )
        )
      )
    ),

    # =======================================================================
    # TIME DATA STATISTICS
    # =======================================================================
    nav_menu(
      title = "Time Data Statistics",
      icon = icon("clock"),

      nav_panel(
        "Summary",
        value = "time_summary_tab",
        withSpinner(DTOutput("table_time_summary"), type = 6, color = "#2c3e50")
      ),

      nav_panel(
        "Shift Hours Analysis",
        withSpinner(DTOutput("table_shift_hours"), type = 6, color = "#2c3e50")
      ),

      nav_panel(
        "Punch Rounding",
        withSpinner(DTOutput("table_rounding_consolidated"), type = 6, color = "#2c3e50")
      ),

      nav_panel(
        "Meal Analysis",
        withSpinner(DTOutput("table_meal_consolidated"), type = 6, color = "#2c3e50")
      ),

      if (show_no_waivers) nav_panel(
        "Meal Violations (no waivers)",
        value = "time_meal_no_waivers_tab",
        navset_card_underline(
          nav_panel(
            "Summary",
            withSpinner(DTOutput("table_meal_5hr_consolidated"), type = 6, color = "#2c3e50")
          ),
          nav_panel(
            "Short Meal Details",
            withSpinner(DTOutput("table_meal_5hr_short_details"), type = 6, color = "#2c3e50")
          ),
          nav_panel(
            "Late Meal Details",
            withSpinner(DTOutput("table_meal_5hr_late_details"), type = 6, color = "#2c3e50")
          )
        )
      ),

      if (show_waivers) nav_panel(
        "Meal Violations (waivers)",
        value = "time_meal_waivers_tab",
        navset_card_underline(
          nav_panel(
            "Summary",
            withSpinner(DTOutput("table_meal_6hr_consolidated"), type = 6, color = "#2c3e50")
          ),
          nav_panel(
            "Short Meal Details",
            withSpinner(DTOutput("table_meal_6hr_short_details"), type = 6, color = "#2c3e50")
          ),
          nav_panel(
            "Late Meal Details",
            withSpinner(DTOutput("table_meal_6hr_late_details"), type = 6, color = "#2c3e50")
          )
        )
      ),

      if (show_hybrid) nav_panel(
        "Meal Violations (hybrid)",
        value = "time_meal_hybrid_tab",
        navset_card_underline(
          nav_panel(
            "Summary",
            withSpinner(DTOutput("table_meal_hybrid_consolidated"), type = 6, color = "#2c3e50")
          ),
          nav_panel(
            "Short Meal Details",
            withSpinner(DTOutput("table_meal_hybrid_short_details"), type = 6, color = "#2c3e50")
          ),
          nav_panel(
            "Late Meal Details",
            withSpinner(DTOutput("table_meal_hybrid_late_details"), type = 6, color = "#2c3e50")
          )
        )
      ),

      nav_panel(
        "Rest Periods",
        withSpinner(DTOutput("table_rest_consolidated"), type = 6, color = "#2c3e50")
      )
    ),

    # =======================================================================
    # PAY DATA STATISTICS
    # =======================================================================
    nav_menu(
      title = "Pay Data Statistics",
      icon = icon("dollar-sign"),

      nav_panel(
        "Summary",
        withSpinner(DTOutput("table_pay_consolidated"), type = 6, color = "#2c3e50")
      ),

      nav_panel(
        "Regular Rate",
        withSpinner(DTOutput("table_rrop_consolidated"), type = 6, color = "#2c3e50")
      ),

      nav_panel(
        "Pay Codes",
        withSpinner(DTOutput("table_pay_codes"), type = 6, color = "#2c3e50")
      ),

      nav_panel(
        "Rate Type Analysis",
        withSpinner(DTOutput("table_rate_type"), type = 6, color = "#2c3e50")
      )
    ),

    # =======================================================================
    # CLASS / INDIVIDUAL CLAIM DAMAGES
    # =======================================================================
    nav_menu(
      title = "Class / Individual Claim Damages",
      icon = icon("gavel"),

      nav_panel(
        "Overview",
        value = "class_overview_tab",
        withSpinner(DTOutput("table_damages_class_overview"), type = 6, color = "#2c3e50")
      ),

      if (show_no_waivers) nav_panel(
        "No Waivers",
        value = "class_no_waivers_tab",
        withSpinner(DTOutput("table_damages_class_no_waivers"), type = 6, color = "#2c3e50")
      ),

      if (show_waivers) nav_panel(
        "Waivers",
        value = "class_waivers_tab",
        withSpinner(DTOutput("table_damages_class_waivers"), type = 6, color = "#2c3e50")
      ),

      if (show_hybrid) nav_panel(
        "Hybrid",
        value = "class_hybrid_tab",
        withSpinner(DTOutput("table_damages_class_hybrid"), type = 6, color = "#2c3e50")
      ),

      nav_panel(
        "Wage Statement Penalties",
        withSpinner(DTOutput("table_damages_wsv"), type = 6, color = "#2c3e50")
      ),

      nav_panel(
        "Waiting Time Penalties",
        withSpinner(DTOutput("table_damages_wt"), type = 6, color = "#2c3e50")
      )
    ),

    # =======================================================================
    # PAGA DAMAGES
    # =======================================================================
    nav_menu(
      title = "PAGA Damages",
      icon = icon("balance-scale"),

      nav_panel(
        "Overview",
        value = "paga_overview_tab",
        withSpinner(DTOutput("table_paga_overview"), type = 6, color = "#2c3e50")
      ),

      if (show_no_waivers) nav_panel(
        "No Waivers",
        value = "paga_no_waivers_tab",
        withSpinner(DTOutput("table_paga_no_waivers"), type = 6, color = "#2c3e50")
      ),

      if (show_waivers) nav_panel(
        "Waivers",
        value = "paga_waivers_tab",
        withSpinner(DTOutput("table_paga_waivers"), type = 6, color = "#2c3e50")
      ),
      if (show_hybrid) nav_panel(
        "Hybrid",
        value = "paga_hybrid_tab",
        withSpinner(DTOutput("table_paga_hybrid"), type = 6, color = "#2c3e50")
      )
    ),

    # =======================================================================
    # EMPLOYEE-PERIOD EXAMPLE TAB
    # =======================================================================
    nav_panel(
      title = "Example",
      icon = icon("user-clock"),

      div(
        style = "height: calc(100vh - 150px); overflow-y: auto; padding: 10px;",
        # (Instructions removed as requested)
        # Punch Detail - time1
        card(
          card_header("Punch Detail (time1)"),
          card_body(
            div(style = "overflow-x: auto;",
                withSpinner(DTOutput("table_example_punches"), type = 6, color = "#2c3e50")
            )
          )
        ),

        # Shift Data - shift_data1
        card(
          card_header("Shift Data (shift_data1)"),
          card_body(
            div(style = "overflow-x: auto;",
                withSpinner(DTOutput("table_example_shift"), type = 6, color = "#2c3e50")
            )
          )
        ),

        # Pay Data - pay1
        card(
          card_header("Pay Data (pay1)"),
          card_body(
            div(style = "overflow-x: auto;",
                withSpinner(DTOutput("table_example_pay"), type = 6, color = "#2c3e50")
            )
          )
        ),

        # Damages - pp_data1/ee_data1
        card(
          card_header("Damages (pp_data1 / ee_data1)"),
          card_body(
            div(style = "overflow-x: auto;",
                withSpinner(DTOutput("table_example_damages"), type = 6, color = "#2c3e50")
            )
          )
        )
      )
    ),

    # =======================================================================
    # DATA DRILLDOWN TAB
    # =======================================================================
    nav_panel(
      title = "Data Drilldown",
      icon = icon("search"),

      div(
        style = "height: calc(100vh - 150px); overflow-y: auto; padding: 10px;",

        card(
          card_header("Drilldown Selector"),
          card_body(
            fluidRow(
              column(
                width = 3,
                selectInput(
                  "drill_table_select",
                  "Table",
                  choices = c(
                    "time1 (Punch Detail)" = "time1",
                    "pay1 (Pay Detail)" = "pay1",
                    "shift_data1 (Shift Detail)" = "shift_data1",
                    "pp_data1 (Period-Level)" = "pp_data1",
                    "ee_data1 (Employee-Level)" = "ee_data1"
                  ),
                  selected = "time1"
                )
              ),
              column(
                width = 4,
                dateRangeInput(
                  "drill_period_range",
                  "Period End Date Range",
                  start = NULL,
                  end = NULL,
                  separator = "to"
                )
              ),
              column(
                width = 3,
                checkboxInput("drill_allow_large", "Allow large load", value = FALSE),
                actionButton("drill_apply", "Load Drilldown", class = "btn-primary btn-sm")
              )
            ),
            fluidRow(
              column(
                width = 9,
                fluidRow(
                  column(
                    width = 4,
                    selectizeInput(
                      "drill_field_select_1",
                      "Field 1",
                      choices = character(0),
                      options = list(placeholder = "Optional field filter")
                    )
                  ),
                  column(
                    width = 8,
                    selectizeInput(
                      "drill_value_select_1",
                      "Value 1",
                      choices = character(0),
                      options = list(placeholder = "Pick value for Field 1", maxOptions = 5000)
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    selectizeInput(
                      "drill_field_select_2",
                      "Field 2",
                      choices = character(0),
                      options = list(placeholder = "Optional field filter")
                    )
                  ),
                  column(
                    width = 8,
                    selectizeInput(
                      "drill_value_select_2",
                      "Value 2",
                      choices = character(0),
                      options = list(placeholder = "Pick value for Field 2", maxOptions = 5000)
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    selectizeInput(
                      "drill_field_select_3",
                      "Field 3",
                      choices = character(0),
                      options = list(placeholder = "Optional field filter")
                    )
                  ),
                  column(
                    width = 8,
                    selectizeInput(
                      "drill_value_select_3",
                      "Value 3",
                      choices = character(0),
                      options = list(placeholder = "Pick value for Field 3", maxOptions = 5000)
                    )
                  )
                )
              ),
              column(
                width = 3,
                div(
                  style = "font-size: 13px; line-height: 1.45; margin-top: 6px;",
                  uiOutput("drill_filter_counts_ui")
                )
              )
            ),
            uiOutput("drill_status_ui")
          )
        ),

        card(
          card_header("Selected Table View"),
          card_body(
            downloadButton("download_drill_viewer", "Download View", class = "btn-sm"),
            div(style = "overflow-x: auto; width: 100%; margin-top: 8px;",
                withSpinner(DTOutput("table_drill_viewer"), type = 6, color = "#2c3e50")
            )
          )
        ),

        card(
          card_header("Punch Detail (time1)"),
          card_body(
            div(style = "overflow-x: auto;",
                withSpinner(DTOutput("table_drill_punches"), type = 6, color = "#2c3e50")
            )
          )
        ),

        card(
          card_header("Shift Data (shift_data1)"),
          card_body(
            div(style = "overflow-x: auto;",
                withSpinner(DTOutput("table_drill_shift"), type = 6, color = "#2c3e50")
            )
          )
        ),

        card(
          card_header("Pay Data (pay1)"),
          card_body(
            div(style = "overflow-x: auto;",
                withSpinner(DTOutput("table_drill_pay"), type = 6, color = "#2c3e50")
            )
          )
        ),

        card(
          card_header("Damages (pp_data1 / ee_data1)"),
          card_body(
            div(style = "overflow-x: auto;",
                withSpinner(DTOutput("table_drill_damages"), type = 6, color = "#2c3e50")
            )
          )
        )
      )
    ),

    # =======================================================================
    # APPENDIX TAB
    # =======================================================================
    nav_panel(
      title = "Appendix",
      icon = icon("book"),

      navset_card_underline(
        nav_panel(
          "Shift Hours",
          withSpinner(DTOutput("table_shift_hrs"), type = 6, color = "#2c3e50")
        ),
        nav_panel(
          "Non-Work Hours",
          withSpinner(DTOutput("table_non_wrk_hrs"), type = 6, color = "#2c3e50")
        ),
        nav_panel(
          "Meal Period Distribution",
          withSpinner(DTOutput("table_meal_period"), type = 6, color = "#2c3e50")
        ),
        nav_panel(
          "Meal Start Times",
          withSpinner(DTOutput("table_meal_start_time"), type = 6, color = "#2c3e50")
        ),
        nav_panel(
          "Meal Quarter Hour",
          withSpinner(DTOutput("table_meal_quarter_hr"), type = 6, color = "#2c3e50")
        ),

        # =======================================================================
        # NOTES & ASSUMPTIONS (with version info and detailed methodology)
        # =======================================================================

        nav_panel(
          "Notes & Assumptions",

          card(
            card_header("Version Information & Detailed Analysis Methodology"),
            card_body(
              div(
                style = "line-height: 1.8;",
                h4("Version Information"),
                p(strong("Dashboard Version: "), textOutput("dashboard_version", inline = TRUE)),
                p(strong("Last Updated: "), textOutput("last_updated", inline = TRUE)),
                hr(),
                uiOutput("general_assumptions_content")
              )
            )
          )
        ),

        # Full Log subtab
        nav_panel(
          "Full Console Log",

          card(
            card_header("Complete Analysis Log"),
            card_body(
              style = "background-color: #f8f9fa;",
              downloadButton("download_log", "Download Log File", class = "btn-sm mb-3"),
              verbatimTextOutput("full_log", placeholder = TRUE)
            )
          )
        )
      )
    ),

    nav_spacer(),

    nav_item(
      tags$span(
        style = "color: white; padding: 8px;",
        icon("info-circle"),
        " ", Sys.Date()
      )
    )
  )
}

# ---- SERVER ----

server <- function(data_list, metric_spec, analysis_tables, metric_group_categories) {
  function(input, output, session) {

    cat("[APP] Shiny server initialized. Waiting for browser session...\n")
    flush.console()
    session$onFlushed(function() {
      cat("[APP] Browser session connected. Rendering dashboard...\n")
      flush.console()
    }, once = TRUE)

    # Cache storage for expensive operations
    cache <- reactiveValues(
      filtered_data_key = NULL,
      filtered_data_value = NULL,
      pipeline_results_key = NULL,
      pipeline_results_value = NULL
    )

    # Extract pre-computed metric group categories
    list2env(metric_group_categories, envir = environment())

    metric_scenarios <- if ("scenario" %in% names(metric_spec)) {
      normalize_scenario_value(metric_spec$scenario)
    } else {
      character(0)
    }

    available_scenarios <- character(0)
    if (!("scenario" %in% names(metric_spec))) {
      available_scenarios <- c("no waivers", "waivers")
    } else {
      if (any(metric_scenarios == "no waivers", na.rm = TRUE)) available_scenarios <- c(available_scenarios, "no waivers")
      if (any(metric_scenarios == "waivers", na.rm = TRUE)) available_scenarios <- c(available_scenarios, "waivers")
      if (any(metric_scenarios == "hybrid", na.rm = TRUE)) available_scenarios <- c(available_scenarios, "hybrid")
      if (length(available_scenarios) == 0) available_scenarios <- c("no waivers", "waivers")
    }

    show_no_waivers <- "no waivers" %in% available_scenarios
    show_waivers <- "waivers" %in% available_scenarios
    show_hybrid <- "hybrid" %in% available_scenarios
    # Original date range
    pay_date_col <- if ("Pay_Period_End" %in% names(data_list$pay1)) {
      "Pay_Period_End"
    } else if ("Pay_Date" %in% names(data_list$pay1)) {
      "Pay_Date"
    } else {
      NULL
    }

    original_date_min <- min(
      min(data_list$shift_data1$Date, na.rm = TRUE),
      min(if (!is.null(pay_date_col)) data_list$pay1[[pay_date_col]] else as.Date(NA), na.rm = TRUE),
      na.rm = TRUE
    )
    original_date_max <- max(
      max(data_list$shift_data1$Date, na.rm = TRUE),
      max(if (!is.null(pay_date_col)) data_list$pay1[[pay_date_col]] else as.Date(NA), na.rm = TRUE),
      na.rm = TRUE
    )

    # Employee filter choices: server-side selectize to avoid pushing large lists to browser
    time_ids <- if (!is.null(data_list$shift_data1) && "ID" %in% names(data_list$shift_data1)) unique(data_list$shift_data1$ID) else character(0)
    pay_ids <- if (!is.null(data_list$pay1) && "Pay_ID" %in% names(data_list$pay1)) unique(data_list$pay1$Pay_ID) else character(0)
    class_ids <- if (!is.null(data_list$class1) && "Class_ID" %in% names(data_list$class1)) unique(data_list$class1$Class_ID) else character(0)
    all_employee_ids <- unique(c(as.character(time_ids), as.character(pay_ids), as.character(class_ids)))
    all_employee_ids <- all_employee_ids[nzchar(all_employee_ids)]
    updateSelectizeInput(
      session, "employee_filter",
      choices = all_employee_ids,
      options = list(placeholder = "All employees"),
      server = TRUE
    )

    # Startup status notification for large datasets
    observeEvent(TRUE, {
      showNotification(
        "Loading filters and initializing dashboard for large datasets...",
        type = "message",
        duration = 6
      )
    }, once = TRUE)

    # Apply dynamic font styling
    observeEvent(input$font_size, {
      req(input$font_size)

      font_css <- sprintf(
        "body, .dataTables_wrapper, .value-box, .card, .sidebar, h1, h2, h3, h4, h5, p, span { font-size: %s !important; }",
        input$font_size
      )

      shinyjs::runjs(sprintf(
        "$('#custom-font-style').remove(); $('head').append('<style id=\"custom-font-style\">%s</style>');",
        font_css
      ))
    }, ignoreNULL = TRUE, ignoreInit = FALSE)

    # Current filters
    current_filters <- reactiveVal(list())

    # Debounce filter inputs to reduce excessive recalculation
    date_range_debounced <- debounce(reactive(input$date_range), 500)
    employee_filter_debounced <- debounce(reactive(input$employee_filter), 300)
    subclass_filter_debounced <- debounce(reactive(input$subclass_filter), 300)

    # Show/hide filter banner
    observe({
      filters <- current_filters()
      if (length(filters) > 0) shinyjs::show("filter_banner") else shinyjs::hide("filter_banner")
    })

    # Reset dates
    observeEvent(input$reset_dates, {
      updateDateRangeInput(session, "date_range", start = original_date_min, end = original_date_max)
    })

    # Apply filters
    observeEvent(input$apply_filters, {
      filters <- list(
        date_min = input$date_range[1],
        date_max = input$date_range[2]
      )

      # Employee ID filter
      if (length(input$employee_filter) > 0) {
        filters$ID <- input$employee_filter
        filters$Pay_ID <- input$employee_filter
        filters$Class_ID <- input$employee_filter
      }

      # Sample filter (multi-select)
      if (length(input$sample_filter) > 0) {
        filters$Sample <- input$sample_filter
        filters$Pay_Sample <- input$sample_filter
      }

      # Subclass filter (multi-select)
      if (length(input$subclass_filter) > 0) {
        filters$Subclass <- input$subclass_filter
        filters$Pay_Subclass <- input$subclass_filter
        filters$Class_Subclass <- input$subclass_filter
      }

      # Key Groups filter (multi-select)
      if (length(input$key_groups_filter) > 0) {
        filters$Key_Gps <- input$key_groups_filter
        filters$Pay_Key_Gps <- input$key_groups_filter
        filters$Class_Key_Gps <- input$key_groups_filter
      }

      # Department filter (multi-select)
      if (length(input$department_filter) > 0) {
        filters$Department <- input$department_filter
        filters$Pay_Department <- input$department_filter
        filters$Class_Department <- input$department_filter
      }

      # Location filter (multi-select)
      if (length(input$location_filter) > 0) {
        filters$Location <- input$location_filter
        filters$Pay_Location <- input$location_filter
        filters$Class_Location <- input$location_filter
      }

      # Department filter (multi-select)
      if (length(input$department_filter) > 0) {
        filters$Department <- input$department_filter
        filters$Pay_Department <- input$department_filter
        filters$Class_Department <- input$department_filter
      }

      # Location filter (multi-select)
      if (length(input$location_filter) > 0) {
        filters$Location <- input$location_filter
        filters$Pay_Location <- input$location_filter
        filters$Class_Location <- input$location_filter
      }

      # Department filter (multi-select)
      if (length(input$department_filter) > 0) {
        filters$Department <- input$department_filter
        filters$Pay_Department <- input$department_filter
        filters$Class_Department <- input$department_filter
      }

      # Location filter (multi-select)
      if (length(input$location_filter) > 0) {
        filters$Location <- input$location_filter
        filters$Pay_Location <- input$location_filter
        filters$Class_Location <- input$location_filter
      }

      current_filters(filters)
    })

    # Reset filters
    observeEvent(input$reset_filters, {
      updateDateRangeInput(session, "date_range", start = original_date_min, end = original_date_max)
      updateSelectizeInput(session, "employee_filter", selected = character(0))
      updateSelectizeInput(session, "sample_filter", selected = character(0))
      updateSelectizeInput(session, "subclass_filter", selected = character(0))
      updateSelectizeInput(session, "key_groups_filter", selected = character(0))
      updateSelectizeInput(session, "department_filter", selected = character(0))
      updateSelectizeInput(session, "location_filter", selected = character(0))
      current_filters(list())
    })

    build_pdf_section_choices <- function() {
      choices <- c(
        "Data Comparison (1-Page Landscape)" = "data_comparison",
        "Summary - Time Data" = "time_summary",
        "Summary - Pay Data" = "pay_summary",
        "Meal Period Analysis" = "meal_analysis"
      )

      if (show_no_waivers) {
        choices <- c(choices, "Meal Period Violations (No Waivers)" = "meal_violations_no_waivers")
      }
      if (show_waivers) {
        choices <- c(choices, "Meal Period Violations (Waivers)" = "meal_violations_waivers")
      }
      if (show_hybrid) {
        choices <- c(choices, "Meal Period Violations (Hybrid)" = "meal_violations_hybrid")
      }

      choices <- c(
        choices,
        "Rest Period Analysis" = "rest_analysis",
        "Shift Hours Analysis" = "shift_hours",
        "Time Punch Rounding" = "time_rounding",
        "Regular Rate - Bonuses" = "regular_rate_bonuses",
        "Regular Rate - Differentials" = "regular_rate_differentials",
        "Regular Rate - RROP" = "regular_rate_rrop"
      )

      if (show_no_waivers) {
        choices <- c(choices, "Class Damages (No Waivers)" = "class_damages_no_waivers")
      }
      if (show_waivers) {
        choices <- c(choices, "Class Damages (Waivers)" = "class_damages_waivers")
      }
      if (show_hybrid) {
        choices <- c(choices, "Class Damages (Hybrid)" = "class_damages_hybrid")
      }

      choices <- c(
        choices,
        "Damages - Wage Statement Penalties" = "damages_wsv",
        "Damages - Waiting Time Penalties" = "damages_wt",
        "PAGA - Summary" = "paga_summary"
      )

      if (length(damages_detail_unique) > 0) {
        for (detail_group in damages_detail_unique) {
          safe_group <- tolower(gsub("[^a-z0-9]+", "_", sub("^Damages - ", "", detail_group)))
          group_scenarios <- character(0)

          if ("scenario" %in% names(metric_spec)) {
            group_scenarios <- normalize_scenario_value(metric_spec$scenario[metric_spec$metric_group == detail_group])
            group_scenarios <- unique(group_scenarios[!is.na(group_scenarios) & nzchar(group_scenarios)])
          }

          split_scenarios <- intersect(c("no waivers", "waivers", "hybrid"), group_scenarios)

          if (length(split_scenarios) > 0) {
            for (scenario in available_scenarios) {
              if (scenario %in% split_scenarios) {
                scenario_key <- gsub(" ", "_", scenario)
                key <- paste0("damages_", safe_group, "_", scenario_key)
                label <- paste0(detail_group, " (", tools::toTitleCase(scenario), ")")
                choices <- c(choices, setNames(key, label))
              }
            }
          } else {
            key <- paste0("damages_", safe_group)
            choices <- c(choices, setNames(key, detail_group))
          }
        }
      }

      if (length(paga_detail_unique) > 0) {
        for (detail_group in paga_detail_unique) {
          safe_group <- tolower(gsub("[^a-z0-9]+", "_", sub("^PAGA - ", "", detail_group)))
          group_scenarios <- character(0)

          if ("scenario" %in% names(metric_spec)) {
            group_scenarios <- normalize_scenario_value(metric_spec$scenario[metric_spec$metric_group == detail_group])
            group_scenarios <- unique(group_scenarios[!is.na(group_scenarios) & nzchar(group_scenarios)])
          }

          split_scenarios <- intersect(c("no waivers", "waivers", "hybrid"), group_scenarios)

          if (length(split_scenarios) > 0) {
            for (scenario in available_scenarios) {
              if (scenario %in% split_scenarios) {
                scenario_key <- gsub(" ", "_", scenario)
                key <- paste0("paga_", safe_group, "_", scenario_key)
                label <- paste0(detail_group, " (", tools::toTitleCase(scenario), ")")
                choices <- c(choices, setNames(key, label))
              }
            }
          } else {
            key <- paste0("paga_", safe_group)
            choices <- c(choices, setNames(key, detail_group))
          }
        }
      }

      choices <- c(
        choices,
        "Pay Codes" = "pay_codes",
        "Rate Type Analysis" = "rate_type_analysis",
        "Appendix Tables" = "appendix",
        "Notes & Assumptions" = "assumptions"
      )

      choices[!duplicated(unname(choices))]
    }

    # PDF Modal Dialog
    observeEvent(input$open_pdf_modal, {
      pdf_choices <- build_pdf_section_choices()
      showModal(modalDialog(
        title = div(
          style = "font-size: 20px; font-weight: bold;",
          icon("file-pdf", style = "margin-right: 10px; color: #667eea;"),
          "PDF Export Options"
        ),
        size = "xl",
        easyClose = TRUE,
        footer = div(
          style = "display: flex; justify-content: flex-end; align-items: center; padding: 10px;",
          modalButton("Cancel"),
          downloadButton("download_pdf", "Generate PDF",
                         class = "btn-primary",
                         icon = icon("file-pdf"),
                         style = "margin-left: 10px;
                                  background: linear-gradient(135deg, #90EE90 0%, #3CB371 50%, #2E8B57 100%);
                                  border: none;
                                  font-weight: bold;
                                  box-shadow: 0 4px 6px rgba(0,0,0,0.15), inset 0 1px 0 rgba(255,255,255,0.4);
                                  color: white;",
                         onclick = "Shiny.setInputValue('pdf_download_clicked', Date.now(), {priority: 'event'});")
        ),

        # PDF Export Content
        div(
          style = "max-height: 70vh; overflow-y: auto; padding: 20px;",

          h5(style = "color: #2c3e50; margin-bottom: 15px;", icon("file-pdf"), " Select PDF Sections"),
          p(style = "color: #7f8c8d; margin-bottom: 20px; font-size: 14px;", "Choose which sections to include in your PDF report (in order of appearance):"),

          checkboxGroupInput(
            "pdf_sections",
            NULL,
            choices = pdf_choices,
            selected = unname(pdf_choices)
          ),

          hr(),

          h5(style = "color: #2c3e50; margin-bottom: 15px;", icon("cog"), " Additional Options"),
          checkboxInput("pdf_include_extrap", "Include Extrapolation Column", value = TRUE),
          checkboxInput("pdf_include_credits", "Include Credit-Adjusted Metrics", value = TRUE),
          checkboxInput("pdf_include_less_prems", "Include Less-Premiums-Adjusted Metrics", value = TRUE),
          checkboxInput("pdf_include_no_waivers", "Include No-Waivers Scenario", value = "no waivers" %in% available_scenarios),
          checkboxInput("pdf_include_waivers", "Include Waivers Scenario", value = "waivers" %in% available_scenarios),
          checkboxInput("pdf_include_hybrid", "Include Hybrid Scenario", value = "hybrid" %in% available_scenarios),

          hr(),

          div(
            style = "text-align: center; margin-top: 20px;",
            actionButton("pdf_select_all", "Select All", class = "btn btn-sm btn-outline-primary", style = "margin-right: 10px;"),
            actionButton("pdf_deselect_all", "Deselect All", class = "btn btn-sm btn-outline-secondary")
          )
        )
      ))
    })

    # PDF Select All button
    observeEvent(input$pdf_select_all, {
      all_sections <- unname(build_pdf_section_choices())
      updateCheckboxGroupInput(session, "pdf_sections", selected = all_sections)
      updateCheckboxInput(session, "pdf_include_extrap", value = TRUE)
      updateCheckboxInput(session, "pdf_include_credits", value = TRUE)
      updateCheckboxInput(session, "pdf_include_less_prems", value = TRUE)
      updateCheckboxInput(session, "pdf_include_no_waivers", value = "no waivers" %in% available_scenarios)
      updateCheckboxInput(session, "pdf_include_waivers", value = "waivers" %in% available_scenarios)
      updateCheckboxInput(session, "pdf_include_hybrid", value = "hybrid" %in% available_scenarios)
    })

    # PDF Deselect All button
    observeEvent(input$pdf_deselect_all, {
      updateCheckboxGroupInput(session, "pdf_sections", selected = character(0))
      updateCheckboxInput(session, "pdf_include_extrap", value = FALSE)
      updateCheckboxInput(session, "pdf_include_credits", value = FALSE)
      updateCheckboxInput(session, "pdf_include_less_prems", value = FALSE)
      updateCheckboxInput(session, "pdf_include_no_waivers", value = FALSE)
      updateCheckboxInput(session, "pdf_include_waivers", value = FALSE)
      updateCheckboxInput(session, "pdf_include_hybrid", value = FALSE)
    })

    observeEvent(input$pdf_download_clicked, {
      removeModal()
    }, ignoreInit = TRUE)

    # Toggle extrapolation columns
    observeEvent(input$toggle_extrap_cols, {
      session$sendCustomMessage('toggleExtrapCols', input$toggle_extrap_cols)
    }, ignoreInit = FALSE)

    # Dynamically show/hide scenario subtabs based on sidebar scenario checkboxes
    observe({
      no_waivers_on <- isTRUE(input$show_scenario_no_waivers)
      waivers_on <- isTRUE(input$show_scenario_waivers)
      hybrid_on <- isTRUE(input$show_scenario_hybrid)

      session$sendCustomMessage('toggleScenarioTabs', list(
        tabs = list(
          time_meal_no_waivers_tab = no_waivers_on,
          time_meal_waivers_tab = waivers_on,
          time_meal_hybrid_tab = hybrid_on,
          class_no_waivers_tab = no_waivers_on,
          class_waivers_tab = waivers_on,
          class_hybrid_tab = hybrid_on,
          paga_no_waivers_tab = no_waivers_on,
          paga_waivers_tab = waivers_on,
          paga_hybrid_tab = hybrid_on
        ),
        fallbacks = list(
          time_meal_no_waivers_tab = "time_summary_tab",
          time_meal_waivers_tab = "time_summary_tab",
          time_meal_hybrid_tab = "time_summary_tab",
          class_no_waivers_tab = "class_overview_tab",
          class_waivers_tab = "class_overview_tab",
          class_hybrid_tab = "class_overview_tab",
          paga_no_waivers_tab = "paga_overview_tab",
          paga_waivers_tab = "paga_overview_tab",
          paga_hybrid_tab = "paga_overview_tab"
        )
      ))
    })

    # Filtered data with precomputed metadata
    filtered_data <- reactive({
      filters <- current_filters()

      # Check cache first
      cache_key <- digest(filters, algo = "xxhash64")
      if (!is.null(cache$filtered_data_key) && cache$filtered_data_key == cache_key) {
        return(cache$filtered_data_value)
      }

      # Validate required data exists
      req(data_list$shift_data1, data_list$pay1)

      shift_filtered <- data_list$shift_data1
      pay_filtered   <- data_list$pay1
      time_filtered  <- data_list$time1
      pay_date_col <- if ("Pay_Period_End" %in% names(pay_filtered)) {
        "Pay_Period_End"
      } else if ("Pay_Date" %in% names(pay_filtered)) {
        "Pay_Date"
      } else {
        NULL
      }

      # Shift filters
      if (!is.null(filters$date_min)) shift_filtered <- shift_filtered[Date >= filters$date_min]
      if (!is.null(filters$date_max)) shift_filtered <- shift_filtered[Date <= filters$date_max]
      if (!is.null(filters$ID))       shift_filtered <- shift_filtered[ID %in% filters$ID]

      # Sample filter - check any column containing "Sample"
      if (!is.null(filters$Sample)) {
        sample_cols <- grep("Sample", names(shift_filtered), ignore.case = TRUE, value = TRUE)
        if (length(sample_cols) > 0) {
          # Match if ANY sample column matches the filter
          matches <- Reduce(`|`, lapply(sample_cols, function(col) shift_filtered[[col]] %in% filters$Sample))
          shift_filtered <- shift_filtered[matches]
        }
      }

      # Subclass filter - check any column containing "Subclass"
      if (!is.null(filters$Subclass)) {
        subclass_cols <- grep("Subclass", names(shift_filtered), ignore.case = TRUE, value = TRUE)
        if (length(subclass_cols) > 0) {
          matches <- Reduce(`|`, lapply(subclass_cols, function(col) shift_filtered[[col]] %in% filters$Subclass))
          shift_filtered <- shift_filtered[matches]
        }
      }

      # Key Groups filter - check any column containing "Key_Gps"
      if (!is.null(filters$Key_Gps)) {
        key_cols <- grep("Key_Gps", names(shift_filtered), ignore.case = TRUE, value = TRUE)
        if (length(key_cols) > 0) {
          matches <- Reduce(`|`, lapply(key_cols, function(col) shift_filtered[[col]] %in% filters$Key_Gps))
          shift_filtered <- shift_filtered[matches]
        }
      }

      # Department filter - check any column containing "Department"
      if (!is.null(filters$Department)) {
        dept_cols <- grep("Department", names(shift_filtered), ignore.case = TRUE, value = TRUE)
        if (length(dept_cols) > 0) {
          matches <- Reduce(`|`, lapply(dept_cols, function(col) shift_filtered[[col]] %in% filters$Department))
          shift_filtered <- shift_filtered[matches]
        }
      }

      # Location filter - check any column containing "Location"
      if (!is.null(filters$Location)) {
        loc_cols <- grep("Location", names(shift_filtered), ignore.case = TRUE, value = TRUE)
        if (length(loc_cols) > 0) {
          matches <- Reduce(`|`, lapply(loc_cols, function(col) shift_filtered[[col]] %in% filters$Location))
          shift_filtered <- shift_filtered[matches]
        }
      }

      # time1 filters
      if (!is.null(time_filtered)) {
        if (!is.null(filters$date_min) && "Date" %in% names(time_filtered)) time_filtered <- time_filtered[Date >= filters$date_min]
        if (!is.null(filters$date_max) && "Date" %in% names(time_filtered)) time_filtered <- time_filtered[Date <= filters$date_max]
        if (!is.null(filters$ID) && "ID" %in% names(time_filtered)) time_filtered <- time_filtered[ID %in% filters$ID]

        if (!is.null(filters$Sample)) {
          sample_cols <- grep("Sample", names(time_filtered), ignore.case = TRUE, value = TRUE)
          if (length(sample_cols) > 0) {
            matches <- Reduce(`|`, lapply(sample_cols, function(col) time_filtered[[col]] %in% filters$Sample))
            time_filtered <- time_filtered[matches]
          }
        }

        if (!is.null(filters$Subclass)) {
          subclass_cols <- grep("Subclass", names(time_filtered), ignore.case = TRUE, value = TRUE)
          if (length(subclass_cols) > 0) {
            matches <- Reduce(`|`, lapply(subclass_cols, function(col) time_filtered[[col]] %in% filters$Subclass))
            time_filtered <- time_filtered[matches]
          }
        }

        if (!is.null(filters$Key_Gps)) {
          key_cols <- grep("Key_Gps", names(time_filtered), ignore.case = TRUE, value = TRUE)
          if (length(key_cols) > 0) {
            matches <- Reduce(`|`, lapply(key_cols, function(col) time_filtered[[col]] %in% filters$Key_Gps))
            time_filtered <- time_filtered[matches]
          }
        }

        if (!is.null(filters$Department)) {
          dept_cols <- grep("Department", names(time_filtered), ignore.case = TRUE, value = TRUE)
          if (length(dept_cols) > 0) {
            matches <- Reduce(`|`, lapply(dept_cols, function(col) time_filtered[[col]] %in% filters$Department))
            time_filtered <- time_filtered[matches]
          }
        }

        if (!is.null(filters$Location)) {
          loc_cols <- grep("Location", names(time_filtered), ignore.case = TRUE, value = TRUE)
          if (length(loc_cols) > 0) {
            matches <- Reduce(`|`, lapply(loc_cols, function(col) time_filtered[[col]] %in% filters$Location))
            time_filtered <- time_filtered[matches]
          }
        }
      }

      # Pay filters
      if (!is.null(filters$date_min) && !is.null(pay_date_col)) {
        pay_filtered <- pay_filtered[get(pay_date_col) >= filters$date_min]
      }
      if (!is.null(filters$date_max) && !is.null(pay_date_col)) {
        pay_filtered <- pay_filtered[get(pay_date_col) <= filters$date_max]
      }
      if (!is.null(filters$Pay_ID))   pay_filtered <- pay_filtered[Pay_ID %in% filters$Pay_ID]

      # Sample filter - check any column containing "Sample"
      if (!is.null(filters$Sample)) {
        sample_cols <- grep("Sample", names(pay_filtered), ignore.case = TRUE, value = TRUE)
        if (length(sample_cols) > 0) {
          matches <- Reduce(`|`, lapply(sample_cols, function(col) pay_filtered[[col]] %in% filters$Sample))
          pay_filtered <- pay_filtered[matches]
        }
      }

      # Subclass filter - check any column containing "Subclass"
      if (!is.null(filters$Subclass)) {
        subclass_cols <- grep("Subclass", names(pay_filtered), ignore.case = TRUE, value = TRUE)
        if (length(subclass_cols) > 0) {
          matches <- Reduce(`|`, lapply(subclass_cols, function(col) pay_filtered[[col]] %in% filters$Subclass))
          pay_filtered <- pay_filtered[matches]
        }
      }

      # Key Groups filter - check any column containing "Key_Gps"
      if (!is.null(filters$Key_Gps)) {
        key_cols <- grep("Key_Gps", names(pay_filtered), ignore.case = TRUE, value = TRUE)
        if (length(key_cols) > 0) {
          matches <- Reduce(`|`, lapply(key_cols, function(col) pay_filtered[[col]] %in% filters$Key_Gps))
          pay_filtered <- pay_filtered[matches]
        }
      }

      # Department filter - check any column containing "Department"
      if (!is.null(filters$Department)) {
        dept_cols <- grep("Department", names(pay_filtered), ignore.case = TRUE, value = TRUE)
        if (length(dept_cols) > 0) {
          matches <- Reduce(`|`, lapply(dept_cols, function(col) pay_filtered[[col]] %in% filters$Department))
          pay_filtered <- pay_filtered[matches]
        }
      }

      # Location filter - check any column containing "Location"
      if (!is.null(filters$Location)) {
        loc_cols <- grep("Location", names(pay_filtered), ignore.case = TRUE, value = TRUE)
        if (length(loc_cols) > 0) {
          matches <- Reduce(`|`, lapply(loc_cols, function(col) pay_filtered[[col]] %in% filters$Location))
          pay_filtered <- pay_filtered[matches]
        }
      }

      # pp_data1
      pp_filtered <- NULL
      if (!is.null(data_list$pp_data1)) {
        pp_filtered <- data_list$pp_data1
        if (!is.null(filters$date_min) && "Period_End" %in% names(pp_filtered)) pp_filtered <- pp_filtered[Period_End >= filters$date_min]
        if (!is.null(filters$date_max) && "Period_End" %in% names(pp_filtered)) pp_filtered <- pp_filtered[Period_End <= filters$date_max]
        if (!is.null(filters$ID)       && "ID" %in% names(pp_filtered))        pp_filtered <- pp_filtered[ID %in% filters$ID]

        # Apply all categorical filters to pp_data1
        if (!is.null(filters$Sample)) {
          sample_cols <- grep("Sample", names(pp_filtered), ignore.case = TRUE, value = TRUE)
          if (length(sample_cols) > 0) {
            matches <- Reduce(`|`, lapply(sample_cols, function(col) pp_filtered[[col]] %in% filters$Sample))
            pp_filtered <- pp_filtered[matches]
          }
        }

        if (!is.null(filters$Subclass)) {
          subclass_cols <- grep("Subclass", names(pp_filtered), ignore.case = TRUE, value = TRUE)
          if (length(subclass_cols) > 0) {
            matches <- Reduce(`|`, lapply(subclass_cols, function(col) pp_filtered[[col]] %in% filters$Subclass))
            pp_filtered <- pp_filtered[matches]
          }
        }

        if (!is.null(filters$Key_Gps)) {
          key_cols <- grep("Key_Gps", names(pp_filtered), ignore.case = TRUE, value = TRUE)
          if (length(key_cols) > 0) {
            matches <- Reduce(`|`, lapply(key_cols, function(col) pp_filtered[[col]] %in% filters$Key_Gps))
            pp_filtered <- pp_filtered[matches]
          }
        }

        if (!is.null(filters$Department)) {
          dept_cols <- grep("Department", names(pp_filtered), ignore.case = TRUE, value = TRUE)
          if (length(dept_cols) > 0) {
            matches <- Reduce(`|`, lapply(dept_cols, function(col) pp_filtered[[col]] %in% filters$Department))
            pp_filtered <- pp_filtered[matches]
          }
        }

        if (!is.null(filters$Location)) {
          loc_cols <- grep("Location", names(pp_filtered), ignore.case = TRUE, value = TRUE)
          if (length(loc_cols) > 0) {
            matches <- Reduce(`|`, lapply(loc_cols, function(col) pp_filtered[[col]] %in% filters$Location))
            pp_filtered <- pp_filtered[matches]
          }
        }
      }

      # ee_data1
      ee_filtered <- NULL
      if (!is.null(data_list$ee_data1)) {
        ee_filtered <- data_list$ee_data1
        if (!is.null(filters$ID) && "ID" %in% names(ee_filtered)) ee_filtered <- ee_filtered[ID %in% filters$ID]

        # Apply all categorical filters to ee_data1
        if (!is.null(filters$Sample)) {
          sample_cols <- grep("Sample", names(ee_filtered), ignore.case = TRUE, value = TRUE)
          if (length(sample_cols) > 0) {
            matches <- Reduce(`|`, lapply(sample_cols, function(col) ee_filtered[[col]] %in% filters$Sample))
            ee_filtered <- ee_filtered[matches]
          }
        }

        if (!is.null(filters$Subclass)) {
          subclass_cols <- grep("Subclass", names(ee_filtered), ignore.case = TRUE, value = TRUE)
          if (length(subclass_cols) > 0) {
            matches <- Reduce(`|`, lapply(subclass_cols, function(col) ee_filtered[[col]] %in% filters$Subclass))
            ee_filtered <- ee_filtered[matches]
          }
        }

        if (!is.null(filters$Key_Gps)) {
          key_cols <- grep("Key_Gps", names(ee_filtered), ignore.case = TRUE, value = TRUE)
          if (length(key_cols) > 0) {
            matches <- Reduce(`|`, lapply(key_cols, function(col) ee_filtered[[col]] %in% filters$Key_Gps))
            ee_filtered <- ee_filtered[matches]
          }
        }

        if (!is.null(filters$Department)) {
          dept_cols <- grep("Department", names(ee_filtered), ignore.case = TRUE, value = TRUE)
          if (length(dept_cols) > 0) {
            matches <- Reduce(`|`, lapply(dept_cols, function(col) ee_filtered[[col]] %in% filters$Department))
            ee_filtered <- ee_filtered[matches]
          }
        }

        if (!is.null(filters$Location)) {
          loc_cols <- grep("Location", names(ee_filtered), ignore.case = TRUE, value = TRUE)
          if (length(loc_cols) > 0) {
            matches <- Reduce(`|`, lapply(loc_cols, function(col) ee_filtered[[col]] %in% filters$Location))
            ee_filtered <- ee_filtered[matches]
          }
        }
      }

      # class1
      class_filtered <- NULL
      if (!is.null(data_list$class1)) {
        class_filtered <- data_list$class1

        if (!is.null(filters$ID) && "Class_ID" %in% names(class_filtered)) {
          class_filtered <- class_filtered[Class_ID %in% filters$ID]
        }
        if (!is.null(filters$Pay_ID) && "Class_ID" %in% names(class_filtered)) {
          class_filtered <- class_filtered[Class_ID %in% filters$Pay_ID]
        }

        # Apply all categorical filters to class1
        if (!is.null(filters$Sample)) {
          sample_cols <- grep("Sample", names(class_filtered), ignore.case = TRUE, value = TRUE)
          if (length(sample_cols) > 0) {
            matches <- Reduce(`|`, lapply(sample_cols, function(col) class_filtered[[col]] %in% filters$Sample))
            class_filtered <- class_filtered[matches]
          }
        }

        if (!is.null(filters$Subclass)) {
          subclass_cols <- grep("Subclass", names(class_filtered), ignore.case = TRUE, value = TRUE)
          if (length(subclass_cols) > 0) {
            matches <- Reduce(`|`, lapply(subclass_cols, function(col) class_filtered[[col]] %in% filters$Subclass))
            class_filtered <- class_filtered[matches]
          }
        }

        if (!is.null(filters$Key_Gps)) {
          key_cols <- grep("Key_Gps", names(class_filtered), ignore.case = TRUE, value = TRUE)
          if (length(key_cols) > 0) {
            matches <- Reduce(`|`, lapply(key_cols, function(col) class_filtered[[col]] %in% filters$Key_Gps))
            class_filtered <- class_filtered[matches]
          }
        }

        if (!is.null(filters$Department)) {
          dept_cols <- grep("Department", names(class_filtered), ignore.case = TRUE, value = TRUE)
          if (length(dept_cols) > 0) {
            matches <- Reduce(`|`, lapply(dept_cols, function(col) class_filtered[[col]] %in% filters$Department))
            class_filtered <- class_filtered[matches]
          }
        }

        if (!is.null(filters$Location)) {
          loc_cols <- grep("Location", names(class_filtered), ignore.case = TRUE, value = TRUE)
          if (length(loc_cols) > 0) {
            matches <- Reduce(`|`, lapply(loc_cols, function(col) class_filtered[[col]] %in% filters$Location))
            class_filtered <- class_filtered[matches]
          }
        }
      }

      shift_years <- if (nrow(shift_filtered) > 0 && "Date" %in% names(shift_filtered)) sort(unique(year(shift_filtered$Date))) else NULL
      pay_years   <- if (nrow(pay_filtered) > 0 && !is.null(pay_date_col)) {
        sort(unique(year(pay_filtered[[pay_date_col]])))
      } else {
        NULL
      }

      shift_key_groups <- if ("Key_Gps" %in% names(shift_filtered)) {
        gps <- unique(shift_filtered$Key_Gps)
        gps <- gps[!is.na(gps) & gps != "" & tolower(gps) != "everyone else"]
        sort(gps)
      } else NULL

      pay_key_groups <- if ("Pay_Key_Gps" %in% names(pay_filtered)) {
        gps <- unique(pay_filtered$Pay_Key_Gps)
        gps <- gps[!is.na(gps) & gps != "" & tolower(gps) != "everyone else"]
        sort(gps)
      } else NULL

      result <- list(
        shift_data1 = shift_filtered,
        time1 = time_filtered,
        pay1 = pay_filtered,
        pp_data1 = pp_filtered,
        ee_data1 = ee_filtered,
        class1 = class_filtered,
        shift_years = shift_years,
        pay_years = pay_years,
        shift_key_groups = shift_key_groups,
        pay_key_groups = pay_key_groups
      )

      # Store in cache
      cache$filtered_data_key <- cache_key
      cache$filtered_data_value <- result

      result
    })

    # Calculate extrapolation environment (cached separately)
    extrap_environment <- reactive({
      # If extrapolation values were calculated in analysis.R, use those
      # Otherwise fall back to calculating from filtered data
      if (!is.null(extrap_values)) {
        return(extrap_values)
      }

      # Fallback: calculate from filtered data (will be wrong if temporal extrapolation applies)
      data <- filtered_data()
      req(data$shift_data1)

      list(
        # Use full class list for employee count (not filtered data)
        extrap_class_ees = if (!is.null(data$class1) && "Class_ID" %in% names(data$class1)) {
          uniqueN(data$class1$Class_ID, na.rm = TRUE)
        } else {
          uniqueN(data$shift_data1$ID, na.rm = TRUE)
        },

        # Extrapolated pay periods based on data
        extrap_class_pps = if (!is.null(data$pp_data1) && "ID_Period_End" %in% names(data$pp_data1)) {
          uniqueN(data$pp_data1$ID_Period_End, na.rm = TRUE)
        } else {
          0
        },

        # Extrapolated weeks based on data
        extrap_class_wks = if (!is.null(data$shift_data1) && "week" %in% names(data$shift_data1)) {
          sum(data$shift_data1$week, na.rm = TRUE)
        } else {
          0
        },

        # Extrapolated shifts based on data
        extrap_class_shifts = if (!is.null(data$shift_data1) && "ID_Shift" %in% names(data$shift_data1)) {
          uniqueN(data$shift_data1$ID_Shift, na.rm = TRUE)
        } else {
          0
        }
      )
    })

    # Run metrics pipeline once with all data and cache results
    pipeline_results <- reactive({
      data <- filtered_data()

      # Check cache first
      cache_key <- digest(list(current_filters(), extrap_environment()), algo = "xxhash64")
      if (!is.null(cache$pipeline_results_key) && cache$pipeline_results_key == cache_key) {
        return(cache$pipeline_results_value)
      }

      # Build custom filters for Key Groups
      custom_filters <- list()

      # Add Key Group filters if they exist
      if (!is.null(data$shift_key_groups) && length(data$shift_key_groups) > 0) {
        for (kg in data$shift_key_groups) {
          custom_filters[[kg]] <- list(
            time_filter = bquote(Key_Gps == .(kg)),
            pay_filter  = bquote(Pay_Key_Gps == .(kg)),
            pp_filter   = bquote(Key_Gps == .(kg)),
            ee_filter   = bquote(Key_Gps == .(kg))
          )
        }
      }

      # Run the pipeline with all data sources and extrapolation environment
      results <- run_metrics_pipeline(
        time_dt = data$shift_data1,
        pay_dt = data$pay1,
        spec = metric_spec,
        pp_dt = data$pp_data1,
        ee_dt = data$ee_data1,
        custom_filters = custom_filters,
        extrap_env = extrap_environment()  # Use cached reactive
      )

      # Store in cache
      cache$pipeline_results_key <- cache_key
      cache$pipeline_results_value <- results

      results
    })

    # Credit/scenario-filtered pipeline results for display
    selected_display_scenarios <- reactive({
      scenarios <- character(0)
      if (isTRUE(input$show_scenario_no_waivers)) scenarios <- c(scenarios, "no waivers")
      if (isTRUE(input$show_scenario_waivers)) scenarios <- c(scenarios, "waivers")
      if (isTRUE(input$show_scenario_hybrid)) scenarios <- c(scenarios, "hybrid")
      intersect(unique(scenarios), available_scenarios)
    })

    display_results <- reactive({
      results <- pipeline_results()

      if ("scenario" %in% names(results)) {
        selected_scenarios <- selected_display_scenarios()
        sc <- normalize_scenario_value(results$scenario)

        if (length(selected_scenarios) > 0) {
          results <- results[sc %in% c(selected_scenarios, "all") | is.na(results$scenario) | is.na(sc)]
        } else {
          results <- results[is.na(results$scenario) | is.na(sc) | sc == "all"]
        }
      }

      if (!isTRUE(input$show_credits)) {
        results <- results[!get_credit_row_mask(results, metric_spec)]
      }
      if (!isTRUE(input$show_less_prems)) {
        results <- results[!get_less_prems_row_mask(results, metric_spec)]
      }

      results
    })

    # Populate filter choices from pp_data1 (source of truth)
    observe({
      pp <- data_list$pp_data1

      # Helper to find columns containing a pattern (case-insensitive)
      find_cols <- function(pattern) {
        if (is.null(pp)) return(character(0))
        grep(pattern, names(pp), ignore.case = TRUE, value = TRUE)
      }

      # Helper to get unique values from columns
      get_unique_values <- function(cols) {
        if (is.null(pp) || length(cols) == 0) return(character(0))
        vals <- unique(unlist(lapply(cols, function(col) unique(pp[[col]]))))
        vals <- vals[!is.na(vals) & vals != ""]
        sort(vals)
      }

      # Key Groups - include "Everyone Else"
      key_gps_cols <- find_cols("Key_Gps")
      all_key_gps <- get_unique_values(key_gps_cols)

      if (length(all_key_gps) > 0) {
        updateSelectizeInput(session, "key_groups_filter",
                             choices = all_key_gps,
                             options = list(placeholder = "All key groups"),
                             server = TRUE)
      } else {
        updateSelectizeInput(session, "key_groups_filter",
                             choices = character(0),
                             options = list(placeholder = "Key Groups not available"),
                             server = TRUE)
      }

      # Subclass
      subclass_cols <- find_cols("Subclass")
      all_subclass <- get_unique_values(subclass_cols)

      if (length(all_subclass) > 0) {
        updateSelectizeInput(session, "subclass_filter",
                             choices = all_subclass,
                             options = list(placeholder = "All subclasses"),
                             server = TRUE)
      } else {
        updateSelectizeInput(session, "subclass_filter",
                             choices = character(0),
                             options = list(placeholder = "Subclass not available"),
                             server = TRUE)
      }

      # Location
      location_cols <- find_cols("Location")
      all_location <- get_unique_values(location_cols)

      if (length(all_location) > 0) {
        updateSelectizeInput(session, "location_filter",
                             choices = all_location,
                             options = list(placeholder = "All locations"),
                             server = TRUE)
      } else {
        updateSelectizeInput(session, "location_filter",
                             choices = character(0),
                             options = list(placeholder = "Location not available"),
                             server = TRUE)
      }

      # Sample
      sample_cols <- find_cols("Sample")
      all_sample <- get_unique_values(sample_cols)

      if (length(all_sample) > 0) {
        updateSelectizeInput(session, "sample_filter",
                             choices = all_sample,
                             options = list(placeholder = "All samples"),
                             server = TRUE)
      } else {
        updateSelectizeInput(session, "sample_filter",
                             choices = character(0),
                             options = list(placeholder = "Sample not available"),
                             server = TRUE)
      }

      # Department
      dept_cols <- find_cols("Department")
      all_dept <- get_unique_values(dept_cols)

      if (length(all_dept) > 0) {
        updateSelectizeInput(session, "department_filter",
                             choices = all_dept,
                             options = list(placeholder = "All departments"),
                             server = TRUE)
      } else {
        updateSelectizeInput(session, "department_filter",
                             choices = character(0),
                             options = list(placeholder = "Department not available"),
                             server = TRUE)
      }
    })

    # ===========================================================================
    # OVERVIEW OUTPUTS
    # ===========================================================================

    output$total_employees_time <- renderText({
      data <- filtered_data()
      prettyNum(uniqueN(data$shift_data1$ID), big.mark = ",")
    })

    output$total_employees_pay <- renderText({
      data <- filtered_data()
      prettyNum(uniqueN(data$pay1$Pay_ID), big.mark = ",")
    })

    output$total_employees_class <- renderText({
      data <- filtered_data()
      if (!is.null(data$class1) && "Class_ID" %in% names(data$class1)) {
        prettyNum(uniqueN(data$class1$Class_ID), big.mark = ",")
      } else {
        "N/A"
      }
    })

    output$time_pay_periods <- renderText({
      data <- filtered_data()
      prettyNum(uniqueN(data$shift_data1$ID_Period_End), big.mark = ",")
    })

    output$pay_pay_periods <- renderText({
      data <- filtered_data()
      prettyNum(uniqueN(data$pay1$Pay_ID_Period_End), big.mark = ",")
    })

    output$total_weeks <- renderText({
      data <- filtered_data()
      prettyNum(uniqueN(data$shift_data1$ID_Week_End), big.mark = ",")
    })

    output$employee_coverage_plot <- renderPlotly({
      data <- filtered_data()
      format(uniqueN(data$shift_data1$ID_Week_End), big.mark = ",")
    })

    output$employee_coverage_plot <- renderPlotly({
      data <- filtered_data()

      # Aggregate by pay period for smooth line graph
      time_emp <- data$shift_data1[, .(
        Employees = uniqueN(ID),
        Type = "Time Data"
      ), by = .(Period = Period_End)]

      pay_date_col <- if ("Pay_Period_End" %in% names(data$pay1)) {
        "Pay_Period_End"
      } else if ("Pay_Date" %in% names(data$pay1)) {
        "Pay_Date"
      } else {
        NULL
      }

      pay_emp <- if (!is.null(pay_date_col)) {
        data$pay1[, .(
          Employees = uniqueN(Pay_ID),
          Type = "Pay Data"
        ), by = .(Period = get(pay_date_col))]
      } else {
        data.table(Period = as.Date(character()), Employees = integer(), Type = character())
      }

      combined <- rbindlist(list(time_emp, pay_emp), fill = TRUE)
      combined <- combined[order(Period)]

      plot_ly(
        combined,
        x = ~Period, y = ~Employees, color = ~Type,
        type = "scatter", mode = "lines+markers",
        colors = c("Time Data" = "#2c3e50", "Pay Data" = "#27ae60")
      ) %>%
        layout(
          title = "Time & Pay Data Comparison During Relevant Period",
          xaxis = list(title = "Pay Period End Date"),
          yaxis = list(title = "Unique Employees"),
          hovermode = "x unified"
        )
    })

    # Venn diagram data calculation
    venn_data <- reactive({
      data <- filtered_data()

      time_ids <- unique(data$shift_data1$ID)
      pay_ids  <- unique(data$pay1$Pay_ID)
      class_ids <- if (!is.null(data$class1) && "Class_ID" %in% names(data$class1)) unique(data$class1$Class_ID) else character(0)

      time_only  <- setdiff(time_ids, union(pay_ids, class_ids))
      pay_only   <- setdiff(pay_ids,  union(time_ids, class_ids))
      class_only <- setdiff(class_ids, union(time_ids, pay_ids))

      time_pay   <- setdiff(intersect(time_ids, pay_ids), class_ids)
      time_class <- setdiff(intersect(time_ids, class_ids), pay_ids)
      pay_class  <- setdiff(intersect(pay_ids, class_ids), time_ids)

      all_three <- intersect(intersect(time_ids, pay_ids), class_ids)

      list(
        time_total = length(time_ids),
        pay_total = length(pay_ids),
        class_total = length(class_ids),
        time_only = length(time_only),
        pay_only = length(pay_only),
        class_only = length(class_only),
        time_pay = length(time_pay),
        time_class = length(time_class),
        pay_class = length(pay_class),
        all_three = length(all_three)
      )
    })

    # ===========================================================================
    # CONSOLIDATED TABLES
    # ===========================================================================

    extrap_factor <- reactive({ 1.0 })

    output$table_time_summary <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_summary_groups, include_years = TRUE)
      create_dt_table(display)
    })

    output$table_shift_hours <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_shift_groups, include_years = TRUE)
      create_dt_table(display)
    })

    output$table_rounding_consolidated <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_rounding_groups, include_years = TRUE)
      create_dt_table(display)
    })

    output$table_meal_consolidated <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_meal_analysis, include_years = TRUE)
      create_dt_table(display)
    })

    output$table_meal_5hr_consolidated <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_meal_violations_5_summary, include_years = TRUE, scenario_filter = "no waivers")
      create_dt_table(display)
    })

    output$table_meal_5hr_short_details <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_meal_violations_5_short, include_years = TRUE, scenario_filter = "no waivers")
      create_dt_table(display)
    })

    output$table_meal_5hr_late_details <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_meal_violations_5_late, include_years = TRUE, scenario_filter = "no waivers")
      create_dt_table(display)
    })

    output$table_meal_6hr_consolidated <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_meal_violations_6_summary, include_years = TRUE, scenario_filter = "waivers")
      create_dt_table(display)
    })

    output$table_meal_6hr_short_details <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_meal_violations_6_short, include_years = TRUE, scenario_filter = "waivers")
      create_dt_table(display)
    })

    output$table_meal_6hr_late_details <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_meal_violations_6_late, include_years = TRUE, scenario_filter = "waivers")
      create_dt_table(display)
    })

    output$table_meal_hybrid_consolidated <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_meal_violations_6_summary, include_years = TRUE, scenario_filter = "hybrid")
      create_dt_table(display)
    })

    output$table_meal_hybrid_short_details <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_meal_violations_6_short, include_years = TRUE, scenario_filter = "hybrid")
      create_dt_table(display)
    })

    output$table_meal_hybrid_late_details <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_meal_violations_6_late, include_years = TRUE, scenario_filter = "hybrid")
      create_dt_table(display)
    })

    output$table_rest_consolidated <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, time_rest, include_years = TRUE)
      create_dt_table(display)
    })

    output$table_pay_consolidated <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, pay_summary_groups, include_years = TRUE)
      create_dt_table(display)
    })

    output$table_rrop_consolidated <- renderDT({
      results <- display_results()
      display <- pipeline_to_display_format(results, pay_regular_rate, include_years = TRUE)

      if (nrow(display) > 0 && "Metric" %in% names(display)) {
        total_rows <- display[grepl("^(Total|Net)", Metric, ignore.case = TRUE)]
        other_rows <- display[!grepl("^(Total|Net)", Metric, ignore.case = TRUE)]
        display <- rbindlist(list(other_rows, total_rows), fill = TRUE)
      }

      create_dt_table(display)
    })

    # ===========================================================================
    # ANALYSIS TABLES (FROM FILES)
    # ===========================================================================
    # Class/Individual Claims - Overview
    output$table_damages_class_overview <- renderDT({
      tryCatch({
        results <- display_results()

        # Build section definitions for overview (all financial metrics)
        sections <- list()

        # Include SUMMARY section with dates and overall employee/pay period counts
        if (length(damages_summary_groups) > 0 && is.character(damages_summary_groups)) {
          sections[[length(sections) + 1]] <- list(
            section_name = "SUMMARY",
            groups = as.character(damages_summary_groups)
          )
        }

        if (length(damages_principal_groups) > 0 && is.character(damages_principal_groups)) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PRINCIPAL",
            groups = as.character(damages_principal_groups)
          )
        }

        if (isTRUE(input$show_credits) && length(damages_credits_groups) > 0 && is.character(damages_credits_groups)) {
          sections[[length(sections) + 1]] <- list(
            section_name = "CREDITS OR OFFSETS",
            groups = as.character(damages_credits_groups)
          )
        }

        if (length(damages_interest_groups) > 0 && is.character(damages_interest_groups)) {
          sections[[length(sections) + 1]] <- list(
            section_name = "INTEREST",
            groups = as.character(damages_interest_groups)
          )
        }

        if (length(damages_subtotal_groups) > 0 && is.character(damages_subtotal_groups)) {
          sections[[length(sections) + 1]] <- list(
            section_name = "SUB-TOTAL (PRINCIPAL + INTEREST)",
            groups = as.character(damages_subtotal_groups)
          )
        }

        if (length(damages_grand_total_groups) > 0 && is.character(damages_grand_total_groups)) {
          sections[[length(sections) + 1]] <- list(
            section_name = "GRAND TOTAL (PRINCIPAL + INTEREST + PENALTIES)",
            groups = as.character(damages_grand_total_groups)
          )
        }

        if (length(sections) == 0) {
          return(datatable(data.table(Message = "No damages overview data available"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        # Overview needs ALL scenarios (not just "all") to show Principal, Interest, Subtotal, Grand Total
        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "no waivers", "waivers", "hybrid"))

        if (is.null(display) || nrow(display) == 0) {
          return(datatable(data.table(Message = "No damages overview data available"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        create_dt_table(display)
      }, error = function(e) {
        datatable(data.table(Error = paste("Error rendering damages overview:", e$message)),
                  rownames = FALSE, options = list(dom = 't'))
      })
    })

    # Class/Individual Claims - No Waivers (dynamically built from damages_detail_unique)
    output$table_damages_class_no_waivers <- renderDT({
      tryCatch({
        results <- display_results()

        # Dynamically build sections from all damages detail groups
        sections <- list()
        for (detail_group in damages_detail_unique) {
          detail_split <- split_by_waiver(metric_groups[metric_groups == detail_group])
          if (length(detail_split$no_waiver) > 0) {
            # Derive section name: "Damages - Meal Premiums" -> "MEAL PREMIUMS DAMAGES"
            section_label <- toupper(sub("^Damages - ", "", detail_group))
            sections[[length(sections) + 1]] <- list(
              section_name = paste0(section_label, " DAMAGES"),
              groups = detail_split$no_waiver
            )
          }
        }

        if (length(sections) == 0) {
          return(datatable(data.table(Message = "No damages data available for no waivers scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "no waivers"))

        if (is.null(display) || nrow(display) == 0) {
          return(datatable(data.table(Message = "No damages data available for no waivers scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        create_dt_table(display)
      }, error = function(e) {
        datatable(data.table(Error = paste("Error rendering damages no waivers:", e$message)),
                  rownames = FALSE, options = list(dom = 't'))
      })
    })

    # Class/Individual Claims - Waivers (dynamically built from damages_detail_unique)
    output$table_damages_class_waivers <- renderDT({
      tryCatch({
        results <- display_results()

        # Dynamically build sections from all damages detail groups
        sections <- list()
        for (detail_group in damages_detail_unique) {
          group_matches <- metric_groups[metric_groups == detail_group]
          if (length(group_matches) > 0) {
            section_label <- toupper(sub("^Damages - ", "", detail_group))
            sections[[length(sections) + 1]] <- list(
              section_name = paste0(section_label, " DAMAGES"),
              groups = group_matches
            )
          }
        }

        if (length(sections) == 0) {
          return(datatable(data.table(Message = "No damages data available for waivers scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "waivers"))

        if (is.null(display) || nrow(display) == 0) {
          return(datatable(data.table(Message = "No damages data available for waivers scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        create_dt_table(display)
      }, error = function(e) {
        datatable(data.table(Error = paste("Error rendering damages waivers:", e$message)),
                  rownames = FALSE, options = list(dom = 't'))
      })
    })

    # Class/Individual Claims - Hybrid (dynamically built from damages_detail_unique)
    output$table_damages_class_hybrid <- renderDT({
      tryCatch({
        results <- display_results()
        sections <- list()
        for (detail_group in damages_detail_unique) {
          group_matches <- metric_groups[metric_groups == detail_group]
          if (length(group_matches) > 0) {
            section_label <- toupper(sub("^Damages - ", "", detail_group))
            sections[[length(sections) + 1]] <- list(
              section_name = paste0(section_label, " DAMAGES"),
              groups = group_matches
            )
          }
        }
        if (length(sections) == 0) {
          return(datatable(data.table(Message = "No damages data available for hybrid scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }
        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "hybrid"))
        if (is.null(display) || nrow(display) == 0) {
          return(datatable(data.table(Message = "No damages data available for hybrid scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }
        create_dt_table(display)
      }, error = function(e) {
        datatable(data.table(Error = paste("Error rendering damages hybrid:", e$message)),
                  rownames = FALSE, options = list(dom = 't'))
      })
    })

    # Class/Individual Claims - Wage Statement Penalties
    output$table_damages_wsv <- renderDT({
      tryCatch({
        results <- display_results()

        if (length(damages_wsv_groups) == 0) {
          return(datatable(data.table(Message = "No wage statement penalty data available"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        display <- pipeline_to_display_format(results, damages_wsv_groups)

        if (is.null(display) || nrow(display) == 0) {
          return(datatable(data.table(Message = "No wage statement penalty data available"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        create_dt_table(display)
      }, error = function(e) {
        datatable(data.table(Error = paste("Error rendering wage statement penalties:", e$message)),
                  rownames = FALSE, options = list(dom = 't'))
      })
    })

    # Class/Individual Claims - Waiting Time Penalties
    output$table_damages_wt <- renderDT({
      tryCatch({
        results <- display_results()

        if (length(damages_wt_groups) == 0) {
          return(datatable(data.table(Message = "No waiting time penalty data available"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        display <- pipeline_to_display_format(results, damages_wt_groups)

        if (is.null(display) || nrow(display) == 0) {
          return(datatable(data.table(Message = "No waiting time penalty data available"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        create_dt_table(display)
      }, error = function(e) {
        datatable(data.table(Error = paste("Error rendering waiting time penalties:", e$message)),
                  rownames = FALSE, options = list(dom = 't'))
      })
    })

    # PAGA - Overview
    output$table_paga_overview <- renderDT({
      tryCatch({
        results <- display_results()

        # Build section definitions for PAGA overview
        # paga_summary_groups includes both basic stats (dates, counts) and financial totals (PAGA totals with all variants)
        sections <- list()

        if (length(paga_summary_groups) > 0 && is.character(paga_summary_groups)) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA SUMMARY",
            groups = as.character(paga_summary_groups)
          )
        }

        if (length(sections) == 0) {
          return(datatable(data.table(Message = "No PAGA summary data available"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        # PAGA Overview needs ALL scenarios to show totals (like Class Damages Overview)
        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "no waivers", "waivers", "hybrid"))

        if (is.null(display) || nrow(display) == 0) {
          return(datatable(data.table(Message = "No PAGA summary data available"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        create_dt_table(display)
      }, error = function(e) {
        datatable(data.table(Error = paste("Error rendering PAGA overview:", e$message)),
                  rownames = FALSE, options = list(dom = 't'))
      })
    })

    # PAGA - No Waivers (dynamically built from paga_detail_unique)
    output$table_paga_no_waivers <- renderDT({
      tryCatch({
        results <- display_results()

        # Dynamically build sections from all PAGA detail groups
        sections <- list()
        for (detail_group in paga_detail_unique) {
          detail_split <- split_by_waiver(metric_groups[metric_groups == detail_group])
          if (length(detail_split$no_waiver) > 0) {
            # Use group name directly as section name (e.g., "PAGA - MEAL PERIODS")
            section_label <- toupper(detail_group)
            sections[[length(sections) + 1]] <- list(
              section_name = section_label,
              groups = detail_split$no_waiver
            )
          }
        }

        if (length(sections) == 0) {
          return(datatable(data.table(Message = "No PAGA data available for no waivers scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "no waivers"))

        if (is.null(display) || nrow(display) == 0) {
          return(datatable(data.table(Message = "No PAGA data available for no waivers scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        create_dt_table(display)
      }, error = function(e) {
        datatable(data.table(Error = paste("Error rendering PAGA no waivers:", e$message)),
                  rownames = FALSE, options = list(dom = 't'))
      })
    })

    # PAGA - Waivers (dynamically built from paga_detail_unique)
    output$table_paga_waivers <- renderDT({
      tryCatch({
        results <- display_results()

        # Dynamically build sections from all PAGA detail groups
        sections <- list()
        for (detail_group in paga_detail_unique) {
          group_matches <- metric_groups[metric_groups == detail_group]
          if (length(group_matches) > 0) {
            section_label <- toupper(detail_group)
            sections[[length(sections) + 1]] <- list(
              section_name = section_label,
              groups = group_matches
            )
          }
        }

        if (length(sections) == 0) {
          return(datatable(data.table(Message = "No PAGA data available for waivers scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "waivers"))

        if (is.null(display) || nrow(display) == 0) {
          return(datatable(data.table(Message = "No PAGA data available for waivers scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }

        create_dt_table(display)
      }, error = function(e) {
        datatable(data.table(Error = paste("Error rendering PAGA waivers:", e$message)),
                  rownames = FALSE, options = list(dom = 't'))
      })
    })

    # PAGA - Hybrid (dynamically built from paga_detail_unique)
    output$table_paga_hybrid <- renderDT({
      tryCatch({
        results <- display_results()
        sections <- list()
        for (detail_group in paga_detail_unique) {
          group_matches <- metric_groups[metric_groups == detail_group]
          if (length(group_matches) > 0) {
            section_label <- toupper(detail_group)
            sections[[length(sections) + 1]] <- list(
              section_name = section_label,
              groups = group_matches
            )
          }
        }
        if (length(sections) == 0) {
          return(datatable(data.table(Message = "No PAGA data available for hybrid scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }
        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "hybrid"))
        if (is.null(display) || nrow(display) == 0) {
          return(datatable(data.table(Message = "No PAGA data available for hybrid scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }
        create_dt_table(display)
      }, error = function(e) {
        datatable(data.table(Error = paste("Error rendering PAGA hybrid:", e$message)),
                  rownames = FALSE, options = list(dom = 't'))
      })
    })

    # ===========================================================================
    # EMPLOYEE-PERIOD EXAMPLE TAB
    # ===========================================================================
    # Keep example-period dropdown synced to globally filtered data
    observe({
      data <- filtered_data()

      all_periods <- if (!is.null(data$shift_data1) && "ID_Period_End" %in% names(data$shift_data1)) {
        unique(as.character(data$shift_data1$ID_Period_End))
      } else if (!is.null(data$pp_data1) && "ID_Period_End" %in% names(data$pp_data1)) {
        unique(as.character(data$pp_data1$ID_Period_End))
      } else if (!is.null(data$time1) && "ID_Period_End" %in% names(data$time1)) {
        unique(as.character(data$time1$ID_Period_End))
      } else {
        character(0)
      }

      all_periods <- sort(all_periods[!is.na(all_periods) & nzchar(all_periods)])
      current_selection <- isolate(input$example_period_select)
      selected_value <- if (!is.null(current_selection) && current_selection %in% all_periods) current_selection else character(0)

      updateSelectizeInput(
        session, "example_period_select",
        choices = all_periods,
        selected = selected_value,
        server = TRUE
      )
    })

    # Helper function to transpose data for display
    transpose_data_for_display <- function(dt, value_col_name = "Value") {
      if (is.null(dt) || nrow(dt) == 0) {
        return(data.table(Metric = "No data available", Value = "-"))
      }

      # Get numeric and important columns
      cols_to_show <- names(dt)[!names(dt) %in% c("ID", "Pay_ID", "Class_ID")]

      # Create transposed table
      result <- data.table(
        Metric = cols_to_show,
        Value = sapply(cols_to_show, function(col) {
          val <- dt[[col]]
          if (length(val) == 0 || all(is.na(val))) return("-")
          if (is.numeric(val)) return(prettyNum(round(val, 2), big.mark = ","))
          if (inherits(val, "Date")) return(as.character(val))
          return(as.character(val))
        })
      )

      setnames(result, "Value", value_col_name)
      return(result)
    }

    # ===========================================================================
    # Case Detail Outputs
    # ===========================================================================

    output$case_name <- renderText({
      if (exists("case_name")) {
        return(case_name)
      }
      return("Not specified")
    })

    output$case_number <- renderText({
      if (exists("case_no")) {
        return(case_no)
      }
      return("Not specified")
    })

    output$date_filed <- renderText({
      if (exists("date_filed")) {
        # Format as full written date (e.g., "July 30, 2020")
        if (inherits(date_filed, "Date")) {
          return(format(date_filed, "%B %d, %Y"))
        }
        return(as.character(date_filed))
      }
      return("Not specified")
    })

    output$relevant_period <- renderText({
      if (exists("class_dmgs_start_date")) {
        # Format as "class_dmgs_start_date to present"
        if (inherits(class_dmgs_start_date, "Date")) {
          formatted_date <- format(class_dmgs_start_date, "%B %d, %Y")
          return(paste0(formatted_date, " to present"))
        }
        return(paste0(as.character(class_dmgs_start_date), " to present"))
      }
      return("Not specified")
    })

    output$mediation_date <- renderText({
      if (exists("mediation_date")) {
        # Format as full written date
        if (inherits(mediation_date, "Date")) {
          return(format(mediation_date, "%B %d, %Y"))
        }
        return(as.character(mediation_date))
      }
      return("Not specified")
    })

    output$sample_size <- renderText({
      if (exists("sample_size")) {
        return(as.character(sample_size))
      }
      return("Not specified")
    })

    # ===========================================================================
    # Notes & Assumptions Tab - Log Outputs
    # ===========================================================================

    # Load log summary if available
    log_summary <- reactive({
      log_summary_file <- file.path(OUT_DIR, CASE_LOG_SUMMARY_FILE)
      if (file.exists(log_summary_file)) {
        readRDS(log_summary_file)
      } else {
        NULL
      }
    })

    # Render setup summary
    output$log_setup_summary <- renderUI({
      summary <- log_summary()
      if (is.null(summary)) {
        return(p("No log data available. Run clean_data.R to generate analysis logs."))
      }

      setup_msgs <- Filter(function(m) m$category == "SETUP", summary$messages)
      if (length(setup_msgs) == 0) {
        return(p("No setup information logged."))
      }

      msg_html <- lapply(setup_msgs, function(m) {
        data_html <- if (!is.null(m$data) && is.list(m$data)) {
          items <- lapply(names(m$data), function(n) {
            tags$li(tags$strong(paste0(n, ":")), m$data[[n]])
          })
          tags$ul(items)
        } else if (!is.null(m$data)) {
          p(style = "margin-left: 20px;", m$data)
        } else {
          NULL
        }

        div(
          style = "margin-bottom: 15px;",
          p(style = "margin-bottom: 5px;", icon("info-circle"), HTML("&nbsp;"), m$message),
          data_html
        )
      })

      div(msg_html)
    })

    # Render data summary
    output$log_data_summary <- renderUI({
      summary <- log_summary()
      if (is.null(summary)) {
        return(p("No log data available."))
      }

      data_msgs <- Filter(function(m) m$category == "DATA_SUMMARY", summary$messages)
      if (length(data_msgs) == 0) {
        return(p("No data summary information logged."))
      }

      msg_html <- lapply(data_msgs, function(m) {
        p(icon("chart-bar"), HTML("&nbsp;"), m$message)
      })

      div(msg_html)
    })

    # Render assumptions
    output$log_assumptions <- renderUI({
      summary <- log_summary()
      if (is.null(summary)) {
        return(p("No log data available."))
      }

      assumption_msgs <- Filter(function(m) m$category == "ASSUMPTION", summary$messages)
      if (length(assumption_msgs) == 0) {
        return(p("No assumptions logged."))
      }

      msg_html <- lapply(assumption_msgs, function(m) {
        data_html <- if (!is.null(m$data) && is.list(m$data)) {
          items <- lapply(names(m$data), function(n) {
            tags$li(tags$strong(paste0(n, ":")), m$data[[n]])
          })
          tags$ul(items)
        } else {
          NULL
        }

        div(
          style = "margin-bottom: 15px;",
          p(style = "margin-bottom: 5px;", icon("sticky-note"), HTML("&nbsp;"), m$message),
          data_html
        )
      })

      div(msg_html)
    })

    # Render full log file
    output$full_log <- renderText({
      log_file <- file.path(OUT_DIR, CASE_LOG_FILE)
      if (file.exists(log_file)) {
        paste(readLines(log_file), collapse = "\n")
      } else {
        "No log file found. Run clean_data.R to generate analysis logs."
      }
    })

    # Download log file
    output$download_log <- downloadHandler(
      filename = function() {
        paste0("analysis_log_", format(Sys.Date(), "%Y%m%d"), ".txt")
      },
      content = function(file) {
        log_file <- file.path(OUT_DIR, CASE_LOG_FILE)
        if (file.exists(log_file)) {
          file.copy(log_file, file)
        } else {
          writeLines("No log file available.", file)
        }
      }
    )

    # Render detailed general assumptions with actual parameter values
    output$general_assumptions_content <- renderUI({
      # Get parameter values from environment
      shift_hrs_cutoff <- if (exists("shift_hrs_cutoff")) shift_hrs_cutoff else 7
      rrop_buffer <- if (exists("rrop_buffer")) rrop_buffer else 0.05
      min_ot_buffer <- if (exists("min_ot_buffer")) min_ot_buffer else 0.25
      max_ot_buffer <- if (exists("max_ot_buffer")) max_ot_buffer else 20
      annual_interest_rate <- if (exists("annual_interest_rate")) annual_interest_rate else 0.07

      # PAGA penalties
      initial_pp_penalty <- if (exists("initial_pp_penalty")) initial_pp_penalty else 100
      subsequent_pp_penalty <- if (exists("subsequent_pp_penalty")) subsequent_pp_penalty else 100
      initial_pp_penalty_226 <- if (exists("initial_pp_penalty_226")) initial_pp_penalty_226 else 250
      subsequent_pp_penalty_226 <- if (exists("subsequent_pp_penalty_226")) subsequent_pp_penalty_226 else 250
      initial_pp_penalty_558 <- if (exists("initial_pp_penalty_558")) initial_pp_penalty_558 else 100
      subsequent_pp_penalty_558 <- if (exists("subsequent_pp_penalty_558")) subsequent_pp_penalty_558 else 100
      penalty_1174 <- if (exists("penalty_1174")) penalty_1174 else 500

      # Meal & Rest period thresholds
      meal_1_threshold_no_waiver <- if (exists("meal_1_threshold_no_waiver")) meal_1_threshold_no_waiver else 5
      meal_2_threshold_no_waiver <- if (exists("meal_2_threshold_no_waiver")) meal_2_threshold_no_waiver else 10
      meal_1_threshold_waiver    <- if (exists("meal_1_threshold_waiver"))    meal_1_threshold_waiver    else 6
      meal_2_threshold_waiver    <- if (exists("meal_2_threshold_waiver"))    meal_2_threshold_waiver    else 12
      rest_threshold             <- if (exists("rest_threshold"))             rest_threshold             else 3.5
      meal_min_hrs               <- if (exists("meal_min_hrs"))               meal_min_hrs               else 0.49
      meal_buffer                <- if (exists("meal_buffer"))                meal_buffer                else 0.01

      # Wage Statement Violation penalty amounts
      wsv_initial_penalty    <- if (exists("wsv_initial_penalty"))    wsv_initial_penalty    else 50
      wsv_subsequent_penalty <- if (exists("wsv_subsequent_penalty")) wsv_subsequent_penalty else 100
      wsv_cap                <- if (exists("wsv_cap"))                wsv_cap                else 4000

      # Waiting Time penalty period (days)
      wt_max_days <- if (exists("wt_max_days")) wt_max_days else 30

      # Sample info
      sample_size <- if (exists("sample_size")) sample_size else "100%"
      sample_size_val <- if (exists("sample_size_val")) sample_size_val else 1

      # Extrapolation factors
      time_extrap_factor <- if (exists("time_extrap_factor")) time_extrap_factor else 1
      wsv_time_extrap_factor <- if (exists("wsv_time_extrap_factor")) wsv_time_extrap_factor else 1
      wt_time_extrap_factor <- if (exists("wt_time_extrap_factor")) wt_time_extrap_factor else 1
      paga_time_extrap_factor <- if (exists("paga_time_extrap_factor")) paga_time_extrap_factor else 1

      # Get dates with formatting
      class_start <- if (exists("class_dmgs_start_date") && inherits(class_dmgs_start_date, "Date")) {
        format(class_dmgs_start_date, "%B %d, %Y")
      } else "4 years prior to complaint date"

      class_end <- if (exists("mediation_date") && inherits(mediation_date, "Date")) {
        format(mediation_date, "%B %d, %Y")
      } else "mediation date"

      paga_start <- if (exists("paga_dmgs_start_date") && inherits(paga_dmgs_start_date, "Date")) {
        format(paga_dmgs_start_date, "%B %d, %Y")
      } else "1 year + 65 days prior to PAGA filing"

      paga_end <- if (exists("mediation_date") && inherits(mediation_date, "Date")) {
        format(mediation_date, "%B %d, %Y")
      } else "mediation date"

      wsv_start <- if (exists("wsv_start_date") && inherits(wsv_start_date, "Date")) {
        format(wsv_start_date, "%B %d, %Y")
      } else "1 year prior to complaint date"

      wsv_end <- if (exists("mediation_date") && inherits(mediation_date, "Date")) {
        format(mediation_date, "%B %d, %Y")
      } else "mediation date"

      wt_start <- if (exists("wt_start_date") && inherits(wt_start_date, "Date")) {
        format(wt_start_date, "%B %d, %Y")
      } else "3 years prior to complaint date"

      wt_end <- if (exists("mediation_date") && inherits(mediation_date, "Date")) {
        format(mediation_date, "%B %d, %Y")
      } else "mediation date"

      # Build extrapolation text
      extrap_text <- if (sample_size_val < 1 || time_extrap_factor < 1) {
        paste0("<h4>Extrapolation Methodology</h4><ul>",
               if (sample_size_val < 1) paste0("<li><strong>Population Extrapolation:</strong> Analysis uses a ", sample_size, " sample of the workforce.</li>") else "",
               if (time_extrap_factor < 1) paste0(
                 "<li><strong>Temporal Extrapolation:</strong> Data coverage extends from the earliest record date to ", class_end, ". ",
                 "Extrapolation factors: ",
                 "Class Period = ", sprintf("%.2f%%", time_extrap_factor * 100),
                 if (wsv_time_extrap_factor < 1) paste0(", WSV Period = ", sprintf("%.2f%%", wsv_time_extrap_factor * 100)) else "",
                 if (wt_time_extrap_factor < 1) paste0(", WT Period = ", sprintf("%.2f%%", wt_time_extrap_factor * 100)) else "",
                 if (paga_time_extrap_factor < 1) paste0(", PAGA Period = ", sprintf("%.2f%%", paga_time_extrap_factor * 100)) else "",
                 "</li>"
               ) else "",
               "<li><strong>Applicability:</strong> Extrapolation only applies to complete analysis results, not to filtered data or individual employee calculations.</li>",
               "</ul>")
      } else {
        ""
      }

      HTML(paste0("
        <div style='line-height: 1.8;'>
          <h4>Version Information</h4>
          <ul>
            <li><strong>Analysis Version:</strong> ", get_case_version_label(), "</li>
          </ul>
          <h4>Data Processing</h4>
          <ul>
            <li><strong>Time Records:</strong> Each shift represents a distinct work period with In/Out punch times. Shifts are analyzed for hours worked, meal periods, and rest periods.</li>
            <li><strong>Pay Records:</strong> Pay data is matched to time data by employee ID and period end date to enable rate validation and damages calculations.</li>
            <li><strong>Missing Data:</strong> Records with missing critical fields (ID, Date) are flagged and may be excluded from analysis.</li>
            <li><strong>Shift Classification:</strong> Shifts are categorized using a ", shift_hrs_cutoff, "-hour cutoff (see Non Work Hours table).</li>
          </ul>

          <h4>Meal & Rest Period Violations</h4>
          <ul>
            <li><strong>Meal Period Timing (No Waivers):</strong> First meal period must start by the end of the ", meal_1_threshold_no_waiver, "th hour of work (shift_hrs > ", meal_1_threshold_no_waiver + 0.01, "). Second meal period required for shifts > ", meal_2_threshold_no_waiver, " hours (shift_hrs > ", meal_2_threshold_no_waiver + 0.01, ").</li>
            <li><strong>Meal Period Timing (Waivers):</strong> When waivers apply, first meal period may be delayed to the end of the ", meal_1_threshold_waiver, "th hour (shift_hrs > ", meal_1_threshold_waiver + 0.01, "). Second meal period delayed to > ", meal_2_threshold_waiver, " hours (shift_hrs > ", meal_2_threshold_waiver + 0.01, ").</li>
            <li><strong>Meal Period Duration:</strong> Minimum 30 minutes (", meal_min_hrs, " hours) required for compliant meal period. Periods between ", meal_buffer, " and ", meal_min_hrs, " hours are flagged as 'Short' violations.</li>
            <li><strong>De Minimis Buffer:</strong> A ", meal_buffer, " hour (", round(meal_buffer * 60, 0), "-second) buffer is applied to meal period calculations to account for rounding and minor timing variances.</li>
            <li><strong>Rest Period Eligibility:</strong> One 10-minute rest period required for shifts > ", rest_threshold, " hours (shift_hrs > ", rest_threshold + 0.01, "). Additional rest periods required for longer shifts (>6 hrs, >10 hrs, >14 hrs per 4-hour rule).</li>
            <li><strong>Waiver Analysis:</strong> Meal period waivers are analyzed as separate scenarios: 'no waivers' uses ", meal_1_threshold_no_waiver, "-hour rule, 'waivers' uses ", meal_1_threshold_waiver, "-hour rule.</li>
          </ul>

          <h4>Regular Rate of Pay (RROP)</h4>
          <ul>
            <li><strong>Calculation Method:</strong> RROP = (Total straight-time compensation including differential pay + non-discretionary bonuses) / (Total straight-time hours). Overtime premiums, discretionary bonuses, and time off are excluded from the calculation.</li>
            <li><strong>De Minimis Buffer:</strong> Under/overpayments below ", rrop_buffer, " ($", sprintf("%.0f", rrop_buffer * 100), " cents) are ignored as acceptable rounding differences.</li>
          </ul>

          <h4>Overtime & Double Time</h4>
          <ul>
            <li><strong>Daily OT:</strong> Hours worked over 8 in a single workday must be paid at 1.5x the regular rate.</li>
            <li><strong>Daily DT:</strong> Hours worked over 12 in a single workday must be paid at 2x the regular rate.</li>
            <li><strong>Weekly OT:</strong> Hours worked over 40 in a workweek must be paid at 1.5x the regular rate (if not already compensated as daily OT/DT).</li>
            <li><strong>7th Day Rules:</strong> Special rules apply for the 7th consecutive day worked in a workweek:<br>
              - First 8 hours on 7th day: 1.5x regular rate (OT)<br>
              - Hours over 8 on 7th day: 2x regular rate (DT)<br>
              These are analyzed separately from standard daily OT/DT calculations.</li>
            <li><strong>Buffer Thresholds:</strong> OT/DT underpayments below ", min_ot_buffer, " hours are treated as acceptable aberrations. Maximum analysis threshold is ", max_ot_buffer, " hours to exclude extreme outliers.</li>
          </ul>

          <h4>Damages Calculations</h4>
          <ul>
            <li><strong>Interest:</strong> Prejudgment interest calculated from violation date to interest through date using ", sprintf("%.0f%%", annual_interest_rate * 100), " annual rate.</li>
            <li><strong>Class Period:</strong> ", class_start, " to ", class_end, "</li>
            <li><strong>PAGA Period:</strong> ", paga_start, " to ", paga_end, "</li>
            <li><strong>Wage Statement Period:</strong> ", wsv_start, " to ", wsv_end, "</li>
            <li><strong>Waiting Time Period:</strong> ", wt_start, " to ", wt_end, "</li>
            <li><strong>Wage Statement Violations:</strong> $", wsv_initial_penalty, " initial pay period penalty + $", wsv_subsequent_penalty, " subsequent pay period penalties, capped at $", formatC(wsv_cap, format = "f", digits = 0, big.mark = ","), " per employee (Labor Code Section 226).</li>
            <li><strong>Waiting Time Penalties:</strong> Up to ", wt_max_days, " days of wages for terminated employees who did not receive timely final payment, calculated using RROP or final base rate (Labor Code Section 203).</li>
          </ul>

          <h4>PAGA Penalties</h4>
          <ul>
            <li><strong>Standard Penalties:</strong> $", initial_pp_penalty, " initial violation + $", subsequent_pp_penalty, " subsequent violations per employee per pay period (Labor Code Section 2699).</li>
            <li><strong>Labor Code Section 226 (Wage Statements):</strong> $", initial_pp_penalty_226, " initial + $", subsequent_pp_penalty_226, " subsequent penalties for wage statement violations.</li>
            <li><strong>Labor Code Section 558 (Meal/Rest):</strong> $", initial_pp_penalty_558, " initial + $", subsequent_pp_penalty_558, " subsequent penalties for meal and rest period violations.</li>
            <li><strong>Labor Code Section 1174:</strong> $", penalty_1174, " penalty for itemized wage statement violations.</li>
          </ul>

          ", extrap_text, "
        </div>
      "))
    })


    # ===========================================================================
    # Version and Documentation Outputs
    # ===========================================================================

    output$dashboard_version <- renderText({
      gsub("^v", "", get_case_version_label())
    })

    output$last_updated <- renderText({
      format(Sys.Date(), "%B %d, %Y")
    })

    # ===========================================================================
    # Filter Banner
    # ===========================================================================

    output$filter_banner_text <- renderUI({
      filters <- current_filters()

      if (length(filters) == 0) {
        return(NULL)
      }

      # Build filter description
      filter_parts <- c()

      # Date range
      if (!is.null(filters$date_min) && !is.null(filters$date_max)) {
        date_str <- paste0("Date: ", format(filters$date_min, "%m/%d/%Y"), " to ", format(filters$date_max, "%m/%d/%Y"))
        filter_parts <- c(filter_parts, date_str)
      }

      # Employee filter
      if (!is.null(filters$ID) && length(filters$ID) > 0) {
        if (length(filters$ID) <= 3) {
          emp_str <- paste0("Employees: ", paste(filters$ID, collapse = ", "))
        } else {
          emp_str <- paste0("Employees: ", length(filters$ID), " selected")
        }
        filter_parts <- c(filter_parts, emp_str)
      }

      # Sample filter
      if (!is.null(filters$Sample)) {
        sample_str <- paste0("Sample: ", ifelse(filters$Sample == 1, "Sample Only (1)", "Non-Sample (0)"))
        filter_parts <- c(filter_parts, sample_str)
      }

      # Subclass filter
      if (!is.null(filters$Subclass) && length(filters$Subclass) > 0) {
        if (length(filters$Subclass) <= 3) {
          subclass_str <- paste0("Subclass: ", paste(filters$Subclass, collapse = ", "))
        } else {
          subclass_str <- paste0("Subclass: ", length(filters$Subclass), " selected")
        }
        filter_parts <- c(filter_parts, subclass_str)
      }

      # Combine all filter descriptions
      if (length(filter_parts) > 0) {
        filter_text <- paste("[FILTERS] ACTIVE FILTERS:", paste(filter_parts, collapse = " | "))
        return(HTML(paste0(filter_text, " | <a href='#' onclick='Shiny.setInputValue(\"reset_filters\", Math.random()); return false;' style='color: white; text-decoration: underline;'>Reset All Filters</a>")))
      }

      return(NULL)
    })

    # ===========================================================================
    # Example Tab Outputs
    # ===========================================================================
    # Punch Detail (time1) - globally filtered + selected employee-period
    output$table_example_punches <- renderDT({
      req(input$example_period_select)
      data <- filtered_data()

      if (is.null(data$time1)) {
        return(datatable(data.table(Message = "No time1 data available"), rownames = FALSE, options = list(dom = 't')))
      }

      if (!"ID_Period_End" %in% names(data$time1)) {
        return(datatable(data.table(Message = paste("ID_Period_End column not found in time1. Available columns:", paste(head(names(data$time1), 20), collapse = ", "))), rownames = FALSE, options = list(dom = 't')))
      }

      filtered <- data$time1[as.character(ID_Period_End) == as.character(input$example_period_select)]

      if (nrow(filtered) == 0) {
        return(datatable(data.table(Message = paste("No punch records for ID_Period_End:", input$example_period_select)), rownames = FALSE, options = list(dom = 't')))
      }

      desired_cols <- c("ID", "Date", "Source", "Pay_Source", "punch_time", "r_punch_time", "punch_type", "hrs_wkd", "mp_hrs", "shift_hrs", "r_shift_hrs", "Hours")
      available_cols <- desired_cols[desired_cols %in% names(filtered)]

      if (length(available_cols) == 0) {
        return(datatable(data.table(Message = "Punch detail columns not available"), rownames = FALSE, options = list(dom = 't')))
      }

      display_data <- filtered[, ..available_cols]

      datatable(
        display_data,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "300px",
          dom = 't'
        ),
        class = 'cell-border stripe hover',
        style = 'bootstrap4'
      )
    })

    # Shift Data (shift_data1) - Show specific columns as requested
    output$table_example_shift <- renderDT({
      req(input$example_period_select)
      data <- filtered_data()

      if (is.null(data$shift_data1) || !"ID_Period_End" %in% names(data$shift_data1)) {
        return(datatable(data.table(Message = "No shift data available"), rownames = FALSE, options = list(dom = 't')))
      }

      # Filter to selected period
      filtered <- data$shift_data1[ID_Period_End == input$example_period_select]

      if (nrow(filtered) == 0) {
        return(datatable(data.table(Message = "No shift data for this period"), rownames = FALSE, options = list(dom = 't')))
      }

      # Select specific columns: ID, Date, shift_hrs, Hours, all mp1 and mp2 columns,
      # mpv and rpv columns, all pp columns (not prior pp columns)
      base_cols <- c("ID", "Date", "shift_hrs", "Hours")

      # Get all mp1 and mp2 related columns
      mp_cols <- grep("^(mp1|mp2|hrs_to_mp|MissMP|LateMP|ShortMP)", names(filtered), value = TRUE)

      # Get ALL columns containing "mpv" anywhere in the name
      mpv_cols <- grep("mpv", names(filtered), value = TRUE, ignore.case = TRUE)

      # Get ALL columns containing "rpv" anywhere in the name
      rpv_cols <- grep("rpv", names(filtered), value = TRUE, ignore.case = TRUE)

      # Get all pp columns (excluding prior_pp columns)
      pp_cols <- grep("^pp_", names(filtered), value = TRUE)
      pp_cols <- pp_cols[!grepl("^prior_pp_", pp_cols)]

      # Combine all columns in order
      priority_cols <- c(base_cols, mp_cols, mpv_cols, rpv_cols, pp_cols)
      available_cols <- unique(priority_cols[priority_cols %in% names(filtered)])

      if (length(available_cols) == 0) {
        return(datatable(data.table(Message = "Requested columns not available"), rownames = FALSE, options = list(dom = 't')))
      }

      display_data <- filtered[, ..available_cols]

      datatable(
        display_data,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 't',
          columnDefs = list(
            list(width = '100px', targets = "_all")
          )
        ),
        class = 'cell-border stripe hover',
        style = 'bootstrap4'
      )
    })

    # Pay Data (pay1) - Show specific columns as requested
    output$table_example_pay <- renderDT({
      req(input$example_period_select)
      data <- filtered_data()

      if (is.null(data$pay1) || !"Pay_ID_Period_End" %in% names(data$pay1)) {
        return(datatable(data.table(Message = "No pay data available"), rownames = FALSE, options = list(dom = 't')))
      }

      # Filter to selected period
      filtered <- data$pay1[Pay_ID_Period_End == input$example_period_select]

      if (nrow(filtered) == 0) {
        return(datatable(data.table(Message = "No pay data for this period"), rownames = FALSE, options = list(dom = 't')))
      }

      # Select specific columns: Pay_ID, Pay_Period_End, Pay_Date, Pay_Code, Pay_Hours,
      # Pay_Rate, Pay_Amount, Calc_Rate, Base_Rate, RROP, rate type, pp_ columns (not prior pp)
      base_cols <- c("Pay_ID", "Pay_Period_End", "Pay_Date", "Source", "Pay_Source", "Pay_Code", "Pay_Hours",
                     "Pay_Rate", "Pay_Amount", "Calc_Rate", "Base_Rate", "RROP")

      # Get rate type column (could be Rate_Type, rate_type, or Rate_Gp)
      rate_type_cols <- grep("^(Rate_Type|rate_type|Rate_Gp)$", names(filtered), value = TRUE)

      # Get all pp_ columns (excluding prior_pp_ columns)
      pp_cols <- grep("^pp_", names(filtered), value = TRUE)
      pp_cols <- pp_cols[!grepl("^prior_pp_", pp_cols)]

      # Combine all columns in order
      priority_cols <- c(base_cols, rate_type_cols, pp_cols)
      available_cols <- unique(priority_cols[priority_cols %in% names(filtered)])

      if (length(available_cols) == 0) {
        return(datatable(data.table(Message = "Requested columns not available"), rownames = FALSE, options = list(dom = 't')))
      }

      display_data <- filtered[, ..available_cols]

      datatable(
        display_data,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 't',
          columnDefs = list(
            list(width = '100px', targets = "_all")
          )
        ),
        class = 'cell-border stripe hover',
        style = 'bootstrap4'
      )
    })

    # Damages Data (pp_data1 / ee_data1) - Show damage columns
    output$table_example_damages <- renderDT({
      req(input$example_period_select)
      data <- filtered_data()

      # Try to get from pp_data1 or ee_data1
      aggregate_data <- NULL

      if (!is.null(data$pp_data1) && "ID_Period_End" %in% names(data$pp_data1)) {
        aggregate_data <- data$pp_data1[ID_Period_End == input$example_period_select]
      } else if (!is.null(data$ee_data1) && "ID" %in% names(data$ee_data1)) {
        # Extract ID from the period string
        emp_id <- sub("_.*", "", input$example_period_select)
        aggregate_data <- data$ee_data1[ID == emp_id]
      }

      if (is.null(aggregate_data) || nrow(aggregate_data) == 0) {
        return(datatable(data.table(Message = "No damage data available"), rownames = FALSE, options = list(dom = 't')))
      }

      # Select only damage-related columns (containing "dmg", "Dmg", "penalty", "Penalty", "PAGA")
      all_cols <- names(aggregate_data)
      damage_cols <- all_cols[grepl("dmg|Dmg|penalty|Penalty|PAGA|paga|violation|Violation", all_cols, ignore.case = TRUE)]

      # Also include ID columns for reference
      id_cols <- c("ID", "Name", "Period_End", "ID_Period_End")
      id_cols_available <- id_cols[id_cols %in% all_cols]

      final_cols <- unique(c(id_cols_available, damage_cols))

      if (length(final_cols) == 0) {
        return(datatable(data.table(Message = "No damage columns available"), rownames = FALSE, options = list(dom = 't')))
      }

      display_data <- aggregate_data[, ..final_cols]

      datatable(
        display_data,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 't',
          columnDefs = list(
            list(width = '120px', targets = "_all")
          )
        ),
        class = 'cell-border stripe hover',
        style = 'bootstrap4'
      )
    })

    # ===========================================================================
    # Data Drilldown Outputs
    # ===========================================================================
    DRILLDOWN_WARN_ROWS <- 100000
    DRILLDOWN_MAX_VALUE_CHOICES <- 5000
    DRILLDOWN_VALUE_FULL_SCAN_MAX <- 300000
    DRILLDOWN_VALUE_SCAN_MAX <- 200000

    drill_filter_ids <- 1:3
    drill_field_ids <- paste0("drill_field_select_", drill_filter_ids)
    drill_value_ids <- paste0("drill_value_select_", drill_filter_ids)

    drill_value_choice_notes <- reactiveVal(setNames(rep("", length(drill_filter_ids)), drill_field_ids))
    drill_value_choice_cache <- reactiveValues(values = list())

    as_date_vector <- function(x) {
      if (inherits(x, "Date")) return(x)
      if (inherits(x, "POSIXt")) return(as.Date(x))
      suppressWarnings(as.Date(x))
    }

    normalize_filter_values <- function(x) {
      if (inherits(x, "POSIXt")) {
        format(x, "%Y-%m-%d %H:%M:%S")
      } else if (inherits(x, "Date")) {
        format(x, "%Y-%m-%d")
      } else {
        as.character(x)
      }
    }

    get_picker_values <- function(dt, table_name, field, idx) {
      cache_key <- paste(table_name, field, nrow(dt), sep = "||")
      cached <- drill_value_choice_cache$values[[cache_key]]
      if (!is.null(cached)) return(cached)

      vals_raw <- dt[[field]]
      vals_raw <- vals_raw[!is.na(vals_raw)]
      non_missing_n <- length(vals_raw)

      sampled <- FALSE
      if (non_missing_n > DRILLDOWN_VALUE_FULL_SCAN_MAX) {
        vals_raw <- vals_raw[seq_len(min(non_missing_n, DRILLDOWN_VALUE_SCAN_MAX))]
        sampled <- TRUE
      }

      vals_chr <- normalize_filter_values(vals_raw)
      vals_chr <- vals_chr[nzchar(vals_chr)]
      vals_chr <- unique(vals_chr)

      limited <- FALSE
      if (length(vals_chr) > DRILLDOWN_MAX_VALUE_CHOICES) {
        vals_chr <- vals_chr[seq_len(DRILLDOWN_MAX_VALUE_CHOICES)]
        limited <- TRUE
      }

      note_parts <- character(0)
      if (sampled) {
        note_parts <- c(note_parts, sprintf(
          "Field %s: scanned first %s of %s non-missing rows for speed",
          idx,
          format(min(non_missing_n, DRILLDOWN_VALUE_SCAN_MAX), big.mark = ","),
          format(non_missing_n, big.mark = ",")
        ))
      }
      if (limited) {
        note_parts <- c(note_parts, sprintf(
          "Field %s: showing first %s unique values",
          idx,
          format(DRILLDOWN_MAX_VALUE_CHOICES, big.mark = ",")
        ))
      }

      result <- list(values = vals_chr, note = paste(note_parts, collapse = "; "))
      drill_value_choice_cache$values[[cache_key]] <- result
      result
    }

    period_col_for_table <- function(dt, table_name) {
      if (is.null(dt) || ncol(dt) == 0) return(NULL)

      if (identical(table_name, "pay1")) {
        if ("Pay_Period_End" %in% names(dt)) return("Pay_Period_End")
        return(NULL)
      }

      if ("Period_End" %in% names(dt)) return("Period_End")
      NULL
    }

    apply_drill_filters_to_dt <- function(dt, table_name, filter_specs, date_start, date_end) {
      if (is.null(dt) || nrow(dt) == 0) return(data.table())
      out <- copy(dt)

      period_col <- period_col_for_table(out, table_name)
      if (!is.null(period_col) && (!is.null(date_start) || !is.null(date_end))) {
        d <- as_date_vector(out[[period_col]])
        keep <- rep(TRUE, length(d))
        if (!is.null(date_start)) keep <- keep & !is.na(d) & d >= date_start
        if (!is.null(date_end)) keep <- keep & !is.na(d) & d <= date_end
        out <- out[keep]
      }

      if (length(filter_specs) > 0 && nrow(out) > 0) {
        for (spec in filter_specs) {
          if (!is.null(spec$field) && nzchar(spec$field) && spec$field %in% names(out) && !is.null(spec$value) && nzchar(spec$value)) {
            vals_chr <- normalize_filter_values(out[[spec$field]])
            out <- out[vals_chr == spec$value]
          }
        }
      }

      out
    }

    drill_source_table <- reactive({
      data <- filtered_data()
      switch(
        input$drill_table_select,
        "time1" = data$time1,
        "pay1" = data$pay1,
        "shift_data1" = data$shift_data1,
        "pp_data1" = data$pp_data1,
        "ee_data1" = data$ee_data1,
        data.table()
      )
    })

    observeEvent(filtered_data(), {
      drill_value_choice_cache$values <- list()
    }, ignoreInit = TRUE)

    observeEvent(input$drill_table_select, {
      drill_value_choice_cache$values <- list()
      dt <- drill_source_table()
      period_col <- period_col_for_table(dt, input$drill_table_select)

      if (!is.null(period_col) && !is.null(dt) && nrow(dt) > 0) {
        d <- as_date_vector(dt[[period_col]])
        d <- d[!is.na(d)]
        if (length(d) > 0) {
          updateDateRangeInput(
            session, "drill_period_range",
            start = min(d),
            end = max(d),
            min = min(d),
            max = max(d)
          )
          return()
        }
      }

      updateDateRangeInput(session, "drill_period_range", start = NULL, end = NULL)
    }, ignoreInit = FALSE)

    observe({
      dt <- drill_source_table()
      field_choices <- if (!is.null(dt) && ncol(dt) > 0) names(dt) else character(0)
      choices_with_blank <- c("", field_choices)

      for (i in drill_filter_ids) {
        field_id <- drill_field_ids[i]
        value_id <- drill_value_ids[i]
        current_field <- isolate(input[[field_id]])
        selected_field <- if (!is.null(current_field) && current_field %in% field_choices) current_field else ""

        updateSelectizeInput(
          session, field_id,
          choices = choices_with_blank,
          selected = selected_field,
          server = TRUE
        )

        if (!nzchar(selected_field)) {
          updateSelectizeInput(session, value_id, choices = "", selected = "", server = TRUE)
        }
      }
    })

    for (i in drill_filter_ids) {
      local({
        idx <- i
        field_id <- drill_field_ids[idx]
        value_id <- drill_value_ids[idx]

        observe({
          dt <- drill_source_table()
          field <- input[[field_id]]
          notes <- drill_value_choice_notes()

          if (is.null(dt) || nrow(dt) == 0 || is.null(field) || !nzchar(field) || !(field %in% names(dt))) {
            notes[[field_id]] <- ""
            drill_value_choice_notes(notes)
            updateSelectizeInput(session, value_id, choices = "", selected = "", server = TRUE)
            return()
          }

          picker_vals <- get_picker_values(dt, input$drill_table_select, field, idx)
          val_chr <- picker_vals$values

          notes[[field_id]] <- picker_vals$note
          drill_value_choice_notes(notes)

          current_value <- isolate(input[[value_id]])
          selected_value <- if (!is.null(current_value) && current_value %in% val_chr) current_value else ""

          updateSelectizeInput(
            session, value_id,
            choices = c("", val_chr),
            selected = selected_value,
            server = TRUE
          )
        })
      })
    }

    drill_filter_specs <- reactive({
      specs <- lapply(drill_filter_ids, function(i) {
        list(
          field = if (!is.null(input[[drill_field_ids[i]]])) trimws(as.character(input[[drill_field_ids[i]]])) else "",
          value = if (!is.null(input[[drill_value_ids[i]]])) trimws(as.character(input[[drill_value_ids[i]]])) else ""
        )
      })
      Filter(function(s) nzchar(s$field) && nzchar(s$value), specs)
    })

    drill_date_start <- reactive({
      rng <- input$drill_period_range
      if (is.null(rng) || length(rng) < 1 || is.na(rng[1])) return(NULL)
      as.Date(rng[1])
    })

    drill_date_end <- reactive({
      rng <- input$drill_period_range
      if (is.null(rng) || length(rng) < 2 || is.na(rng[2])) return(NULL)
      as.Date(rng[2])
    })

    empty_drill_counts <- function() {
      list(
        time1 = NA_integer_,
        pay1 = NA_integer_,
        shift_data1 = NA_integer_,
        pp_data1 = NA_integer_,
        ee_data1 = NA_integer_
      )
    }

    safe_nrow <- function(dt) {
      if (is.null(dt)) return(NA_integer_)
      as.integer(nrow(dt))
    }

    drill_context <- reactiveVal(list(
      state = "idle",
      message = "No drilldown loaded yet. Select table/date/filters, then click 'Load Drilldown'.",
      counts = empty_drill_counts()
    ))

    output$drill_filter_counts_ui <- renderUI({
      ctx <- drill_context()
      counts <- ctx$counts
      if (is.null(counts)) counts <- empty_drill_counts()

      notes <- unlist(drill_value_choice_notes(), use.names = FALSE)
      notes <- notes[nzchar(notes)]

      tags$div(
        style = "font-size: 13px; line-height: 1.4; margin-top: 4px;",
        tags$div(style = "font-weight: 700; margin-bottom: 6px;", "Filtered Row Counts"),
        tags$ul(
          style = "margin: 0 0 8px 16px; padding: 0;",
          lapply(names(counts), function(nm) {
            val <- counts[[nm]]
            tags$li(
              tags$span(style = "font-weight: 600;", paste0(nm, ": ")),
              tags$span(style = "font-family: monospace;", if (is.na(val)) "N/A" else format(val, big.mark = ","))
            )
          })
        ),
        if (length(notes) > 0) {
          tagList(
            tags$div(style = "margin-top: 8px; font-style: italic; font-size: 12px;", "Value Picker Notes"),
            tags$ul(
              style = "margin: 0 0 0 16px; padding: 0; font-size: 12px;",
              lapply(notes, tags$li)
            )
          )
        }
      )
    })

    observeEvent(list(
      input$drill_table_select,
      input$drill_period_range,
      input$drill_field_select_1, input$drill_value_select_1,
      input$drill_field_select_2, input$drill_value_select_2,
      input$drill_field_select_3, input$drill_value_select_3
    ), {
      if (isTRUE(input$drill_apply > 0)) {
        prev_ctx <- drill_context()
        drill_context(list(
          state = "idle",
          message = "Selection changed. Click 'Load Drilldown' to refresh.",
          counts = if (!is.null(prev_ctx$counts)) prev_ctx$counts else empty_drill_counts()
        ))
      }
    }, ignoreInit = TRUE)

    observeEvent(input$drill_apply, {
      prev_ctx <- drill_context()
      drill_context(list(
        state = "loading",
        message = "Loading drilldown... please wait.",
        counts = if (!is.null(prev_ctx$counts)) prev_ctx$counts else empty_drill_counts()
      ))

      withProgress(message = "Loading drilldown...", value = 0, {
        incProgress(0.08, detail = "Reading selected table")

        data <- filtered_data()
        table_name <- input$drill_table_select
        dt <- drill_source_table()
        specs <- drill_filter_specs()
        date_start <- drill_date_start()
        date_end <- drill_date_end()

        if (is.null(dt) || nrow(dt) == 0) {
          drill_context(list(
            state = "empty",
            message = "Selected table has no rows under current global filters.",
            counts = empty_drill_counts()
          ))
          return()
        }

        incProgress(0.25, detail = "Applying table/date filters")
        selected_rows <- apply_drill_filters_to_dt(dt, table_name, specs, date_start, date_end)

        if (is.null(selected_rows) || nrow(selected_rows) == 0) {
          drill_context(list(
            state = "empty",
            message = "No rows matched selected filters.",
            counts = empty_drill_counts()
          ))
          return()
        }

        if (nrow(selected_rows) > DRILLDOWN_WARN_ROWS && !isTRUE(input$drill_allow_large)) {
          drill_context(list(
            state = "large",
            message = paste0(
              "Matched ", format(nrow(selected_rows), big.mark = ","),
              " rows. Check 'Allow large load' and click 'Load Drilldown' again to continue."
            ),
            matched_rows = nrow(selected_rows),
            counts = empty_drill_counts()
          ))
          return()
        }

        incProgress(0.4, detail = "Resolving employee/period keys")
        id_period_keys <- character(0)
        pay_period_keys <- character(0)
        id_keys <- character(0)

        if ("ID_Period_End" %in% names(selected_rows)) id_period_keys <- unique(as.character(selected_rows$ID_Period_End))
        if ("Period_End" %in% names(selected_rows)) id_period_keys <- unique(c(id_period_keys, as.character(selected_rows$Period_End)))
        if ("Pay_ID_Period_End" %in% names(selected_rows)) pay_period_keys <- unique(as.character(selected_rows$Pay_ID_Period_End))
        if ("Pay_Period_End" %in% names(selected_rows)) pay_period_keys <- unique(c(pay_period_keys, as.character(selected_rows$Pay_Period_End)))
        if ("ID" %in% names(selected_rows)) id_keys <- unique(c(id_keys, as.character(selected_rows$ID)))
        if ("Pay_ID" %in% names(selected_rows)) id_keys <- unique(c(id_keys, as.character(selected_rows$Pay_ID)))

        if (length(id_keys) > 0) {
          if (!is.null(data$shift_data1) && all(c("ID", "ID_Period_End") %in% names(data$shift_data1))) {
            id_period_keys <- unique(c(id_period_keys, as.character(data$shift_data1[as.character(ID) %in% id_keys, ID_Period_End])))
          }
          if (!is.null(data$pay1) && all(c("Pay_ID", "Pay_ID_Period_End") %in% names(data$pay1))) {
            pay_period_keys <- unique(c(pay_period_keys, as.character(data$pay1[as.character(Pay_ID) %in% id_keys, Pay_ID_Period_End])))
          }
        }

        if (length(id_period_keys) > 0 && length(pay_period_keys) == 0) pay_period_keys <- id_period_keys
        if (length(pay_period_keys) > 0 && length(id_period_keys) == 0) id_period_keys <- pay_period_keys

        filter_context_table <- function(dt_in, nm) {
          if (is.null(dt_in) || nrow(dt_in) == 0) return(data.table())
          dt_out <- copy(dt_in)

          if (identical(nm, "pay1") && "Pay_ID_Period_End" %in% names(dt_out) && length(pay_period_keys) > 0) {
            dt_out <- dt_out[as.character(Pay_ID_Period_End) %in% pay_period_keys]
          } else if ("ID_Period_End" %in% names(dt_out) && length(id_period_keys) > 0) {
            dt_out <- dt_out[as.character(ID_Period_End) %in% id_period_keys]
          } else if ("Period_End" %in% names(dt_out) && length(id_period_keys) > 0) {
            dt_out <- dt_out[as.character(Period_End) %in% id_period_keys]
          } else if ("ID" %in% names(dt_out) && length(id_keys) > 0) {
            dt_out <- dt_out[as.character(ID) %in% id_keys]
          } else if ("Pay_ID" %in% names(dt_out) && length(id_keys) > 0) {
            dt_out <- dt_out[as.character(Pay_ID) %in% id_keys]
          } else {
            dt_out <- data.table()
          }

          apply_drill_filters_to_dt(dt_out, nm, filter_specs = list(), date_start = date_start, date_end = date_end)
        }

        incProgress(0.55, detail = "Building time context")
        drill_time <- if (identical(table_name, "time1")) selected_rows else filter_context_table(data$time1, "time1")

        incProgress(0.68, detail = "Building shift context")
        drill_shift <- if (identical(table_name, "shift_data1")) selected_rows else filter_context_table(data$shift_data1, "shift_data1")

        incProgress(0.8, detail = "Building pay context")
        drill_pay <- if (identical(table_name, "pay1")) selected_rows else filter_context_table(data$pay1, "pay1")

        incProgress(0.9, detail = "Building damages context")
        drill_pp <- if (identical(table_name, "pp_data1")) selected_rows else filter_context_table(data$pp_data1, "pp_data1")
        drill_ee <- if (identical(table_name, "ee_data1")) selected_rows else filter_context_table(data$ee_data1, "ee_data1")
        drill_damage <- if (!is.null(drill_pp) && nrow(drill_pp) > 0) drill_pp else drill_ee

        counts <- list(
          time1 = safe_nrow(drill_time),
          pay1 = safe_nrow(drill_pay),
          shift_data1 = safe_nrow(drill_shift),
          pp_data1 = safe_nrow(drill_pp),
          ee_data1 = safe_nrow(drill_ee)
        )

        incProgress(1, detail = "Finalizing")
        drill_context(list(
          state = "ready",
          table_name = table_name,
          matched_rows = nrow(selected_rows),
          selected_rows = selected_rows,
          time_rows = drill_time,
          shift_rows = drill_shift,
          pay_rows = drill_pay,
          damage_rows = drill_damage,
          pp_rows = drill_pp,
          ee_rows = drill_ee,
          counts = counts
        ))
      })
    }, ignoreInit = TRUE)

    output$drill_status_ui <- renderUI({
      ctx <- drill_context()

      msg_color <- switch(
        ctx$state,
        "loading" = "#1f4e79",
        "large" = "#8a6d3b",
        "empty" = "#8a6d3b",
        "idle" = "#8a6d3b",
        "#8a6d3b"
      )

      msg_line <- if (!is.null(ctx$message) && nzchar(ctx$message)) {
        tags$p(style = paste0("margin: 6px 0 0 0; color: ", msg_color, ";"), ctx$message)
      } else {
        NULL
      }

      info_line <- if (!is.null(ctx$matched_rows) && isTRUE(ctx$matched_rows > 0) && identical(ctx$state, "ready")) {
        tags$p(style = "margin: 6px 0 0 0; color: #2c3e50;",
               paste0("Loaded ", format(ctx$matched_rows, big.mark = ","), " matching rows from selected table."))
      } else {
        NULL
      }

      tagList(msg_line, info_line)
    })

    drill_message_table <- function(msg) {
      datatable(data.table(Message = msg), rownames = FALSE, options = list(dom = 't'))
    }

    output$table_drill_viewer <- renderDT({
      ctx <- drill_context()
      if (!identical(ctx$state, "ready") || is.null(ctx$selected_rows) || nrow(ctx$selected_rows) == 0) {
        return(drill_message_table(if (!is.null(ctx$message)) ctx$message else "No drilldown loaded."))
      }

      datatable(
        ctx$selected_rows,
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          lengthMenu = list(c(10, 25, 50, 100, 250), c("10", "25", "50", "100", "250")),
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv')
        ),
        class = 'cell-border stripe hover',
        style = 'bootstrap4'
      )
    }, server = TRUE)

    output$download_drill_viewer <- downloadHandler(
      filename = function() {
        tbl <- if (!is.null(input$drill_table_select) && nzchar(input$drill_table_select)) input$drill_table_select else "table"
        paste0("drilldown_view_", tbl, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        ctx <- drill_context()
        if (!identical(ctx$state, "ready") || is.null(ctx$selected_rows) || nrow(ctx$selected_rows) == 0) {
          fwrite(data.table(Message = "No rows available for download"), file)
          return()
        }

        dt <- copy(ctx$selected_rows)
        rows_all <- input$table_drill_viewer_rows_all
        if (!is.null(rows_all) && nrow(dt) > 0) {
          if (length(rows_all) == 0) {
            dt <- dt[0]
          } else {
            valid_rows <- rows_all[rows_all >= 1 & rows_all <= nrow(dt)]
            dt <- if (length(valid_rows) > 0) dt[valid_rows] else dt[0]
          }
        }

        if (nrow(dt) == 0) {
          fwrite(data.table(Message = "No rows available for download"), file)
        } else {
          fwrite(dt, file)
        }
      }
    )

    output$table_drill_punches <- renderDT({
      ctx <- drill_context()
      dt <- ctx$time_rows
      if (!identical(ctx$state, "ready") || is.null(dt) || nrow(dt) == 0) {
        return(drill_message_table(if (!is.null(ctx$message)) ctx$message else "No punch rows available for drilldown."))
      }

      desired_cols <- c("ID", "Date", "Source", "Pay_Source", "punch_time", "r_punch_time", "punch_type", "hrs_wkd", "mp_hrs", "shift_hrs", "r_shift_hrs", "Hours")
      available_cols <- desired_cols[desired_cols %in% names(dt)]
      if (length(available_cols) == 0) return(drill_message_table("No punch detail columns available."))

      datatable(
        dt[, ..available_cols],
        rownames = FALSE,
        options = list(paging = FALSE, scrollX = TRUE, scrollY = "300px", dom = 't'),
        class = 'cell-border stripe hover',
        style = 'bootstrap4'
      )
    })

    output$table_drill_shift <- renderDT({
      ctx <- drill_context()
      dt <- ctx$shift_rows
      if (!identical(ctx$state, "ready") || is.null(dt) || nrow(dt) == 0) {
        return(drill_message_table(if (!is.null(ctx$message)) ctx$message else "No shift rows available for drilldown."))
      }

      base_cols <- c("ID", "Date", "shift_hrs", "Hours")
      mp_cols <- grep("^(mp1|mp2|hrs_to_mp|MissMP|LateMP|ShortMP)", names(dt), value = TRUE)
      mpv_cols <- grep("mpv", names(dt), value = TRUE, ignore.case = TRUE)
      rpv_cols <- grep("rpv", names(dt), value = TRUE, ignore.case = TRUE)
      pp_cols <- grep("^pp_", names(dt), value = TRUE)
      pp_cols <- pp_cols[!grepl("^prior_pp_", pp_cols)]
      available_cols <- unique(c(base_cols, mp_cols, mpv_cols, rpv_cols, pp_cols))
      available_cols <- available_cols[available_cols %in% names(dt)]
      if (length(available_cols) == 0) return(drill_message_table("No shift detail columns available."))

      datatable(
        dt[, ..available_cols],
        rownames = FALSE,
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 't',
          columnDefs = list(list(width = '100px', targets = "_all"))
        ),
        class = 'cell-border stripe hover',
        style = 'bootstrap4'
      )
    })

    output$table_drill_pay <- renderDT({
      ctx <- drill_context()
      dt <- ctx$pay_rows
      if (!identical(ctx$state, "ready") || is.null(dt) || nrow(dt) == 0) {
        return(drill_message_table(if (!is.null(ctx$message)) ctx$message else "No pay rows available for drilldown."))
      }

      base_cols <- c("Pay_ID", "Pay_Period_End", "Pay_Date", "Source", "Pay_Source", "Pay_Code", "Pay_Hours", "Pay_Rate", "Pay_Amount", "Calc_Rate", "Base_Rate", "RROP")
      rate_type_cols <- grep("^(Rate_Type|rate_type|Rate_Gp)$", names(dt), value = TRUE)
      pp_cols <- grep("^pp_", names(dt), value = TRUE)
      pp_cols <- pp_cols[!grepl("^prior_pp_", pp_cols)]
      available_cols <- unique(c(base_cols, rate_type_cols, pp_cols))
      available_cols <- available_cols[available_cols %in% names(dt)]
      if (length(available_cols) == 0) return(drill_message_table("No pay detail columns available."))

      datatable(
        dt[, ..available_cols],
        rownames = FALSE,
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 't',
          columnDefs = list(list(width = '100px', targets = "_all"))
        ),
        class = 'cell-border stripe hover',
        style = 'bootstrap4'
      )
    })

    output$table_drill_damages <- renderDT({
      ctx <- drill_context()
      dt <- ctx$damage_rows
      if (!identical(ctx$state, "ready") || is.null(dt) || nrow(dt) == 0) {
        return(drill_message_table(if (!is.null(ctx$message)) ctx$message else "No damages rows available for drilldown."))
      }

      all_cols <- names(dt)
      damage_cols <- all_cols[grepl("dmg|Dmg|penalty|Penalty|PAGA|paga|violation|Violation", all_cols, ignore.case = TRUE)]
      id_cols <- c("ID", "Name", "Period_End", "ID_Period_End", "Pay_ID_Period_End")
      final_cols <- unique(c(id_cols[id_cols %in% all_cols], damage_cols))
      if (length(final_cols) == 0) return(drill_message_table("No damage columns available."))

      datatable(
        dt[, ..final_cols],
        rownames = FALSE,
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 't',
          columnDefs = list(list(width = '120px', targets = "_all"))
        ),
        class = 'cell-border stripe hover',
        style = 'bootstrap4'
      )
    })
    # ===========================================================================
    # ANALYSIS TABLES (FROM FILES)
    # ===========================================================================
    output$table_pay_codes <- renderDT({
      create_dt_table(analysis_tables$pay_code_summary, metric_col = "Pay Code")
    })

    output$table_rate_type <- renderDT({
      create_dt_table(analysis_tables$rate_type_analysis, metric_col = "Rate Type")
    })

    output$table_shift_hrs <- renderDT({
      create_dt_table(analysis_tables$shift_hrs, metric_col = "Shift Hrs")
    })

    output$table_non_wrk_hrs <- renderDT({
      create_dt_table(analysis_tables$non_wrk_hrs, metric_col = "Non Work Hrs")
    })

    output$table_meal_period <- renderDT({
      create_dt_table(analysis_tables$meal_period, metric_col = "Meal Period Hrs")
    })

    output$table_meal_start_time <- renderDT({
      create_dt_table(analysis_tables$meal_start_time, metric_col = "Meal Start Time")
    })

    output$table_meal_quarter_hr <- renderDT({
      create_dt_table(analysis_tables$meal_quarter_hr, metric_col = "Quarter Hour Type")
    })

    # Overlap Matrix Table
    output$overlap_matrix_table <- renderDT({
      venn <- venn_data()

      # Create matrix table
      matrix_data <- data.table(
        Category = c(
          "In Time Data Only",
          "In Pay Data Only",
          if (venn$class_total > 0) "In Class Data Only",
          "In Time & Pay Only",
          if (venn$class_total > 0) "In Time & Class Only",
          if (venn$class_total > 0) "In Pay & Class Only",
          if (venn$class_total > 0) "In All Three Sources",
          "TOTAL UNIQUE EMPLOYEES"
        ),
        Count = c(
          venn$time_only,
          venn$pay_only,
          if (venn$class_total > 0) venn$class_only,
          venn$time_pay,
          if (venn$class_total > 0) venn$time_class,
          if (venn$class_total > 0) venn$pay_class,
          if (venn$class_total > 0) venn$all_three,
          length(unique(c(
            unique(filtered_data()$shift_data1$ID),
            unique(filtered_data()$pay1$Pay_ID),
            if (!is.null(filtered_data()$class1)) unique(filtered_data()$class1$Class_ID) else character(0)
          )))
        ),
        `In Time` = c(
          "Y", "", if (venn$class_total > 0) "",
          "Y", if (venn$class_total > 0) "Y", if (venn$class_total > 0) "",
          if (venn$class_total > 0) "Y", "-"
        ),
        `In Pay` = c(
          "", "Y", if (venn$class_total > 0) "",
          "Y", if (venn$class_total > 0) "", if (venn$class_total > 0) "Y",
          if (venn$class_total > 0) "Y", "-"
        ),
        `In Class` = if (venn$class_total > 0) c(
          "", "", "Y",
          "", "Y", "Y",
          "Y", "-"
        ) else NULL
      )

      datatable(
        matrix_data,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          dom = 't',
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-center', targets = 1:(ncol(matrix_data) - 1))
          )
        ),
        class = 'cell-border stripe hover',
        style = 'bootstrap4'
      )
    })

    output$employee_period_table <- renderDT({
      create_dt_table(analysis_tables$employee_comparison, metric_col = "ID")
    })

    # Employee Comparison (all_ids) - creates a table showing which IDs are in which data sources
    output$employee_comparison_all_ids <- renderDT({
      data <- filtered_data()

      # Get unique employee IDs from each source
      time_ids <- unique(data$shift_data1$ID)
      pay_ids  <- unique(data$pay1$Pay_ID)
      class_ids <- if (!is.null(data$class1) && "Class_ID" %in% names(data$class1)) {
        unique(data$class1$Class_ID)
      } else {
        character(0)
      }

      # Get all unique IDs
      all_ids <- sort(unique(c(time_ids, pay_ids, class_ids)))

      # Create all_ids data.table
      all_ids_dt <- data.table(
        ID = all_ids,
        In_Time = ifelse(all_ids %in% time_ids, "Yes", "No"),
        In_Pay = ifelse(all_ids %in% pay_ids, "Yes", "No"),
        In_Class = ifelse(all_ids %in% class_ids, "Yes", "No")
      )

      # Add a category column
      all_ids_dt[, Category := fcase(
        In_Time == "Yes" & In_Pay == "Yes" & In_Class == "Yes", "All Three",
        In_Time == "Yes" & In_Pay == "Yes" & In_Class == "No", "Time & Pay Only",
        In_Time == "Yes" & In_Pay == "No" & In_Class == "Yes", "Time & Class Only",
        In_Time == "No" & In_Pay == "Yes" & In_Class == "Yes", "Pay & Class Only",
        In_Time == "Yes" & In_Pay == "No" & In_Class == "No", "Time Only",
        In_Time == "No" & In_Pay == "Yes" & In_Class == "No", "Pay Only",
        In_Time == "No" & In_Pay == "No" & In_Class == "Yes", "Class Only",
        default = "Unknown"
      )]

      # Reorder columns
      setcolorder(all_ids_dt, c("ID", "Category", "In_Time", "In_Pay", "In_Class"))

      create_dt_table(all_ids_dt, metric_col = "ID")
    })

    # Employee Pay Period Comparison for the Data Comparison tab
    output$employee_comparison_table <- renderDT({
      create_dt_table(analysis_tables$employee_comparison, metric_col = "ID")
    })

    # ===========================================================================
    # DOWNLOAD HANDLER
    # ===========================================================================



    # =========================================================================
    # DOWNLOAD EXCEL HANDLER
    # =========================================================================
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0(gsub("[^A-Za-z0-9_-]", "_", case_name), "_Analysis_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        message("Excel export starting...")

        data <- filtered_data()
        results <- pipeline_results()

        wb <- openxlsx::createWorkbook()
        hs <- openxlsx::createStyle(fontSize = 11, fontColour = "#FFFFFF", halign = "center",
                                    fgFill = "#2c3e50", textDecoration = "bold")

        add_sheet <- function(name, dt) {
          if (is.null(dt) || nrow(dt) == 0) return()
          nm <- substr(name, 1, 31)
          openxlsx::addWorksheet(wb, nm)
          openxlsx::writeData(wb, nm, dt, headerStyle = hs)
          openxlsx::setColWidths(wb, nm, cols = 1:ncol(dt), widths = "auto")
          openxlsx::freezePane(wb, nm, firstRow = TRUE)
        }

        # Summary
        openxlsx::addWorksheet(wb, "Summary")
        summ <- data.table(
          Item = c("Case Name", "Report Date", "Employees (Time)", "Employees (Pay)", "Shifts"),
          Value = c(case_name, as.character(Sys.Date()),
                    as.character(uniqueN(data$shift_data1$ID)),
                    as.character(uniqueN(data$pay1$Pay_ID)),
                    as.character(nrow(data$shift_data1)))
        )
        openxlsx::writeData(wb, "Summary", summ, headerStyle = hs)

        # Time Summary
        if (length(time_summary_groups) > 0) {
          tbl <- pipeline_to_display_format(results, time_summary_groups)
          add_sheet("Time - Summary", tbl)
        }

        # Pay Summary
        if (length(pay_summary_groups) > 0) {
          tbl <- pipeline_to_display_format(results, pay_summary_groups)
          add_sheet("Pay - Summary", tbl)
        }

        # Class Damages Overview
        class_groups <- c(damages_summary_groups, damages_principal_groups, damages_credits_groups,
                          damages_interest_groups, damages_subtotal_groups, damages_grand_total_groups)
        if (length(class_groups) > 0) {
          tbl <- pipeline_to_display_format(results, class_groups)
          add_sheet("Class - Overview", tbl)
        }

        # Individual Class Damages (dynamically from damages_detail_unique)
        for (dg in damages_detail_unique) {
          dg_matches <- metric_groups[metric_groups == dg]
          if (length(dg_matches) > 0) {
            # Sheet name: strip "Damages - " prefix, truncate to 31 chars (Excel limit)
            sheet_label <- substr(paste0("Class - ", sub("^Damages - ", "", dg)), 1, 31)
            add_sheet(sheet_label, pipeline_to_display_format(results, dg_matches))
          }
        }
        if (length(damages_wsv_groups) > 0) add_sheet("Class - Wage Stmt", pipeline_to_display_format(results, damages_wsv_groups))
        if (length(damages_wt_groups) > 0) add_sheet("Class - Waiting Time", pipeline_to_display_format(results, damages_wt_groups))

        # PAGA Overview
        if (length(paga_summary_groups) > 0) add_sheet("PAGA - Overview", pipeline_to_display_format(results, paga_summary_groups))
        # PAGA detail groups (dynamically from paga_detail_unique)
        for (pg in paga_detail_unique) {
          pg_matches <- metric_groups[metric_groups == pg]
          if (length(pg_matches) > 0) {
            sheet_label <- substr(sub("^PAGA - ", "PAGA - ", pg), 1, 31)
            add_sheet(sheet_label, pipeline_to_display_format(results, pg_matches))
          }
        }

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        message("Excel export complete: ", file)
      }
    )
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0(gsub("[^A-Za-z0-9_-]", "_", case_name), "_Report_", Sys.Date(), ".pdf")
      },
      contentType = "application/pdf",
      content = function(file) {
        message("PDF export starting via generate_pdf.R...")

        # Source generate_pdf.R from configured master path (always source to avoid stale functions)
        pdf_script <- if (exists("PDF_SCRIPT")) {
          PDF_SCRIPT
        } else if (exists("MASTER_SCRIPTS_DIR")) {
          file.path(MASTER_SCRIPTS_DIR, "generate_pdf.R")
        } else {
          "D:/Shared/Master_Scripts/generate_pdf.R"
        }

        if (!file.exists(pdf_script)) {
          stop("Cannot find generate_pdf.R at configured path: ", pdf_script)
        }

        message("Loading generate_pdf.R from: ", pdf_script)
        source(pdf_script, local = FALSE)


        # Get data needed by generate_report() - it checks environment variables
        data <- filtered_data()
        shift_data1 <- data$shift_data1  # Make available in environment
        pay1 <- data$pay1               # Make available in environment
        class1 <- data$class1           # Make available in environment
        results_raw <- pipeline_results()
        message("Data loaded for generate_report()")

        # Get selected sections from checkbox group
        selected <- input$pdf_sections

        # Respect scenario toggles at subsection level as well
        if (!isTRUE(input$pdf_include_no_waivers)) {
          selected <- selected[!grepl("no_waivers", selected)]
        }
        if (!isTRUE(input$pdf_include_waivers)) {
          waiver_only <- grepl("waivers", selected) & !grepl("no_waivers", selected)
          selected <- selected[!waiver_only]
        }
        if (!isTRUE(input$pdf_include_hybrid)) {
          selected <- selected[!grepl("hybrid", selected)]
        }
        # Map checkbox selections to high-level section categories
        pdf_sections <- c()

        # Time section - any time-related checkbox
        if (any(c("time_summary", "meal_analysis", "meal_violations_no_waivers", "meal_violations_waivers", "meal_violations_hybrid",
                  "rest_analysis", "shift_hours", "time_rounding") %in% selected)) {
          pdf_sections <- c(pdf_sections, "time")
        }

        # Pay section - any pay-related checkbox
        if (any(c("pay_summary", "regular_rate_bonuses", "regular_rate_differentials", "regular_rate_rrop") %in% selected)) {
          pdf_sections <- c(pdf_sections, "pay")
        }

        # Class section - any class damages checkbox
        if (any(grepl("^class_damages_|^damages_", selected))) {
          pdf_sections <- c(pdf_sections, "class")
        }

        # PAGA section - any PAGA checkbox
        if (any(grepl("^paga_", selected))) {
          pdf_sections <- c(pdf_sections, "paga")
        }

        # Analysis section - pay codes or rate type
        if (any(c("pay_codes", "rate_type_analysis") %in% selected)) {
          pdf_sections <- c(pdf_sections, "analysis")
        }

        # Build scenario vectors from modal scenario checkboxes
        pdf_enabled_scenarios <- character(0)
        if (isTRUE(input$pdf_include_no_waivers)) pdf_enabled_scenarios <- c(pdf_enabled_scenarios, "no waivers")
        if (isTRUE(input$pdf_include_waivers)) pdf_enabled_scenarios <- c(pdf_enabled_scenarios, "waivers")
        if (isTRUE(input$pdf_include_hybrid)) pdf_enabled_scenarios <- c(pdf_enabled_scenarios, "hybrid")
        pdf_enabled_scenarios <- intersect(unique(pdf_enabled_scenarios), available_scenarios)

        # Keep subsection intent (if selected), but constrain to enabled scenario toggles.
        section_scenarios <- character(0)
        if (any(grepl("no_waivers", selected))) section_scenarios <- c(section_scenarios, "no waivers")
        if (any(grepl("waivers", selected) & !grepl("no_waivers", selected))) section_scenarios <- c(section_scenarios, "waivers")
        if (any(grepl("hybrid", selected))) section_scenarios <- c(section_scenarios, "hybrid")
        section_scenarios <- intersect(unique(section_scenarios), available_scenarios)

        if (length(section_scenarios) == 0) {
          section_scenarios <- available_scenarios
        }

        class_scenarios <- intersect(section_scenarios, pdf_enabled_scenarios)
        paga_scenarios <- pdf_enabled_scenarios

        # Pre-filter pipeline results for PDF so export always matches app-style
        # scenario and credit/less-premiums visibility, regardless of PDF script version.
        results_pdf <- copy(results_raw)

        if ("scenario" %in% names(results_pdf)) {
          sc <- normalize_scenario_value(results_pdf$scenario)
          if (length(pdf_enabled_scenarios) > 0) {
            results_pdf <- results_pdf[
              sc %in% c(pdf_enabled_scenarios, "all") | is.na(results_pdf$scenario) | is.na(sc)
            ]
          } else {
            results_pdf <- results_pdf[
              is.na(results_pdf$scenario) | is.na(sc) | sc == "all"
            ]
          }
        }

        if (!isTRUE(input$pdf_include_credits)) {
          results_pdf <- results_pdf[!get_credit_row_mask(results_pdf, metric_spec)]
        }
        if (!isTRUE(input$pdf_include_less_prems)) {
          results_pdf <- results_pdf[!get_less_prems_row_mask(results_pdf, metric_spec)]
        }

        # Make filtered results available to generate_report() via environment
        results <- results_pdf
        # Determine include flags from selections
        include_data_comparison <- "data_comparison" %in% selected
        include_appendix <- "appendix" %in% selected
        include_assumptions <- "assumptions" %in% selected

        message("PDF sections selected: ", paste(pdf_sections, collapse = ", "))
        message("Class scenarios: ", paste(class_scenarios, collapse = ", "))
        message("Enabled PDF scenarios: ", paste(pdf_enabled_scenarios, collapse = ", "))
        message("PDF results rows after modal filters: ", format(nrow(results), big.mark = ","))
        message("Include data comparison: ", include_data_comparison)
        message("Include appendix: ", include_appendix)
        message("Include assumptions: ", include_assumptions)

        # Call standalone PDF generator.
        # Keep backward compatibility if an older generate_pdf.R is sourced.
        gr_args <- list(
          output_file = file,
          sections = pdf_sections,
          include_extrap = isTRUE(input$pdf_include_extrap),
          include_credits = isTRUE(input$pdf_include_credits),
          include_less_prems = isTRUE(input$pdf_include_less_prems),
          include_no_waivers = isTRUE(input$pdf_include_no_waivers),
          include_waivers = isTRUE(input$pdf_include_waivers),
          include_hybrid = isTRUE(input$pdf_include_hybrid),
          include_appendix = include_appendix,
          include_data_comparison = include_data_comparison,
          include_assumptions = include_assumptions,
          class_scenarios = class_scenarios,
          paga_scenarios = paga_scenarios,
          selected_subsections = selected,
          verbose = FALSE
        )

        gr_formals <- tryCatch(names(formals(generate_report)), error = function(e) character(0))
        if (length(gr_formals) > 0) {
          gr_args <- gr_args[names(gr_args) %in% gr_formals]
        }

        do.call(generate_report, gr_args)

        message("PDF generation complete")

      }
    )
  }
}

# ---- RUN APP ----

app_boot_start <- Sys.time()
cat("[APP] Starting dashboard bootstrap...\n")
flush.console()

cat("[APP] Step 1/5: Loading case data files...\n")
flush.console()
data_list <- load_data()

cat("[APP] Step 2/5: Loading metric spec...\n")
flush.console()
metric_spec <- load_metric_spec()

# Load extrapolation values if they exist
extrap_values_file <- file.path(OUT_DIR, "extrapolation_values.rds")
if (file.exists(extrap_values_file)) {
  cat("[APP] Loading extrapolation values... ")
  flush.console()
  extrap_values <- readRDS(extrap_values_file)
  cat("done\n")
} else {
  extrap_values <- NULL
  cat("[APP] No extrapolation values found - using runtime calculation\n")
}
flush.console()

cat("[APP] Step 3/5: Pre-computing metric groups...\n")
flush.console()
# Categorize metric groups for consolidation (done once at startup for performance)
metric_groups <- unique(metric_spec$metric_group)
# Build metric_group_categories with static time/pay groups and dynamic damages/PAGA groups
metric_group_categories <- list(
  time_summary_groups   = metric_groups[grepl("^Summary - Time Data$", metric_groups)],
  time_shift_groups     = metric_groups[grepl("^Shift Hours Analysis", metric_groups)],
  time_rounding_groups  = metric_groups[grepl("^Time Punch Rounding", metric_groups)],
  time_meal_analysis    = metric_groups[grepl("^Meal Period Analysis", metric_groups)],

  # Meal violations - split into summary and detail groups
  time_meal_violations_5_summary = metric_groups[grepl("^Meal Period Violations(_h)?$", metric_groups)],
  time_meal_violations_5_short   = metric_groups[grepl("^Meal Period Violations(_h)? - Short Detail(_h)?$", metric_groups)],
  time_meal_violations_5_late    = metric_groups[grepl("^Meal Period Violations(_h)? - Late Detail(_h)?$", metric_groups)],

  time_meal_violations_6_summary = metric_groups[grepl("^Meal Period Violations(_h)?$", metric_groups)],
  time_meal_violations_6_short   = metric_groups[grepl("^Meal Period Violations(_h)? - Short Detail(_h)?$", metric_groups)],
  time_meal_violations_6_late    = metric_groups[grepl("^Meal Period Violations(_h)? - Late Detail(_h)?$", metric_groups)],

  time_rest = metric_groups[grepl("^Rest Period Analysis", metric_groups)],

  pay_summary_groups = metric_groups[grepl("^Summary - Pay Data$", metric_groups)],
  pay_regular_rate = metric_groups[grepl("^Regular Rate", metric_groups)],

  # Damages overview groups (fixed structure for overview tab)
  damages_summary_groups = metric_groups[grepl("^Damages - Summary$", metric_groups)],
  damages_credits_groups = metric_groups[grepl("^Damages - Credits or Offsets", metric_groups)],
  damages_principal_groups = metric_groups[grepl("^Damages - Principal", metric_groups)],
  damages_interest_groups = metric_groups[grepl("^Damages - Interest", metric_groups)],
  damages_subtotal_groups = metric_groups[grepl("^Damages - Sub-Total", metric_groups)],
  damages_grand_total_groups = metric_groups[grepl("^Damages - Grand Total", metric_groups)],
  damages_wsv_groups = metric_groups[grepl("^Damages - Wage Statement Penalties", metric_groups)],
  damages_wt_groups = metric_groups[grepl("^Damages - Waiting Time Penalties", metric_groups)],

  # PAGA overview groups (fixed structure for overview tab)
  paga_summary_groups = metric_groups[grepl("^PAGA - Summary$", metric_groups)]
)

# Dynamically discover damages detail groups (everything starting with "Damages - " except overview/penalty/total)
damages_overview_patterns <- c("Summary", "Principal", "Interest", "Sub-Total",
                               "Grand Total", "Credits or Offsets",
                               "Wage Statement Penalties", "Waiting Time Penalties")
all_damages_groups <- metric_groups[grepl("^Damages - ", metric_groups)]
damages_detail_unique <- unique(all_damages_groups[!sapply(all_damages_groups, function(g) {
  any(sapply(damages_overview_patterns, function(p) grepl(paste0("^Damages - ", p), g)))
})])
cat("[APP]   Dynamic damages detail groups: ", paste(damages_detail_unique, collapse = ", "), "\n", sep = "")
flush.console()

# Add each damages detail group to metric_group_categories dynamically
for (dg in damages_detail_unique) {
  safe_name <- paste0("damages_detail_", gsub("[^a-zA-Z0-9]", "_", dg))
  metric_group_categories[[safe_name]] <- metric_groups[metric_groups == dg]
}

# Dynamically discover PAGA detail groups (everything starting with "PAGA - " except Summary)
all_paga_groups <- metric_groups[grepl("^PAGA - ", metric_groups)]
paga_detail_unique <- unique(all_paga_groups[!grepl("^PAGA - Summary$", all_paga_groups)])
cat("[APP]   Dynamic PAGA detail groups: ", paste(paga_detail_unique, collapse = ", "), "\n", sep = "")
flush.console()

# Add each PAGA detail group to metric_group_categories dynamically
for (pg in paga_detail_unique) {
  safe_name <- paste0("paga_detail_", gsub("[^a-zA-Z0-9]", "_", pg))
  metric_group_categories[[safe_name]] <- metric_groups[metric_groups == pg]
}

# Add the dynamic group lists and raw metric_groups so they're available inside server via list2env
metric_group_categories[["damages_detail_unique"]] <- damages_detail_unique
metric_group_categories[["paga_detail_unique"]] <- paga_detail_unique
metric_group_categories[["metric_groups"]] <- metric_groups

cat("[APP]   Total metric group categories: ", length(metric_group_categories), "\n", sep = "")
flush.console()

cat("[APP] Step 4/5: Loading analysis tables...\n")
flush.console()
analysis_tables <- list(
  pay_code_summary    = load_analysis_table(PAY_CODE_SUMMARY_FILE),
  rate_type_analysis  = load_analysis_table(RATE_TYPE_ANALYSIS_FILE),
  shift_hrs           = load_analysis_table(SHIFT_HRS_FILE),
  non_wrk_hrs         = load_analysis_table(NON_WRK_HRS_FILE),
  meal_period         = load_analysis_table(MEAL_PERIOD_FILE),
  meal_start_time     = load_analysis_table(MEAL_START_TIME_FILE),
  meal_quarter_hr     = load_analysis_table(MEAL_QUARTER_HR_FILE),
  employee_comparison = load_analysis_table(EMPLOYEE_COMPARISON_FILE)
)

cat("[APP] Step 5/5: Launching dashboard...\n")
cat("[APP] Bootstrap completed in ", sprintf("%.1f", as.numeric(difftime(Sys.time(), app_boot_start, units = "secs"))), " seconds\n", sep = "")
flush.console()
shinyApp(
  ui = ui(data_list, metric_spec),
  server = server(data_list, metric_spec, analysis_tables, metric_group_categories)
)

