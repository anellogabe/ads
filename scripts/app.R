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
    message("✓ Chrome found at: ", path)
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
      message("✓ Chrome found via pagedown: ", chrome_found)
    } else {
      message("⚠ Chrome not found - PDF export may not work. Set CHROME env var manually.")
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

# ---- CASE METADATA (optional - safe defaults) ----

if (!exists("case_name"))       case_name <- "Wage & Hour Analysis"
if (!exists("case_no"))         case_no <- "Not specified"
if (!exists("sample_size"))     sample_size <- "Not specified"
if (!exists("date_filed"))      date_filed <- Sys.Date()
if (!exists("complaint_date"))  complaint_date <- Sys.Date()
if (!exists("mediation_date"))  mediation_date <- Sys.Date()
if (!exists("class_dmgs_start_date")) class_dmgs_start_date <- Sys.Date() %m-% years(4)

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
      message("Loading: ", file)
      readRDS(path)
    } else {
      message("Not found (skipping): ", file)
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
  
  spec <- fread(metrics_spec_path)
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
  if (!file.exists(path)) return(NULL)
  
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
pipeline_to_display_format <- function(pipeline_results, group_names = NULL, include_years = TRUE) {
  if (is.null(pipeline_results) || nrow(pipeline_results) == 0) return(data.table())
  
  # Filter by metric groups if specified
  dt <- if (!is.null(group_names) && length(group_names) > 0) {
    pipeline_results[metric_group %in% group_names]
  } else {
    copy(pipeline_results)
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
    
    # If scenario column exists in spec, filter by it; otherwise use label-based filtering
    if (!is.null(scenario_filter) && "scenario" %in% names(dt)) {
      dt <- dt[
        is.na(scenario) |
          scenario == "" |
          scenario %in% scenario_filter
      ]
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
  # Always left-align the metric_col, plus "Key Group"/"Key Gp"/"Pay Code"/"Pay Code Categories" if present
  left_align_cols <- c(metric_col, "Key Group", "Key Gp", "Pay Code", "Pay Code Categories")
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
      options = list(placeholder = "All employees...")
    ),
    selectizeInput(
      "department_filter",
      "Department",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "All departments... (coming soon)")
    ),
    selectizeInput(
      "location_filter",
      "Location",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "All locations... (coming soon)")
    ),
    selectizeInput(
      "sample_filter",
      "Sample",
      choices = c("All" = "all", "Sample Only (1)" = "1", "Non-Sample (0)" = "0"),
      selected = "all",
      multiple = FALSE
    ),
    selectInput(
      "subclass_filter",
      "Subclass(es)",
      choices = c("All Employees" = "all", "Drivers" = "driver", "Aides" = "aide"),
      selected = "all",
      multiple = FALSE
    ),
    selectizeInput(
      "key_groups_filter",
      "Key Groups (Named Plaintiff(s), etc)",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "All key groups...")
    ),
    
    hr(),
    
    checkboxInput("show_extrapolation", "Show Extrapolated Values", value = FALSE),
    
    hr(),
    
    actionButton("apply_filters", "Apply Filters", class = "btn-primary w-100"),
    actionButton("reset_filters", "Reset All Filters", class = "btn-outline-secondary w-100 mt-2"),
    
    hr(),
   
    # Toggle extrapolation columns
    checkboxInput("toggle_extrap_cols", "Show Extrapolated Values", value = TRUE),
    
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
  
  # Version information
  app_version <- "v1.0.1"
  
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
      
      nav_panel(
        "Meal Violations (no waivers)",
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
      
      nav_panel(
        "Meal Violations (waivers)",
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
        withSpinner(DTOutput("table_damages_class_overview"), type = 6, color = "#2c3e50")
      ),
      
      nav_panel(
        "No Waivers",
        withSpinner(DTOutput("table_damages_class_no_waivers"), type = 6, color = "#2c3e50")
      ),
      
      nav_panel(
        "Waivers",
        withSpinner(DTOutput("table_damages_class_waivers"), type = 6, color = "#2c3e50")
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
        withSpinner(DTOutput("table_paga_overview"), type = 6, color = "#2c3e50")
      ),
      
      nav_panel(
        "No Waivers",
        withSpinner(DTOutput("table_paga_no_waivers"), type = 6, color = "#2c3e50")
      ),
      
      nav_panel(
        "Waivers",
        withSpinner(DTOutput("table_paga_waivers"), type = 6, color = "#2c3e50")
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
        
        # Employee-Period selection has been moved to sidebar
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
    
    # Cache storage for expensive operations
    cache <- reactiveValues(
      filtered_data_key = NULL,
      filtered_data_value = NULL,
      pipeline_results_key = NULL,
      pipeline_results_value = NULL
    )
    
    # Extract pre-computed metric group categories
    list2env(metric_group_categories, envir = environment())
    
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
    original_date_max <- max(
      max(data_list$shift_data1$Date, na.rm = TRUE),
      max(if (!is.null(pay_date_col)) data_list$pay1[[pay_date_col]] else as.Date(NA), na.rm = TRUE),
      na.rm = TRUE
    )
    
    # Server-side selectize for employee filter
    all_employee_ids <- c(data_list$shift_data1$ID, data_list$pay1$Pay_ID)
    if (!is.null(data_list$class1) && "Class_ID" %in% names(data_list$class1)) {
      all_employee_ids <- c(all_employee_ids, data_list$class1$Class_ID)
    }
    all_employee_ids <- sort(unique(all_employee_ids))
    updateSelectizeInput(session, "employee_filter", choices = all_employee_ids)
    
    # Server-side selectize for employee filter
    all_employee_ids <- c(data_list$shift_data1$ID, data_list$pay1$Pay_ID)
    if (!is.null(data_list$class1) && "Class_ID" %in% names(data_list$class1)) {
      all_employee_ids <- c(all_employee_ids, data_list$class1$Class_ID)
    }
    all_employee_ids <- sort(unique(all_employee_ids))
    updateSelectizeInput(session, "employee_filter", choices = all_employee_ids)
    
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
      
      if (length(input$employee_filter) > 0) {
        filters$ID     <- input$employee_filter
        filters$Pay_ID <- input$employee_filter
      }
      
      if (!is.null(input$sample_filter) && input$sample_filter != "all") {
        filters$Sample <- as.integer(input$sample_filter)
      }
      
      if (!is.null(input$subclass_filter) && input$subclass_filter != "all") {
        filters$Subclass     <- input$subclass_filter
        filters$Pay_Subclass <- input$subclass_filter
      }
      
      if (length(input$key_groups_filter) > 0) {
        filters$Key_Gps       <- input$key_groups_filter
        filters$Pay_Key_Gps   <- input$key_groups_filter
        filters$Class_Key_Gps <- input$key_groups_filter
      }
      
      # Subclass filter
      if (!is.null(input$subclass_filter) && input$subclass_filter != "all") {
        filters$Subclass <- input$subclass_filter
        filters$Pay_Subclass <- input$subclass_filter
      }
      
      # Key Groups filter
      if (length(input$key_groups_filter) > 0) {
        filters$Key_Gps <- input$key_groups_filter
        filters$Pay_Key_Gps <- input$key_groups_filter
        filters$Class_Key_Gps <- input$key_groups_filter
      }
      
      current_filters(filters)
    })
    
    # Reset filters
    observeEvent(input$reset_filters, {
      updateDateRangeInput(session, "date_range", start = original_date_min, end = original_date_max)
      updateSelectizeInput(session, "employee_filter", selected = character(0))
      updateSelectizeInput(session, "sample_filter", selected = "all")
      updateSelectInput(session, "subclass_filter", selected = "all")
      updateSelectizeInput(session, "key_groups_filter", selected = character(0))
      current_filters(list())
    })
    
    # PDF Modal Dialog
    observeEvent(input$open_pdf_modal, {
      showModal(modalDialog(
        title = div(
          style = "font-size: 20px; font-weight: bold;",
          icon("file-pdf", style = "margin-right: 10px; color: #667eea;"),
          "PDF Export Options"
        ),
        size = "xl",
        easyClose = TRUE,
        footer = div(
          style = "display: flex; justify-content: space-between; align-items: center; padding-left: 0;",
          div(
            style = "margin-left: 0;",
            actionButton("pdf_select_all", "Select All", class = "btn-sm btn-outline-primary"),
            actionButton("pdf_deselect_all", "Deselect All", class = "btn-sm btn-outline-secondary", style = "margin-left: 10px;")
          ),
          div(
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
          )
        ),
        
        # PDF Export Content
        div(
          style = "max-height: 70vh; overflow-y: auto; padding: 20px;",
          
          h5(style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; margin-bottom: 20px;",
             icon("clock"), " Time Data Statistics"),
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            checkboxGroupInput(
              "pdf_sections_col1",
              NULL,
              choices = c(
                "Overview Statistics" = "overview",
                "Time - Summary" = "time_summary",
                "Time - Shift Hours" = "time_shift_hours",
                "Time - Punch Rounding" = "time_rounding"
              ),
              selected = c("overview", "time_summary")
            ),
            checkboxGroupInput(
              "pdf_sections_col2",
              NULL,
              choices = c(
                "Meal - Analysis" = "meal_analysis",
                "Meal - Violations (no waivers)" = "meal_5hr",
                "Meal - Violations (waivers)" = "meal_6hr",
                "Rest Periods" = "rest_periods"
              ),
              selected = c()
            )
          ),
          
          hr(),
          h5(style = "color: #2c3e50; border-bottom: 2px solid #27ae60; padding-bottom: 10px; margin-bottom: 20px;",
             icon("dollar-sign"), " Pay Data Statistics"),
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            checkboxGroupInput(
              "pdf_sections_col3",
              NULL,
              choices = c(
                "Pay - Summary" = "pay_summary",
                "Pay - Regular Rate" = "pay_regular_rate",
                "Pay - Codes" = "pay_codes",
                "Pay - Rate Type" = "rate_type_analysis"
              ),
              selected = c("pay_summary")
            )
          ),
          
          hr(),
          h5(style = "color: #2c3e50; border-bottom: 2px solid #e74c3c; padding-bottom: 10px; margin-bottom: 20px;",
             icon("gavel"), " Class / Individual Claim Damages"),
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            checkboxGroupInput(
              "pdf_damages_class_col1",
              NULL,
              choices = c(
                "Overview (Summary)" = "damages_class_overview",
                "Meal Premium Damages" = "damages_meal",
                "Rest Premium Damages" = "damages_rest",
                "RROP Damages" = "damages_rrop"
              ),
              selected = c()
            ),
            checkboxGroupInput(
              "pdf_damages_class_col2",
              NULL,
              choices = c(
                "Off-the-Clock Damages" = "damages_otc",
                "Unpaid OT/DT Damages" = "damages_unpaid_ot",
                "Min Wage Damages" = "damages_min_wage",
                "Unreimbursed Expenses" = "damages_expenses"
              ),
              selected = c()
            ),
            checkboxGroupInput(
              "pdf_damages_class_col3",
              NULL,
              choices = c(
                "Wage Statement Penalties" = "damages_wsv",
                "Waiting Time Penalties" = "damages_wt"
              ),
              selected = c()
            )
          ),
          
          hr(),
          h5(style = "color: #2c3e50; border-bottom: 2px solid #9b59b6; padding-bottom: 10px; margin-bottom: 20px;",
             icon("balance-scale"), " PAGA Damages"),
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            checkboxGroupInput(
              "pdf_paga_col1",
              NULL,
              choices = c(
                "Overview (Summary)" = "paga_overview",
                "Meal Periods" = "paga_meal",
                "Rest Periods" = "paga_rest",
                "RROP" = "paga_rrop"
              ),
              selected = c()
            ),
            checkboxGroupInput(
              "pdf_paga_col2",
              NULL,
              choices = c(
                "Wage Statement (226)" = "paga_226",
                "Unpaid Wages (558)" = "paga_558",
                "Min Wage (1197.1)" = "paga_min_wage"
              ),
              selected = c()
            ),
            checkboxGroupInput(
              "pdf_paga_col3",
              NULL,
              choices = c(
                "Unreimbursed Expenses (2802)" = "paga_expenses",
                "Recordkeeping (1174.1)" = "paga_recordkeeping",
                "Waiting Time (203)" = "paga_waiting_time"
              ),
              selected = c()
            )
          ),
          
          hr(),
          h5(style = "color: #2c3e50; border-bottom: 2px solid #95a5a6; padding-bottom: 10px; margin-bottom: 20px;",
             icon("book"), " Additional Options"),
          div(
            checkboxInput("pdf_include_data_comparison", "Data Comparison (1-Page Landscape)", value = TRUE),
            checkboxInput("pdf_include_extrap", "Include Extrapolation Column", value = FALSE),
            checkboxInput("pdf_include_appendix", "Appendix Tables (All)", value = FALSE)
          )
        )
      ))
    })
    
    # PDF Select All button
    observeEvent(input$pdf_select_all, {
      # Time/Pay sections
      all_choices_col1 <- c("overview", "time_summary", "time_shift_hours", "time_rounding")
      all_choices_col2 <- c("meal_analysis", "meal_5hr", "meal_6hr", "rest_periods")
      all_choices_col3 <- c("pay_summary", "pay_regular_rate", "pay_codes", "rate_type_analysis")
      
      # Damages - Class sections
      all_damages_class_col1 <- c("damages_class_overview", "damages_meal", "damages_rest", "damages_rrop")
      all_damages_class_col2 <- c("damages_otc", "damages_unpaid_ot", "damages_min_wage", "damages_expenses")
      all_damages_class_col3 <- c("damages_wsv", "damages_wt")
      
      # PAGA sections
      all_paga_col1 <- c("paga_overview", "paga_meal", "paga_rest", "paga_rrop")
      all_paga_col2 <- c("paga_226", "paga_558", "paga_min_wage")
      all_paga_col3 <- c("paga_expenses", "paga_recordkeeping", "paga_waiting_time")
      
      updateCheckboxGroupInput(session, "pdf_sections_col1", selected = all_choices_col1)
      updateCheckboxGroupInput(session, "pdf_sections_col2", selected = all_choices_col2)
      updateCheckboxGroupInput(session, "pdf_sections_col3", selected = all_choices_col3)
      
      updateCheckboxGroupInput(session, "pdf_damages_class_col1", selected = all_damages_class_col1)
      updateCheckboxGroupInput(session, "pdf_damages_class_col2", selected = all_damages_class_col2)
      updateCheckboxGroupInput(session, "pdf_damages_class_col3", selected = all_damages_class_col3)
      
      updateCheckboxGroupInput(session, "pdf_paga_col1", selected = all_paga_col1)
      updateCheckboxGroupInput(session, "pdf_paga_col2", selected = all_paga_col2)
      updateCheckboxGroupInput(session, "pdf_paga_col3", selected = all_paga_col3)
      
      updateCheckboxInput(session, "pdf_include_appendix", value = TRUE)
      updateCheckboxInput(session, "pdf_include_data_comparison", value = TRUE)
      updateCheckboxInput(session, "pdf_include_extrap", value = FALSE)
    })
    
    # PDF Deselect All button
    observeEvent(input$pdf_deselect_all, {
      updateCheckboxGroupInput(session, "pdf_sections_col1", selected = character(0))
      updateCheckboxGroupInput(session, "pdf_sections_col2", selected = character(0))
      updateCheckboxGroupInput(session, "pdf_sections_col3", selected = character(0))
      
      updateCheckboxGroupInput(session, "pdf_damages_class_col1", selected = character(0))
      updateCheckboxGroupInput(session, "pdf_damages_class_col2", selected = character(0))
      updateCheckboxGroupInput(session, "pdf_damages_class_col3", selected = character(0))
      
      updateCheckboxGroupInput(session, "pdf_paga_col1", selected = character(0))
      updateCheckboxGroupInput(session, "pdf_paga_col2", selected = character(0))
      updateCheckboxGroupInput(session, "pdf_paga_col3", selected = character(0))
      
      updateCheckboxInput(session, "pdf_include_appendix", value = FALSE)
      updateCheckboxInput(session, "pdf_include_data_comparison", value = FALSE)
      updateCheckboxInput(session, "pdf_include_extrap", value = FALSE)
    })
    
    observeEvent(input$pdf_download_clicked, {
      removeModal()
    }, ignoreInit = TRUE)
    
    # Toggle extrapolation columns
    observeEvent(input$toggle_extrap_cols, {
      session$sendCustomMessage('toggleExtrapCols', input$toggle_extrap_cols)
    }, ignoreInit = FALSE)
    
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
      
      if (!is.null(filters$Sample) && "Sample" %in% names(shift_filtered)) {
        shift_filtered <- shift_filtered[Sample == filters$Sample]
      }
      
      if (!is.null(filters$Subclass) && "Subclass" %in% names(shift_filtered)) {
        shift_filtered <- shift_filtered[grepl(filters$Subclass, Subclass, ignore.case = TRUE)]
      }
      
      if (!is.null(filters$Key_Gps) && "Key_Gps" %in% names(shift_filtered)) {
        shift_filtered <- shift_filtered[Key_Gps %in% filters$Key_Gps]
      }
      
      # Pay filters
      if (!is.null(filters$date_min) && !is.null(pay_date_col)) {
        pay_filtered <- pay_filtered[get(pay_date_col) >= filters$date_min]
      }
      if (!is.null(filters$date_max) && !is.null(pay_date_col)) {
        pay_filtered <- pay_filtered[get(pay_date_col) <= filters$date_max]
      }
      if (!is.null(filters$Pay_ID))   pay_filtered <- pay_filtered[Pay_ID %in% filters$Pay_ID]
      
      if (!is.null(filters$Sample) && "Pay_Sample" %in% names(pay_filtered)) {
        pay_filtered <- pay_filtered[Pay_Sample == filters$Sample]
      }
      if (!is.null(filters$Pay_ID))   pay_filtered <- pay_filtered[Pay_ID %in% filters$Pay_ID]
      
      if (!is.null(filters$Pay_Subclass) && "Pay_Subclass" %in% names(pay_filtered)) {
        pay_filtered <- pay_filtered[grepl(filters$Pay_Subclass, Pay_Subclass, ignore.case = TRUE)]
      }
      
      if (!is.null(filters$Subclass) && "Subclass" %in% names(pay_filtered)) {
        pay_filtered <- pay_filtered[grepl(filters$Subclass, Subclass, ignore.case = TRUE)]
      }
      
      if (!is.null(filters$Pay_Key_Gps) && "Pay_Key_Gps" %in% names(pay_filtered)) {
        pay_filtered <- pay_filtered[Pay_Key_Gps %in% filters$Pay_Key_Gps]
      }
      
      # pp_data1
      pp_filtered <- NULL
      if (!is.null(data_list$pp_data1)) {
        pp_filtered <- data_list$pp_data1
        if (!is.null(filters$date_min) && "Period_End" %in% names(pp_filtered)) pp_filtered <- pp_filtered[Period_End >= filters$date_min]
        if (!is.null(filters$date_max) && "Period_End" %in% names(pp_filtered)) pp_filtered <- pp_filtered[Period_End <= filters$date_max]
        if (!is.null(filters$ID)       && "ID" %in% names(pp_filtered))        pp_filtered <- pp_filtered[ID %in% filters$ID]
      }
      
      # ee_data1
      ee_filtered <- NULL
      if (!is.null(data_list$ee_data1)) {
        ee_filtered <- data_list$ee_data1
        if (!is.null(filters$ID) && "ID" %in% names(ee_filtered)) ee_filtered <- ee_filtered[ID %in% filters$ID]
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
        if (!is.null(filters$Sample) && "Sample" %in% names(class_filtered)) {
          class_filtered <- class_filtered[Sample == filters$Sample]
        }
        if (!is.null(filters$Subclass) && "Subclass" %in% names(class_filtered)) {
          class_filtered <- class_filtered[grepl(filters$Subclass, Subclass, ignore.case = TRUE)]
        }
        if (!is.null(filters$Class_Key_Gps) && "Class_Key_Gps" %in% names(class_filtered)) {
          class_filtered <- class_filtered[Class_Key_Gps %in% filters$Class_Key_Gps]
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
    
    # Populate Key Groups filter choices
    observe({
      time_key_gps <- if (!is.null(data_list$shift_data1) && "Key_Gps" %in% names(data_list$shift_data1)) unique(data_list$shift_data1$Key_Gps) else character(0)
      pay_key_gps  <- if (!is.null(data_list$pay1)        && "Pay_Key_Gps" %in% names(data_list$pay1))        unique(data_list$pay1$Pay_Key_Gps) else character(0)
      class_key_gps<- if (!is.null(data_list$class1)      && "Class_Key_Gps" %in% names(data_list$class1))    unique(data_list$class1$Class_Key_Gps) else character(0)
      
      all_key_gps <- unique(c(time_key_gps, pay_key_gps, class_key_gps))
      all_key_gps <- all_key_gps[!is.na(all_key_gps) & all_key_gps != "" & tolower(all_key_gps) != "everyone else"]
      all_key_gps <- sort(all_key_gps)
      
      updateSelectizeInput(session, "key_groups_filter", choices = all_key_gps, server = TRUE)
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
      results <- pipeline_results()
      display <- pipeline_to_display_format(results, time_summary_groups, include_years = TRUE)
      create_dt_table(display)
    })
    
    output$table_shift_hours <- renderDT({
      results <- pipeline_results()
      display <- pipeline_to_display_format(results, time_shift_groups, include_years = TRUE)
      create_dt_table(display)
    })
    
    output$table_rounding_consolidated <- renderDT({
      results <- pipeline_results()
      display <- pipeline_to_display_format(results, time_rounding_groups, include_years = TRUE)
      create_dt_table(display)
    })
    
    output$table_meal_consolidated <- renderDT({
      results <- pipeline_results()
      display <- pipeline_to_display_format(results, time_meal_analysis, include_years = TRUE)
      create_dt_table(display)
    })
    
    output$table_meal_5hr_consolidated <- renderDT({
      results <- pipeline_results()
      display <- pipeline_to_display_format(results, time_meal_violations_5_summary, include_years = TRUE)
      create_dt_table(display)
    })
    
    output$table_meal_5hr_short_details <- renderDT({
      results <- pipeline_results()
      display <- pipeline_to_display_format(results, time_meal_violations_5_short, include_years = TRUE)
      create_dt_table(display)
    })
    
    output$table_meal_5hr_late_details <- renderDT({
      results <- pipeline_results()
      display <- pipeline_to_display_format(results, time_meal_violations_5_late, include_years = TRUE)
      create_dt_table(display)
    })
    
    output$table_meal_6hr_consolidated <- renderDT({
      results <- pipeline_results()
      display <- pipeline_to_display_format(results, time_meal_violations_6_summary, include_years = TRUE)
      create_dt_table(display)
    })
    
    output$table_meal_6hr_short_details <- renderDT({
      results <- pipeline_results()
      display <- pipeline_to_display_format(results, time_meal_violations_6_short, include_years = TRUE)
      create_dt_table(display)
    })
    
    output$table_meal_6hr_late_details <- renderDT({
      results <- pipeline_results()
      display <- pipeline_to_display_format(results, time_meal_violations_6_late, include_years = TRUE)
      create_dt_table(display)
    })
    
    output$table_rest_consolidated <- renderDT({
      results <- pipeline_results()
      display <- pipeline_to_display_format(results, time_rest, include_years = TRUE)
      create_dt_table(display)
    })
    
    output$table_pay_consolidated <- renderDT({
      results <- pipeline_results()
      display <- pipeline_to_display_format(results, pay_summary_groups, include_years = TRUE)
      create_dt_table(display)
    })
    
    output$table_rrop_consolidated <- renderDT({
      results <- pipeline_results()
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
        results <- pipeline_results()
        
        # Build section definitions for overview (summary metrics)
        sections <- list()
        
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
        
        if (length(damages_credits_groups) > 0 && is.character(damages_credits_groups)) {
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
        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "no waivers", "waivers"))
        
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
    
    # Class/Individual Claims - No Waivers
    output$table_damages_class_no_waivers <- renderDT({
      tryCatch({
        results <- pipeline_results()
        
        # Split each metric group by waiver status
        meal_split <- split_by_waiver(damages_meal_groups)
        rest_split <- split_by_waiver(damages_rest_groups)
        rrop_split <- split_by_waiver(damages_rrop_groups)
        otc_split <- split_by_waiver(damages_otc_groups)
        unpaid_ot_split <- split_by_waiver(damages_unpaid_ot_groups)
        min_wage_split <- split_by_waiver(damages_min_wage_groups)
        expenses_split <- split_by_waiver(damages_expenses_groups)
        wsv_split <- split_by_waiver(damages_wsv_groups)
        wt_split <- split_by_waiver(damages_wt_groups)
        total_split <- split_by_waiver(damages_class_total_groups)
        
        # Build section definitions for no-waiver metrics
        sections <- list()
        
        if (length(meal_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "MEAL PERIOD DAMAGES",
            groups = meal_split$no_waiver
          )
        }
        
        if (length(rest_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "REST PERIOD DAMAGES",
            groups = rest_split$no_waiver
          )
        }
        
        if (length(rrop_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "RROP DAMAGES",
            groups = rrop_split$no_waiver
          )
        }
        
        if (length(otc_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "OFF-THE-CLOCK DAMAGES",
            groups = otc_split$no_waiver
          )
        }
        
        if (length(unpaid_ot_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "UNPAID OT/DT DAMAGES",
            groups = unpaid_ot_split$no_waiver
          )
        }
        
        if (length(min_wage_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "UNPAID WAGES (MIN WAGE) DAMAGES",
            groups = min_wage_split$no_waiver
          )
        }
        
        if (length(expenses_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "UNREIMBURSED EXPENSES DAMAGES",
            groups = expenses_split$no_waiver
          )
        }
        
        if (length(wsv_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "WAGE STATEMENT PENALTIES",
            groups = wsv_split$no_waiver
          )
        }
        
        if (length(wt_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "WAITING TIME PENALTIES",
            groups = wt_split$no_waiver
          )
        }
        
        # TOTAL DAMAGES removed - now in Overview tab with all scenarios
        
        if (length(sections) == 0) {
          return(datatable(data.table(Message = "No damages data available for no waivers scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }
        
        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "no waivers"))
        
        # Filter out waiver metrics from no-waiver tab based on metric labels
        # (fallback for old spec without scenario column)
        display <- filter_metrics_by_label(display, include_waivers = FALSE)
        
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
    
    # Class/Individual Claims - Waivers
    output$table_damages_class_waivers <- renderDT({
      tryCatch({
        results <- pipeline_results()
        
        # Build section definitions for ALL damages metrics (same as no waivers)
        # but will filter by waivers scenario
        sections <- list()
        
        if (length(damages_meal_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "MEAL PERIOD DAMAGES",
            groups = damages_meal_groups
          )
        }
        
        if (length(damages_rest_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "REST PERIOD DAMAGES",
            groups = damages_rest_groups
          )
        }
        
        if (length(damages_rrop_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "RROP DAMAGES",
            groups = damages_rrop_groups
          )
        }
        
        if (length(damages_otc_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "OFF-THE-CLOCK DAMAGES",
            groups = damages_otc_groups
          )
        }
        
        if (length(damages_unpaid_ot_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "UNPAID OT/DT DAMAGES",
            groups = damages_unpaid_ot_groups
          )
        }
        
        if (length(damages_min_wage_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "UNPAID WAGES (MIN WAGE) DAMAGES",
            groups = damages_min_wage_groups
          )
        }
        
        if (length(damages_expenses_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "UNREIMBURSED EXPENSES DAMAGES",
            groups = damages_expenses_groups
          )
        }
        
        if (length(damages_wsv_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "WAGE STATEMENT PENALTIES",
            groups = damages_wsv_groups
          )
        }
        
        if (length(damages_wt_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "WAITING TIME PENALTIES",
            groups = damages_wt_groups
          )
        }
        
        # TOTAL DAMAGES removed - now in Overview tab with all scenarios
        
        if (length(sections) == 0) {
          return(datatable(data.table(Message = "No damages data available for waivers scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }
        
        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "waivers"))
        
        # Filter out no-waiver metrics from waiver tab based on metric labels
        # (fallback for old spec without scenario column)
        display <- filter_metrics_by_label(display, include_waivers = TRUE)
        
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
    
    # PAGA - Overview
    output$table_paga_overview <- renderDT({
      tryCatch({
        results <- pipeline_results()
        
        # Build section definitions for PAGA overview (summary metrics)
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
        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "no waivers", "waivers"))
        
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
    
    # PAGA - No Waivers
    output$table_paga_no_waivers <- renderDT({
      tryCatch({
        results <- pipeline_results()
        
        # Split each PAGA metric group by waiver status
        meal_split <- split_by_waiver(paga_meal_groups)
        rest_split <- split_by_waiver(paga_rest_groups)
        rrop_split <- split_by_waiver(paga_rrop_groups)
        s226_split <- split_by_waiver(paga_226_groups)
        s558_split <- split_by_waiver(paga_558_groups)
        min_wage_split <- split_by_waiver(paga_min_wage_groups)
        expenses_split <- split_by_waiver(paga_expenses_groups)
        recordkeeping_split <- split_by_waiver(paga_recordkeeping_groups)
        waiting_time_split <- split_by_waiver(paga_waiting_time_groups)
        
        # Build section definitions for no-waiver metrics
        sections <- list()
        
        if (length(meal_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - MEAL PERIODS",
            groups = meal_split$no_waiver
          )
        }
        
        if (length(rest_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - REST PERIODS",
            groups = rest_split$no_waiver
          )
        }
        
        if (length(rrop_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - REGULAR RATE (RROP)",
            groups = rrop_split$no_waiver
          )
        }
        
        if (length(s226_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - WAGE STATEMENT (226)",
            groups = s226_split$no_waiver
          )
        }
        
        if (length(s558_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - UNPAID WAGES (558)",
            groups = s558_split$no_waiver
          )
        }
        
        if (length(min_wage_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - MIN WAGE (1197.1)",
            groups = min_wage_split$no_waiver
          )
        }
        
        if (length(expenses_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - UNREIMBURSED EXPENSES (2802)",
            groups = expenses_split$no_waiver
          )
        }
        
        if (length(recordkeeping_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - RECORDKEEPING (1174)",
            groups = recordkeeping_split$no_waiver
          )
        }
        
        if (length(waiting_time_split$no_waiver) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - WAITING TIME (203)",
            groups = waiting_time_split$no_waiver
          )
        }
        
        if (length(sections) == 0) {
          return(datatable(data.table(Message = "No PAGA data available for no waivers scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }
        
        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "no waivers"))
        
        # Filter out waiver metrics from no-waiver tab based on metric labels
        # (fallback for old spec without scenario column)
        display <- filter_metrics_by_label(display, include_waivers = FALSE)
        
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
    
    # PAGA - Waivers
    output$table_paga_waivers <- renderDT({
      tryCatch({
        results <- pipeline_results()
        
        # Build section definitions for ALL PAGA metrics (same as no waivers)
        # but will filter by waivers scenario
        sections <- list()
        
        if (length(paga_meal_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - MEAL PERIODS",
            groups = paga_meal_groups
          )
        }
        
        if (length(paga_rest_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - REST PERIODS",
            groups = paga_rest_groups
          )
        }
        
        if (length(paga_rrop_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - REGULAR RATE (RROP)",
            groups = paga_rrop_groups
          )
        }
        
        if (length(paga_226_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - WAGE STATEMENT (226)",
            groups = paga_226_groups
          )
        }
        
        if (length(paga_558_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - UNPAID WAGES (558)",
            groups = paga_558_groups
          )
        }
        
        if (length(paga_min_wage_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - MIN WAGE (1197.1)",
            groups = paga_min_wage_groups
          )
        }
        
        if (length(paga_expenses_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - UNREIMBURSED EXPENSES (2802)",
            groups = paga_expenses_groups
          )
        }
        
        if (length(paga_recordkeeping_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - RECORDKEEPING (1174)",
            groups = paga_recordkeeping_groups
          )
        }
        
        if (length(paga_waiting_time_groups) > 0) {
          sections[[length(sections) + 1]] <- list(
            section_name = "PAGA - WAITING TIME (203)",
            groups = paga_waiting_time_groups
          )
        }
        
        if (length(sections) == 0) {
          return(datatable(data.table(Message = "No PAGA data available for waivers scenario"),
                           rownames = FALSE, options = list(dom = 't')))
        }
        
        display <- pipeline_to_damages_format(results, sections, scenario_filter = c("all", "waivers"))
        
        # Filter out no-waiver metrics from waiver tab based on metric labels
        # (fallback for old spec without scenario column)
        display <- filter_metrics_by_label(display, include_waivers = TRUE)
        
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
    
    # ===========================================================================
    # EMPLOYEE-PERIOD EXAMPLE TAB
    # ===========================================================================
    
    # Populate dropdown with combined unique employee-periods
    observe({
      data <- filtered_data()
      
      # Get unique ID_Period_End from shift data
      shift_periods <- if (!is.null(data$shift_data1) && "ID_Period_End" %in% names(data$shift_data1)) {
        unique(data$shift_data1$ID_Period_End)
      } else {
        character(0)
      }
      
      # Get unique Pay_ID_Period_End from pay data
      pay_periods <- if (!is.null(data$pay1) && "Pay_ID_Period_End" %in% names(data$pay1)) {
        unique(data$pay1$Pay_ID_Period_End)
      } else {
        character(0)
      }
      
      # Combine and sort unique periods
      all_periods <- sort(unique(c(shift_periods, pay_periods)))
      
      updateSelectizeInput(session, "example_period_select", choices = all_periods)
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
      log_summary_file <- file.path(OUT_DIR, "analysis_log_summary.rds")
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
      log_file <- file.path(OUT_DIR, "analysis_log.txt")
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
        log_file <- file.path(OUT_DIR, "analysis_log.txt")
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
          <h4>Data Processing</h4>
          <ul>
            <li><strong>Time Records:</strong> Each shift represents a distinct work period with In/Out punch times. Shifts are analyzed for hours worked, meal periods, and rest periods.</li>
            <li><strong>Pay Records:</strong> Pay data is matched to time data by employee ID and period end date to enable rate validation and damages calculations.</li>
            <li><strong>Missing Data:</strong> Records with missing critical fields (ID, Date) are flagged and may be excluded from analysis.</li>
            <li><strong>Shift Classification:</strong> Shifts are categorized using a ", shift_hrs_cutoff, "-hour cutoff (see Non Work Hours table).</li>
          </ul>

          <h4>Meal & Rest Period Violations</h4>
          <ul>
            <li><strong>Meal Period Timing (No Waivers):</strong> First meal period must start by the end of the 5th hour of work (shift_hrs > 5.01). Second meal period required for shifts > 10 hours (shift_hrs > 10.01).</li>
            <li><strong>Meal Period Timing (Waivers):</strong> When waivers apply, first meal period may be delayed to the end of the 6th hour (shift_hrs > 6.01). Second meal period delayed to > 12 hours (shift_hrs > 12.01).</li>
            <li><strong>Meal Period Duration:</strong> Minimum 30 minutes (0.49 hours) required for compliant meal period. Periods between 0.01 and 0.49 hours are flagged as 'Short' violations.</li>
            <li><strong>De Minimis Buffer:</strong> A 0.01 hour (36-second) buffer is applied to meal period calculations to account for rounding and minor timing variances.</li>
            <li><strong>Rest Period Eligibility:</strong> One 10-minute rest period required for shifts > 3.5 hours (shift_hrs > 3.51). Additional rest periods required for longer shifts (>6 hrs, >10 hrs, >14 hrs per 4-hour rule).</li>
            <li><strong>Waiver Analysis:</strong> Meal period waivers are analyzed as separate scenarios: 'no waivers' uses 5-hour rule, 'waivers' uses 6-hour rule.</li>
          </ul>

          <h4>Regular Rate of Pay (RROP)</h4>
          <ul>
            <li><strong>Calculation Method:</strong> RROP = (Total straight-time compensation including differential pay + non-discretionary bonuses) ÷ (Total straight-time hours). Overtime premiums, discretionary bonuses, and time off are excluded from the calculation.</li>
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
            <li><strong>Wage Statement Violations:</strong> $50 initial pay period penalty + $100 subsequent pay period penalties, capped at $4,000 per employee (Labor Code §226).</li>
            <li><strong>Waiting Time Penalties:</strong> Up to 30 days of wages for terminated employees who did not receive timely final payment, calculated using RROP or final base rate (Labor Code §203).</li>
          </ul>

          <h4>PAGA Penalties</h4>
          <ul>
            <li><strong>Standard Penalties:</strong> $", initial_pp_penalty, " initial violation + $", subsequent_pp_penalty, " subsequent violations per employee per pay period (Labor Code §2699).</li>
            <li><strong>Labor Code §226 (Wage Statements):</strong> $", initial_pp_penalty_226, " initial + $", subsequent_pp_penalty_226, " subsequent penalties for wage statement violations.</li>
            <li><strong>Labor Code §558 (Meal/Rest):</strong> $", initial_pp_penalty_558, " initial + $", subsequent_pp_penalty_558, " subsequent penalties for meal and rest period violations.</li>
            <li><strong>Labor Code §1174:</strong> $", penalty_1174, " penalty for itemized wage statement violations.</li>
          </ul>

          ", extrap_text, "
        </div>
      "))
    })

    # ===========================================================================
    # Version and Documentation Outputs
    # ===========================================================================
    
    output$dashboard_version <- renderText({
      if (exists("app_version")) {
        gsub("^v", "", app_version)  # Remove 'v' prefix if present
      } else {
        "1.0.1"
      }
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
        filter_text <- paste("⚠ ACTIVE FILTERS:", paste(filter_parts, collapse = " | "))
        return(HTML(paste0(filter_text, " | <a href='#' onclick='Shiny.setInputValue(\"reset_filters\", Math.random()); return false;' style='color: white; text-decoration: underline;'>Reset All Filters</a>")))
      }
      
      return(NULL)
    })
    
    # ===========================================================================
    # Example Tab Outputs
    # ===========================================================================
    
    # Punch Detail (time1) - Show punch records as small table
    output$table_example_punches <- renderDT({
      req(input$example_period_select)

      if (is.null(data_list$time1)) {
        return(datatable(data.table(Message = "No time1 data available"), rownames = FALSE, options = list(dom = 't')))
      }

      # Check if ID_Period_End exists
      if (!"ID_Period_End" %in% names(data_list$time1)) {
        return(datatable(data.table(Message = "ID_Period_End column not found in time1"), rownames = FALSE, options = list(dom = 't')))
      }

      # Filter to selected period
      filtered <- data_list$time1[ID_Period_End == input$example_period_select]

      if (nrow(filtered) == 0) {
        return(datatable(data.table(Message = "No punch records for this period"), rownames = FALSE, options = list(dom = 't')))
      }

      # Select specific punch columns - use exact names from your data
      # Based on your column list: Date, ID, punch_time, punch_type, hrs_wkd, mp_hrs, shift_hrs, Hours
      desired_cols <- c("ID", "Date", "punch_time", "punch_type", "hrs_wkd", "mp_hrs", "shift_hrs", "Hours")
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
      base_cols <- c("Pay_ID", "Pay_Period_End", "Pay_Date", "Pay_Code", "Pay_Hours",
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
          "✓", "", if (venn$class_total > 0) "",
          "✓", if (venn$class_total > 0) "✓", if (venn$class_total > 0) "",
          if (venn$class_total > 0) "✓", "-"
        ),
        `In Pay` = c(
          "", "✓", if (venn$class_total > 0) "",
          "✓", if (venn$class_total > 0) "", if (venn$class_total > 0) "✓",
          if (venn$class_total > 0) "✓", "-"
        ),
        `In Class` = if (venn$class_total > 0) c(
          "", "", "✓",
          "", "✓", "✓",
          "✓", "-"
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
        rownames = FALSE,
        class = 'cell-border stripe hover compact'
      ) %>%
        formatStyle(
          'Category',
          target = 'row',
          fontWeight = styleEqual('TOTAL UNIQUE EMPLOYEES', 'bold')
        ) %>%
        formatStyle(
          'Count',
          target = 'row',
          fontWeight = styleEqual(
            matrix_data[Category == "TOTAL UNIQUE EMPLOYEES", Count],
            'bold'
          )
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
        
        # Individual Class Damages
        if (length(damages_meal_groups) > 0) add_sheet("Class - Meal", pipeline_to_display_format(results, damages_meal_groups))
        if (length(damages_rest_groups) > 0) add_sheet("Class - Rest", pipeline_to_display_format(results, damages_rest_groups))
        if (length(damages_rrop_groups) > 0) add_sheet("Class - RROP", pipeline_to_display_format(results, damages_rrop_groups))
        if (length(damages_otc_groups) > 0) add_sheet("Class - OTC", pipeline_to_display_format(results, damages_otc_groups))
        if (length(damages_unpaid_ot_groups) > 0) add_sheet("Class - Unpaid OT", pipeline_to_display_format(results, damages_unpaid_ot_groups))
        if (length(damages_min_wage_groups) > 0) add_sheet("Class - Min Wage", pipeline_to_display_format(results, damages_min_wage_groups))
        if (length(damages_expenses_groups) > 0) add_sheet("Class - Expenses", pipeline_to_display_format(results, damages_expenses_groups))
        if (length(damages_wsv_groups) > 0) add_sheet("Class - Wage Stmt", pipeline_to_display_format(results, damages_wsv_groups))
        if (length(damages_wt_groups) > 0) add_sheet("Class - Waiting Time", pipeline_to_display_format(results, damages_wt_groups))
        
        # PAGA Overview
        if (length(paga_summary_groups) > 0) add_sheet("PAGA - Overview", pipeline_to_display_format(results, paga_summary_groups))
        if (length(paga_meal_groups) > 0) add_sheet("PAGA - Meal", pipeline_to_display_format(results, paga_meal_groups))
        if (length(paga_rest_groups) > 0) add_sheet("PAGA - Rest", pipeline_to_display_format(results, paga_rest_groups))
        if (length(paga_rrop_groups) > 0) add_sheet("PAGA - RROP", pipeline_to_display_format(results, paga_rrop_groups))
        if (length(paga_226_groups) > 0) add_sheet("PAGA - 226", pipeline_to_display_format(results, paga_226_groups))
        if (length(paga_558_groups) > 0) add_sheet("PAGA - 558", pipeline_to_display_format(results, paga_558_groups))
        
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
        
        # Source generate_pdf.R if not already loaded
        if (!exists("generate_report")) {
          source(file.path(CASE_DIR, "scripts", "generate_pdf.R"))
        }
        
        
        # Get data needed by generate_report() - it checks environment variables
        data <- filtered_data()
        shift_data1 <- data$shift_data1  # Make available in environment
        pay1 <- data$pay1               # Make available in environment
        class1 <- data$class1           # Make available in environment
        results <- pipeline_results()    # Make available as "results"
        message("Data loaded for generate_report()")
        
        
        # Call standalone PDF generator
        generate_report(
          output_file = file,
          sections = c("time", "pay", "class", "paga", "analysis"),
          include_extrap = isTRUE(input$pdf_include_extrap),
          include_appendix = isTRUE(input$pdf_include_appendix),
          include_data_comparison = isTRUE(input$pdf_include_data_comparison),
          verbose = FALSE  # Don't show progress bar in Shiny
        )
        
        message("PDF generation complete")
        
      }
    )
  }
}

# ---- RUN APP ----

message("Loading data...")
data_list <- load_data()
metric_spec <- load_metric_spec()

message("Pre-computing metric groups...")
# Categorize metric groups for consolidation (done once at startup for performance)
metric_groups <- unique(metric_spec$metric_group)
metric_group_categories <- list(
  time_summary_groups   = metric_groups[grepl("^Summary - Time Data$", metric_groups)],
  time_shift_groups     = metric_groups[grepl("^Shift Hours Analysis", metric_groups)],
  time_rounding_groups  = metric_groups[grepl("^Time Punch Rounding", metric_groups)],
  time_meal_analysis    = metric_groups[grepl("^Meal Period Analysis", metric_groups)],
  
  # Meal violations - split into summary and detail groups
  time_meal_violations_5_summary = metric_groups[grepl("^Meal Period Violations$", metric_groups)],
  time_meal_violations_5_short   = metric_groups[grepl("^Meal Period Violations - Short Detail", metric_groups)],
  time_meal_violations_5_late    = metric_groups[grepl("^Meal Period Violations - Late Detail", metric_groups)],
  
  time_meal_violations_6_summary = metric_groups[grepl("^Meal Period Violations$", metric_groups)],
  time_meal_violations_6_short   = metric_groups[grepl("^Meal Period Violations - Short Detail", metric_groups)],
  time_meal_violations_6_late    = metric_groups[grepl("^Meal Period Violations - Late Detail", metric_groups)],
  
  time_rest = metric_groups[grepl("^Rest Period Analysis", metric_groups)],
  
  pay_summary_groups = metric_groups[grepl("^Summary - Pay Data$", metric_groups)],
  pay_regular_rate = metric_groups[grepl("^Regular Rate", metric_groups)],
  
  # Damages metric groups (Class/Individual Claims)
  damages_summary_groups = metric_groups[grepl("^Damages - Summary$", metric_groups)],
  damages_credits_groups = metric_groups[grepl("^Damages - Credits or Offsets", metric_groups)],
  damages_principal_groups = metric_groups[grepl("^Damages - Principal", metric_groups)],
  damages_interest_groups = metric_groups[grepl("^Damages - Interest", metric_groups)],
  damages_subtotal_groups = metric_groups[grepl("^Damages - Sub-Total", metric_groups)],
  damages_grand_total_groups = metric_groups[grepl("^Damages - Grand Total", metric_groups)],
  
  damages_meal_groups = metric_groups[grepl("^Damages - Meal Premiums", metric_groups)],
  damages_rest_groups = metric_groups[grepl("^Damages - Rest Premiums", metric_groups)],
  damages_rrop_groups = metric_groups[grepl("^Damages - Regular Rate of Pay", metric_groups)],
  
  damages_otc_groups       = metric_groups[grepl("^Damages - Off-the-Clock", metric_groups)],
  damages_unpaid_ot_groups = metric_groups[grepl("^Damages - Unpaid OT/DT", metric_groups)],
  damages_min_wage_groups  = metric_groups[grepl("^Damages - Unpaid Wages \\(Min Wage\\)", metric_groups)],
  damages_expenses_groups  = metric_groups[grepl("^Damages - Unreimbursed Expenses", metric_groups)],
  
  damages_wsv_groups         = metric_groups[grepl("^Damages - Wage Statement Penalties", metric_groups)],
  damages_wt_groups          = metric_groups[grepl("^Damages - Waiting Time Penalties", metric_groups)],
  damages_class_total_groups = metric_groups[grepl("^Damages - Grand Total", metric_groups)],
  
  # PAGA metric groups
  paga_summary_groups = metric_groups[grepl("^PAGA - Summary$", metric_groups)],
  
  paga_meal_groups = metric_groups[grepl("^PAGA - Meal Periods", metric_groups)],
  paga_rest_groups = metric_groups[grepl("^PAGA - Rest Periods", metric_groups)],
  paga_rrop_groups = metric_groups[grepl("^PAGA - Regular Rate", metric_groups)],
  paga_226_groups  = metric_groups[grepl("^PAGA - Wage Statement", metric_groups)],
  paga_558_groups  = metric_groups[grepl("^PAGA - Unpaid Wages", metric_groups)],
  
  paga_min_wage_groups      = metric_groups[grepl("^PAGA - Min Wage", metric_groups)],
  paga_expenses_groups      = metric_groups[grepl("^PAGA - Unreimbursed Expenses", metric_groups)],
  paga_recordkeeping_groups = metric_groups[grepl("^PAGA - Recordkeeping", metric_groups)],
  paga_waiting_time_groups  = metric_groups[grepl("^PAGA - Waiting Time", metric_groups)]
)

message("Loading analysis tables...")
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

message("Starting dashboard...")
shinyApp(
  ui = ui(data_list, metric_spec),
  server = server(data_list, metric_spec, analysis_tables, metric_group_categories)
)