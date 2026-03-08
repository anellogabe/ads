# ==============================================================================
# PROPRIETARY AND CONFIDENTIAL
# Anello Data Solutions LLC
# 
# This file contains proprietary information and trade secrets.
# Unauthorized copying, distribution, or use is strictly prohibited.
# For authorized use by ANELLO DATA SOLUTIONS LLC contracted analysts only.

# ============================================================================
# STANDALONE PDF REPORT GENERATOR
# Run this script directly without launching the dashboard
#
# Usage:
#   source("generate_pdf.R")
#   generate_report()                                    
#   generate_report(sections = "time")                   
#   generate_report(sections = c("time", "pay"))         
#   generate_report(include_extrap = TRUE)               
#   generate_report(include_appendix = TRUE)             
#   generate_report(include_data_comparison = TRUE)      
#   generate_report(output_file = "My_Report.pdf")       
# ============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(pagedown)
})

# ============================================================================
# MAIN FUNCTION
# ============================================================================

generate_report <- function(
    output_file = NULL,
    sections = c("time", "pay", "class", "paga", "analysis"),
    include_extrap = FALSE,
    include_credits = TRUE,
    include_appendix = FALSE,
    include_data_comparison = FALSE,
    include_assumptions = TRUE,
    class_scenarios = c("no waivers", "waivers", "hybrid"),
    paga_scenarios = c("no waivers", "waivers", "hybrid"),
    # Granular subsection control: character vector of checkbox keys from the PDF
    # modal (e.g. c("time_summary", "meal_analysis", ...)). NULL means include all.
    selected_subsections = NULL,
    verbose = TRUE
) {

    # Helper: returns TRUE if a specific subsection key is enabled.
  # When selected_subsections is NULL every subsection is included.
  include_sub <- function(key) is.null(selected_subsections) || key %in% selected_subsections

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

  is_truthy_flag <- function(x) {
    if (is.null(x)) return(logical(0))
    if (is.logical(x)) return(!is.na(x) & x)
    if (is.numeric(x)) return(!is.na(x) & x != 0)
    lx <- tolower(trimws(as.character(x)))
    !is.na(lx) & lx %in% c("true", "t", "1", "yes", "y")
  }

  get_credit_row_mask <- function(dt, spec = NULL) {
    if (is.null(dt) || nrow(dt) == 0) return(logical(0))

    mask <- rep(FALSE, nrow(dt))

    if ("other_credit" %in% names(dt)) {
      mask <- mask | is_truthy_flag(dt$other_credit)
    }

    if (!is.null(spec) &&
        all(c("metric_group", "metric_label", "other_credit") %in% names(spec)) &&
        all(c("metric_group", "metric_label") %in% names(dt))) {
      spec_dt <- as.data.table(spec)
      spec_dt <- spec_dt[is_truthy_flag(other_credit), .(metric_group, metric_label)]

      if (nrow(spec_dt) > 0) {
        spec_keys <- unique(paste(spec_dt$metric_group, spec_dt$metric_label, sep = "\r"))
        dt_keys <- paste(dt$metric_group, dt$metric_label, sep = "\r")
        mask <- mask | (dt_keys %in% spec_keys)
      }
    }

    mask
  }
  local_sections <- sections
  local_class_scenarios <- unique(normalize_scenario_value(class_scenarios))
  local_class_scenarios <- local_class_scenarios[!is.na(local_class_scenarios) & nzchar(local_class_scenarios)]
  local_paga_scenarios <- unique(normalize_scenario_value(paga_scenarios))
  local_paga_scenarios <- local_paga_scenarios[!is.na(local_paga_scenarios) & nzchar(local_paga_scenarios)]
  
  # Count steps more accurately based on new section structure
  total_steps <- 5  # Base: loading data, spec, analysis tables, generating PDF, final
  current_step <- 0
  
  if ("time" %in% local_sections) total_steps <- total_steps + 30  # summaries, meal analysis, violations, rest, rounding
  if ("pay" %in% local_sections) total_steps <- total_steps + 5   # summary, regular rate sections
  if ("class" %in% local_sections) total_steps <- total_steps + 15 # damages overview + breakdowns
  if ("paga" %in% local_sections) total_steps <- total_steps + 12  # summary + all PAGA sections
  if ("analysis" %in% local_sections) total_steps <- total_steps + 2
  if (include_appendix) total_steps <- total_steps + 12  # shift hours + distributions
  if (include_data_comparison) total_steps <- total_steps + 1
  if (include_assumptions) total_steps <- total_steps + 1  # assumptions summary
  
  progress <- function(msg) {
    current_step <<- current_step + 1
    if (verbose) {
      pct <- min(100, round(current_step / max(total_steps, 1) * 100))
      filled <- min(30, max(0, round(pct / 100 * 30)))
      bar <- paste0("[", strrep("=", filled), strrep(" ", 30 - filled), "]")
      cat(sprintf("\r%s %3d%% | %s", bar, pct, msg))
      if (current_step >= total_steps) cat("\n")
      flush.console()
    }
  }
  
  cat("\n")
  cat("==================================================\n")
  cat("         STANDALONE PDF GENERATOR\n")
  cat("==================================================\n")
  cat("Sections:", paste(local_sections, collapse = ", "), "\n")
  cat("Include Extrapolation:", include_extrap, "\n")
  cat("Include Credits:", include_credits, "\n")
  cat("Include Appendix:", include_appendix, "\n")
  cat("Include Data Comparison:", include_data_comparison, "\n")
  cat("==================================================\n\n")
  
  DATA_DIR <- OUT_DIR
  cat("DATA_DIR:", DATA_DIR, "\n\n")
  
  progress("Loading data for case info")
  
  # Load data - use existing from environment or load from files
  # Dashboard uses shift_data1, standalone may use shift1
  if (exists("shift_data1") && is.data.table(shift_data1)) {
    local_shift <- shift_data1
  } else if (exists("shift1") && is.data.table(shift1)) {
    local_shift <- shift1
  } else {
    local_shift <- readRDS(file.path(DATA_DIR, "Time Shift Data.rds"))
  }
  
  if (exists("pay1") && is.data.table(pay1)) {
    local_pay <- pay1
  } else {
    local_pay <- readRDS(file.path(DATA_DIR, "Pay Data.rds"))
  }
  
  local_class <- if (exists("class1") && is.data.table(class1)) {
    class1
  } else {
    tryCatch({
      class_file <- file.path(PROCESSED_DIR, "class_processed.rds")
      if (file.exists(class_file)) readRDS(class_file) else NULL
    }, error = function(e) NULL)
  }
  
  progress("Loading metric spec")
  
  # Use existing metrics_spec from environment if available, otherwise load from file
  if (exists("metrics_spec") && is.data.table(metrics_spec)) {
    metric_spec <- copy(metrics_spec)
  } else if (exists("metric_spec") && is.data.table(metric_spec)) {
    metric_spec <- copy(metric_spec)
  } else {
    spec_path <- file.path(CASE_DIR, "scripts", "metrics_spec.csv")
    if (!file.exists(spec_path)) stop("Cannot find metrics_spec.csv and no metrics_spec in environment")
    metric_spec <- fread(spec_path)
    setDT(metric_spec)
  }
  
  # Helper to fix date columns in tables
  fix_date_columns <- function(dt) {
    if (!is.null(dt) && nrow(dt) > 0) {
      for (col in names(dt)) {
        # Check if it looks like a date column and is character
        if (grepl("date", col, ignore.case = TRUE) && is.character(dt[[col]])) {
          message("  Converting column '", col, "' from character to Date")
          dt[[col]] <- tryCatch(
            as.Date(dt[[col]]),
            error = function(e) {
              message("  Warning: Could not convert '", col, "' to Date: ", e$message)
              dt[[col]]  # Keep as character if conversion fails
            }
          )
        }
      }
    }
    return(dt)
  }
  
  # Use existing final_table/results from environment if available
  # Dashboard uses "results", standalone may use "final_table"
  if (exists("results") && is.data.table(results)) {
    results_table <- copy(results)
    results_table <- fix_date_columns(results_table)
    cat("Using existing results from environment\n")
  } else if (exists("final_table") && is.data.table(final_table)) {
    results_table <- copy(final_table)
    results_table <- fix_date_columns(results_table)
    cat("Using existing final_table from environment\n")
  } else {
    # Try to load from file
    results_file <- file.path(DATA_DIR, "Analysis.rds")
    if (file.exists(results_file)) {
      results_table <- readRDS(results_file)
      results_table <- fix_date_columns(results_table)
      cat("Loaded results from Analysis.rds\n")
    } else {
      stop("Cannot find results/final_table in environment or Analysis.rds file")
    }
  }
  
  metric_groups <- unique(metric_spec$metric_group)
  
  # Match exact metric_group names from metric spec
  # TIME
  time_summary <- metric_groups[grepl("^Summary - Time Data$", metric_groups)]
  pay_summary <- metric_groups[grepl("^Summary - Pay Data$", metric_groups)]
  
  # Meal Period Analysis
  meal_analysis <- metric_groups[grepl("^Meal Period Analysis$", metric_groups)]
  meal_analysis_punches <- metric_groups[grepl("^Meal Period Analysis - Meal Periods with Time Punches$", metric_groups)]
  meal_analysis_punches_rounded <- metric_groups[grepl("^Meal Period Analysis - Meal Periods with Time Punches \\(rounded", metric_groups)]
  meal_analysis_no_punches <- metric_groups[grepl("^Meal Period Analysis - Meal Periods w/o Time Punches", metric_groups)]
  
  # Meal Period Violations
  meal_violations_summary <- metric_groups[grepl("^Meal Period Violations(_h)?$", metric_groups)]
  meal_violations_late <- metric_groups[grepl("^Meal Period Violations(_h)? - Late Detail(_h)?$", metric_groups)]
  meal_violations_short <- metric_groups[grepl("^Meal Period Violations(_h)? - Short Detail(_h)?$", metric_groups)]
  
  # Rest Period
  rest_analysis <- metric_groups[grepl("^Rest Period Analysis", metric_groups)]
  
  # Shift Hours Analysis
  shift_employee <- metric_groups[grepl("^Shift Hours Analysis - Employee Level", metric_groups)]
  shift_shift <- metric_groups[grepl("^Shift Hours Analysis - Shift Level", metric_groups)]
  shift_week <- metric_groups[grepl("^Shift Hours Analysis - Week Level", metric_groups)]
  shift_pp <- metric_groups[grepl("^Shift Hours Analysis - Pay Period Level", metric_groups)]
  shift_total <- metric_groups[grepl("^Shift Hours Analysis - Total Hours", metric_groups)]
  
  # Time Punch Rounding
  rounding_employee <- metric_groups[grepl("^Time Punch Rounding - Employee Level", metric_groups)]
  rounding_shift <- metric_groups[grepl("^Time Punch Rounding - Shift Level", metric_groups)]
  rounding_week <- metric_groups[grepl("^Time Punch Rounding - Week Level", metric_groups)]
  rounding_pp <- metric_groups[grepl("^Time Punch Rounding - Pay Period Level", metric_groups)]
  rounding_preshift_in <- metric_groups[grepl("^Time Punch Rounding - Pre-Shift In", metric_groups)]
  rounding_detail_preshift <- metric_groups[grepl("^Time Punch Detail Rounding - Pre-Shift In", metric_groups)]
  rounding_detail_midshift_out <- metric_groups[grepl("^Time Punch Detail Rounding - Mid-Shift Out", metric_groups)]
  rounding_detail_midshift_in <- metric_groups[grepl("^Time Punch Detail Rounding - Mid-Shift In", metric_groups)]
  rounding_detail_postshift <- metric_groups[grepl("^Time Punch Detail Rounding - Post-Shift Out", metric_groups)]
  rounding_total <- metric_groups[grepl("^Time Punch Rounding - Total Hours", metric_groups)]
  
  # Regular Rate - split by Bonuses, Differentials, RROP
  regular_rate_bonuses <- metric_groups[grepl("^Regular Rate - Bonuses", metric_groups)]
  regular_rate_differentials <- metric_groups[grepl("^Regular Rate - Differentials", metric_groups)]
  regular_rate_rrop <- metric_groups[grepl("^Regular Rate - RROP", metric_groups)]
  
  # Damages - overview groups (fixed structure)
  damages_summary <- metric_groups[grepl("^Damages - Summary$", metric_groups)]
  damages_principal <- metric_groups[grepl("^Damages - Principal$", metric_groups)]
  damages_interest <- metric_groups[grepl("^Damages - Interest$", metric_groups)]
  damages_subtotal <- metric_groups[grepl("^Damages - Sub-Total", metric_groups)]
  damages_wsv <- metric_groups[grepl("^Damages - Wage Statement Penalties", metric_groups)]
  damages_wt <- metric_groups[grepl("^Damages - Waiting Time Penalties", metric_groups)]
  damages_grand_total <- metric_groups[grepl("^Damages - Grand Total", metric_groups)]
  damages_credits <- metric_groups[grepl("^Damages - Credits or Offsets", metric_groups)]
  
  # Damages - dynamically discover ALL detail groups (anything starting with "Damages - "
  # that is NOT an overview/penalty/total group)
  damages_overview_patterns <- c("Summary", "Principal", "Interest", "Sub-Total",
                                 "Grand Total", "Credits or Offsets",
                                 "Wage Statement Penalties", "Waiting Time Penalties")
  all_damages_groups <- metric_groups[grepl("^Damages - ", metric_groups)]
  damages_detail_groups <- all_damages_groups[!sapply(all_damages_groups, function(g) {
    any(sapply(damages_overview_patterns, function(p) grepl(paste0("^Damages - ", p), g)))
  })]
  # Get unique detail group names (e.g., "Damages - Meal Premiums", "Damages - Rest Premiums", etc.)
  damages_detail_unique <- unique(damages_detail_groups)
  message("  Dynamic damages detail groups found: ", paste(damages_detail_unique, collapse = ", "))
  
  # PAGA - overview groups (fixed structure)
  paga_summary <- metric_groups[grepl("^PAGA - Summary$", metric_groups)]
  
  # PAGA - dynamically discover ALL detail groups (anything starting with "PAGA - " that is NOT Summary)
  all_paga_groups <- metric_groups[grepl("^PAGA - ", metric_groups)]
  paga_detail_groups <- all_paga_groups[!grepl("^PAGA - Summary$", all_paga_groups)]
  paga_detail_unique <- unique(paga_detail_groups)
  message("  Dynamic PAGA detail groups found: ", paste(paga_detail_unique, collapse = ", "))
  
  progress("Loading analysis tables")
  
  # Helper to safely load RDS files with better error reporting
  safe_load_rds <- function(file_path, table_name) {
    tryCatch({
      message("  Loading ", table_name, " from ", basename(file_path))
      dt <- readRDS(file_path)
      
      # Convert any Date columns that were saved as character back to Date
      if (!is.null(dt) && nrow(dt) > 0) {
        for (col in names(dt)) {
          # Check if it looks like a date column (contains "date" in name)
          if (grepl("date", col, ignore.case = TRUE) && is.character(dt[[col]])) {
            message("    Converting ", col, " from character to Date")
            dt[[col]] <- tryCatch(
              as.Date(dt[[col]]),
              error = function(e) {
                message("    Warning: Could not convert ", col, " to Date: ", e$message)
                dt[[col]]  # Keep as character if conversion fails
              }
            )
          }
        }
      }
      
      dt
    }, error = function(e) {
      message("  Error loading ", table_name, ": ", e$message)
      NULL
    })
  }
  
  analysis_tables <- list(
    pay_code_summary = safe_load_rds(file.path(DATA_DIR, "Pay_Code_Summary.rds"), "Pay_Code_Summary"),
    rate_type_analysis = safe_load_rds(file.path(DATA_DIR, "Rate_Type_Analysis.rds"), "Rate_Type_Analysis"),
    shift_hrs = safe_load_rds(file.path(DATA_DIR, "Shift_Hrs_Table.rds"), "Shift_Hrs_Table"),
    non_wrk_hrs = safe_load_rds(file.path(DATA_DIR, "Non_Work_Hrs_Table.rds"), "Non_Work_Hrs_Table"),
    meal_period = safe_load_rds(file.path(DATA_DIR, "Meal_Period_Table.rds"), "Meal_Period_Table"),
    meal_start_time = safe_load_rds(file.path(DATA_DIR, "Meal_Start_Time_Table.rds"), "Meal_Start_Time_Table"),
    meal_quarter_hr = safe_load_rds(file.path(DATA_DIR, "Meal_Quarter_Hour_Table.rds"), "Meal_Quarter_Hour_Table")
  )
  
  # Helper functions
  
  # Filter results_table by metric groups and optionally by scenario
  # scenario can be: "no waivers", "waivers", "all", or NULL (no filter)
  get_group_data <- function(group_names, scenario_filter = NULL) {
    if (length(group_names) == 0) return(data.table())
    if (!"metric_group" %in% names(results_table)) return(data.table())
    
    dt <- results_table[metric_group %in% group_names]
    if (nrow(dt) == 0) return(data.table())
    
        # Filter by scenario if specified.
    if (!is.null(scenario_filter) && "scenario" %in% names(dt)) {
      sf <- unique(normalize_scenario_value(scenario_filter))
      sf <- sf[!is.na(sf) & nzchar(sf)]
      sc <- normalize_scenario_value(dt$scenario)
      if (length(sf) > 0) {
        dt <- dt[sc %in% c(sf, "all") | is.na(dt$scenario) | is.na(sc)]
      }
    }

    if (nrow(dt) == 0) return(data.table())

        # Hide all credit-adjusted rows when include_credits = FALSE.
    if (!include_credits) {
      dt <- dt[!get_credit_row_mask(dt, metric_spec)]
    }
    if (nrow(dt) == 0) return(data.table())

    # Remove metadata columns: metric_group, scenario, credit flags
    credit_flag_cols <- intersect(c("meal_rest_prems_credit", "other_credit"), names(dt))
    display_cols <- setdiff(names(dt), c("metric_group", "scenario", credit_flag_cols))
    dt <- dt[, ..display_cols]
    if ("metric_label" %in% names(dt)) setnames(dt, "metric_label", "Metric")

    dt
  }
  
  format_col <- function(x) gsub("_", " ", x)
  
  format_cell <- function(val, is_dollar_row) {
    if (is.na(val) || val == "" || val == "-") return(if(is.na(val)) "" else val)
    if (is_dollar_row) {
      num_val <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", gsub(",", "", as.character(val)))))
      if (!is.na(num_val)) return(paste0("$", formatC(num_val, format = "f", digits = 2, big.mark = ",")))
    }
    as.character(val)
  }
  
  add_tbl <- function(dt, title, compact = FALSE, no_page_break = FALSE, hide_years = FALSE) {
    if (is.null(dt) || nrow(dt) == 0) return("")
    if (!include_extrap && "Extrapolated" %in% names(dt)) dt <- dt[, !names(dt) %in% "Extrapolated", with = FALSE]
    
    # Hide year columns if requested (columns that are just 4-digit years)
    if (hide_years) {
      year_cols <- names(dt)[grepl("^[0-9]{4}$", names(dt))]
      if (length(year_cols) > 0) dt <- dt[, !names(dt) %in% year_cols, with = FALSE]
    }
    
    cols <- names(dt)
    value_cols <- setdiff(cols, cols[1])
    if (length(value_cols) > 0) {
      keep <- sapply(1:nrow(dt), function(i) any(sapply(value_cols, function(col) { v <- dt[[col]][i]; !is.na(v) && v != "-" && v != "" && v != "0" && v != "$0.00" })))
      dt <- dt[keep, ]
      if (nrow(dt) == 0) return("")
    }
    hdr <- paste0("<th>", format_col(cols), "</th>", collapse = "")
    rows <- sapply(1:nrow(dt), function(i) {
      is_dollar <- grepl("\\$", as.character(dt[[cols[1]]][i]))
      vals <- sapply(cols, function(col) if (col == cols[1]) { if (is.na(dt[[col]][i])) "" else as.character(dt[[col]][i]) } else format_cell(dt[[col]][i], is_dollar))
      if (is_dollar) paste0("<tr style=\"font-weight:bold;\"><td>", paste(vals, collapse = "</td><td>"), "</td></tr>")
      else paste0("<tr><td>", paste(vals, collapse = "</td><td>"), "</td></tr>")
    })
    tbl_class <- if (compact) ' class="compact"' else ''
    page_break <- if (no_page_break) '' else '<div class="page-break"></div>'
    paste0(page_break, '<h2>', title, '</h2><table', tbl_class, '><thead><tr>', hdr, '</tr></thead><tbody>', paste(rows, collapse = ""), '</tbody></table>')
  }
  
  add_simple_tbl <- function(dt, title, compact = FALSE, extra_class = NULL) {
    if (is.null(dt) || nrow(dt) == 0) return("")
    cols <- names(dt)
    hdr <- paste0("<th>", format_col(cols), "</th>", collapse = "")
    rows <- sapply(1:nrow(dt), function(i) paste0("<tr><td>", paste(sapply(cols, function(col) if (is.na(dt[[col]][i])) "" else as.character(dt[[col]][i])), collapse = "</td><td>"), "</td></tr>"))
    
    # Build class string
    classes <- c()
    if (compact) classes <- c(classes, "compact")
    if (!is.null(extra_class)) classes <- c(classes, extra_class)
    tbl_class <- if (length(classes) > 0) paste0(' class="', paste(classes, collapse = " "), '"') else ''
    
    paste0('<div class="page-break"></div><h2>', title, '</h2><table', tbl_class, '><thead><tr>', hdr, '</tr></thead><tbody>', paste(rows, collapse = ""), '</tbody></table>')
  }
  

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

    version_file <- file.path(DATA_DIR, "version.txt")
    if (file.exists(version_file)) {
      v <- tryCatch(readLines(version_file, n = 1, warn = FALSE), error = function(e) "")
      v <- pick_version(v)
      if (!is.na(v)) return(v)
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
  # Build HTML
  rpt <- if (exists("case_name") && !is.null(case_name)) case_name else "Report"
  cno <- if (exists("case_no") && !is.null(case_no) && nzchar(case_no)) paste0(" (", case_no, ")") else ""
  n_emp_time <- prettyNum(uniqueN(local_shift$ID), big.mark = ",")
  n_emp_pay <- prettyNum(uniqueN(local_pay$Pay_ID), big.mark = ",")
  n_emp_class <- if (!is.null(local_class) && "Class_ID" %in% names(local_class)) prettyNum(uniqueN(local_class$Class_ID), big.mark = ",") else "N/A"
  n_shifts <- prettyNum(nrow(local_shift), big.mark = ",")
  n_pp_time <- prettyNum(uniqueN(local_shift$ID_Period_End), big.mark = ",")
  n_pp_pay <- prettyNum(uniqueN(local_pay$Pay_ID_Period_End), big.mark = ",")
  n_weeks <- prettyNum(uniqueN(local_shift$ID_Week_End), big.mark = ",")
  sample_info <- if (exists("sample_size")) as.character(sample_size) else "Not specified"
  app_version <- get_case_version_label()
  report_timestamp <- format(Sys.time(), "%B %d, %Y %I:%M %p")
  footer_text <- paste0(if (exists("contract_footer") && !is.na(contract_footer) && nzchar(contract_footer)) paste0(contract_footer, " | ") else "", "Anello Data Solutions LLC | ", app_version, " | Generated: ", report_timestamp)
  
  html <- paste0('<!DOCTYPE html><html><head><meta charset="UTF-8">
<style>
@page { size: legal landscape; margin: 0.5in;
  @top-left { content: "', rpt, cno, '"; font-weight: bold; font-size: 10pt; }
  @top-right { content: "CONFIDENTIAL - WORK PRODUCT"; color: #8B0000; font-weight: bold; font-size: 10pt; }
  @bottom-left { content: "', footer_text, '"; font-size: 7pt; color: #aaa; }
  @bottom-right { content: ""; }
}
body { font-family: Arial, sans-serif; font-size: 10pt; }
h1 { color: #2c3e50; border-bottom: 3px solid #50C878; padding-bottom: 8px; }
h2 { color: #34495e; margin-top: 20px; border-bottom: 2px solid #50C878; padding-bottom: 4px; }
table { border-collapse: collapse; margin: 10px 0; width: 100%; font-size: 8pt; }
thead { display: table-header-group; }
th { background: linear-gradient(to bottom, #5CDB95, #3CB371); color: white; padding: 4px 5px; text-align: center; font-weight: bold; }
th:first-child { text-align: left; }
td { padding: 3px 5px; border-bottom: 1px solid #ddd; text-align: center; line-height: 1.3; }
td:first-child { text-align: left; }
tr:nth-child(even) { background: #f8f8f8; }
.page-break { page-break-before: always; }
.case-tbl { width: 60%; font-size: 9pt; }
.case-tbl td { text-align: left; padding: 6px 10px; }
.case-tbl td:first-child { font-weight: bold; background: linear-gradient(to right, #e8f5e9, #f5f5f5); width: 40%; }
table.compact th { padding: 4px 5px; font-size: 8pt; }
table.compact td { padding: 3px 5px; font-size: 8pt; line-height: 1.3; }
table.pay-code-table th:last-child { text-align: left; }
table.pay-code-table td:last-child { text-align: left; }
.assumptions { font-size: 9pt; line-height: 1.6; margin: 15px 0; }
.assumptions h3 { color: #2c3e50; margin-top: 15px; margin-bottom: 8px; border-bottom: 1px solid #ccc; padding-bottom: 3px; }
.assumptions ul { margin: 5px 0; padding-left: 20px; }
.assumptions li { margin: 3px 0; }
</style></head><body>
<h1>', rpt, '</h1>
<h2>Case Information</h2>
<table class="case-tbl">
<tr><td>Case Name</td><td>', rpt, '</td></tr>
<tr><td>Case Number</td><td>', if(exists("case_no") && !is.null(case_no)) case_no else "N/A", '</td></tr>
<tr><td>Date Filed</td><td>', if(exists("date_filed") && !is.null(date_filed)) format(as.Date(date_filed), "%B %d, %Y") else "N/A", '</td></tr>
<tr><td>Relevant Period</td><td>', if(exists("class_dmgs_start_date") && !is.null(class_dmgs_start_date)) paste0(format(as.Date(class_dmgs_start_date), "%B %d, %Y"), " to present") else "N/A", '</td></tr>
<tr><td>Mediation Date</td><td>', if(exists("mediation_date") && !is.null(mediation_date)) format(as.Date(mediation_date), "%B %d, %Y") else "N/A", '</td></tr>
<tr><td>Sample Information</td><td>', sample_info, '</td></tr>
<tr><td>Analysis Version</td><td>', app_version, '</td></tr>
</table>
<h2>Data Summary</h2>
<table class="case-tbl">
<tr><td>Employees (Time Data)</td><td>', n_emp_time, '</td></tr>
<tr><td>Employees (Pay Data)</td><td>', n_emp_pay, '</td></tr>
<tr><td>Employees (Class List)</td><td>', n_emp_class, '</td></tr>
<tr><td>Pay Periods (Time)</td><td>', n_pp_time, '</td></tr>
<tr><td>Pay Periods (Pay)</td><td>', n_pp_pay, '</td></tr>
<tr><td>Weeks (Time)</td><td>', n_weeks, '</td></tr>
<tr><td>Shifts (Time)</td><td>', n_shifts, '</td></tr>
</table>')
  
  # ==========================================================================
  # BUILD SECTIONS IN METRIC_GROUP ORDER
  # ==========================================================================
  
  # Helper to add section with scenario filter
  add_section <- function(groups, title, scenario = NULL, compact = FALSE, hide_years = FALSE) {
    if (length(groups) == 0) return("")
    dt <- get_group_data(groups, scenario)
    if (nrow(dt) == 0) return("")
    progress(title)
    add_tbl(dt, title, compact = compact, hide_years = hide_years)
  }
  
  # ----- SUMMARY - TIME & PAY -----
  if ("time" %in% local_sections && include_sub("time_summary")) {
    html <- paste0(html, add_section(time_summary, "Summary - Time Data"))
  }
  if ("pay" %in% local_sections && include_sub("pay_summary")) {
    html <- paste0(html, add_section(pay_summary, "Summary - Pay Data"))
  }

  # ----- MEAL PERIOD ANALYSIS (merged into one page) -----
  if ("time" %in% local_sections && include_sub("meal_analysis")) {
    all_meal_analysis <- c(meal_analysis, meal_analysis_punches, meal_analysis_punches_rounded, meal_analysis_no_punches)
    if (length(all_meal_analysis) > 0) {
      meal_data <- get_group_data(all_meal_analysis)
      if (nrow(meal_data) > 0) {
        progress("Meal Period Analysis")
        html <- paste0(html, add_tbl(meal_data, "Meal Period Analysis"))
      }
    }
  }

  # ----- MEAL PERIOD VIOLATIONS (split by waiver scenario) -----
  if ("time" %in% local_sections) {
    # No Waivers
    if (include_sub("meal_violations_no_waivers")) {
      html <- paste0(html, add_section(meal_violations_summary, "Meal Period Violations (No Waivers)", "no waivers"))
      html <- paste0(html, add_section(meal_violations_late, "Meal Period Violations - Late Detail (No Waivers)", "no waivers", compact = TRUE))
      html <- paste0(html, add_section(meal_violations_short, "Meal Period Violations - Short Detail (No Waivers)", "no waivers", compact = TRUE))
    }
        # Waivers
    if (include_sub("meal_violations_waivers")) {
      html <- paste0(html, add_section(meal_violations_summary, "Meal Period Violations (Waivers)", "waivers"))
      html <- paste0(html, add_section(meal_violations_late, "Meal Period Violations - Late Detail (Waivers)", "waivers", compact = TRUE))
      html <- paste0(html, add_section(meal_violations_short, "Meal Period Violations - Short Detail (Waivers)", "waivers", compact = TRUE))
    }
    # Hybrid
    if (include_sub("meal_violations_hybrid")) {
      html <- paste0(html, add_section(meal_violations_summary, "Meal Period Violations (Hybrid)", "hybrid"))
      html <- paste0(html, add_section(meal_violations_late, "Meal Period Violations - Late Detail (Hybrid)", "hybrid", compact = TRUE))
      html <- paste0(html, add_section(meal_violations_short, "Meal Period Violations - Short Detail (Hybrid)", "hybrid", compact = TRUE))
    }
  }

  # ----- REST PERIOD ANALYSIS -----
  if ("time" %in% local_sections && include_sub("rest_analysis")) {
    html <- paste0(html, add_section(rest_analysis, "Rest Period Analysis & Violations"))
  }

  # ----- SHIFT HOURS ANALYSIS (all levels combined into one page) -----
  if ("time" %in% local_sections && include_sub("shift_hours")) {
    all_shift_groups <- c(shift_employee, shift_shift, shift_week, shift_pp, shift_total)
    if (length(all_shift_groups) > 0) {
      shift_data <- get_group_data(all_shift_groups)
      if (nrow(shift_data) > 0) {
        progress("Shift Hours Analysis")
        html <- paste0(html, add_tbl(shift_data, "Shift Hours Analysis"))
      }
    }
  }

  # ----- TIME PUNCH ROUNDING (all levels combined into one page) -----
  if ("time" %in% local_sections && include_sub("time_rounding")) {
    all_rounding_groups <- c(rounding_employee, rounding_shift, rounding_week, rounding_pp,
                             rounding_preshift_in, rounding_detail_preshift, rounding_detail_midshift_out,
                             rounding_detail_midshift_in, rounding_detail_postshift, rounding_total)
    if (length(all_rounding_groups) > 0) {
      rounding_data <- get_group_data(all_rounding_groups)
      if (nrow(rounding_data) > 0) {
        progress("Time Punch Rounding")
        html <- paste0(html, add_tbl(rounding_data, "Time Punch Rounding"))
      }
    }
  }

  # ----- REGULAR RATE (split by Bonuses, Differentials, RROP) -----
  if ("pay" %in% local_sections) {
    if (include_sub("regular_rate_bonuses"))      html <- paste0(html, add_section(regular_rate_bonuses, "Regular Rate - Bonuses"))
    if (include_sub("regular_rate_differentials")) html <- paste0(html, add_section(regular_rate_differentials, "Regular Rate - Differentials"))
    if (include_sub("regular_rate_rrop"))          html <- paste0(html, add_section(regular_rate_rrop, "Regular Rate - RROP"))
  }
  
  # ----- CLASS DAMAGES -----
  if ("class" %in% local_sections) {
    # Part 1: Summary through Sub-Total
    damages_part1 <- c(damages_summary, damages_principal, damages_interest, damages_subtotal)
    # Part 2: Wage Statement Penalties, Waiting Time Penalties, Grand Total (new page)
    damages_part2 <- c(damages_wsv, damages_wt, damages_grand_total)

    # Loop through selected class scenarios to avoid duplication
    for (scenario in local_class_scenarios) {
      scenario_label <- tools::toTitleCase(scenario)
      sub_key <- paste0("class_damages_", gsub(" ", "_", scenario))  # e.g. "class_damages_no_waivers"
      if (!include_sub(sub_key)) next

      progress(paste0("Class Damages (", scenario_label, ")"))
      part1 <- get_group_data(damages_part1, scenario)
      part2 <- get_group_data(damages_part2, scenario)

      if (nrow(part1) > 0) {
        html <- paste0(html, add_tbl(part1, paste0("Class Damages (", scenario_label, ")"), hide_years = TRUE))
      }
      if (nrow(part2) > 0) {
        html <- paste0(html, add_tbl(part2, paste0("Class Damages (", scenario_label, ") - Penalties"), hide_years = TRUE))
      }
    }

    # Dynamically render each damages detail group
    # For each unique detail group, check if it has scenario-specific rows (waivers/no waivers)
    # If so, render per-scenario; otherwise render once with no scenario filter
    for (detail_group in damages_detail_unique) {
      group_scenarios <- unique(normalize_scenario_value(metric_spec$scenario[metric_spec$metric_group == detail_group]))

      if (any(c("no waivers", "waivers", "hybrid") %in% group_scenarios)) {
        # Has scenario-specific rows - render only selected scenarios
        for (scenario in local_class_scenarios) {
          if (scenario %in% group_scenarios) {
            # Build a checkbox key for this detail group + scenario
            safe_group <- tolower(gsub("[^a-z0-9]+", "_", sub("^Damages - ", "", detail_group)))
            sub_key <- paste0("damages_", safe_group, "_", gsub(" ", "_", scenario))
            if (!include_sub(sub_key)) next
            scenario_label <- tools::toTitleCase(scenario)
            html <- paste0(html, add_section(detail_group, paste0(detail_group, " (", scenario_label, ")"), scenario, hide_years = TRUE))
          }
        }
      } else {
        # No scenario split (uses "all") - build a generic key
        safe_group <- tolower(gsub("[^a-z0-9]+", "_", sub("^Damages - ", "", detail_group)))
        sub_key <- paste0("damages_", safe_group)
        if (!include_sub(sub_key)) next
        html <- paste0(html, add_section(detail_group, detail_group, hide_years = TRUE))
      }
    }
  }

  # ----- PAGA PENALTIES -----
  if ("paga" %in% local_sections) {
    # PAGA Summary - all scenarios together
    if (include_sub("paga_summary")) {
      html <- paste0(html, add_section(paga_summary, "PAGA - Summary", hide_years = TRUE))
    }

    # Dynamically render each PAGA detail group
    for (detail_group in paga_detail_unique) {
      group_scenarios <- unique(normalize_scenario_value(metric_spec$scenario[metric_spec$metric_group == detail_group]))

      if (any(c("no waivers", "waivers", "hybrid") %in% group_scenarios)) {
        for (scenario in local_paga_scenarios) {
          if (scenario %in% group_scenarios) {
            safe_group <- tolower(gsub("[^a-z0-9]+", "_", sub("^PAGA - ", "", detail_group)))
            sub_key <- paste0("paga_", safe_group, "_", gsub(" ", "_", scenario))
            if (!include_sub(sub_key)) next
            scenario_label <- tools::toTitleCase(scenario)
            html <- paste0(html, add_section(detail_group, paste0(detail_group, " (", scenario_label, ")"), scenario, hide_years = TRUE))
          }
        }
      } else {
        safe_group <- tolower(gsub("[^a-z0-9]+", "_", sub("^PAGA - ", "", detail_group)))
        sub_key <- paste0("paga_", safe_group)
        if (!include_sub(sub_key)) next
        html <- paste0(html, add_section(detail_group, detail_group, hide_years = TRUE))
      }
    }
  }

  # ANALYSIS - use compact styling
  if ("analysis" %in% local_sections) {
    if (include_sub("pay_codes") && !is.null(analysis_tables$pay_code_summary) && nrow(analysis_tables$pay_code_summary) > 0) {
      progress("Pay Codes"); html <- paste0(html, add_simple_tbl(analysis_tables$pay_code_summary, "Pay Analysis - Pay Codes", compact = TRUE, extra_class = "pay-code-table"))
    }
    if (include_sub("rate_type_analysis") && !is.null(analysis_tables$rate_type_analysis) && nrow(analysis_tables$rate_type_analysis) > 0) {
      progress("Rate Type"); html <- paste0(html, add_simple_tbl(analysis_tables$rate_type_analysis, "Pay Analysis - Rate Type", compact = TRUE, extra_class = "pay-code-table"))
    }
  }
  
  # APPENDIX - Distribution tables only (Shift Hours Analysis is now in main TIME section)
  if (include_appendix) {
    # Distribution tables
    if (!is.null(analysis_tables$shift_hrs) && nrow(analysis_tables$shift_hrs) > 0) { progress("Appendix - Shift Hrs Dist"); html <- paste0(html, add_simple_tbl(analysis_tables$shift_hrs, "Appendix - Shift Hours Distribution", compact = TRUE)) }
    if (!is.null(analysis_tables$non_wrk_hrs) && nrow(analysis_tables$non_wrk_hrs) > 0) { progress("Appendix - Non-Work Hrs"); html <- paste0(html, add_simple_tbl(analysis_tables$non_wrk_hrs, "Appendix - Non-Work Hours Distribution", compact = TRUE)) }
    if (!is.null(analysis_tables$meal_period) && nrow(analysis_tables$meal_period) > 0) { progress("Appendix - Meal Period"); html <- paste0(html, add_simple_tbl(analysis_tables$meal_period, "Appendix - Meal Period Distribution", compact = TRUE)) }
    if (!is.null(analysis_tables$meal_start_time) && nrow(analysis_tables$meal_start_time) > 0) { progress("Appendix - Meal Start"); html <- paste0(html, add_simple_tbl(analysis_tables$meal_start_time, "Appendix - Meal Start Time Distribution", compact = TRUE)) }
    if (!is.null(analysis_tables$meal_quarter_hr) && nrow(analysis_tables$meal_quarter_hr) > 0) { progress("Appendix - Meal Qtr Hr"); html <- paste0(html, add_simple_tbl(analysis_tables$meal_quarter_hr, "Appendix - Meal Quarter Hour Analysis", compact = TRUE)) }
  }
  
  # NOTES & ASSUMPTIONS SUMMARY
  if (include_assumptions) {
    progress("Adding Notes & Assumptions")
    
    # Get parameter values with robust defaults
    get_param <- function(nm, default) tryCatch(if (exists(nm, inherits = TRUE)) get(nm, inherits = TRUE) else default, error = function(e) default)

    shift_hrs_cutoff          <- get_param("shift_hrs_cutoff",          7)
    rrop_buffer               <- get_param("rrop_buffer",               0.05)
    min_ot_buffer             <- get_param("min_ot_buffer",             0.25)
    max_ot_buffer             <- get_param("max_ot_buffer",             20)
    annual_interest_rate      <- get_param("annual_interest_rate",      0.07)
    initial_pp_penalty        <- get_param("initial_pp_penalty",        100)
    subsequent_pp_penalty     <- get_param("subsequent_pp_penalty",     100)
    initial_pp_penalty_226    <- get_param("initial_pp_penalty_226",    250)
    subsequent_pp_penalty_226 <- get_param("subsequent_pp_penalty_226", 250)
    initial_pp_penalty_558    <- get_param("initial_pp_penalty_558",    100)
    subsequent_pp_penalty_558 <- get_param("subsequent_pp_penalty_558", 100)
    penalty_1174              <- get_param("penalty_1174",              500)

    # Meal & Rest period thresholds
    meal_1_threshold_no_waiver <- get_param("meal_1_threshold_no_waiver", 5)
    meal_2_threshold_no_waiver <- get_param("meal_2_threshold_no_waiver", 10)
    meal_1_threshold_waiver    <- get_param("meal_1_threshold_waiver",    6)
    meal_2_threshold_waiver    <- get_param("meal_2_threshold_waiver",    12)
    rest_threshold             <- get_param("rest_threshold",             3.5)
    meal_min_hrs               <- get_param("meal_min_hrs",               0.49)
    meal_buffer                <- get_param("meal_buffer",                0.01)

    # Wage Statement Violation penalty amounts
    wsv_initial_penalty    <- get_param("wsv_initial_penalty",    50)
    wsv_subsequent_penalty <- get_param("wsv_subsequent_penalty", 100)
    wsv_cap                <- get_param("wsv_cap",                4000)

    # Waiting Time penalty period (days)
    wt_max_days <- get_param("wt_max_days", 30)

    assumptions_html <- tryCatch({
      paste0('
<div class="page-break"></div>
<h2>Notes & Assumptions</h2>
<div class="assumptions">
  <h3>Data Processing</h3>
  <ul>
    <li><strong>Analysis Version:</strong> ', app_version, '.</li>
    <li><strong>Shift Classification:</strong> Shifts are categorized using a ', shift_hrs_cutoff, '-hour cutoff.</li>
    <li><strong>Time Records:</strong> Each shift represents a distinct work period with In/Out punch times.</li>
    <li><strong>Pay Records:</strong> Pay data is matched to time data by employee ID and period end date.</li>
  </ul>

  <h3>Meal &amp; Rest Period Violations</h3>
  <ul>
    <li><strong>Meal Period Timing (No Waivers):</strong> First meal must start by end of ', meal_1_threshold_no_waiver, 'th hour (shift_hrs &gt; ', meal_1_threshold_no_waiver + 0.01, '). Second meal required for shifts &gt; ', meal_2_threshold_no_waiver, ' hours (shift_hrs &gt; ', meal_2_threshold_no_waiver + 0.01, ').</li>
    <li><strong>Meal Period Timing (Waivers):</strong> First meal may be delayed to end of ', meal_1_threshold_waiver, 'th hour (shift_hrs &gt; ', meal_1_threshold_waiver + 0.01, '). Second meal &gt; ', meal_2_threshold_waiver, ' hours (shift_hrs &gt; ', meal_2_threshold_waiver + 0.01, ').</li>
    <li><strong>Meal Period Duration:</strong> Minimum 30 minutes (', meal_min_hrs, ' hours) required. ', meal_buffer, ' hour buffer applied.</li>
    <li><strong>Rest Period Eligibility:</strong> One 10-minute rest period required for shifts &gt; ', rest_threshold, ' hours (shift_hrs &gt; ', rest_threshold + 0.01, ').</li>
  </ul>

  <h3>Regular Rate of Pay (RROP)</h3>
  <ul>
    <li><strong>Calculation:</strong> Total straight-time compensation &divide; Total straight-time hours. Excludes overtime premiums and time off.</li>
    <li><strong>De Minimis Buffer:</strong> Under/overpayments below ', sprintf("$%.2f", rrop_buffer), ' ignored as acceptable rounding.</li>
  </ul>

  <h3>Overtime &amp; Double Time</h3>
  <ul>
    <li><strong>Daily OT:</strong> Hours over 8 in a workday paid at 1.5x regular rate.</li>
    <li><strong>Daily DT:</strong> Hours over 12 in a workday paid at 2x regular rate.</li>
    <li><strong>Weekly OT:</strong> Hours over 40 in a workweek paid at 1.5x (if not already OT/DT).</li>
    <li><strong>7th Day Rules:</strong> First 8 hours on 7th day at 1.5x, over 8 at 2x.</li>
    <li><strong>Buffer Thresholds:</strong> Underpayments below ', min_ot_buffer, ' hours treated as acceptable. Max threshold ', max_ot_buffer, ' hours.</li>
  </ul>

  <h3>Damages Calculations</h3>
  <ul>
    <li><strong>Interest:</strong> Prejudgment interest at ', sprintf("%.0f%%", annual_interest_rate * 100), ' annually.</li>
    <li><strong>Wage Statement Violations:</strong> $', wsv_initial_penalty, ' initial + $', wsv_subsequent_penalty, ' subsequent penalties, capped at $', formatC(wsv_cap, format = "f", digits = 0, big.mark = ","), ' per employee (Labor Code &sect;226).</li>
    <li><strong>Waiting Time Penalties:</strong> Up to ', wt_max_days, ' days wages for terminated employees (Labor Code &sect;203).</li>
  </ul>

  <h3>PAGA Penalties</h3>
  <ul>
    <li><strong>Standard:</strong> $', initial_pp_penalty, ' initial + $', subsequent_pp_penalty, ' subsequent per employee per pay period (Labor Code &sect;2699).</li>
    <li><strong>Labor Code &sect;226:</strong> $', initial_pp_penalty_226, ' initial + $', subsequent_pp_penalty_226, ' subsequent for wage statement violations.</li>
    <li><strong>Labor Code &sect;558 (Meal/Rest):</strong> $', initial_pp_penalty_558, ' initial + $', subsequent_pp_penalty_558, ' subsequent for meal and rest period violations.</li>
    <li><strong>Labor Code &sect;1174:</strong> $', penalty_1174, ' penalty for itemized wage statement violations.</li>
  </ul>
</div>')
    }, error = function(e) {
      message("Error generating assumptions HTML: ", e$message)
      ""
    })
    
    html <- paste0(html, assumptions_html)
    if (verbose) message("\n  Assumptions section added (", nchar(assumptions_html), " characters)")
  }
  
  html <- paste0(html, '</body></html>')
  
  # Generate PDF
  progress("Generating PDF")
  tmp_html <- tempfile(fileext = ".html")
  writeLines(html, tmp_html)
  if (is.null(output_file)) output_file <- file.path(DATA_DIR, paste0(gsub("[^A-Za-z0-9_-]", "_", rpt), "_Report_", Sys.Date(), ".pdf"))
  main_pdf <- tempfile(fileext = ".pdf")
  pagedown::chrome_print(input = tmp_html, output = main_pdf, verbose = 0, options = list(landscape = TRUE, paperWidth = 14, paperHeight = 8.5))
  
  if (include_data_comparison) {
    progress("Appending Data Comparison")
    data_comp_pdf <- file.path(DATA_DIR, "Data Comparison.pdf")
    if (file.exists(data_comp_pdf) && requireNamespace("pdftools", quietly = TRUE)) pdftools::pdf_combine(c(main_pdf, data_comp_pdf), output = output_file)
    else { file.copy(main_pdf, output_file, overwrite = TRUE); if (!file.exists(data_comp_pdf)) cat("\nNote: Data Comparison.pdf not found\n") }
  } else file.copy(main_pdf, output_file, overwrite = TRUE)
  
  unlink(tmp_html); unlink(main_pdf)
  cat("\n\n==================================================\n")
  cat("                 PDF GENERATED\n")
  cat("==================================================\n")
  cat("Output:", output_file, "\n")
  cat("==================================================\n\n")
  invisible(output_file)
}

# Shortcuts
generate_full_report <- function(output_file = NULL, include_extrap = TRUE, include_credits = TRUE, include_appendix = TRUE, include_data_comparison = TRUE) {
  generate_report(output_file = output_file, sections = c("time", "pay", "class", "paga", "analysis"), include_extrap = include_extrap, include_credits = include_credits, include_appendix = include_appendix, include_data_comparison = include_data_comparison)
}
generate_time_report <- function(output_file = NULL, include_extrap = FALSE, include_credits = TRUE, include_appendix = FALSE, include_data_comparison = FALSE) {
  generate_report(output_file = output_file, sections = "time", include_extrap = include_extrap, include_credits = include_credits, include_appendix = include_appendix, include_data_comparison = include_data_comparison)
}
generate_pay_report <- function(output_file = NULL, include_extrap = FALSE, include_credits = TRUE, include_appendix = FALSE, include_data_comparison = FALSE) {
  generate_report(output_file = output_file, sections = "pay", include_extrap = include_extrap, include_credits = include_credits, include_appendix = include_appendix, include_data_comparison = include_data_comparison)
}
generate_time_pay_report <- function(output_file = NULL, include_extrap = FALSE, include_credits = TRUE, include_appendix = FALSE, include_data_comparison = FALSE) {
  generate_report(output_file = output_file, sections = c("time", "pay", "analysis"), include_extrap = include_extrap, include_credits = include_credits, include_appendix = include_appendix, include_data_comparison = include_data_comparison)
}
generate_class_report <- function(output_file = NULL, include_extrap = TRUE, include_credits = TRUE, include_appendix = TRUE, include_data_comparison = TRUE) {
  generate_report(output_file = output_file, sections = c("time", "pay", "class", "analysis"), include_extrap = include_extrap, include_credits = include_credits, include_appendix = include_appendix, include_data_comparison = include_data_comparison)
}
generate_paga_report <- function(output_file = NULL, include_extrap = TRUE, include_credits = TRUE, include_appendix = TRUE, include_data_comparison = TRUE) {
  generate_report(output_file = output_file, sections = c("time", "pay", "paga", "analysis"), include_extrap = include_extrap, include_credits = include_credits, include_appendix = include_appendix, include_data_comparison = include_data_comparison)
}
generate_damages_report <- function(output_file = NULL, include_extrap = FALSE, include_credits = TRUE, include_appendix = FALSE, include_data_comparison = FALSE) {
  generate_report(output_file = output_file, sections = c("class", "paga"), include_extrap = include_extrap, include_credits = include_credits, include_appendix = include_appendix, include_data_comparison = include_data_comparison)
}
generate_no_damages_report <- function(output_file = NULL, include_extrap = FALSE, include_credits = TRUE, include_appendix = FALSE, include_data_comparison = FALSE) {
  generate_report(output_file = output_file, sections = c("time", "pay", "analysis"), include_extrap = include_extrap, include_credits = include_credits, include_appendix = include_appendix, include_data_comparison = include_data_comparison)
}

cat("\n==================================================\n")
cat("      STANDALONE PDF GENERATOR LOADED\n")
cat("==================================================\n\n")
cat("Usage:\n")
cat("  generate_report()                     # All sections\n")
cat("  generate_report(sections = 'time')    # Time only\n")
cat("  generate_report(sections = 'class')   # Class damages only\n")
cat("  generate_report(sections = 'paga')    # PAGA penalties only\n\n")
cat("Shortcuts:\n")
cat("  generate_full_report()       # Everything (Class + PAGA)\n")
cat("  generate_class_report()      # All except PAGA penalties\n")
cat("  generate_paga_report()       # All except Class damages\n")
cat("  generate_damages_report()    # Class + PAGA only (no time/pay)\n")
cat("  generate_no_damages_report() # Time + Pay only (no damages)\n")
cat("  generate_time_report()       # Time section only\n")
cat("  generate_pay_report()        # Pay section only\n")
cat("  generate_time_pay_report()   # Time + Pay + Analysis\n\n")
cat("Sections: time, pay, class, paga, analysis\n")
cat("==================================================\n")
