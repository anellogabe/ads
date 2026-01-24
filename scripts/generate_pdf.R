# ============================================================================
# STANDALONE PDF REPORT GENERATOR
# ============================================================================
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

# Source functions.R - try multiple locations
if (!exists("load_metric_spec")) {
  # Try current directory
  
  if (file.exists("functions.R")) {
    source("functions.R")
  } else if (exists("SCRIPTS_DIR") && file.exists(file.path(SCRIPTS_DIR, "functions.R"))) {
    source(file.path(SCRIPTS_DIR, "functions.R"))
  } else if (exists("ADS_REPO") && file.exists(file.path(ADS_REPO, "scripts", "functions.R"))) {
    source(file.path(ADS_REPO, "scripts", "functions.R"))
  } else {
    stop("Cannot find functions.R - please source it first or set SCRIPTS_DIR/ADS_REPO")
  }
}

# ============================================================================
# MAIN FUNCTION
# ============================================================================

generate_report <- function(
    output_file = NULL,
    sections = c("time", "pay", "class", "paga", "analysis"),
    include_extrap = FALSE,
    include_appendix = FALSE,
    include_data_comparison = FALSE,
    verbose = TRUE
) {
  
  local_sections <- sections
  
  total_steps <- 5
  current_step <- 0
  
  if ("time" %in% local_sections) total_steps <- total_steps + 7
  if ("pay" %in% local_sections) total_steps <- total_steps + 2
  if ("class" %in% local_sections) total_steps <- total_steps + 10
  if ("paga" %in% local_sections) total_steps <- total_steps + 10
  if ("analysis" %in% local_sections) total_steps <- total_steps + 2
  if (include_appendix) total_steps <- total_steps + 5
  if (include_data_comparison) total_steps <- total_steps + 1
  
  progress <- function(msg) {
    current_step <<- current_step + 1
    if (verbose) {
      pct <- round(current_step / max(total_steps, 1) * 100)
      filled <- round(pct / 100 * 30)
      bar <- paste0("[", strrep("=", filled), strrep(" ", 30 - filled), "]")
      cat(sprintf("\r%s %3d%% | %s", bar, pct, msg))
      if (current_step == total_steps) cat("\n")
      flush.console()
    }
  }
  
  cat("\n")
  cat("==================================================\n")
  cat("         STANDALONE PDF GENERATOR\n")
  cat("==================================================\n")
  cat("Sections:", paste(local_sections, collapse = ", "), "\n")
  cat("Include Extrapolation:", include_extrap, "\n")
  cat("Include Appendix:", include_appendix, "\n")
  cat("Include Data Comparison:", include_data_comparison, "\n")
  cat("==================================================\n\n")
  
  paths <- init_case_paths(set_globals = TRUE)
  DATA_DIR <- paths$OUT_DIR
  cat("DATA_DIR:", DATA_DIR, "\n\n")
  
  progress("Loading data for case info")
  
  # Load data - use existing from environment or load from files
  if (exists("shift_data1") && is.data.table(shift_data1)) {
    local_shift <- shift_data1
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
      class_file <- file.path(paths$PROCESSED_DIR, "class_processed.rds")
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
    spec_path <- file.path(paths$CASE_DIR, "scripts", "metrics_spec.csv")
    if (!file.exists(spec_path)) stop("Cannot find metrics_spec.csv and no metrics_spec in environment")
    metric_spec <- fread(spec_path)
    setDT(metric_spec)
  }
  
  # Use existing final_table from analysis.R if available
  if (exists("final_table") && is.data.table(final_table)) {
    results_table <- copy(final_table)
    cat("Using existing final_table from environment\n")
  } else {
    # Try to load from file
    results_file <- file.path(DATA_DIR, "Analysis.rds")
    if (file.exists(results_file)) {
      results_table <- readRDS(results_file)
      cat("Loaded results from Analysis.rds\n")
    } else {
      stop("Cannot find final_table in environment or Analysis.rds file")
    }
  }
  
  metric_groups <- unique(metric_spec$metric_group)
  
  time_summary_groups <- metric_groups[grepl("^Summary - Time Data$", metric_groups)]
  time_shift_groups <- metric_groups[grepl("^Shift Hours Analysis", metric_groups)]
  time_rounding_groups <- metric_groups[grepl("^Time Punch Rounding", metric_groups)]
  time_meal_analysis <- metric_groups[grepl("^Meal Period Analysis", metric_groups)]
  time_meal_violations_5_summary <- metric_groups[grepl("^Meal Period Violations$", metric_groups)]
  time_meal_violations_5_short <- metric_groups[grepl("^Meal Period Violations - Short Detail", metric_groups)]
  time_meal_violations_5_late <- metric_groups[grepl("^Meal Period Violations - Late Detail", metric_groups)]
  time_meal_violations_6_summary <- metric_groups[grepl("^Meal Period Violations \\(6", metric_groups)]
  time_meal_violations_6_short <- metric_groups[grepl("^Meal Period Violations \\(6.*Short", metric_groups)]
  time_meal_violations_6_late <- metric_groups[grepl("^Meal Period Violations \\(6.*Late", metric_groups)]
  time_rest <- metric_groups[grepl("^Rest Period Analysis", metric_groups)]
  
  pay_summary_groups <- metric_groups[grepl("^Summary - Pay Data$", metric_groups)]
  pay_regular_rate <- metric_groups[grepl("^Regular Rate", metric_groups)]
  
  damages_summary_groups <- metric_groups[grepl("^Damages Summary", metric_groups)]
  damages_principal_groups <- metric_groups[grepl("^Damages - Principal", metric_groups)]
  damages_credits_groups <- metric_groups[grepl("^Damages - Credits", metric_groups)]
  damages_interest_groups <- metric_groups[grepl("^Damages - Interest", metric_groups)]
  damages_subtotal_groups <- metric_groups[grepl("^Damages - Subtotal", metric_groups)]
  damages_grand_total_groups <- metric_groups[grepl("^Damages - Grand Total", metric_groups)]
  damages_meal_groups <- metric_groups[grepl("^Class Damages.*Meal", metric_groups)]
  damages_rest_groups <- metric_groups[grepl("^Class Damages.*Rest", metric_groups)]
  damages_rrop_groups <- metric_groups[grepl("^Class Damages.*RROP|Regular Rate", metric_groups)]
  damages_otc_groups <- metric_groups[grepl("^Class Damages.*Off.the.Clock|OTC", metric_groups)]
  damages_unpaid_ot_groups <- metric_groups[grepl("^Class Damages.*Unpaid OT|Overtime", metric_groups)]
  damages_min_wage_groups <- metric_groups[grepl("^Class Damages.*Min.*Wage", metric_groups)]
  damages_expenses_groups <- metric_groups[grepl("^Class Damages.*Expense", metric_groups)]
  damages_wsv_groups <- metric_groups[grepl("^Class Damages.*Wage Statement|WSV", metric_groups)]
  damages_wt_groups <- metric_groups[grepl("^Class Damages.*Waiting Time|WT", metric_groups)]
  
  paga_summary_groups <- metric_groups[grepl("^PAGA Summary|^PAGA Overview", metric_groups)]
  paga_meal_groups <- metric_groups[grepl("^PAGA.*Meal", metric_groups)]
  paga_rest_groups <- metric_groups[grepl("^PAGA.*Rest", metric_groups)]
  paga_rrop_groups <- metric_groups[grepl("^PAGA.*RROP|^PAGA.*Regular Rate", metric_groups)]
  paga_226_groups <- metric_groups[grepl("^PAGA.*226|^PAGA.*Wage Statement", metric_groups)]
  paga_558_groups <- metric_groups[grepl("^PAGA.*558|^PAGA.*Unpaid", metric_groups)]
  paga_min_wage_groups <- metric_groups[grepl("^PAGA.*Min.*Wage", metric_groups)]
  paga_expenses_groups <- metric_groups[grepl("^PAGA.*Expense", metric_groups)]
  paga_recordkeeping_groups <- metric_groups[grepl("^PAGA.*Recordkeeping|^PAGA.*Record", metric_groups)]
  paga_waiting_time_groups <- metric_groups[grepl("^PAGA.*Waiting", metric_groups)]
  
  progress("Loading analysis tables")
  analysis_tables <- list(
    pay_code_summary = tryCatch(readRDS(file.path(DATA_DIR, "Pay_Code_Summary.rds")), error = function(e) NULL),
    rate_type_analysis = tryCatch(readRDS(file.path(DATA_DIR, "Rate_Type_Analysis.rds")), error = function(e) NULL),
    shift_hrs = tryCatch(readRDS(file.path(DATA_DIR, "Shift_Hrs_Table.rds")), error = function(e) NULL),
    non_wrk_hrs = tryCatch(readRDS(file.path(DATA_DIR, "Non_Work_Hrs_Table.rds")), error = function(e) NULL),
    meal_period = tryCatch(readRDS(file.path(DATA_DIR, "Meal_Period_Table.rds")), error = function(e) NULL),
    meal_start_time = tryCatch(readRDS(file.path(DATA_DIR, "Meal_Start_Time_Table.rds")), error = function(e) NULL),
    meal_quarter_hr = tryCatch(readRDS(file.path(DATA_DIR, "Meal_Quarter_Hour_Table.rds")), error = function(e) NULL)
  )
  
  # Helper functions
  
  # Filter results_table by metric groups
  get_group_data <- function(group_names) {
    if (length(group_names) == 0) return(data.table())
    if (!"metric_group" %in% names(results_table)) return(data.table())
    
    dt <- results_table[metric_group %in% group_names]
    if (nrow(dt) == 0) return(data.table())
    
    # Remove metric_group column for display, rename metric_label to Metric
    display_cols <- setdiff(names(dt), c("metric_group", "scenario"))
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
  
  add_tbl <- function(dt, title) {
    if (is.null(dt) || nrow(dt) == 0) return("")
    if (!include_extrap && "Extrapolated" %in% names(dt)) dt <- dt[, !names(dt) %in% "Extrapolated", with = FALSE]
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
    paste0('<div class="page-break"></div><h2>', title, '</h2><table><thead><tr>', hdr, '</tr></thead><tbody>', paste(rows, collapse = ""), '</tbody></table>')
  }
  
  add_simple_tbl <- function(dt, title) {
    if (is.null(dt) || nrow(dt) == 0) return("")
    cols <- names(dt)
    hdr <- paste0("<th>", format_col(cols), "</th>", collapse = "")
    rows <- sapply(1:nrow(dt), function(i) paste0("<tr><td>", paste(sapply(cols, function(col) if (is.na(dt[[col]][i])) "" else as.character(dt[[col]][i])), collapse = "</td><td>"), "</td></tr>"))
    paste0('<div class="page-break"></div><h2>', title, '</h2><table><thead><tr>', hdr, '</tr></thead><tbody>', paste(rows, collapse = ""), '</tbody></table>')
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
  app_version <- "1.0.0"
  report_timestamp <- format(Sys.time(), "%B %d, %Y %I:%M %p")
  footer_text <- paste0(if (exists("contract_footer") && !is.na(contract_footer) && nzchar(contract_footer)) paste0(contract_footer, " | ") else "", "Anello Data Solutions LLC | v", app_version, " | Generated: ", report_timestamp)
  
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
table { border-collapse: collapse; margin: 10px 0; width: 100%; font-size: 9pt; }
thead { display: table-header-group; }
th { background: linear-gradient(to bottom, #5CDB95, #3CB371); color: white; padding: 6px; text-align: center; font-weight: bold; }
th:first-child { text-align: left; }
td { padding: 5px; border-bottom: 1px solid #ddd; text-align: center; }
td:first-child { text-align: left; }
tr:nth-child(even) { background: #f8f8f8; }
.page-break { page-break-before: always; }
.case-tbl { width: 60%; }
.case-tbl td { text-align: left; padding: 6px 10px; }
.case-tbl td:first-child { font-weight: bold; background: linear-gradient(to right, #e8f5e9, #f5f5f5); width: 40%; }
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
  
  # TIME
  if ("time" %in% local_sections) {
    if (length(time_summary_groups) > 0) { progress("Time Summary"); html <- paste0(html, add_tbl(get_group_data(time_summary_groups), "Time Summary")) }
    if (length(time_shift_groups) > 0) { progress("Shift Hours"); html <- paste0(html, add_tbl(get_group_data(time_shift_groups), "Shift Hours")) }
    if (length(time_rounding_groups) > 0) { progress("Punch Rounding"); html <- paste0(html, add_tbl(get_group_data(time_rounding_groups), "Punch Rounding")) }
    if (length(time_meal_analysis) > 0) { progress("Meal Analysis"); html <- paste0(html, add_tbl(get_group_data(time_meal_analysis), "Meal Analysis")) }
    meal_5 <- c(time_meal_violations_5_summary, time_meal_violations_5_short, time_meal_violations_5_late)
    if (length(meal_5) > 0) { progress("Meal Violations (No Waivers)"); html <- paste0(html, add_tbl(get_group_data(meal_5), "Meal Violations (No Waivers)")) }
    meal_6 <- c(time_meal_violations_6_summary, time_meal_violations_6_short, time_meal_violations_6_late)
    if (length(meal_6) > 0) { progress("Meal Violations (Waivers)"); html <- paste0(html, add_tbl(get_group_data(meal_6), "Meal Violations (Waivers)")) }
    if (length(time_rest) > 0) { progress("Rest Violations"); html <- paste0(html, add_tbl(get_group_data(time_rest), "Rest Violations")) }
  }
  
  # PAY
  if ("pay" %in% local_sections) {
    if (length(pay_summary_groups) > 0) { progress("Pay Summary"); html <- paste0(html, add_tbl(get_group_data(pay_summary_groups), "Pay Summary")) }
    if (length(pay_regular_rate) > 0) { progress("Regular Rate"); html <- paste0(html, add_tbl(get_group_data(pay_regular_rate), "Regular Rate")) }
  }
  
  # CLASS
  if ("class" %in% local_sections) {
    class_ov <- c(damages_summary_groups, damages_principal_groups, damages_credits_groups, damages_interest_groups, damages_subtotal_groups, damages_grand_total_groups)
    if (length(class_ov) > 0) { progress("Class Overview"); html <- paste0(html, add_tbl(get_group_data(class_ov), "Class Damages Overview")) }
    if (length(damages_meal_groups) > 0) { progress("Class - Meal"); html <- paste0(html, add_tbl(get_group_data(damages_meal_groups), "Class - Meal Premiums")) }
    if (length(damages_rest_groups) > 0) { progress("Class - Rest"); html <- paste0(html, add_tbl(get_group_data(damages_rest_groups), "Class - Rest Premiums")) }
    if (length(damages_rrop_groups) > 0) { progress("Class - RROP"); html <- paste0(html, add_tbl(get_group_data(damages_rrop_groups), "Class - RROP")) }
    if (length(damages_otc_groups) > 0) { progress("Class - OTC"); html <- paste0(html, add_tbl(get_group_data(damages_otc_groups), "Class - Off-the-Clock")) }
    if (length(damages_unpaid_ot_groups) > 0) { progress("Class - Unpaid OT"); html <- paste0(html, add_tbl(get_group_data(damages_unpaid_ot_groups), "Class - Unpaid OT/DT")) }
    if (length(damages_min_wage_groups) > 0) { progress("Class - Min Wage"); html <- paste0(html, add_tbl(get_group_data(damages_min_wage_groups), "Class - Minimum Wage")) }
    if (length(damages_expenses_groups) > 0) { progress("Class - Expenses"); html <- paste0(html, add_tbl(get_group_data(damages_expenses_groups), "Class - Expenses")) }
    if (length(damages_wsv_groups) > 0) { progress("Class - Wage Stmt"); html <- paste0(html, add_tbl(get_group_data(damages_wsv_groups), "Class - Wage Statement")) }
    if (length(damages_wt_groups) > 0) { progress("Class - Waiting Time"); html <- paste0(html, add_tbl(get_group_data(damages_wt_groups), "Class - Waiting Time")) }
  }
  
  # PAGA
  if ("paga" %in% local_sections) {
    if (length(paga_summary_groups) > 0) { progress("PAGA Summary"); html <- paste0(html, add_tbl(get_group_data(paga_summary_groups), "PAGA Summary")) }
    if (length(paga_meal_groups) > 0) { progress("PAGA - Meal"); html <- paste0(html, add_tbl(get_group_data(paga_meal_groups), "PAGA - Meal")) }
    if (length(paga_rest_groups) > 0) { progress("PAGA - Rest"); html <- paste0(html, add_tbl(get_group_data(paga_rest_groups), "PAGA - Rest")) }
    if (length(paga_rrop_groups) > 0) { progress("PAGA - RROP"); html <- paste0(html, add_tbl(get_group_data(paga_rrop_groups), "PAGA - RROP")) }
    if (length(paga_226_groups) > 0) { progress("PAGA - 226"); html <- paste0(html, add_tbl(get_group_data(paga_226_groups), "PAGA - Wage Statement (226)")) }
    if (length(paga_558_groups) > 0) { progress("PAGA - 558"); html <- paste0(html, add_tbl(get_group_data(paga_558_groups), "PAGA - Unpaid Wages (558)")) }
    if (length(paga_min_wage_groups) > 0) { progress("PAGA - Min Wage"); html <- paste0(html, add_tbl(get_group_data(paga_min_wage_groups), "PAGA - Minimum Wage")) }
    if (length(paga_expenses_groups) > 0) { progress("PAGA - Expenses"); html <- paste0(html, add_tbl(get_group_data(paga_expenses_groups), "PAGA - Expenses")) }
    if (length(paga_recordkeeping_groups) > 0) { progress("PAGA - Recordkeeping"); html <- paste0(html, add_tbl(get_group_data(paga_recordkeeping_groups), "PAGA - Recordkeeping")) }
    if (length(paga_waiting_time_groups) > 0) { progress("PAGA - Waiting Time"); html <- paste0(html, add_tbl(get_group_data(paga_waiting_time_groups), "PAGA - Waiting Time")) }
  }
  
  # ANALYSIS
  if ("analysis" %in% local_sections) {
    if (!is.null(analysis_tables$pay_code_summary) && nrow(analysis_tables$pay_code_summary) > 0) { progress("Pay Codes"); html <- paste0(html, add_simple_tbl(analysis_tables$pay_code_summary, "Pay Analysis - Pay Codes")) }
    if (!is.null(analysis_tables$rate_type_analysis) && nrow(analysis_tables$rate_type_analysis) > 0) { progress("Rate Type"); html <- paste0(html, add_simple_tbl(analysis_tables$rate_type_analysis, "Pay Analysis - Rate Type")) }
  }
  
  # APPENDIX
  if (include_appendix) {
    if (!is.null(analysis_tables$shift_hrs) && nrow(analysis_tables$shift_hrs) > 0) { progress("Appendix - Shift Hrs"); html <- paste0(html, add_simple_tbl(analysis_tables$shift_hrs, "Appendix - Shift Hours Distribution")) }
    if (!is.null(analysis_tables$non_wrk_hrs) && nrow(analysis_tables$non_wrk_hrs) > 0) { progress("Appendix - Non-Work Hrs"); html <- paste0(html, add_simple_tbl(analysis_tables$non_wrk_hrs, "Appendix - Non-Work Hours Distribution")) }
    if (!is.null(analysis_tables$meal_period) && nrow(analysis_tables$meal_period) > 0) { progress("Appendix - Meal Period"); html <- paste0(html, add_simple_tbl(analysis_tables$meal_period, "Appendix - Meal Period Distribution")) }
    if (!is.null(analysis_tables$meal_start_time) && nrow(analysis_tables$meal_start_time) > 0) { progress("Appendix - Meal Start"); html <- paste0(html, add_simple_tbl(analysis_tables$meal_start_time, "Appendix - Meal Start Time Distribution")) }
    if (!is.null(analysis_tables$meal_quarter_hr) && nrow(analysis_tables$meal_quarter_hr) > 0) { progress("Appendix - Meal Qtr Hr"); html <- paste0(html, add_simple_tbl(analysis_tables$meal_quarter_hr, "Appendix - Meal Quarter Hour Analysis")) }
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
generate_full_report <- function(output_file = NULL) generate_report(output_file = output_file, sections = c("time", "pay", "class", "paga", "analysis"), include_appendix = TRUE, include_data_comparison = TRUE)
generate_time_report <- function(output_file = NULL) generate_report(output_file = output_file, sections = "time")
generate_pay_report <- function(output_file = NULL) generate_report(output_file = output_file, sections = "pay")
generate_time_pay_report <- function(output_file = NULL) generate_report(output_file = output_file, sections = c("time", "pay", "analysis"))
generate_class_report <- function(output_file = NULL) generate_report(output_file = output_file, sections = "class")
generate_paga_report <- function(output_file = NULL) generate_report(output_file = output_file, sections = "paga")
generate_damages_report <- function(output_file = NULL) generate_report(output_file = output_file, sections = c("class", "paga"))
generate_no_damages_report <- function(output_file = NULL) generate_report(output_file = output_file, sections = c("time", "pay", "analysis"))

cat("\n==================================================\n")
cat("      STANDALONE PDF GENERATOR LOADED\n")
cat("==================================================\n\n")
cat("Usage:\n")
cat("  generate_report()                     # All sections\n")
cat("  generate_report(sections = 'time')    # Time only\n")
cat("  generate_report(sections = 'class')   # Class only\n")
cat("  generate_report(sections = 'paga')    # PAGA only\n\n")
cat("Shortcuts:\n")
cat("  generate_full_report()       generate_time_report()\n")
cat("  generate_pay_report()        generate_time_pay_report()\n")
cat("  generate_class_report()      generate_paga_report()\n")
cat("  generate_damages_report()    generate_no_damages_report()\n\n")
cat("Sections: time, pay, class, paga, analysis\n")
cat("==================================================\n")