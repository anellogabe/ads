# =============================================================================
# Wage & Hour Compliance Dashboard - COMPREHENSIVE VERSION
# =============================================================================

library(shiny)
library(bslib)
library(data.table)
library(lubridate)
library(DT)
library(plotly)
library(here)
library(shinycssloaders)
library(shinyjs)

# =============================================================================
# CONFIGURATION
# =============================================================================

DATA_DIR <- here("output")
SCRIPTS_DIR <- here("scripts")

# Data file names
SHIFT_DATA_FILE <- "shift_data1.rds"
PAY_DATA_FILE <- "pay1.rds"
TIME_DATA_FILE <- "time1.rds"
CLASS_DATA_FILE <- "class1.rds"
METRIC_SPEC_FILE <- "metrics_spec.csv"
CASE_CONFIG_FILE <- "case_config.rds"

# Analysis table files
SHIFT_HRS_FILE <- "Shift_Hrs_Table.csv"
NON_WRK_HRS_FILE <- "Non_Work_Hrs_Table.csv"
MEAL_PERIOD_FILE <- "Meal_Period_Table.csv"
MEAL_START_TIME_FILE <- "Meal_Start_Time_Table.csv"
MEAL_QUARTER_HR_FILE <- "Meal_Quarter_Hour_Table.csv"
PAY_CODE_SUMMARY_FILE <- "Pay_Code_Summary.csv"
DATA_COMPARISON_FILE <- "Data Comparison.csv"
EMPLOYEE_COMPARISON_FILE <- "Employee Pay Period Comparison.csv"

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

# Format column names: underscore to space, proper case
format_col_name <- function(name) {
  name %>%
    gsub("_", " ", .) %>%
    tools::toTitleCase()
}

# Format all column names in a data.table
format_all_cols <- function(dt) {
  setnames(dt, names(dt), sapply(names(dt), format_col_name))
  dt
}

# =============================================================================
# DATA LOADING
# =============================================================================

load_data <- function() {
  shift_path <- file.path(DATA_DIR, SHIFT_DATA_FILE)
  pay_path <- file.path(DATA_DIR, PAY_DATA_FILE)
  time_path <- file.path(DATA_DIR, TIME_DATA_FILE)
  class_path <- file.path(DATA_DIR, CLASS_DATA_FILE)

  result <- list()

  # Load shift data (required)
  if (file.exists(shift_path)) {
    message("Loading shift data...")
    result$shift_data1 <- readRDS(shift_path)
  } else {
    stop("Cannot find shift data file: ", shift_path)
  }

  # Load pay data (required)
  if (file.exists(pay_path)) {
    message("Loading pay data...")
    result$pay1 <- readRDS(pay_path)
  } else {
    stop("Cannot find pay data file: ", pay_path)
  }

  # Load time data (optional)
  if (file.exists(time_path)) {
    message("Loading time data...")
    result$time1 <- readRDS(time_path)
  } else {
    result$time1 <- NULL
  }

  # Load class data (optional)
  if (file.exists(class_path)) {
    message("Loading class data...")
    result$class1 <- readRDS(class_path)
  } else {
    result$class1 <- NULL
  }

  result
}

load_metric_spec <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(SCRIPTS_DIR, METRIC_SPEC_FILE)
  }

  if (!file.exists(path)) {
    stop("Cannot find metrics spec file: ", path)
  }

  message("Loading metric spec...")
  spec <- fread(path)
  spec[, metric_order := .I]
  spec
}

load_case_config <- function() {
  config_path <- file.path(DATA_DIR, CASE_CONFIG_FILE)

  if (file.exists(config_path)) {
    return(readRDS(config_path))
  }

  # Default configuration
  list(
    case_name = "Unknown",
    case_number = "Unknown",
    date_filed = NA,
    relevant_period_start = NA,
    relevant_period_end = NA,
    paga_period_start = NA,
    paga_period_end = NA,
    sample_size = "Unknown",
    sample_type = "Unknown",
    mediation_date = NA,
    class_cert_deadline = NA,
    notes = "",
    extrapolation_enabled = FALSE,
    extrapolation_factor = 1.0,
    extrapolation_method = "None"
  )
}

# Load analysis tables
load_analysis_table <- function(filename) {
  filepath <- file.path(DATA_DIR, filename)
  if (file.exists(filepath)) {
    dt <- fread(filepath)
    # Remove rows where first column is NA, empty, or "0"
    if (nrow(dt) > 0 && ncol(dt) > 0) {
      first_col <- names(dt)[1]
      dt <- dt[!(is.na(get(first_col)) | get(first_col) == "" | get(first_col) == "0")]
    }
    return(format_all_cols(dt))
  }
  NULL
}

# =============================================================================
# METRIC CALCULATION (OPTIMIZED)
# =============================================================================

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

      if (grepl("pay", source, ignore.case = TRUE)) {
        src_data <- data_list$pay1
        src_key_col <- "Pay_Key_Gps"
        src_date_col <- "Pay_Period_End"
        src_years <- data_list$pay_years
        src_key_groups <- data_list$pay_key_groups
      } else {
        src_data <- data_list$shift_data1
        src_key_col <- "Key_Gps"
        src_date_col <- "Date"
        src_years <- data_list$shift_years
        src_key_groups <- data_list$shift_key_groups
      }

      if (nrow(src_data) == 0) {
        result_row <- data.table(Metric = row$metric_label, All_Data = "-")
        return(result_row)
      }

      all_data_result <- calculate_single_metric(src_data, row$expr, row$digits, row$denom)

      result_row <- data.table(
        Metric = row$metric_label,
        All_Data = format_metric_value(all_data_result$value, all_data_result$pct)
      )

      # Add year columns
      if (!is.null(src_years) && length(src_years) > 0 && src_date_col %in% names(src_data)) {
        for (yr in src_years) {
          yr_data <- src_data[year(get(src_date_col)) == yr]
          yr_result <- calculate_single_metric(yr_data, row$expr, row$digits, row$denom)
          result_row[, (as.character(yr)) := format_metric_value(yr_result$value, yr_result$pct)]
        }
      }

      # Add key group columns
      if (!is.null(src_key_groups) && length(src_key_groups) > 0 && src_key_col %in% names(src_data)) {
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

format_metric_value <- function(val, pct = NA) {
  if (is.null(val) || length(val) == 0 || (is.atomic(val) && all(is.na(val)))) {
    return("-")
  }

  if (inherits(val, "Date")) {
    return(as.character(val))
  }

  if (is.numeric(val)) {
    if (abs(val) >= 1000) {
      formatted <- format(round(val, 2), big.mark = ",", nsmall = if (val %% 1 == 0) 0 else 2)
    } else {
      formatted <- as.character(round(val, 2))
    }

    if (!is.na(pct)) {
      formatted <- paste0(formatted, " (", pct, "%)")
    }
    return(formatted)
  }

  as.character(val)
}

# =============================================================================
# UI HELPER FUNCTIONS
# =============================================================================

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

  # Determine which columns are metrics vs values
  metric_cols_idx <- which(names(dt) == metric_col) - 1  # 0-indexed for JS
  value_cols_idx <- setdiff(seq_along(names(dt)) - 1, metric_cols_idx)

  datatable(
    dt,
    options = list(
      paging = FALSE,  # Remove pagination entirely
      scrollX = TRUE,
      scrollY = "600px",
      dom = 'frti',  # Removed 'p' for pagination
      columnDefs = list(
        list(className = 'dt-left dt-head-left', targets = metric_cols_idx),
        list(className = 'dt-center dt-head-center', targets = value_cols_idx)
      ),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#2c3e50', 'color': '#fff'});",
        "}"
      )
    ),
    rownames = FALSE,
    class = 'cell-border stripe hover compact',
    style = 'bootstrap4'
  )
}

# =============================================================================
# FILTER SIDEBAR
# =============================================================================

filter_sidebar <- function(data_list) {
  shift_data <- data_list$shift_data1
  pay_data <- data_list$pay1

  date_min <- min(
    min(shift_data$Date, na.rm = TRUE),
    min(pay_data$Pay_Date, na.rm = TRUE),
    na.rm = TRUE
  )
  date_max <- max(
    max(shift_data$Date, na.rm = TRUE),
    max(pay_data$Pay_Date, na.rm = TRUE),
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

    hr(),

    checkboxInput("show_extrapolation", "Show Extrapolated Values", value = FALSE),

    hr(),

    actionButton("apply_filters", "Apply Filters", class = "btn-primary w-100"),
    actionButton("reset_filters", "Reset All Filters", class = "btn-outline-secondary w-100 mt-2"),

    hr(),

    # Display Settings
    h5("Display Settings"),
    layout_columns(
      col_widths = c(6, 6),
      selectInput("font_family", "Font",
                  choices = c("Calibri" = "Calibri, sans-serif", "Times New Roman" = "'Times New Roman', serif"),
                  selected = "Calibri, sans-serif"),
      selectInput("font_size", "Font Size",
                  choices = c("Small" = "12px", "Medium" = "14px", "Large" = "16px", "X-Large" = "18px"),
                  selected = "14px")
    ),

    hr(),

    downloadButton("download_report", "Download CSV Report", class = "w-100 mt-2")
  )
}

# =============================================================================
# UI
# =============================================================================

ui <- function(data_list, metric_spec, case_config) {

  page_navbar(
    title = "Wage & Hour Compliance Dashboard",
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2c3e50",
      "navbar-bg" = "#2c3e50"
    ),
    sidebar = filter_sidebar(data_list),

    # Initialize shinyjs
    useShinyjs(),

    # Filter status banner and custom CSS
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
          z-index: 1000;
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
      "))
    ),

    div(id = "filter_banner", style = "display: none;", "⚠ FILTERS ACTIVE - Click 'Reset All Filters' to clear"),

    # =======================================================================
    # CASE ANALYSIS DETAIL TAB
    # =======================================================================
    nav_panel(
      title = "Case Detail",
      icon = icon("file-contract"),

      card(
        card_header("Case Configuration"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),

            div(
              h5("Case Information"),
              p(strong("Case Name: "), textOutput("case_name", inline = TRUE)),
              p(strong("Case Number: "), textOutput("case_number", inline = TRUE)),
              p(strong("Date Filed: "), textOutput("date_filed", inline = TRUE))
            ),

            div(
              h5("Analysis Parameters"),
              p(strong("Relevant Period: "), textOutput("relevant_period", inline = TRUE)),
              p(strong("Sample Size: "), textOutput("sample_size", inline = TRUE)),
              p(strong("Sample Type: "), textOutput("sample_type", inline = TRUE))
            )
          )
        )
      ),

      card(
        card_header("Export to PDF"),
        card_body(
          h5("Select Sections to Include:"),
          checkboxInput("pdf_select_all_appendix", "Appendix (All)", value = FALSE),
          checkboxGroupInput(
            "pdf_sections",
            NULL,
            choices = c(
              "Overview Statistics" = "overview",
              "Time Analysis - Summary" = "time_summary",
              "Time Analysis - Shift Hours Analysis" = "time_shift_hours",
              "Time Analysis - Punch Rounding" = "time_rounding",
              "Meal & Rest Periods - Meal Analysis" = "meal_analysis",
              "Meal & Rest Periods - Meal Violations (>5 hrs)" = "meal_5hr",
              "Meal & Rest Periods - Meal Violations (>6 hrs)" = "meal_6hr",
              "Meal & Rest Periods - Rest Periods" = "rest_periods",
              "Pay Analysis - Summary" = "pay_summary",
              "Pay Analysis - Regular Rate" = "pay_regular_rate",
              "Appendix - Shift Hours" = "appendix_shift",
              "Appendix - Non-Work Hours" = "appendix_nonwork",
              "Appendix - Meal Period Distribution" = "appendix_meal",
              "Appendix - Meal Start Times" = "appendix_meal_start",
              "Appendix - Meal Quarter Hour" = "appendix_meal_quarter"
            ),
            selected = c("overview", "time_summary", "time_shift_hours", "time_rounding",
                        "meal_analysis", "meal_5hr", "meal_6hr", "rest_periods",
                        "pay_summary", "pay_regular_rate")
          ),
          hr(),
          downloadButton("download_pdf", "Generate PDF Report",
                        class = "btn-primary btn-lg",
                        icon = icon("file-pdf"))
        )
      )
    ),

    # =======================================================================
    # OVERVIEW TAB
    # =======================================================================
    nav_panel(
      title = "Overview",
      icon = icon("dashboard"),

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
        col_widths = c(6, 6),

        value_box(
          title = "Pay Periods",
          value = textOutput("total_pay_periods_combined"),
          showcase = icon("calendar"),
          theme = "secondary",
          p(class = "small text-muted",
            "Time: ", textOutput("time_pay_periods", inline = TRUE), " | ",
            "Pay: ", textOutput("pay_pay_periods", inline = TRUE))
        ),
        value_box(
          title = "Weeks",
          value = textOutput("total_weeks"),
          showcase = icon("calendar-week"),
          theme = "secondary"
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

    # =======================================================================
    # DATA COMPARISON TAB
    # =======================================================================
    nav_panel(
      title = "Data Comparison",
      icon = icon("venn-diagram"),

      layout_columns(
        col_widths = c(8, 4),

        card(
          card_header("Employee Overlap Visualization"),
          card_body(
            checkboxGroupInput(
              "venn_sources",
              "Select Data Sources:",
              choices = c("Time Data (shift_data1)" = "time",
                         "Pay Data (pay1)" = "pay",
                         "Class Data (class1)" = "class"),
              selected = c("time", "pay", "class"),
              inline = TRUE
            ),
            withSpinner(plotlyOutput("venn_diagram_plot", height = "500px"), type = 6, color = "#2c3e50")
          )
        ),

        card(
          card_header("Coverage Statistics"),
          card_body(
            withSpinner(uiOutput("coverage_statistics"), type = 6, color = "#2c3e50")
          )
        )
      ),

      layout_columns(
        col_widths = c(12),

        card(
          card_header("Employee-Period Comparison Detail"),
          card_body(
            withSpinner(DTOutput("employee_period_table"), type = 6, color = "#2c3e50")
          )
        )
      )
    ),

    # =======================================================================
    # TIME ANALYSIS TAB (CONSOLIDATED)
    # =======================================================================
    nav_panel(
      title = "Time Analysis",
      icon = icon("clock"),

      navset_card_underline(
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
        )
      )
    ),

    # =======================================================================
    # MEAL & REST PERIODS
    # =======================================================================
    nav_menu(
      title = "Meal & Rest",
      icon = icon("utensils"),

      nav_panel(
        "Meal Analysis",
        withSpinner(DTOutput("table_meal_consolidated"), type = 6, color = "#2c3e50")
      ),

      nav_panel(
        "Meal Violations >5hrs",
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
        "Meal Violations >6hrs",
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
    # PAY SUMMARY TAB (CONSOLIDATED)
    # =======================================================================
    nav_panel(
      title = "Pay Summary",
      icon = icon("dollar-sign"),

      withSpinner(DTOutput("table_pay_consolidated"), type = 6, color = "#2c3e50")
    ),

    # =======================================================================
    # REGULAR RATE TAB (CONSOLIDATED)
    # =======================================================================
    nav_panel(
      title = "Regular Rate",
      icon = icon("calculator"),

      withSpinner(DTOutput("table_rrop_consolidated"), type = 6, color = "#2c3e50")
    ),

    # =======================================================================
    # PAY CODES TAB
    # =======================================================================
    nav_panel(
      title = "Pay Codes",
      icon = icon("tags"),

      withSpinner(DTOutput("table_pay_codes"), type = 6, color = "#2c3e50")
    ),

    # =======================================================================
    # DAMAGES TAB
    # =======================================================================
    nav_panel(
      title = "Damages",
      icon = icon("gavel"),

      navset_card_underline(
        nav_panel(
          "All Damages",
          under_construction_card("Total Damages Summary")
        ),
        nav_panel(
          "Meal",
          under_construction_card("Meal Violation Damages")
        ),
        nav_panel(
          "Rest",
          under_construction_card("Rest Violation Damages")
        ),
        nav_panel(
          "PAGA",
          under_construction_card("PAGA Penalties")
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

# =============================================================================
# SERVER (Part 1 - will continue in next message due to length)
# =============================================================================

server <- function(data_list, metric_spec, case_config_init, analysis_tables) {
  function(input, output, session) {

    # Case configuration
    case_config <- reactiveVal(case_config_init)

    # Categorize metric groups for consolidation
    metric_groups <- unique(metric_spec$metric_group)
    time_summary_groups <- metric_groups[grepl("^Time Summary$", metric_groups)]
    time_shift_groups <- metric_groups[grepl("^Time Shift Hours Analysis", metric_groups)]
    time_rounding_groups <- metric_groups[grepl("^Time Punch Rounding", metric_groups)]
    time_meal_analysis <- metric_groups[grepl("^Time Meal Period Analysis", metric_groups)]
    time_meal_violations_5 <- metric_groups[grepl("^Time Meal Violations \\(>5", metric_groups)]
    time_meal_violations_6 <- metric_groups[grepl("^Time Meal Violations \\(>6", metric_groups)]
    time_rest <- metric_groups[grepl("^Time Rest", metric_groups)]
    pay_summary_groups <- metric_groups[grepl("^Pay Summary$|^Pay Overtime$|^Pay Double Time$|^Pay Meal Premiums$|^Pay Rest Premiums$|^Pay Bonuses$|^Pay Shift Differentials$|^Pay Sick Pay$", metric_groups)]
    pay_regular_rate <- metric_groups[grepl("^Pay Regular Rate", metric_groups)]

    # Original date range
    original_date_min <- min(
      min(data_list$shift_data1$Date, na.rm = TRUE),
      min(data_list$pay1$Pay_Date, na.rm = TRUE),
      na.rm = TRUE
    )
    original_date_max <- max(
      max(data_list$shift_data1$Date, na.rm = TRUE),
      max(data_list$pay1$Pay_Date, na.rm = TRUE),
      na.rm = TRUE
    )

    # Server-side selectize for employee filter
    all_employee_ids <- sort(unique(c(data_list$shift_data1$ID, data_list$pay1$Pay_ID)))
    updateSelectizeInput(
      session,
      "employee_filter",
      choices = all_employee_ids,
      server = TRUE
    )

    # Apply dynamic font styling
    observe({
      shinyjs::runjs(paste0(
        "$('#custom-font-style').remove();",
        "$('head').append('<style id=\"custom-font-style\">",
        "body, .dataTables_wrapper, .value-box, .card, .sidebar { ",
        "font-family: ", input$font_family, " !important; ",
        "font-size: ", input$font_size, " !important; }",
        "</style>');"
      ))
    })

    # Current filters
    current_filters <- reactiveVal(list())

    # Show/hide filter banner
    observe({
      filters <- current_filters()
      if (length(filters) > 0) {
        shinyjs::show("filter_banner")
      } else {
        shinyjs::hide("filter_banner")
      }
    })

    # Reset dates
    observeEvent(input$reset_dates, {
      updateDateRangeInput(session, "date_range",
                           start = original_date_min,
                           end = original_date_max)
    })

    # Apply filters
    observeEvent(input$apply_filters, {
      filters <- list(
        date_min = input$date_range[1],
        date_max = input$date_range[2]
      )

      if (length(input$employee_filter) > 0) {
        filters$ID <- input$employee_filter
        filters$Pay_ID <- input$employee_filter
      }

      current_filters(filters)
    })

    # Reset filters
    observeEvent(input$reset_filters, {
      updateDateRangeInput(session, "date_range",
                           start = original_date_min,
                           end = original_date_max)
      updateSelectizeInput(session, "employee_filter", selected = character(0))
      current_filters(list())
    })

    # Appendix checkbox toggle
    observeEvent(input$pdf_select_all_appendix, {
      appendix_items <- c("appendix_shift", "appendix_nonwork", "appendix_meal",
                          "appendix_meal_start", "appendix_meal_quarter")
      current_selection <- input$pdf_sections

      if (input$pdf_select_all_appendix) {
        # Add all appendix items
        new_selection <- unique(c(current_selection, appendix_items))
      } else {
        # Remove all appendix items
        new_selection <- setdiff(current_selection, appendix_items)
      }

      updateCheckboxGroupInput(session, "pdf_sections", selected = new_selection)
    })

    # Filtered data with precomputed metadata
    filtered_data <- reactive({
      filters <- current_filters()

      shift_filtered <- copy(data_list$shift_data1)
      pay_filtered <- copy(data_list$pay1)

      # Apply filters
      if (!is.null(filters$date_min)) {
        shift_filtered <- shift_filtered[Date >= filters$date_min]
      }
      if (!is.null(filters$date_max)) {
        shift_filtered <- shift_filtered[Date <= filters$date_max]
      }
      if (!is.null(filters$ID)) {
        shift_filtered <- shift_filtered[ID %in% filters$ID]
      }

      if (!is.null(filters$date_min)) {
        pay_filtered <- pay_filtered[Pay_Period_End >= filters$date_min]
      }
      if (!is.null(filters$date_max)) {
        pay_filtered <- pay_filtered[Pay_Period_End <= filters$date_max]
      }
      if (!is.null(filters$Pay_ID)) {
        pay_filtered <- pay_filtered[Pay_ID %in% filters$Pay_ID]
      }

      # Precompute years and key groups
      shift_years <- if (nrow(shift_filtered) > 0 && "Date" %in% names(shift_filtered)) {
        sort(unique(year(shift_filtered$Date)))
      } else NULL

      pay_years <- if (nrow(pay_filtered) > 0 && "Pay_Period_End" %in% names(pay_filtered)) {
        sort(unique(year(pay_filtered$Pay_Period_End)))
      } else NULL

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

      list(
        shift_data1 = shift_filtered,
        pay1 = pay_filtered,
        shift_years = shift_years,
        pay_years = pay_years,
        shift_key_groups = shift_key_groups,
        pay_key_groups = pay_key_groups
      )
    })

    # ===========================================================================
    # CASE CONFIGURATION OUTPUTS
    # ===========================================================================

    output$case_name <- renderText({
      config <- case_config()
      config$case_name %||% "Unknown"
    })

    output$case_number <- renderText({
      config <- case_config()
      config$case_number %||% "Unknown"
    })

    output$date_filed <- renderText({
      config <- case_config()
      if (!is.null(config$date_filed) && !is.na(config$date_filed)) {
        as.character(config$date_filed)
      } else {
        "Unknown"
      }
    })

    output$relevant_period <- renderText({
      config <- case_config()
      start <- config$relevant_period_start
      end <- config$relevant_period_end

      if (!is.null(start) && !is.na(start) && !is.null(end) && !is.na(end)) {
        paste(start, "to", end)
      } else {
        "Unknown"
      }
    })

    output$sample_size <- renderText({
      config <- case_config()
      config$sample_size %||% "Unknown"
    })

    output$sample_type <- renderText({
      config <- case_config()
      config$sample_type %||% "Unknown"
    })

    # ===========================================================================
    # OVERVIEW OUTPUTS
    # ===========================================================================

    output$total_employees_time <- renderText({
      data <- filtered_data()
      format(uniqueN(data$shift_data1$ID), big.mark = ",")
    })

    output$total_employees_pay <- renderText({
      data <- filtered_data()
      format(uniqueN(data$pay1$Pay_ID), big.mark = ",")
    })

    output$total_employees_class <- renderText({
      if (!is.null(data_list$class1) && "Class_ID" %in% names(data_list$class1)) {
        format(uniqueN(data_list$class1$Class_ID), big.mark = ",")
      } else {
        "N/A"
      }
    })

    output$total_pay_periods_combined <- renderText({
      data <- filtered_data()
      time_pp <- uniqueN(data$shift_data1$ID_Period_End)
      pay_pp <- uniqueN(data$pay1$Pay_ID_Period_End)
      total <- uniqueN(c(data$shift_data1$ID_Period_End, data$pay1$Pay_ID_Period_End))
      format(total, big.mark = ",")
    })

    output$time_pay_periods <- renderText({
      data <- filtered_data()
      format(uniqueN(data$shift_data1$ID_Period_End), big.mark = ",")
    })

    output$pay_pay_periods <- renderText({
      data <- filtered_data()
      format(uniqueN(data$pay1$Pay_ID_Period_End), big.mark = ",")
    })

    output$total_weeks <- renderText({
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

      pay_emp <- data$pay1[, .(
        Employees = uniqueN(Pay_ID),
        Type = "Pay Data"
      ), by = .(Period = Pay_Period_End)]

      combined <- rbindlist(list(time_emp, pay_emp))
      combined <- combined[order(Period)]

      plot_ly(combined, x = ~Period, y = ~Employees, color = ~Type,
              type = 'scatter', mode = 'lines+markers',
              colors = c("Time Data" = "#2c3e50", "Pay Data" = "#27ae60")) %>%
        layout(
          title = "Time & Pay Data Comparison During Relevant Period",
          xaxis = list(title = "Pay Period End Date"),
          yaxis = list(title = "Unique Employees"),
          hovermode = 'x unified'
        )
    })

    # Venn diagram data calculation
    venn_data <- reactive({
      data <- filtered_data()

      # Get unique employee IDs from each source
      time_ids <- unique(data$shift_data1$ID)
      pay_ids <- unique(data$pay1$Pay_ID)
      class_ids <- if (!is.null(data_list$class1) && "Class_ID" %in% names(data_list$class1)) {
        unique(data_list$class1$Class_ID)
      } else {
        character(0)
      }

      # Calculate overlaps
      time_only <- setdiff(time_ids, union(pay_ids, class_ids))
      pay_only <- setdiff(pay_ids, union(time_ids, class_ids))
      class_only <- setdiff(class_ids, union(time_ids, pay_ids))

      time_pay <- setdiff(intersect(time_ids, pay_ids), class_ids)
      time_class <- setdiff(intersect(time_ids, class_ids), pay_ids)
      pay_class <- setdiff(intersect(pay_ids, class_ids), time_ids)

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

    output$venn_diagram_plot <- renderPlotly({
      venn <- venn_data()
      sources <- input$venn_sources

      # Create overlapping Venn diagram using plotly
      plot_ly() %>%
        layout(
          title = "Employee Data Overlap",
          xaxis = list(range = c(-3, 3), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
          yaxis = list(range = c(-2.5, 2.5), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
          shapes = list(),
          annotations = list(),
          showlegend = FALSE
        ) -> p

      shapes_list <- list()
      annot_list <- list()

      # Two-circle Venn diagram (Time and Pay only)
      if (length(sources) == 2 && "time" %in% sources && "pay" %in% sources) {
        # Time circle (left, dark blue)
        shapes_list[[length(shapes_list) + 1]] <- list(
          type = "circle",
          xref = "x", yref = "y",
          x0 = -2, y0 = -1, x1 = 0, y1 = 1,
          line = list(color = "#2c3e50", width = 3),
          fillcolor = "rgba(44, 62, 80, 0.3)"
        )

        # Pay circle (right, green)
        shapes_list[[length(shapes_list) + 1]] <- list(
          type = "circle",
          xref = "x", yref = "y",
          x0 = 0, y0 = -1, x1 = 2, y1 = 1,
          line = list(color = "#27ae60", width = 3),
          fillcolor = "rgba(39, 174, 96, 0.3)"
        )

        # Labels outside circles
        annot_list[[length(annot_list) + 1]] <- list(
          x = -1.5, y = 1.5,
          text = paste0("<b>Time Data</b><br>", format(venn$time_total, big.mark = ",")),
          showarrow = FALSE,
          font = list(size = 12, color = "#2c3e50")
        )

        annot_list[[length(annot_list) + 1]] <- list(
          x = 1.5, y = 1.5,
          text = paste0("<b>Pay Data</b><br>", format(venn$pay_total, big.mark = ",")),
          showarrow = FALSE,
          font = list(size = 12, color = "#27ae60")
        )

        # Time only (left region)
        if (venn$time_only > 0) {
          annot_list[[length(annot_list) + 1]] <- list(
            x = -1.3, y = 0,
            text = format(venn$time_only, big.mark = ","),
            showarrow = FALSE,
            font = list(size = 14, color = "#2c3e50", weight = "bold")
          )
        }

        # Overlap region (center)
        overlap_count <- venn$time_pay + venn$all_three
        if (overlap_count > 0) {
          annot_list[[length(annot_list) + 1]] <- list(
            x = 0, y = 0,
            text = format(overlap_count, big.mark = ","),
            showarrow = FALSE,
            font = list(size = 16, color = "#34495e", weight = "bold"),
            bgcolor = "rgba(255,255,255,0.8)",
            borderpad = 4
          )
        }

        # Pay only (right region)
        if (venn$pay_only > 0) {
          annot_list[[length(annot_list) + 1]] <- list(
            x = 1.3, y = 0,
            text = format(venn$pay_only, big.mark = ","),
            showarrow = FALSE,
            font = list(size = 14, color = "#27ae60", weight = "bold")
          )
        }
      }

      # Three-circle Venn diagram
      else if (length(sources) == 3 && venn$class_total > 0) {
        # Time circle (top left, dark blue)
        shapes_list[[length(shapes_list) + 1]] <- list(
          type = "circle",
          xref = "x", yref = "y",
          x0 = -2, y0 = 0.2, x1 = 0, y1 = 2.2,
          line = list(color = "#2c3e50", width = 3),
          fillcolor = "rgba(44, 62, 80, 0.25)"
        )

        # Pay circle (top right, green)
        shapes_list[[length(shapes_list) + 1]] <- list(
          type = "circle",
          xref = "x", yref = "y",
          x0 = 0, y0 = 0.2, x1 = 2, y1 = 2.2,
          line = list(color = "#27ae60", width = 3),
          fillcolor = "rgba(39, 174, 96, 0.25)"
        )

        # Class circle (bottom, orange)
        shapes_list[[length(shapes_list) + 1]] <- list(
          type = "circle",
          xref = "x", yref = "y",
          x0 = -1, y0 = -2, x1 = 1, y1 = 0,
          line = list(color = "#e67e22", width = 3),
          fillcolor = "rgba(230, 126, 34, 0.25)"
        )

        # Labels
        annot_list[[length(annot_list) + 1]] <- list(
          x = -1.5, y = 2.2,
          text = paste0("<b>Time</b> (", format(venn$time_total, big.mark = ","), ")"),
          showarrow = FALSE,
          font = list(size = 11, color = "#2c3e50")
        )

        annot_list[[length(annot_list) + 1]] <- list(
          x = 1.5, y = 2.2,
          text = paste0("<b>Pay</b> (", format(venn$pay_total, big.mark = ","), ")"),
          showarrow = FALSE,
          font = list(size = 11, color = "#27ae60")
        )

        annot_list[[length(annot_list) + 1]] <- list(
          x = 0, y = -2.2,
          text = paste0("<b>Class</b> (", format(venn$class_total, big.mark = ","), ")"),
          showarrow = FALSE,
          font = list(size = 11, color = "#e67e22")
        )

        # Add counts in regions
        # Time only (top left)
        if (venn$time_only > 0) {
          annot_list[[length(annot_list) + 1]] <- list(
            x = -1.4, y = 1.5,
            text = format(venn$time_only, big.mark = ","),
            showarrow = FALSE,
            font = list(size = 12, color = "#2c3e50", weight = "bold")
          )
        }

        # Pay only (top right)
        if (venn$pay_only > 0) {
          annot_list[[length(annot_list) + 1]] <- list(
            x = 1.4, y = 1.5,
            text = format(venn$pay_only, big.mark = ","),
            showarrow = FALSE,
            font = list(size = 12, color = "#27ae60", weight = "bold")
          )
        }

        # Class only (bottom)
        if (venn$class_only > 0) {
          annot_list[[length(annot_list) + 1]] <- list(
            x = 0, y = -1.3,
            text = format(venn$class_only, big.mark = ","),
            showarrow = FALSE,
            font = list(size = 12, color = "#e67e22", weight = "bold")
          )
        }

        # Time & Pay overlap (top center)
        if (venn$time_pay > 0) {
          annot_list[[length(annot_list) + 1]] <- list(
            x = 0, y = 1.5,
            text = format(venn$time_pay, big.mark = ","),
            showarrow = FALSE,
            font = list(size = 12, color = "#34495e", weight = "bold")
          )
        }

        # Time & Class overlap (left)
        if (venn$time_class > 0) {
          annot_list[[length(annot_list) + 1]] <- list(
            x = -0.9, y = 0.3,
            text = format(venn$time_class, big.mark = ","),
            showarrow = FALSE,
            font = list(size = 12, color = "#34495e", weight = "bold")
          )
        }

        # Pay & Class overlap (right)
        if (venn$pay_class > 0) {
          annot_list[[length(annot_list) + 1]] <- list(
            x = 0.9, y = 0.3,
            text = format(venn$pay_class, big.mark = ","),
            showarrow = FALSE,
            font = list(size = 12, color = "#34495e", weight = "bold")
          )
        }

        # All three overlap (center)
        if (venn$all_three > 0) {
          annot_list[[length(annot_list) + 1]] <- list(
            x = 0, y = 0.7,
            text = format(venn$all_three, big.mark = ","),
            showarrow = FALSE,
            font = list(size = 14, color = "#000000", weight = "bold"),
            bgcolor = "rgba(255,255,255,0.9)",
            borderpad = 4
          )
        }
      }

      # Single circle or other combinations - simplified view
      else {
        if ("time" %in% sources) {
          shapes_list[[length(shapes_list) + 1]] <- list(
            type = "circle",
            xref = "x", yref = "y",
            x0 = -1, y0 = -1, x1 = 1, y1 = 1,
            line = list(color = "#2c3e50", width = 3),
            fillcolor = "rgba(44, 62, 80, 0.3)"
          )
          annot_list[[length(annot_list) + 1]] <- list(
            x = 0, y = 0,
            text = paste0("<b>Time Data</b><br>", format(venn$time_total, big.mark = ",")),
            showarrow = FALSE,
            font = list(size = 14, color = "#2c3e50")
          )
        }
      }

      p %>% layout(shapes = shapes_list, annotations = annot_list)
    })

    output$coverage_statistics <- renderUI({
      venn <- venn_data()

      tagList(
        tags$ul(
          tags$li(HTML(paste0("<strong>Time Data:</strong> ", format(venn$time_total, big.mark = ","), " employees"))),
          tags$li(HTML(paste0("<strong>Pay Data:</strong> ", format(venn$pay_total, big.mark = ","), " employees"))),
          if (venn$class_total > 0) {
            tags$li(HTML(paste0("<strong>Class Data:</strong> ", format(venn$class_total, big.mark = ","), " employees")))
          },
          tags$br(),
          tags$li(HTML(paste0("<strong>In All Sources:</strong> ", format(venn$all_three, big.mark = ","), " employees"))),
          tags$li(HTML(paste0("<strong>Time & Pay Only:</strong> ", format(venn$time_pay, big.mark = ","), " employees"))),
          if (venn$class_total > 0) {
            tags$li(HTML(paste0("<strong>Time & Class Only:</strong> ", format(venn$time_class, big.mark = ","), " employees")))
          },
          if (venn$class_total > 0) {
            tags$li(HTML(paste0("<strong>Pay & Class Only:</strong> ", format(venn$pay_class, big.mark = ","), " employees")))
          },
          tags$br(),
          tags$li(HTML(paste0("<strong>Time Only:</strong> ", format(venn$time_only, big.mark = ","), " employees"))),
          tags$li(HTML(paste0("<strong>Pay Only:</strong> ", format(venn$pay_only, big.mark = ","), " employees"))),
          if (venn$class_total > 0) {
            tags$li(HTML(paste0("<strong>Class Only:</strong> ", format(venn$class_only, big.mark = ","), " employees")))
          }
        )
      )
    })

    # ===========================================================================
    # CONSOLIDATED TABLES
    # ===========================================================================

    # Get extrapolation factor
    extrap_factor <- reactive({
      config <- case_config()
      if (!is.null(input$show_extrapolation) && input$show_extrapolation &&
          !is.null(config$extrapolation_enabled) && config$extrapolation_enabled) {
        config$extrapolation_factor %||% 1.0
      } else {
        1.0
      }
    })

    # Time Analysis - Summary
    output$table_time_summary <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_summary_groups, current_filters(), factor)

      create_dt_table(results)
    })

    # Time Analysis - Shift Hours
    output$table_shift_hours <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_shift_groups, current_filters(), factor)

      create_dt_table(results)
    })

    # Rounding Consolidated
    output$table_rounding_consolidated <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_rounding_groups, current_filters(), factor)

      create_dt_table(results)
    })

    # Meal Analysis Consolidated
    output$table_meal_consolidated <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_analysis, current_filters(), factor)

      create_dt_table(results)
    })

    # Meal 5hr Consolidated
    output$table_meal_5hr_consolidated <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_5, current_filters(), factor)

      create_dt_table(results)
    })

    # Meal 5hr Short Details
    output$table_meal_5hr_short_details <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_5, current_filters(), factor)
      # Filter to only short meal metrics
      if (nrow(results) > 0 && "Metric" %in% names(results)) {
        results <- results[grepl("short|mins short", Metric, ignore.case = TRUE)]
      }

      create_dt_table(results)
    })

    # Meal 5hr Late Details
    output$table_meal_5hr_late_details <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_5, current_filters(), factor)
      # Filter to only late meal metrics
      if (nrow(results) > 0 && "Metric" %in% names(results)) {
        results <- results[grepl("late|mins late", Metric, ignore.case = TRUE)]
      }

      create_dt_table(results)
    })

    # Meal 6hr Consolidated
    output$table_meal_6hr_consolidated <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_6, current_filters(), factor)

      create_dt_table(results)
    })

    # Meal 6hr Short Details
    output$table_meal_6hr_short_details <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_6, current_filters(), factor)
      # Filter to only short meal metrics
      if (nrow(results) > 0 && "Metric" %in% names(results)) {
        results <- results[grepl("short|mins short", Metric, ignore.case = TRUE)]
      }

      create_dt_table(results)
    })

    # Meal 6hr Late Details
    output$table_meal_6hr_late_details <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_6, current_filters(), factor)
      # Filter to only late meal metrics
      if (nrow(results) > 0 && "Metric" %in% names(results)) {
        results <- results[grepl("late|mins late", Metric, ignore.case = TRUE)]
      }

      create_dt_table(results)
    })

    # Rest Consolidated
    output$table_rest_consolidated <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_rest, current_filters(), factor)

      create_dt_table(results)
    })

    # Pay Summary Consolidated
    output$table_pay_consolidated <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, pay_summary_groups, current_filters(), factor)

      create_dt_table(results)
    })

    # RROP Consolidated
    output$table_rrop_consolidated <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, pay_regular_rate, current_filters(), factor)

      # Move Total and Net rows to end
      if (nrow(results) > 0 && "Metric" %in% names(results)) {
        total_rows <- results[grepl("^(Total|Net)", Metric, ignore.case = TRUE)]
        other_rows <- results[!grepl("^(Total|Net)", Metric, ignore.case = TRUE)]
        results <- rbindlist(list(other_rows, total_rows), fill = TRUE)
      }

      create_dt_table(results)
    })

    # ===========================================================================
    # ANALYSIS TABLES (FROM FILES)
    # ===========================================================================

    output$table_pay_codes <- renderDT({
      create_dt_table(analysis_tables$pay_code_summary, metric_col = "Pay Code")
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

    output$employee_period_table <- renderDT({
      create_dt_table(analysis_tables$employee_comparison, metric_col = "ID")
    })

    # ===========================================================================
    # DOWNLOAD HANDLER
    # ===========================================================================

    output$download_report <- downloadHandler(
      filename = function() {
        paste0("wage_hour_report_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Show notification
        showNotification("Generating CSV report...", type = "message", duration = 2)

        # Generate full report CSV
        data <- filtered_data()
        factor <- extrap_factor()

        all_groups <- unique(metric_spec$metric_group)
        results <- calculate_group_metrics(data, metric_spec, all_groups, current_filters(), factor)

        fwrite(results, file)

        showNotification("CSV report ready! Check your downloads.", type = "message", duration = 3)
      }
    )

    # PDF Download Handler
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("Wage_Hour_Report_", format(Sys.Date(), "%Y%m%d"), ".html")
      },
      content = function(file) {
        # Show notification
        showNotification("Generating PDF report (HTML format)... This may take a moment.",
                        type = "message", duration = 3)

        data <- filtered_data()
        config <- case_config()
        sections <- input$pdf_sections

        # Get case name
        case_name <- if (!is.null(config$case_name) && config$case_name != "") {
          config$case_name
        } else {
          "Unknown Case"
        }

        # Start building HTML
        html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Wage & Hour Analysis Report</title>
  <style>
    @page {
      size: legal landscape;
      margin: 0.25in;
    }

    @media print {
      @page {
        @top-left {
          content: "', case_name, '";
          font-size: 10pt;
        }
        @top-right {
          content: "Report Date: ', format(Sys.Date(), "%B %d, %Y"), '";
          font-size: 10pt;
        }
        @bottom-center {
          content: "Page " counter(page) " of " counter(pages);
          font-size: 9pt;
        }
      }

      thead { display: table-header-group; }
      tfoot { display: table-footer-group; }
      .page-break { page-break-before: always; }
    }

    body {
      font-family: Arial, sans-serif;
      font-size: 10pt;
      margin: 0;
      padding: 20px;
    }

    h1 {
      color: #2c3e50;
      border-bottom: 3px solid #3498db;
      padding-bottom: 10px;
      margin-top: 30px;
      font-size: 18pt;
    }

    h2 {
      color: #34495e;
      border-bottom: 2px solid #95a5a6;
      padding-bottom: 5px;
      margin-top: 20px;
      font-size: 14pt;
    }

    h3 {
      color: #34495e;
      margin-top: 15px;
      font-size: 12pt;
    }

    table {
      width: 100%;
      border-collapse: collapse;
      margin: 15px 0;
      font-size: 9pt;
    }

    th {
      background-color: #2c3e50;
      color: white;
      padding: 8px;
      text-align: left;
      font-weight: bold;
    }

    td {
      padding: 6px 8px;
      border-bottom: 1px solid #ddd;
    }

    tr:nth-child(even) {
      background-color: #f9f9f9;
    }

    .metric-col {
      text-align: left;
      font-weight: 500;
    }

    .value-col {
      text-align: center;
    }

    .stat-box {
      display: inline-block;
      background: #ecf0f1;
      padding: 10px 20px;
      margin: 10px 10px 10px 0;
      border-left: 4px solid #3498db;
    }

    .stat-label {
      font-size: 9pt;
      color: #7f8c8d;
      margin-bottom: 5px;
    }

    .stat-value {
      font-size: 16pt;
      font-weight: bold;
      color: #2c3e50;
    }
  </style>
</head>
<body>
')

        # Page 1: Case Information
        if ("case_info" %in% sections) {
          html_content <- paste0(html_content, '
  <h1>📋 Case Information</h1>
  <div style="margin: 20px 0;">
    <p><strong>Case Name:</strong> ', case_name, '</p>
    <p><strong>Case Number:</strong> ', ifelse(!is.null(config$case_number), config$case_number, "N/A"), '</p>
    <p><strong>Date Filed:</strong> ', ifelse(!is.null(config$date_filed), config$date_filed, "N/A"), '</p>
    <p><strong>Report Generated:</strong> ', format(Sys.Date(), "%B %d, %Y"), '</p>
  </div>
')
        }

        # Overview Statistics
        if ("overview" %in% sections) {
          html_content <- paste0(html_content, '
  <h1>📊 Overview Statistics</h1>
  <div style="margin: 20px 0;">
    <div class="stat-box">
      <div class="stat-label">Employees (Time)</div>
      <div class="stat-value">', format(uniqueN(data$shift_data1$ID), big.mark = ","), '</div>
    </div>
    <div class="stat-box">
      <div class="stat-label">Employees (Pay)</div>
      <div class="stat-value">', format(uniqueN(data$pay1$Pay_ID), big.mark = ","), '</div>
    </div>
    <div class="stat-box">
      <div class="stat-label">Total Shifts</div>
      <div class="stat-value">', format(nrow(data$shift_data1), big.mark = ","), '</div>
    </div>
    <div class="stat-box">
      <div class="stat-label">Total Weeks</div>
      <div class="stat-value">', format(uniqueN(data$shift_data1$ID_Week_End), big.mark = ","), '</div>
    </div>
    <div class="stat-box">
      <div class="stat-label">Pay Periods (Combined)</div>
      <div class="stat-value">', format(uniqueN(c(data$shift_data1$ID_Period_End, data$pay1$Pay_ID_Period_End)), big.mark = ","), '</div>
    </div>
  </div>
')
        }

        # Helper function to add table HTML
        add_table <- function(dt_table, title, icon = "📊") {
          if (nrow(dt_table) == 0) return("")

          # Limit rows for performance
          max_rows <- min(nrow(dt_table), 500)  # Reduce from 1000 to 500
          dt_table <- dt_table[1:max_rows]

          # Format column names
          col_names <- format_col_name(names(dt_table))

          # Build HTML using vectors for performance
          html_parts <- character()
          html_parts[1] <- paste0('\n  <h2>', icon, ' ', title, '</h2>\n  <table>\n    <thead>\n      <tr>')

          # Add headers
          header_cells <- paste0('<th>', col_names, '</th>')
          html_parts[2] <- paste0('\n        ', paste(header_cells, collapse = '\n        '))

          html_parts[3] <- '\n      </tr>\n    </thead>\n    <tbody>'

          # Build rows efficiently
          row_html <- character(max_rows)
          for (i in 1:max_rows) {
            cells <- character(ncol(dt_table))
            for (j in 1:ncol(dt_table)) {
              val <- dt_table[i, j, with = FALSE][[1]]
              val <- if (is.na(val)) "" else as.character(val)
              class_attr <- if (j == 1) 'metric-col' else 'value-col'
              cells[j] <- paste0('<td class="', class_attr, '">', val, '</td>')
            }
            row_html[i] <- paste0('\n      <tr>', paste(cells, collapse = ''), '</tr>')
          }

          html_parts[4] <- paste(row_html, collapse = '')
          html_parts[5] <- '\n    </tbody>\n  </table>\n'

          return(paste(html_parts, collapse = ''))
        }

        # Time Analysis - Summary
        if ("time_summary" %in% sections && length(time_summary_groups) > 0) {
          results <- calculate_group_metrics(data, metric_spec, time_summary_groups, current_filters(), extrap_factor())
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(results, "Time Analysis - Summary", "⏰"))
        }

        # Time Analysis - Shift Hours Analysis
        if ("time_shift_hours" %in% sections && length(time_shift_groups) > 0) {
          results <- calculate_group_metrics(data, metric_spec, time_shift_groups, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Time Analysis - Shift Hours Analysis", "📊"))
        }

        # Time Analysis - Punch Rounding
        if ("time_rounding" %in% sections && length(time_rounding_groups) > 0) {
          results <- calculate_group_metrics(data, metric_spec, time_rounding_groups, current_filters(), extrap_factor())
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(results, "Time Analysis - Punch Rounding", "🔄"))
        }

        # Meal Analysis
        if ("meal_analysis" %in% sections && length(time_meal_analysis) > 0) {
          results <- calculate_group_metrics(data, metric_spec, time_meal_analysis, current_filters(), extrap_factor())
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(results, "Meal Period Analysis", "🍽️"))
        }

        # Meal Violations >5 hrs
        if ("meal_5hr" %in% sections && length(time_meal_violations_5) > 0) {
          results <- calculate_group_metrics(data, metric_spec, time_meal_violations_5, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Meal Violations (>5 hours)", "⚠️"))
        }

        # Meal Violations >6 hrs
        if ("meal_6hr" %in% sections && length(time_meal_violations_6) > 0) {
          results <- calculate_group_metrics(data, metric_spec, time_meal_violations_6, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Meal Violations (>6 hours)", "⚠️"))
        }

        # Rest Periods
        if ("rest_periods" %in% sections && length(time_rest) > 0) {
          results <- calculate_group_metrics(data, metric_spec, time_rest, current_filters(), extrap_factor())
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(results, "Rest Periods", "☕"))
        }

        # Pay Analysis - Summary
        if ("pay_summary" %in% sections && length(pay_summary_groups) > 0) {
          results <- calculate_group_metrics(data, metric_spec, pay_summary_groups, current_filters(), extrap_factor())
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(results, "Pay Analysis - Summary", "💰"))
        }

        # Pay Analysis - Regular Rate
        if ("pay_regular_rate" %in% sections && length(pay_regular_rate) > 0) {
          results <- calculate_group_metrics(data, metric_spec, pay_regular_rate, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Pay Analysis - Regular Rate", "💵"))
        }

        # Appendix Tables
        if ("appendix_shift" %in% sections && !is.null(analysis_tables$shift_hrs)) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(analysis_tables$shift_hrs, "Appendix - Shift Hours", "📑"))
        }

        if ("appendix_nonwork" %in% sections && !is.null(analysis_tables$non_wrk_hrs)) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(analysis_tables$non_wrk_hrs, "Appendix - Non-Work Hours", "📑"))
        }

        if ("appendix_meal" %in% sections && !is.null(analysis_tables$meal_period)) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(analysis_tables$meal_period, "Appendix - Meal Period Distribution", "📑"))
        }

        if ("appendix_meal_start" %in% sections && !is.null(analysis_tables$meal_start_time)) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(analysis_tables$meal_start_time, "Appendix - Meal Start Times", "📑"))
        }

        if ("appendix_meal_quarter" %in% sections && !is.null(analysis_tables$meal_quarter_hr)) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(analysis_tables$meal_quarter_hr, "Appendix - Meal Quarter Hour", "📑"))
        }

        # Close HTML
        html_content <- paste0(html_content, '\n</body>\n</html>')

        # Write to file
        writeLines(html_content, file)

        showNotification("PDF report generated! Open the HTML file and use browser Print to PDF (Ctrl+P)",
                        type = "message", duration = 10)
      }
    )
  }
}

# =============================================================================
# RUN APP
# =============================================================================

message("Loading data...")
data_list <- load_data()
metric_spec <- load_metric_spec()
case_config <- load_case_config()

message("Loading analysis tables...")
analysis_tables <- list(
  pay_code_summary = load_analysis_table(PAY_CODE_SUMMARY_FILE),
  shift_hrs = load_analysis_table(SHIFT_HRS_FILE),
  non_wrk_hrs = load_analysis_table(NON_WRK_HRS_FILE),
  meal_period = load_analysis_table(MEAL_PERIOD_FILE),
  meal_start_time = load_analysis_table(MEAL_START_TIME_FILE),
  meal_quarter_hr = load_analysis_table(MEAL_QUARTER_HR_FILE),
  employee_comparison = load_analysis_table(EMPLOYEE_COMPARISON_FILE)
)

message("Starting dashboard...")
shinyApp(
  ui = ui(data_list, metric_spec, case_config),
  server = server(data_list, metric_spec, case_config, analysis_tables)
)
