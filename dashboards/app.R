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

# Create DT output with proper formatting
create_dt_table <- function(dt, metric_col = "Metric", page_length = 50) {
  if (is.null(dt) || nrow(dt) == 0) {
    return(datatable(data.table(Message = "No data available"), rownames = FALSE, options = list(dom = 't')))
  }

  # Determine which columns are metrics vs values
  metric_cols_idx <- which(names(dt) == metric_col) - 1  # 0-indexed for JS
  value_cols_idx <- setdiff(seq_along(names(dt)) - 1, metric_cols_idx)

  datatable(
    dt,
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      scrollY = "600px",
      dom = 'frtip',
      columnDefs = list(
        list(className = 'dt-left', targets = metric_cols_idx),
        list(className = 'dt-center', targets = value_cols_idx)
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

    # Font controls
    h5("Display Settings"),
    layout_columns(
      col_widths = c(6, 6),
      selectInput("font_family", "Font",
                  choices = c("Default" = "inherit", "Times New Roman" = "Times New Roman"),
                  selected = "inherit"),
      selectInput("font_size", "Font Size",
                  choices = c("Small" = "12px", "Medium" = "14px", "Large" = "16px", "X-Large" = "18px"),
                  selected = "14px")
    ),

    hr(),

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

    hr(),

    checkboxInput("show_extrapolation", "Show Extrapolated Values", value = FALSE),

    hr(),

    actionButton("apply_filters", "Apply Filters", class = "btn-primary w-100"),
    actionButton("reset_filters", "Reset All Filters", class = "btn-outline-secondary w-100 mt-2"),

    hr(),

    downloadButton("download_report", "Download CSV Report", class = "w-100 mt-2"),
    actionButton("print_report", "Print Report (PDF)", icon = icon("print"), class = "btn-info w-100 mt-2")
  )
}

# =============================================================================
# UI
# =============================================================================

ui <- function(data_list, metric_spec, case_config) {

  metric_groups <- unique(metric_spec$metric_group)

  # Categorize metric groups for consolidation
  time_summary_groups <- metric_groups[grepl("^Time Summary$", metric_groups)]
  time_shift_groups <- metric_groups[grepl("^Time Shift Hours Analysis", metric_groups)]
  time_rounding_groups <- metric_groups[grepl("^Time Punch Rounding", metric_groups)]
  time_meal_analysis <- metric_groups[grepl("^Time Meal Period Analysis", metric_groups)]
  time_meal_violations_5 <- metric_groups[grepl("^Time Meal Violations \\(>5", metric_groups)]
  time_meal_violations_6 <- metric_groups[grepl("^Time Meal Violations \\(>6", metric_groups)]
  time_rest <- metric_groups[grepl("^Time Rest", metric_groups)]
  pay_summary_groups <- metric_groups[grepl("^Pay Summary$|^Pay Overtime$|^Pay Double Time$|^Pay Meal Premiums$|^Pay Rest Premiums$|^Pay Bonuses$|^Pay Shift Differentials$|^Pay Sick Pay$", metric_groups)]
  pay_regular_rate <- metric_groups[grepl("^Pay Regular Rate", metric_groups)]

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

    # Filter status banner
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
      "))
    ),

    div(id = "filter_banner", "⚠ FILTERS ACTIVE - Click 'Reset All Filters' to clear"),

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
          theme = "primary"
        ),
        value_box(
          title = "Employees (Pay)",
          value = textOutput("total_employees_pay"),
          showcase = icon("users"),
          theme = "info"
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
        col_widths = c(6, 6),

        card(
          card_header("Employee Overlap Summary"),
          card_body(
            under_construction_card("Interactive Venn Diagram",
                                   "Click to toggle data sources and see overlaps")
          )
        ),

        card(
          card_header("Coverage Statistics"),
          card_body(
            under_construction_card("Coverage Metrics",
                                   "Detailed breakdown of employee coverage across data sources")
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
          "Summary & Levels (All-in-One)",
          withSpinner(DTOutput("table_time_consolidated"), type = 6, color = "#2c3e50")
        ),
        nav_panel(
          "Punch Rounding (All-in-One)",
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
        withSpinner(DTOutput("table_meal_5hr_consolidated"), type = 6, color = "#2c3e50")
      ),

      nav_panel(
        "Meal Violations >6hrs",
        withSpinner(DTOutput("table_meal_6hr_consolidated"), type = 6, color = "#2c3e50")
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
    # PAY CODES TAB
    # =======================================================================
    nav_panel(
      title = "Pay Codes",
      icon = icon("tags"),

      withSpinner(DTOutput("table_pay_codes"), type = 6, color = "#2c3e50")
    ),

    # =======================================================================
    # REGULAR RATE TAB (CONSOLIDATED)
    # =======================================================================
    nav_panel(
      title = "Regular Rate (RROP)",
      icon = icon("calculator"),

      withSpinner(DTOutput("table_rrop_consolidated"), type = 6, color = "#2c3e50")
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
      insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = tags$style(HTML(paste0(
          "body, .dataTables_wrapper { font-family: ", input$font_family, "; font-size: ", input$font_size, "; }"
        )))
      )
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
      if (!is.null(data_list$class1)) {
        format(uniqueN(data_list$class1$Class_ID), big.mark = ",")
      } else {
        "N/A"
      }
    })

    output$total_pay_periods_combined <- renderText({
      data <- filtered_data()
      time_pp <- uniqueN(data$shift_data1$Period_End)
      pay_pp <- uniqueN(data$pay1$Pay_Period_End)
      total <- uniqueN(c(data$shift_data1$Period_End, data$pay1$Pay_Period_End))
      format(total, big.mark = ",")
    })

    output$time_pay_periods <- renderText({
      data <- filtered_data()
      format(uniqueN(data$shift_data1$Period_End), big.mark = ",")
    })

    output$pay_pay_periods <- renderText({
      data <- filtered_data()
      format(uniqueN(data$pay1$Pay_Period_End), big.mark = ",")
    })

    output$total_weeks <- renderText({
      data <- filtered_data()
      format(uniqueN(data$shift_data1$ID_Week_End), big.mark = ",")
    })

    output$employee_coverage_plot <- renderPlotly({
      data <- filtered_data()

      # Aggregate by period
      time_emp <- data$shift_data1[, .(
        Employees = uniqueN(ID),
        Type = "Time Data"
      ), by = .(Period = Date)]

      pay_emp <- data$pay1[, .(
        Employees = uniqueN(Pay_ID),
        Type = "Pay Data"
      ), by = .(Period = Pay_Period_End)]

      combined <- rbindlist(list(time_emp, pay_emp))

      plot_ly(combined, x = ~Period, y = ~Employees, color = ~Type,
              type = 'scatter', mode = 'lines+markers') %>%
        layout(
          xaxis = list(title = "Period"),
          yaxis = list(title = "Unique Employees"),
          hovermode = 'x unified'
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

    # Time Analysis Consolidated
    output$table_time_consolidated <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      # Combine all time analysis groups
      all_groups <- c(time_summary_groups, time_shift_groups)
      results <- calculate_group_metrics(data, metric_spec, all_groups, current_filters(), factor)

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

    # Meal 6hr Consolidated
    output$table_meal_6hr_consolidated <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_6, current_filters(), factor)

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
        # Generate full report CSV
        data <- filtered_data()
        factor <- extrap_factor()

        all_groups <- unique(metric_spec$metric_group)
        results <- calculate_group_metrics(data, metric_spec, all_groups, current_filters(), factor)

        fwrite(results, file)
      }
    )

    # Print report placeholder
    observeEvent(input$print_report, {
      showNotification("PDF printing feature under development", type = "message")
    })
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
