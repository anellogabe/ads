# =============================================================================
# Wage & Hour Compliance Dashboard
# Comprehensive case analysis with flexible metric-driven reporting
# =============================================================================

library(shiny)
library(bslib)
library(data.table)
library(lubridate)
library(DT)
library(plotly)
library(here)

# For loading spinners
if (requireNamespace("shinycssloaders", quietly = TRUE)) {
  library(shinycssloaders)
  use_spinner <- TRUE
} else {
  use_spinner <- FALSE
  withSpinner <- function(x, ...) x
}

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

# Case configuration file (will be created if doesn't exist)
CASE_CONFIG_FILE <- "case_config.rds"

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
    message("Loading shift data from: ", normalizePath(shift_path))
    result$shift_data1 <- readRDS(shift_path)
  } else {
    stop(paste0(
      "\n\nCannot find shift data file:\n  ", normalizePath(shift_path, mustWork = FALSE),
      "\n\nMake sure you've run analysis.R and saved shift_data1.rds to output/"
    ))
  }

  # Load pay data (required)
  if (file.exists(pay_path)) {
    message("Loading pay data from: ", normalizePath(pay_path))
    result$pay1 <- readRDS(pay_path)
  } else {
    stop(paste0(
      "\n\nCannot find pay data file:\n  ", normalizePath(pay_path, mustWork = FALSE),
      "\n\nMake sure you've run analysis.R and saved pay1.rds to output/"
    ))
  }

  # Load time data (optional)
  if (file.exists(time_path)) {
    message("Loading time data from: ", normalizePath(time_path))
    result$time1 <- readRDS(time_path)
  } else {
    message("Time data not found (optional): ", normalizePath(time_path, mustWork = FALSE))
    result$time1 <- NULL
  }

  # Load class data (optional)
  if (file.exists(class_path)) {
    message("Loading class data from: ", normalizePath(class_path))
    result$class1 <- readRDS(class_path)
  } else {
    message("Class data not found (optional): ", normalizePath(class_path, mustWork = FALSE))
    result$class1 <- NULL
  }

  result
}

# Load metric specification
load_metric_spec <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(SCRIPTS_DIR, METRIC_SPEC_FILE)
  }

  if (!file.exists(path)) {
    stop(paste0(
      "\n\nCannot find metrics spec file:\n  ", normalizePath(path, mustWork = FALSE)
    ))
  }

  message("Loading metric spec from: ", normalizePath(path))
  spec <- fread(path)
  spec[, metric_order := .I]
  spec
}

# Load or create case configuration
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

save_case_config <- function(config) {
  config_path <- file.path(DATA_DIR, CASE_CONFIG_FILE)
  saveRDS(config, config_path)
}

# =============================================================================
# METRIC CALCULATION HELPERS
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

calculate_group_metrics <- function(data_list, spec, group_name, filters = list(), extrapolation_factor = 1.0) {
  group_spec <- spec[metric_group == group_name]
  if (nrow(group_spec) == 0) return(data.table())

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
      result_row <- data.table(metric_label = row$metric_label, All_Data = "-")
      return(result_row)
    }

    all_data_result <- calculate_single_metric(src_data, row$expr, row$digits, row$denom)

    result_row <- data.table(
      metric_label = row$metric_label,
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

create_metric_table_output <- function(output_id, show_extrapolation = TRUE) {
  DTOutput(output_id)
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

  locations <- unique(c(
    if ("Location" %in% names(shift_data)) shift_data$Location else NULL,
    if ("Pay_Location" %in% names(pay_data)) pay_data$Pay_Location else NULL
  ))
  locations <- sort(unique(na.omit(locations)))

  sidebar(
    title = "Filters",
    width = 300,

    dateRangeInput(
      "date_range",
      "Date Range",
      start = date_min,
      end = date_max,
      min = date_min,
      max = date_max
    ),

    selectizeInput(
      "employee_filter",
      "Employee ID(s)",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "Type to search employees...")
    ),

    if (length(locations) > 0) {
      selectizeInput(
        "location_filter",
        "Location(s)",
        choices = c("All" = "", locations),
        multiple = TRUE,
        options = list(placeholder = "All Locations")
      )
    },

    hr(),

    checkboxInput("show_extrapolation", "Show Extrapolated Values", value = FALSE),

    hr(),

    actionButton("apply_filters", "Apply Filters", class = "btn-primary w-100"),
    actionButton("reset_filters", "Reset Filters", class = "btn-outline-secondary w-100 mt-2"),

    hr(),

    downloadButton("download_report", "Download Report", class = "w-100")
  )
}

# =============================================================================
# UI
# =============================================================================

ui <- function(data_list, metric_spec, case_config) {

  metric_groups <- unique(metric_spec$metric_group)

  # Categorize metric groups
  time_meal_analysis <- metric_groups[grepl("^Time Meal Period Analysis", metric_groups)]
  time_meal_violations_5 <- metric_groups[grepl("^Time Meal Violations \\(>5", metric_groups)]
  time_meal_violations_6 <- metric_groups[grepl("^Time Meal Violations \\(>6", metric_groups)]
  time_rest <- metric_groups[grepl("^Time Rest", metric_groups)]
  time_shift_hours <- metric_groups[grepl("^Time Shift Hours", metric_groups)]
  time_rounding <- metric_groups[grepl("^Time Punch Rounding", metric_groups)]
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

    # =======================================================================
    # CASE ANALYSIS DETAIL TAB
    # =======================================================================
    nav_panel(
      title = "Case Analysis Detail",
      icon = icon("file-contract"),

      layout_columns(
        col_widths = c(12),

        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Case Configuration",
            actionButton("edit_case_config", "Edit", class = "btn-sm btn-outline-primary")
          ),
          card_body(
            layout_columns(
              col_widths = c(6, 6),

              div(
                h5("Case Information"),
                p(strong("Case Name: "), textOutput("case_name", inline = TRUE)),
                p(strong("Case Number: "), textOutput("case_number", inline = TRUE)),
                p(strong("Date Filed: "), textOutput("date_filed", inline = TRUE)),
                p(strong("Mediation Date: "), textOutput("mediation_date", inline = TRUE)),
                p(strong("Class Cert Deadline: "), textOutput("class_cert_deadline", inline = TRUE))
              ),

              div(
                h5("Analysis Parameters"),
                p(strong("Relevant Period: "), textOutput("relevant_period", inline = TRUE)),
                p(strong("PAGA Period: "), textOutput("paga_period", inline = TRUE)),
                p(strong("Sample Size: "), textOutput("sample_size", inline = TRUE)),
                p(strong("Sample Type: "), textOutput("sample_type", inline = TRUE))
              )
            ),

            hr(),

            h5("Extrapolation Settings"),
            layout_columns(
              col_widths = c(4, 4, 4),

              div(
                p(strong("Enabled: "), textOutput("extrap_enabled", inline = TRUE))
              ),
              div(
                p(strong("Method: "), textOutput("extrap_method", inline = TRUE))
              ),
              div(
                p(strong("Factor: "), textOutput("extrap_factor", inline = TRUE))
              )
            ),

            hr(),

            h5("Notes"),
            verbatimTextOutput("case_notes")
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
          title = "Employees (Time Data)",
          value = textOutput("total_employees_time"),
          showcase = icon("users"),
          theme = "primary"
        ),
        value_box(
          title = "Employees (Pay Data)",
          value = textOutput("total_employees_pay"),
          showcase = icon("users"),
          theme = "info"
        ),
        value_box(
          title = "Employees (Class List)",
          value = textOutput("total_employees_class"),
          showcase = icon("users"),
          theme = "success"
        )
      ),

      layout_columns(
        col_widths = c(4, 4, 4),

        value_box(
          title = "Total Shifts",
          value = textOutput("total_shifts"),
          showcase = icon("clock"),
          theme = "secondary"
        ),
        value_box(
          title = "Pay Periods",
          value = textOutput("total_pay_periods"),
          showcase = icon("calendar"),
          theme = "secondary"
        ),
        value_box(
          title = "Weeks Analyzed",
          value = textOutput("total_weeks"),
          showcase = icon("calendar-week"),
          theme = "secondary"
        )
      ),

      layout_columns(
        col_widths = c(12),

        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Data Coverage Comparison",
            actionLink("view_comparison_detail", "View Detailed Table", icon = icon("table"))
          ),
          card_body(
            under_construction_card(
              "Data Comparison Chart",
              "Interactive comparison chart showing employee coverage across time data, pay data, and class list will appear here."
            )
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Shift Distribution"),
          card_body(
            plotlyOutput("shift_dist_plot", height = "300px")
          )
        ),

        card(
          card_header("Monthly Activity"),
          card_body(
            plotlyOutput("date_coverage_plot", height = "300px")
          )
        )
      )
    ),

    # =======================================================================
    # TIME ANALYSIS TAB (Consolidated)
    # =======================================================================
    nav_panel(
      title = "Time Analysis",
      icon = icon("clock"),

      navset_card_underline(
        nav_panel(
          "Summary",
          DTOutput("table_time_summary")
        ),
        nav_panel(
          "Shift/Week/Pay Period Levels",

          h5(class = "mt-3", icon("building"), " Shift Level Analysis"),
          DTOutput("table_shift_level"),

          hr(),

          h5(class = "mt-3", icon("calendar-week"), " Week Level Analysis"),
          DTOutput("table_week_level"),

          hr(),

          h5(class = "mt-3", icon("calendar-alt"), " Pay Period Level Analysis"),
          DTOutput("table_pay_period_level"),

          hr(),

          h5(class = "mt-3", icon("calculator"), " Time Totals"),
          DTOutput("table_time_totals")
        ),
        nav_panel(
          "Punch Rounding Analysis",

          h5(class = "mt-3", "Rounding by Punch Type"),
          DTOutput("table_rounding_by_type"),

          hr(),

          h5(class = "mt-3", "Rounding Levels Summary"),
          layout_columns(
            col_widths = c(6, 6),

            div(
              h6("Shift & Employee Level"),
              DTOutput("table_rounding_shift_emp")
            ),
            div(
              h6("Week & Pay Period Level"),
              DTOutput("table_rounding_week_pp")
            )
          ),

          hr(),

          h5(class = "mt-3", "Rounding Time Totals"),
          DTOutput("table_rounding_totals")
        )
      )
    ),

    # =======================================================================
    # MEAL PERIOD TABS
    # =======================================================================
    nav_menu(
      title = "Meal Periods",
      icon = icon("utensils"),

      nav_panel(
        "Meal Analysis",
        if (length(time_meal_analysis) > 0) {
          navset_card_underline(
            !!!lapply(time_meal_analysis, function(grp) {
              nav_panel(
                gsub("^Time Meal Period Analysis\\s*\\(?(.*)\\)?$", "\\1", grp),
                DTOutput(paste0("table_", make.names(grp)))
              )
            })
          )
        } else {
          under_construction_card("Meal Analysis", "Meal period analysis data will appear here.")
        }
      ),

      nav_panel(
        "Violations (>5 hrs)",
        if (length(time_meal_violations_5) > 0) {
          navset_card_underline(
            !!!lapply(time_meal_violations_5, function(grp) {
              nav_panel(
                gsub("^Time Meal Violations \\(>5 hrs\\)\\s*-?\\s*(.*)$", "\\1", grp),
                DTOutput(paste0("table_", make.names(grp)))
              )
            })
          )
        } else {
          under_construction_card("Meal Violations >5hrs", "Meal violation data will appear here.")
        }
      ),

      nav_panel(
        "Violations (>6 hrs)",
        if (length(time_meal_violations_6) > 0) {
          navset_card_underline(
            !!!lapply(time_meal_violations_6, function(grp) {
              nav_panel(
                gsub("^Time Meal Violations \\(>6 hrs\\)\\s*-?\\s*(.*)$", "\\1", grp),
                DTOutput(paste0("table_", make.names(grp)))
              )
            })
          )
        } else {
          under_construction_card("Meal Violations >6hrs", "Meal violation data will appear here.")
        }
      )
    ),

    # =======================================================================
    # REST PERIODS TAB
    # =======================================================================
    nav_panel(
      title = "Rest Periods",
      icon = icon("pause-circle"),

      if (length(time_rest) > 0) {
        navset_card_underline(
          !!!lapply(time_rest, function(grp) {
            nav_panel(
              gsub("^Time Rest\\s*(.*)$", "\\1", grp),
              DTOutput(paste0("table_", make.names(grp)))
            )
          })
        )
      } else {
        under_construction_card("Rest Periods", "Rest period analysis data will appear here.")
      }
    ),

    # =======================================================================
    # PAY SUMMARY TAB (Consolidated)
    # =======================================================================
    nav_panel(
      title = "Pay Summary",
      icon = icon("dollar-sign"),

      navset_card_underline(
        nav_panel(
          "Overview",
          DTOutput("table_pay_summary")
        ),
        nav_panel(
          "Overtime",
          DTOutput("table_pay_overtime")
        ),
        nav_panel(
          "Double Time",
          DTOutput("table_pay_doubletime")
        ),
        nav_panel(
          "Premiums",

          h5(class = "mt-3", "Meal Premiums"),
          DTOutput("table_pay_meal_premiums"),

          hr(),

          h5(class = "mt-3", "Rest Premiums"),
          DTOutput("table_pay_rest_premiums")
        ),
        nav_panel(
          "Bonuses & Differentials",

          h5(class = "mt-3", "Bonuses"),
          DTOutput("table_pay_bonuses"),

          hr(),

          h5(class = "mt-3", "Shift Differentials"),
          DTOutput("table_pay_shift_diff")
        ),
        nav_panel(
          "Other",

          h5(class = "mt-3", "Sick Pay"),
          DTOutput("table_pay_sick")
        )
      )
    ),

    # =======================================================================
    # PAY CODES TAB (New)
    # =======================================================================
    nav_panel(
      title = "Pay Codes",
      icon = icon("tags"),

      navset_card_underline(
        nav_panel(
          "Pay Code Summary",
          under_construction_card(
            "Pay Code Summary Table",
            "Summary table from pay_code_summary_tbl will appear here."
          )
        ),
        nav_panel(
          "Rate Type Summary",
          under_construction_card(
            "Rate Type Summary Table",
            "Detailed rate type analysis from rate_type_summary will appear here."
          )
        )
      )
    ),

    # =======================================================================
    # REGULAR RATE TAB
    # =======================================================================
    nav_panel(
      title = "Regular Rate (RROP)",
      icon = icon("calculator"),

      if (length(pay_regular_rate) > 0) {
        navset_card_underline(
          !!!lapply(pay_regular_rate, function(grp) {
            nav_panel(
              gsub("^Pay Regular Rate\\s*-?\\s*(.*)$", "\\1", grp),
              DTOutput(paste0("table_", make.names(grp)))
            )
          })
        )
      } else {
        under_construction_card("Regular Rate Analysis", "RROP analysis data will appear here.")
      }
    ),

    # =======================================================================
    # DAMAGES TAB (New Consolidated)
    # =======================================================================
    nav_panel(
      title = "Damages",
      icon = icon("gavel"),

      navset_card_underline(
        nav_panel(
          "All Damages Summary",
          under_construction_card(
            "Total Damages Summary",
            "Comprehensive damages summary across all categories will appear here."
          )
        ),
        nav_panel(
          "Meal Violations",

          h5(class = "mt-3", "Meal Damages (No Waivers)"),
          under_construction_card("Meal damages calculation without waivers"),

          hr(),

          h5(class = "mt-3", "Meal Damages (With Waivers)"),
          under_construction_card("Meal damages calculation with waivers applied")
        ),
        nav_panel(
          "Rest Violations",
          under_construction_card("Rest period violation damages")
        ),
        nav_panel(
          "Time & Rounding",

          h5(class = "mt-3", "Clock Rounding Damages"),
          under_construction_card("Time rounding damages"),

          hr(),

          h5(class = "mt-3", "Off-the-Clock Damages"),
          under_construction_card("Off-the-clock work damages")
        ),
        nav_panel(
          "Pay Issues",

          h5(class = "mt-3", "Regular Rate (RROP) Damages"),
          under_construction_card("Regular rate of pay underpayments"),

          hr(),

          h5(class = "mt-3", "Unreimbursed Expenses"),
          under_construction_card("Unreimbursed business expenses"),

          hr(),

          h5(class = "mt-3", "Waiting Time Penalties"),
          under_construction_card("Waiting time penalties for final pay")
        ),
        nav_panel(
          "Wage Statements",
          under_construction_card("Wage statement violation damages")
        ),
        nav_panel(
          "PAGA",

          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              "PAGA Penalties",
              actionButton("toggle_paga_detail", "Expand Details", class = "btn-sm btn-outline-secondary")
            ),
            card_body(
              under_construction_card("PAGA penalties summary"),

              hr(),

              conditionalPanel(
                condition = "input.toggle_paga_detail % 2 == 1",

                h6("PAGA Breakdown by Violation Type"),
                under_construction_card("Detailed PAGA breakdown")
              )
            )
          )
        )
      )
    ),

    # =======================================================================
    # APPENDIX TAB (New)
    # =======================================================================
    nav_panel(
      title = "Appendix",
      icon = icon("book"),

      navset_card_underline(
        nav_panel(
          "Shift Hours Distribution",
          under_construction_card(
            "Shift Hours Table",
            "Distribution of shift hours from shift_hrs_tbl will appear here."
          )
        ),
        nav_panel(
          "Non-Work Hours",
          under_construction_card(
            "Non-Work Hours Table",
            "Analysis of non-work hours from non_wrk_hrs_tbl will appear here."
          )
        ),
        nav_panel(
          "Meal Period Distribution",
          under_construction_card(
            "Meal Period Table",
            "Meal period distribution from meal_period_tbl will appear here."
          )
        ),
        nav_panel(
          "Meal Start Times",
          under_construction_card(
            "Meal Start Time Analysis",
            "Top meal start times from meal_start_time_tbl will appear here."
          )
        ),
        nav_panel(
          "Meal Quarter Hour",
          under_construction_card(
            "Meal Quarter Hour Analysis",
            "Meal period timing by quarter hour from meal_quarter_hour_tbl will appear here."
          )
        )
      )
    ),

    # =======================================================================
    # FULL REPORT TAB
    # =======================================================================
    nav_panel(
      title = "Full Report",
      icon = icon("file-alt"),

      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Complete Metrics Report",
          actionButton("load_full_report", "Load Full Report", class = "btn-primary btn-sm")
        ),
        card_body(
          DTOutput("full_report_table")
        )
      )
    ),

    nav_spacer(),

    nav_item(
      tags$span(
        style = "color: white; padding: 8px;",
        icon("info-circle"),
        " Analysis Date: ", Sys.Date()
      )
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

server <- function(data_list, metric_spec, case_config_init) {
  function(input, output, session) {

    # Case configuration
    case_config <- reactiveVal(case_config_init)

    # Server-side selectize for employee filter
    all_employee_ids <- sort(unique(c(data_list$shift_data1$ID, data_list$pay1$Pay_ID)))
    updateSelectizeInput(
      session,
      "employee_filter",
      choices = all_employee_ids,
      server = TRUE
    )

    # Current filters
    current_filters <- reactiveVal(list())

    # Apply filters
    observeEvent(input$apply_filters, {
      filters <- list(
        date_min = input$date_range[1],
        date_max = input$date_range[2]
      )

      if (length(input$employee_filter) > 0 && !("" %in% input$employee_filter)) {
        filters$ID <- input$employee_filter
        filters$Pay_ID <- input$employee_filter
      }

      if (!is.null(input$location_filter) && length(input$location_filter) > 0 && !("" %in% input$location_filter)) {
        filters$Location <- input$location_filter
        filters$Pay_Location <- input$location_filter
      }

      current_filters(filters)
    })

    # Reset filters
    observeEvent(input$reset_filters, {
      updateDateRangeInput(session, "date_range",
                           start = min(data_list$shift_data1$Date, na.rm = TRUE),
                           end = max(data_list$shift_data1$Date, na.rm = TRUE)
      )
      updateSelectizeInput(session, "employee_filter", selected = character(0))
      if (!is.null(input$location_filter)) {
        updateSelectizeInput(session, "location_filter", selected = character(0))
      }
      current_filters(list())
    })

    # Filtered data with precomputed metadata
    filtered_data <- reactive({
      filters <- current_filters()

      shift_filtered <- copy(data_list$shift_data1)
      pay_filtered <- copy(data_list$pay1)

      # Apply filters to shift data
      if (!is.null(filters$date_min)) {
        shift_filtered <- shift_filtered[Date >= filters$date_min]
      }
      if (!is.null(filters$date_max)) {
        shift_filtered <- shift_filtered[Date <= filters$date_max]
      }
      if (!is.null(filters$ID)) {
        shift_filtered <- shift_filtered[ID %in% filters$ID]
      }
      if (!is.null(filters$Location) && "Location" %in% names(shift_filtered)) {
        shift_filtered <- shift_filtered[Location %in% filters$Location]
      }

      # Apply filters to pay data
      if (!is.null(filters$date_min)) {
        pay_filtered <- pay_filtered[Pay_Period_End >= filters$date_min]
      }
      if (!is.null(filters$date_max)) {
        pay_filtered <- pay_filtered[Pay_Period_End <= filters$date_max]
      }
      if (!is.null(filters$Pay_ID)) {
        pay_filtered <- pay_filtered[Pay_ID %in% filters$Pay_ID]
      }
      if (!is.null(filters$Pay_Location) && "Pay_Location" %in% names(pay_filtered)) {
        pay_filtered <- pay_filtered[Pay_Location %in% filters$Pay_Location]
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

    output$mediation_date <- renderText({
      config <- case_config()
      if (!is.null(config$mediation_date) && !is.na(config$mediation_date)) {
        as.character(config$mediation_date)
      } else {
        "-"
      }
    })

    output$class_cert_deadline <- renderText({
      config <- case_config()
      if (!is.null(config$class_cert_deadline) && !is.na(config$class_cert_deadline)) {
        as.character(config$class_cert_deadline)
      } else {
        "-"
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

    output$paga_period <- renderText({
      config <- case_config()
      start <- config$paga_period_start
      end <- config$paga_period_end

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

    output$extrap_enabled <- renderText({
      config <- case_config()
      if (!is.null(config$extrapolation_enabled) && config$extrapolation_enabled) {
        "Yes"
      } else {
        "No"
      }
    })

    output$extrap_method <- renderText({
      config <- case_config()
      config$extrapolation_method %||% "None"
    })

    output$extrap_factor <- renderText({
      config <- case_config()
      factor <- config$extrapolation_factor %||% 1.0
      as.character(round(factor, 2))
    })

    output$case_notes <- renderText({
      config <- case_config()
      notes <- config$notes %||% ""
      if (notes == "") "(No notes)" else notes
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

    output$total_shifts <- renderText({
      data <- filtered_data()
      format(nrow(data$shift_data1), big.mark = ",")
    })

    output$total_pay_periods <- renderText({
      data <- filtered_data()
      format(uniqueN(data$pay1$Pay_ID_Period_End), big.mark = ",")
    })

    output$total_weeks <- renderText({
      data <- filtered_data()
      format(uniqueN(data$shift_data1$ID_Week_End), big.mark = ",")
    })

    output$date_coverage_plot <- renderPlotly({
      data <- filtered_data()

      shift_monthly <- data$shift_data1[, .(
        Shifts = .N,
        Employees = uniqueN(ID)
      ), by = .(Month = floor_date(Date, "month"))]

      plot_ly(shift_monthly, x = ~Month) %>%
        add_bars(y = ~Shifts, name = "Shifts", marker = list(color = "#3498db")) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Shift Count"),
          showlegend = FALSE
        )
    })

    output$shift_dist_plot <- renderPlotly({
      data <- filtered_data()

      shift_hours <- data$shift_data1[!is.na(shift_hrs) & shift_hrs > 0, shift_hrs]

      plot_ly(x = shift_hours, type = "histogram",
              marker = list(color = "#2ecc71"),
              nbinsx = 30) %>%
        layout(
          xaxis = list(title = "Shift Hours"),
          yaxis = list(title = "Count")
        )
    })

    # ===========================================================================
    # METRIC TABLE GENERATION
    # ===========================================================================

    metric_groups <- unique(metric_spec$metric_group)

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

    # Generate tables for each metric group
    lapply(metric_groups, function(grp) {
      output_id <- paste0("table_", make.names(grp))

      output[[output_id]] <- renderDT({
        data <- filtered_data()
        factor <- extrap_factor()

        results <- calculate_group_metrics(data, metric_spec, grp, current_filters(), factor)

        if (nrow(results) == 0) {
          return(datatable(data.table(Metric = "No data available"), rownames = FALSE))
        }

        setnames(results, "metric_label", "Metric", skip_absent = TRUE)

        # Hide Extrapolated column if not enabled
        if ("Extrapolated" %in% names(results) && factor <= 1.0) {
          results[, Extrapolated := NULL]
        }

        datatable(
          results,
          options = list(
            pageLength = 25,
            dom = 'frtip',
            scrollX = TRUE,
            columnDefs = list(
              list(className = 'dt-left', targets = 0),
              list(className = 'dt-center', targets = "_all")
            )
          ),
          rownames = FALSE,
          class = 'cell-border stripe hover'
        )
      })
    })

    # Placeholder tables for consolidated sections
    output$table_time_summary <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      time_summary_groups <- metric_groups[grepl("^Time Summary$", metric_groups)]
      if (length(time_summary_groups) > 0) {
        results <- calculate_group_metrics(data, metric_spec, time_summary_groups[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    # Shift/Week/Pay Period Level tables
    output$table_shift_level <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("Shift Level$", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_week_level <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("Week Level$", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_pay_period_level <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("Pay Period Level$", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_time_totals <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("Time Totals$", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    # Punch Rounding tables
    output$table_rounding_by_type <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("Punch Rounding.*Pre_Shift|Mid_Shift|Post", metric_groups)]
      if (length(grp) > 0) {
        all_results <- lapply(grp, function(g) {
          results <- calculate_group_metrics(data, metric_spec, g, current_filters(), factor)
          if (nrow(results) > 0) {
            results[, Section := gsub(".*Analysis - ", "", g)]
            results
          } else {
            NULL
          }
        })
        combined <- rbindlist(Filter(Negate(is.null), all_results), fill = TRUE)
        if (nrow(combined) > 0) {
          setnames(combined, "metric_label", "Metric", skip_absent = TRUE)
          setcolorder(combined, c("Section", names(combined)[names(combined) != "Section"]))
          return(datatable(combined, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_rounding_shift_emp <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("Rounding.*Employee Level|Shift Level", metric_groups)]
      if (length(grp) > 0) {
        all_results <- lapply(grp, function(g) calculate_group_metrics(data, metric_spec, g, current_filters(), factor))
        combined <- rbindlist(all_results, fill = TRUE)
        if (nrow(combined) > 0) {
          setnames(combined, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(combined, options = list(pageLength = 15, scrollX = TRUE, dom = 't'), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_rounding_week_pp <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("Rounding.*Week Level|Pay Period Level", metric_groups)]
      if (length(grp) > 0) {
        all_results <- lapply(grp, function(g) calculate_group_metrics(data, metric_spec, g, current_filters(), factor))
        combined <- rbindlist(all_results, fill = TRUE)
        if (nrow(combined) > 0) {
          setnames(combined, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(combined, options = list(pageLength = 15, scrollX = TRUE, dom = 't'), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_rounding_totals <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("Rounding.*Time Totals", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    # Pay Summary consolidated tables
    output$table_pay_summary <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("^Pay Summary$", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_pay_overtime <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("^Pay Overtime$", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_pay_doubletime <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("^Pay Double Time$", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_pay_meal_premiums <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("^Pay Meal Premiums$", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_pay_rest_premiums <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("^Pay Rest Premiums$", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_pay_bonuses <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("^Pay Bonuses$", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_pay_shift_diff <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("^Pay Shift Differentials$", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    output$table_pay_sick <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()
      grp <- metric_groups[grepl("^Pay Sick Pay$", metric_groups)]
      if (length(grp) > 0) {
        results <- calculate_group_metrics(data, metric_spec, grp[1], current_filters(), factor)
        if (nrow(results) > 0) {
          setnames(results, "metric_label", "Metric", skip_absent = TRUE)
          return(datatable(results, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE))
        }
      }
      datatable(data.table(Metric = "No data available"), rownames = FALSE)
    })

    # ===========================================================================
    # FULL REPORT
    # ===========================================================================

    full_report_data <- reactiveVal(NULL)

    observeEvent(input$load_full_report, {
      showNotification("Loading full report...", type = "message", duration = NULL, id = "loading_report")

      data <- filtered_data()
      factor <- extrap_factor()

      all_results <- lapply(metric_groups, function(grp) {
        results <- calculate_group_metrics(data, metric_spec, grp, current_filters(), factor)
        if (nrow(results) > 0) {
          results[, Group := grp]
          setcolorder(results, c("Group", names(results)[names(results) != "Group"]))
        }
        results
      })

      combined <- rbindlist(all_results, fill = TRUE)
      full_report_data(combined)

      removeNotification("loading_report")
      showNotification("Full report loaded!", type = "message", duration = 3)
    })

    output$full_report_table <- renderDT({
      data <- full_report_data()

      if (is.null(data)) {
        return(NULL)
      }

      if (nrow(data) == 0) {
        return(datatable(data.table(Metric = "No data available"), rownames = FALSE))
      }

      setnames(data, "metric_label", "Metric", skip_absent = TRUE)

      datatable(
        data,
        options = list(
          pageLength = 50,
          dom = 'Bfrtip',
          scrollX = TRUE,
          buttons = c('csv', 'excel'),
          columnDefs = list(
            list(className = 'dt-left', targets = c(0, 1)),
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        filter = 'top',
        class = 'cell-border stripe hover',
        extensions = 'Buttons'
      )
    })

    # ===========================================================================
    # DOWNLOAD HANDLER
    # ===========================================================================

    output$download_report <- downloadHandler(
      filename = function() {
        paste0("wage_hour_report_", Sys.Date(), ".csv")
      },
      content = function(file) {
        showNotification("Generating report...", type = "message", duration = NULL, id = "dl_report")

        data <- filtered_data()
        factor <- extrap_factor()

        all_results <- lapply(metric_groups, function(grp) {
          results <- calculate_group_metrics(data, metric_spec, grp, current_filters(), factor)
          if (nrow(results) > 0) {
            results[, metric_group := grp]
            setcolorder(results, c("metric_group", names(results)[names(results) != "metric_group"]))
          }
          results
        })

        combined <- rbindlist(all_results, fill = TRUE)

        fwrite(combined, file)

        removeNotification("dl_report")
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

message("Starting dashboard...")
shinyApp(
  ui = ui(data_list, metric_spec, case_config),
  server = server(data_list, metric_spec, case_config)
)
