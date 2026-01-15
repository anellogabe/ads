# =============================================================================
# Wage & Hour Compliance Dashboard
# Driven by metrics_spec.csv - Each metric group is a page with filtering
# =============================================================================

library(shiny)
library(bslib)
library(data.table)
library(lubridate)
library(DT)
library(plotly)
library(here)

# For loading spinners (install if needed: install.packages("shinycssloaders"))
if (requireNamespace("shinycssloaders", quietly = TRUE)) {
  library(shinycssloaders)
  use_spinner <- TRUE
} else {
  use_spinner <- FALSE
  withSpinner <- function(x, ...) x  
}

# =============================================================================
# CONFIGURATION - Using here() for project-relative paths
# =============================================================================

DATA_DIR <- here("output")
SCRIPTS_DIR <- here("scripts")

# Data file names (from analysis.R output)
SHIFT_DATA_FILE <- "shift_data1.rds"
PAY_DATA_FILE <- "pay1.rds"
METRIC_SPEC_FILE <- "metrics_spec.csv"

# =============================================================================
# DATA LOADING & METRIC SPEC PARSING
# =============================================================================

# Load processed data
load_data <- function() {
  shift_path <- file.path(DATA_DIR, SHIFT_DATA_FILE)
  pay_path <- file.path(DATA_DIR, PAY_DATA_FILE)
  
  # Check if files exist and provide helpful error messages
  if (!file.exists(shift_path)) {
    stop(paste0(
      "\n\nCannot find shift data file:\n  ", normalizePath(shift_path, mustWork = FALSE),
      "\n\nMake sure you've run analysis.R and saved shift_data1.rds to output/",
      "\nAdd this to end of analysis.R: saveRDS(shift_data1, here('output', 'shift_data1.rds'))"
    ))
  }
  if (!file.exists(pay_path)) {
    stop(paste0(
      "\n\nCannot find pay data file:\n  ", normalizePath(pay_path, mustWork = FALSE),
      "\n\nMake sure you've run analysis.R and saved pay1.rds to output/",
      "\nAdd this to end of analysis.R: saveRDS(pay1, here('output', 'pay1.rds'))"
    ))
  }
  
  message("Loading shift data from: ", normalizePath(shift_path))
  message("Loading pay data from: ", normalizePath(pay_path))
  
  list(
    shift_data1 = readRDS(shift_path),
    pay1 = readRDS(pay_path)
  )
}

# Load and parse metric spec
load_metric_spec <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(SCRIPTS_DIR, METRIC_SPEC_FILE)
  }
  
  if (!file.exists(path)) {
    stop(paste0(
      "\n\nCannot find metrics spec file:\n  ", normalizePath(path, mustWork = FALSE),
      "\n\nPlease update SCRIPTS_DIR at the top of app.R to point to your scripts folder.",
      "\nCurrent SCRIPTS_DIR: ", SCRIPTS_DIR
    ))
  }
  
  message("Loading metric spec from: ", normalizePath(path))
  
  spec <- fread(path)
  spec[, metric_order := .I]
  spec[, metric_type := fcase(
    grepl("date", metric_label, ignore.case = TRUE), "date",
    grepl("percent", metric_label, ignore.case = TRUE), "percent",
    grepl("\\$", metric_label), "currency",
    default = "value"
  )]
  spec
}

# =============================================================================
# DENOMINATOR DEFINITIONS
# =============================================================================

get_denominator_value <- function(data, denom_name, source_type) {
  if (is.na(denom_name) || denom_name == "") return(NA_real_)
  
  result <- tryCatch({
    switch(denom_name,
           # Shift-based denominators
           "shifts_all" = data[, .N],
           "shifts_gt_3_5" = data[shift_hrs > 3.5, .N],
           "shifts_gt_5" = data[shift_hrs > 5, .N],
           "shifts_gt_6" = data[shift_hrs > 6, .N],
           "shifts_gt_10" = data[shift_hrs > 10, .N],
           "shifts_gt_12" = data[shift_hrs > 12, .N],
           
           # Late meal denominators
           "shifts_gt_5_late_meals" = data[shift_hrs > 5 & LateMP1 == 1, .N],
           "shifts_gt_6_late_meals" = data[shift_hrs > 6 & LateMP1_w == 1, .N],
           
           # Short meal denominators
           "shifts_gt_5_short_meals" = data[shift_hrs > 5 & ShortMP1 == 1, .N],
           "shifts_gt_6_short_meals" = data[shift_hrs > 6 & ShortMP1_w == 1, .N],
           
           # Meal period denominators
           "meal_periods" = data[, sum(shift_mps, na.rm = TRUE)],
           "auto_meal_periods" = data[, sum(!is.na(auto_mp), na.rm = TRUE)],
           
           # Rest period denominators
           "rest_periods" = data[, sum(shift_rps, na.rm = TRUE)],
           
           # Employee denominators (time data)
           "employees" = data[, uniqueN(ID)],
           
           # Week denominators
           "weeks" = data[, uniqueN(ID_Week_End)],
           
           # Pay period denominators (time data)
           "pay_periods" = data[, uniqueN(ID_Period_End)],
           
           # Analyzed shifts denominators (rounding analysis)
           "analyzed_shifts_round" = data[, sum(shifts_analyzed, na.rm = TRUE)],
           "r_analyzed_shifts_round" = data[, sum(r_shifts_analyzed, na.rm = TRUE)],
           
           # Pay-based denominators
           "employees_pay" = data[, uniqueN(Pay_ID)],
           "pay_periods_pay" = data[, uniqueN(Pay_ID_Period_End)],
           
           # Default
           NA_real_
    )
  }, error = function(e) NA_real_)
  
  return(result)
}

# =============================================================================
# METRIC EVALUATION ENGINE
# =============================================================================

# Transform dplyr expressions to data.table equivalents
transform_expr <- function(expr_str) {
  if (is.na(expr_str) || expr_str == "") return(expr_str)
  
  # Replace n_distinct() with uniqueN()
  expr_str <- gsub("n_distinct\\(", "uniqueN(", expr_str)
  
  # Replace n() with .N (for counting rows)
  expr_str <- gsub("\\bn\\(\\)", ".N", expr_str)
  
  expr_str
}

evaluate_metric <- function(data, expr_str, digits = NA) {
  if (is.na(expr_str) || expr_str == "") return(NA)
  if (nrow(data) == 0) return(NA)
  
  # Transform dplyr syntax to data.table
  
  expr_str <- transform_expr(expr_str)
  
  result <- tryCatch({
    # Parse and evaluate expression in data.table context
    expr <- parse(text = expr_str)
    val <- data[, eval(expr)]
    
    # Apply rounding if specified
    if (!is.na(digits) && is.numeric(val)) {
      val <- round(val, digits)
    }
    val
  }, error = function(e) {
    # Uncomment for debugging:
    # message("Error evaluating: ", expr_str, " - ", e$message)
    NA
  })
  
  return(result)
}

# Calculate metrics for a single data subset
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

# Calculate all metrics for a given group with All Data, Years, and Key_Gps columns
calculate_group_metrics <- function(data_list, spec, group_name, filters = list()) {
  group_spec <- spec[metric_group == group_name]
  if (nrow(group_spec) == 0) return(data.table())
  
  # Determine source type for this group
  first_source <- group_spec$source[1]
  is_pay_group <- grepl("pay", first_source, ignore.case = TRUE)
  
  # Get precomputed years and key_groups from data_list
  if (is_pay_group) {
    years <- data_list$pay_years
    key_groups <- data_list$pay_key_groups
  } else {
    years <- data_list$shift_years
    key_groups <- data_list$shift_key_groups
  }
  
  # Build results for each metric
  results <- lapply(seq_len(nrow(group_spec)), function(i) {
    row <- group_spec[i]
    source <- row$source
    
    # Get appropriate data source for this metric (already filtered)
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
    
    # Calculate All Data
    all_data_result <- calculate_single_metric(src_data, row$expr, row$digits, row$denom)
    
    result_row <- data.table(
      metric_label = row$metric_label,
      All_Data = format_metric_value(all_data_result$value, all_data_result$pct)
    )
    
    # Calculate for each year (use precomputed years)
    if (!is.null(src_years) && length(src_years) > 0 && src_date_col %in% names(src_data)) {
      for (yr in src_years) {
        yr_data <- src_data[year(get(src_date_col)) == yr]
        yr_result <- calculate_single_metric(yr_data, row$expr, row$digits, row$denom)
        result_row[, (as.character(yr)) := format_metric_value(yr_result$value, yr_result$pct)]
      }
    }
    
    # Calculate for each Key_Gps (use precomputed key_groups)
    if (!is.null(src_key_groups) && length(src_key_groups) > 0 && src_key_col %in% names(src_data)) {
      for (gp in src_key_groups) {
        gp_data <- src_data[get(src_key_col) == gp]
        gp_result <- calculate_single_metric(gp_data, row$expr, row$digits, row$denom)
        result_row[, (gp) := format_metric_value(gp_result$value, gp_result$pct)]
      }
    }
    
    result_row
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

  # Filter data rows based on label content
  if (include_waivers) {
    # WAIVERS TAB: Include metrics with "(waivers)" in label, exclude "(no waivers)"
    keep <- is_header |
           (grepl("\\(waivers\\)", metrics_table$Metric, ignore.case = TRUE) &
            !grepl("\\(no\\s+waivers\\)", metrics_table$Metric, ignore.case = TRUE))
  } else {
    # NO WAIVERS TAB: Include metrics with "(no waivers)" in label OR no waiver designation, exclude "(waivers)"
    keep <- is_header |
           grepl("\\(no\\s+waivers\\)", metrics_table$Metric, ignore.case = TRUE) |
           (!grepl("\\(waivers\\)", metrics_table$Metric, ignore.case = TRUE) &
            !grepl("\\(no\\s+waivers\\)", metrics_table$Metric, ignore.case = TRUE))
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

# Format metric value for display
format_metric_value <- function(val, pct = NA) {
  if (is.null(val) || length(val) == 0 || (is.atomic(val) && all(is.na(val)))) {
    return("-")
  }
  
  if (inherits(val, "Date")) {
    return(as.character(val))
  }
  
  if (is.numeric(val)) {
    # Format number
    if (abs(val) >= 1000) {
      formatted <- format(round(val, 2), big.mark = ",", nsmall = if (val %% 1 == 0) 0 else 2)
    } else {
      formatted <- as.character(round(val, 2))
    }
    
    # Add percentage if available
    if (!is.na(pct)) {
      formatted <- paste0(formatted, " (", pct, "%)")
    }
    return(formatted)
  }
  
  as.character(val)
}

# =============================================================================
# UI COMPONENTS
# =============================================================================

# Create filter sidebar
filter_sidebar <- function(data_list) {
  # Get unique values for filters
  shift_data <- data_list$shift_data1
  pay_data <- data_list$pay1
  
  # Date range
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
  
  # Locations (if available)
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
    
    # Server-side selectize - choices populated in server
    selectizeInput(
      "employee_filter",
      "Employee ID(s)",
      choices = NULL,  # Will be updated server-side
      multiple = TRUE,
      options = list(placeholder = "Type to search employees...")
    ),
    
    # Location filter if available
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
    
    actionButton("apply_filters", "Apply Filters", class = "btn-primary w-100"),
    actionButton("reset_filters", "Reset Filters", class = "btn-outline-secondary w-100 mt-2"),
    
    hr(),
    
    downloadButton("download_report", "Download Report", class = "w-100")
  )
}

# Create metrics table card
metrics_table_card <- function(id, title) {
  card(
    card_header(title),
    card_body(
      DTOutput(paste0("table_", id))
    )
  )
}

# =============================================================================
# SHINY APP
# =============================================================================

ui <- function(data_list, metric_spec) {
  
  # Get unique metric groups for tabs
  metric_groups <- unique(metric_spec$metric_group)
  
  # Categorize groups into logical sections
  time_summary <- metric_groups[grepl("^Time Summary|^Time Meal Period Analysis|^Time Rest Period Analysis", metric_groups)]
  time_meal_violations <- metric_groups[grepl("^Time Meal Violations", metric_groups)]
  time_rest_violations <- metric_groups[grepl("^Time Rest Violations", metric_groups)]
  time_shift_hours <- metric_groups[grepl("^Time Shift Hours", metric_groups)]
  time_rounding <- metric_groups[grepl("^Time Punch Rounding", metric_groups)]
  
  pay_summary <- metric_groups[grepl("^Pay Summary|^Pay Overtime|^Pay Double Time", metric_groups)]
  pay_premiums <- metric_groups[grepl("^Pay Meal Premiums|^Pay Rest Premiums|^Pay Sick Pay", metric_groups)]
  pay_bonuses <- metric_groups[grepl("^Pay Bonuses|^Pay Shift Diff", metric_groups)]
  pay_rrop <- metric_groups[grepl("^Pay Regular Rate", metric_groups)]
  
  # Helper function to create nav_menu only if groups exist
  create_nav_menu <- function(title, icon_name, groups, label_fn) {
    if (length(groups) == 0) return(NULL)
    nav_menu(
      title = title,
      icon = icon(icon_name),
      !!!lapply(groups, function(grp) {
        nav_panel(
          title = label_fn(grp),
          card(
            card_header(grp),
            card_body(
              withSpinner(DTOutput(paste0("table_", make.names(grp))), type = 6, color = "#2c3e50")
            )
          )
        )
      })
    )
  }
  
  # Build navigation items, filtering out NULLs
  nav_items <- Filter(Negate(is.null), list(
    # Overview Tab
    nav_panel(
      title = "Overview",
      icon = icon("dashboard"),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        
        value_box(
          title = "Total Employees",
          value = textOutput("total_employees"),
          showcase = icon("users"),
          theme = "primary"
        ),
        value_box(
          title = "Total Shifts",
          value = textOutput("total_shifts"),
          showcase = icon("clock"),
          theme = "info"
        ),
        value_box(
          title = "Pay Periods",
          value = textOutput("total_pay_periods"),
          showcase = icon("calendar"),
          theme = "success"
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Date Range Coverage"),
          card_body(
            plotlyOutput("date_coverage_plot", height = "300px")
          )
        ),
        card(
          card_header("Shift Distribution"),
          card_body(
            plotlyOutput("shift_dist_plot", height = "300px")
          )
        )
      )
    ),
    
    create_nav_menu("Time Summary", "clock", time_summary, function(g) gsub("^Time ", "", g)),
    create_nav_menu("Meal Violations", "utensils", time_meal_violations, function(g) gsub("^Time Meal Violations ", "", g)),
    create_nav_menu("Rest Violations", "pause-circle", time_rest_violations, function(g) gsub("^Time ", "", g)),
    create_nav_menu("Shift Hours", "chart-bar", time_shift_hours, function(g) gsub("^Time Shift Hours Analysis ", "", gsub("^Time Shift Hours Analysis$", "Overview", g))),
    create_nav_menu("Punch Rounding", "sync", time_rounding, function(g) gsub("^Time Punch Rounding Analysis ", "", gsub("^Time Punch Rounding Analysis$", "Overview", g))),
    create_nav_menu("Pay Summary", "dollar-sign", pay_summary, function(g) gsub("^Pay ", "", g)),
    create_nav_menu("Pay Premiums", "coins", pay_premiums, function(g) gsub("^Pay ", "", g)),
    create_nav_menu("Bonuses & Diffs", "gift", pay_bonuses, function(g) gsub("^Pay ", "", g)),
    create_nav_menu("Regular Rate", "calculator", pay_rrop, function(g) gsub("^Pay Regular Rate - ", "", g)),
    
    # Full Report Tab
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
    )
  ))
  
  # Build the page
  do.call(
    page_navbar,
    c(
      list(
        title = "Wage & Hour Compliance Dashboard",
        theme = bs_theme(
          version = 5,
          bootswatch = "flatly",
          primary = "#2c3e50",
          "navbar-bg" = "#2c3e50"
        ),
        sidebar = filter_sidebar(data_list)
      ),
      nav_items
    )
  )
}

server <- function(data_list, metric_spec) {
  function(input, output, session) {
    
    # Server-side selectize for employee filter
    all_employee_ids <- sort(unique(c(data_list$shift_data1$ID, data_list$pay1$Pay_ID)))
    updateSelectizeInput(
      session, 
      "employee_filter",
      choices = all_employee_ids,
      server = TRUE
    )
    
    # Reactive: Current filters
    current_filters <- reactiveVal(list())
    
    # Cache for calculated metrics (invalidates when filters change)
    metrics_cache <- reactiveValues()
    
    # Clear cache when filters change
    observeEvent(current_filters(), {
      metrics_cache$data <- list()
    }, ignoreInit = TRUE)
    
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
    
    # Reactive: Filtered data with precomputed metadata
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
      
      # Precompute years from shift data
      shift_years <- if (nrow(shift_filtered) > 0 && "Date" %in% names(shift_filtered)) {
        sort(unique(year(shift_filtered$Date)))
      } else NULL
      
      # Precompute years from pay data
      pay_years <- if (nrow(pay_filtered) > 0 && "Pay_Period_End" %in% names(pay_filtered)) {
        sort(unique(year(pay_filtered$Pay_Period_End)))
      } else NULL
      
      # Precompute Key_Gps from shift data
      shift_key_groups <- if ("Key_Gps" %in% names(shift_filtered)) {
        gps <- unique(shift_filtered$Key_Gps)
        gps <- gps[!is.na(gps) & gps != "" & tolower(gps) != "everyone else"]
        sort(gps)
      } else NULL
      
      # Precompute Pay_Key_Gps from pay data
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
    
    # Overview value boxes
    output$total_employees <- renderText({
      data <- filtered_data()
      format(uniqueN(c(data$shift_data1$ID, data$pay1$Pay_ID)), big.mark = ",")
    })
    
    output$total_shifts <- renderText({
      data <- filtered_data()
      format(nrow(data$shift_data1), big.mark = ",")
    })
    
    output$total_pay_periods <- renderText({
      data <- filtered_data()
      format(uniqueN(data$pay1$Pay_ID_Period_End), big.mark = ",")
    })
    
    # Overview plots
    output$date_coverage_plot <- renderPlotly({
      data <- filtered_data()
      
      # Aggregate shifts by month
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
      
      # Shift hour distribution
      shift_hours <- data$shift_data1[!is.na(shift_hrs) & shift_hrs > 0, shift_hrs]
      
      plot_ly(x = shift_hours, type = "histogram", 
              marker = list(color = "#2ecc71"),
              nbinsx = 30) %>%
        layout(
          xaxis = list(title = "Shift Hours"),
          yaxis = list(title = "Count")
        )
    })
    
    # Generate metric tables for each group
    metric_groups <- unique(metric_spec$metric_group)
    
    lapply(metric_groups, function(grp) {
      output_id <- paste0("table_", make.names(grp))
      
      output[[output_id]] <- renderDT({
        data <- filtered_data()
        
        results <- calculate_group_metrics(data, metric_spec, grp, current_filters())
        
        if (nrow(results) == 0) {
          return(datatable(data.table(Metric = "No data available"), rownames = FALSE))
        }
        
        # Rename first column for display
        setnames(results, "metric_label", "Metric")
        
        datatable(
          results,
          options = list(
            pageLength = 25,
            dom = 'frtip',
            scrollX = TRUE,
            columnDefs = list(
              list(className = 'dt-left', targets = 0),
              list(className = 'dt-center', targets = seq_len(ncol(results) - 1))
            )
          ),
          rownames = FALSE,
          class = 'cell-border stripe hover'
        )
      })
    })
    
    # Full report table - lazy load with button
    full_report_data <- reactiveVal(NULL)
    
    observeEvent(input$load_full_report, {
      showNotification("Loading full report...", type = "message", duration = NULL, id = "loading_report")
      
      data <- filtered_data()
      
      all_results <- lapply(metric_groups, function(grp) {
        results <- calculate_group_metrics(data, metric_spec, grp, current_filters())
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
          ),
          deferRender = TRUE,
          scroller = TRUE
        ),
        rownames = FALSE,
        filter = 'top',
        class = 'cell-border stripe hover',
        extensions = c('Buttons', 'Scroller')
      )
    })
    
    # Download handler
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("wage_hour_report_", Sys.Date(), ".csv")
      },
      content = function(file) {
        showNotification("Generating report...", type = "message", duration = NULL, id = "dl_report")
        
        data <- filtered_data()
        
        all_results <- lapply(metric_groups, function(grp) {
          results <- calculate_group_metrics(data, metric_spec, grp, current_filters())
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

      # Two-source overlaps
      if (venn$time_pay > 0 && all(c("time", "pay") %in% sources)) {
        categories <- c(categories, "Time & Pay")
        counts <- c(counts, venn$time_pay)
        colors <- c(colors, "#3498db")
      }
      if (venn$time_class > 0 && all(c("time", "class") %in% sources) && venn$class_total > 0) {
        categories <- c(categories, "Time & Class")
        counts <- c(counts, venn$time_class)
        colors <- c(colors, "#9b59b6")
      }
      if (venn$pay_class > 0 && all(c("pay", "class") %in% sources) && venn$class_total > 0) {
        categories <- c(categories, "Pay & Class")
        counts <- c(counts, venn$pay_class)
        colors <- c(colors, "#e67e22")
      }

      # Single-source only
      if (venn$time_only > 0 && "time" %in% sources) {
        categories <- c(categories, "Time Only")
        counts <- c(counts, venn$time_only)
        colors <- c(colors, "#2c3e50")
      }
      if (venn$pay_only > 0 && "pay" %in% sources) {
        categories <- c(categories, "Pay Only")
        counts <- c(counts, venn$pay_only)
        colors <- c(colors, "#27ae60")
      }
      if (venn$class_only > 0 && "class" %in% sources && venn$class_total > 0) {
        categories <- c(categories, "Class Only")
        counts <- c(counts, venn$class_only)
        colors <- c(colors, "#c0392b")
      }

      # Create vertical bar chart
      plot_ly(
        x = categories,
        y = counts,
        type = "bar",
        marker = list(color = colors),
        text = paste0(format(counts, big.mark = ","), "<br>employees"),
        textposition = "outside",
        textfont = list(color = "#2c3e50", size = 14, weight = "bold"),
        hovertemplate = paste0(
          "<b>%{x}</b><br>",
          "Count: %{y:,} employees<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title = list(
            text = "Employee Data Overlap Analysis",
            font = list(size = 18, weight = "bold")
          ),
          xaxis = list(
            title = "",
            showgrid = FALSE,
            tickangle = -45,
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = "Number of Employees",
            showgrid = TRUE,
            gridcolor = "#ecf0f1"
          ),
          showlegend = FALSE,
          hovermode = "closest",
          margin = list(l = 80, r = 50, t = 80, b = 120),
          plot_bgcolor = "#ffffff",
          paper_bgcolor = "#ffffff"
        )
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
      1.0
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

    # Meal 5hr (no waivers) - Summary
    output$table_meal_5hr_consolidated <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_5_summary, current_filters(), factor)

      create_dt_table(results)
    })

    # Meal 5hr (no waivers) - Short Details
    output$table_meal_5hr_short_details <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_5_short, current_filters(), factor)

      create_dt_table(results)
    })

    # Meal 5hr (no waivers) - Late Details
    output$table_meal_5hr_late_details <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_5_late, current_filters(), factor)

      create_dt_table(results)
    })

    # Meal 6hr (waivers) - Summary
    output$table_meal_6hr_consolidated <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_6_summary, current_filters(), factor)

      create_dt_table(results)
    })

    # Meal 6hr (waivers) - Short Details
    output$table_meal_6hr_short_details <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_6_short, current_filters(), factor)

      create_dt_table(results)
    })

    # Meal 6hr (waivers) - Late Details
    output$table_meal_6hr_late_details <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      results <- calculate_group_metrics(data, metric_spec, time_meal_violations_6_late, current_filters(), factor)

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
    # CONSOLIDATED DAMAGES TABLES
    # ===========================================================================

    # Class/Individual Claims - No Waivers
    output$table_damages_class_no_waivers <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      # Split each metric group by waiver status
      meal_split <- split_by_waiver(damages_meal_groups)
      rest_split <- split_by_waiver(damages_rest_groups)
      rrop_split <- split_by_waiver(damages_rrop_groups)
      otc_split <- split_by_waiver(damages_otc_groups)
      rounding_split <- split_by_waiver(damages_rounding_groups)
      unpaid_ot_split <- split_by_waiver(damages_unpaid_ot_groups)
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

      if (length(rounding_split$no_waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "CLOCK ROUNDING DAMAGES",
          groups = rounding_split$no_waiver
        )
      }

      if (length(unpaid_ot_split$no_waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "UNPAID OT/DT DAMAGES",
          groups = unpaid_ot_split$no_waiver
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

      if (length(total_split$no_waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "TOTAL DAMAGES",
          groups = total_split$no_waiver
        )
      }

      results <- combine_damages_with_headers(data, metric_spec, sections, current_filters(), factor)
      # Filter out waiver metrics from no-waiver tab based on metric labels
      results <- filter_metrics_by_label(results, include_waivers = FALSE)
      create_dt_table(results)
    })

    # Class/Individual Claims - Waivers
    output$table_damages_class_waivers <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

      # Split each metric group by waiver status
      meal_split <- split_by_waiver(damages_meal_groups)
      rest_split <- split_by_waiver(damages_rest_groups)
      rrop_split <- split_by_waiver(damages_rrop_groups)
      otc_split <- split_by_waiver(damages_otc_groups)
      rounding_split <- split_by_waiver(damages_rounding_groups)
      unpaid_ot_split <- split_by_waiver(damages_unpaid_ot_groups)
      expenses_split <- split_by_waiver(damages_expenses_groups)
      wsv_split <- split_by_waiver(damages_wsv_groups)
      wt_split <- split_by_waiver(damages_wt_groups)
      total_split <- split_by_waiver(damages_class_total_groups)

      # Build section definitions for waiver metrics
      sections <- list()

      if (length(meal_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "MEAL PERIOD DAMAGES",
          groups = meal_split$waiver
        )
      }

      if (length(rest_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "REST PERIOD DAMAGES",
          groups = rest_split$waiver
        )
      }

      if (length(rrop_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "RROP DAMAGES",
          groups = rrop_split$waiver
        )
      }

      if (length(otc_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "OFF-THE-CLOCK DAMAGES",
          groups = otc_split$waiver
        )
      }

      if (length(rounding_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "CLOCK ROUNDING DAMAGES",
          groups = rounding_split$waiver
        )
      }

      if (length(unpaid_ot_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "UNPAID OT/DT DAMAGES",
          groups = unpaid_ot_split$waiver
        )
      }

      if (length(expenses_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "UNREIMBURSED EXPENSES DAMAGES",
          groups = expenses_split$waiver
        )
      }

      if (length(wsv_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "WAGE STATEMENT PENALTIES",
          groups = wsv_split$waiver
        )
      }

      if (length(wt_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "WAITING TIME PENALTIES",
          groups = wt_split$waiver
        )
      }

      if (length(total_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "TOTAL DAMAGES",
          groups = total_split$waiver
        )
      }

      results <- combine_damages_with_headers(data, metric_spec, sections, current_filters(), factor)
      # Filter out no-waiver metrics from waiver tab based on metric labels
      results <- filter_metrics_by_label(results, include_waivers = TRUE)
      create_dt_table(results)
    })

    # PAGA - No Waivers
    output$table_paga_no_waivers <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

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
      total_split <- split_by_waiver(paga_total_groups)

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

      if (length(total_split$no_waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "PAGA - TOTAL",
          groups = total_split$no_waiver
        )
      }

      results <- combine_damages_with_headers(data, metric_spec, sections, current_filters(), factor)
      # Filter out waiver metrics from no-waiver tab based on metric labels
      results <- filter_metrics_by_label(results, include_waivers = FALSE)
      create_dt_table(results)
    })

    # PAGA - Waivers
    output$table_paga_waivers <- renderDT({
      data <- filtered_data()
      factor <- extrap_factor()

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
      total_split <- split_by_waiver(paga_total_groups)

      # Build section definitions for waiver metrics
      sections <- list()

      if (length(meal_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "PAGA - MEAL PERIODS",
          groups = meal_split$waiver
        )
      }

      if (length(rest_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "PAGA - REST PERIODS",
          groups = rest_split$waiver
        )
      }

      if (length(rrop_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "PAGA - REGULAR RATE (RROP)",
          groups = rrop_split$waiver
        )
      }

      if (length(s226_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "PAGA - WAGE STATEMENT (226)",
          groups = s226_split$waiver
        )
      }

      if (length(s558_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "PAGA - UNPAID WAGES (558)",
          groups = s558_split$waiver
        )
      }

      if (length(min_wage_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "PAGA - MIN WAGE (1197.1)",
          groups = min_wage_split$waiver
        )
      }

      if (length(expenses_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "PAGA - UNREIMBURSED EXPENSES (2802)",
          groups = expenses_split$waiver
        )
      }

      if (length(recordkeeping_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "PAGA - RECORDKEEPING (1174)",
          groups = recordkeeping_split$waiver
        )
      }

      if (length(waiting_time_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "PAGA - WAITING TIME (203)",
          groups = waiting_time_split$waiver
        )
      }

      if (length(total_split$waiver) > 0) {
        sections[[length(sections) + 1]] <- list(
          section_name = "PAGA - TOTAL",
          groups = total_split$waiver
        )
      }

      results <- combine_damages_with_headers(data, metric_spec, sections, current_filters(), factor)
      # Filter out no-waiver metrics from waiver tab based on metric labels
      results <- filter_metrics_by_label(results, include_waivers = TRUE)
      create_dt_table(results)
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
          if (is.numeric(val)) return(format(round(val, 2), big.mark = ","))
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
      if (exists("complaint_date")) {
        # Format as "complaint_date to present"
        if (inherits(complaint_date, "Date")) {
          formatted_date <- format(complaint_date, "%B %d, %Y")
          return(paste0(formatted_date, " to present"))
        }
        return(paste0(as.character(complaint_date), " to present"))
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

      if (is.null(data_list$time1) || !"ID_Period_End" %in% names(data_list$time1)) {
        return(datatable(data.table(Message = "No time1 data available"), rownames = FALSE, options = list(dom = 't')))
      }

      # Filter to selected period
      filtered <- data_list$time1[ID_Period_End == input$example_period_select]

      if (nrow(filtered) == 0) {
        return(datatable(data.table(Message = "No punch records for this period"), rownames = FALSE, options = list(dom = 't')))
      }

      # Select punch detail columns: ID, Name, Date, punch_time, punch_type
      punch_cols <- c("ID", "Name", "Date", "punch_time", "punch_type")
      available_cols <- punch_cols[punch_cols %in% names(filtered)]

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
        class = 'cell-border stripe hover compact',
        style = 'bootstrap4'
      )
    })

    # Shift Data (shift_data1) - Show all meal/rest violation columns horizontally
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

      # Key columns for shift data
      priority_cols <- c("ID", "Name", "Date", "shift_hrs",
                        "MissMP1", "LateMP1", "ShortMP1", "MissMP2", "LateMP2", "ShortMP2",
                        "MissMP1_w", "LateMP1_w", "ShortMP1_w", "MissMP2_w", "LateMP2_w", "ShortMP2_w",
                        "mpv_shift", "mpv_shift_w", "wk_shift_hrs", "wk_Hours",
                        "mpv_per_pp", "mpv_per_pp_w", "rpv_per_pp",
                        "pp_shift_hrs", "pp_Hours",
                        "MissRP1", "LateRP1", "ShortRP1", "MissRP2", "LateRP2", "ShortRP2",
                        "rpv_shift", "Source", "Page", "Sheet")

      # Get available columns in priority order
      available_cols <- priority_cols[priority_cols %in% names(filtered)]

      # Add any remaining columns not in priority list
      remaining_cols <- setdiff(names(filtered), c(available_cols, "ID_Period_End", "ID_Week_End", "Period_End"))
      final_cols <- c(available_cols, remaining_cols)

      display_data <- filtered[, ..final_cols]

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
        class = 'cell-border stripe hover compact',
        style = 'bootstrap4'
      )
    })

    # Pay Data (pay1) - Show all pay columns horizontally
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

      # Key pay columns based on the green image
      priority_cols <- c("Pay_ID", "Pay_Name", "Pay_Date", "Pay_Period_End", "Pay_Code", "Pay_Hours", "Pay_Amount",
                        "Base_Rate1", "Base_Rate2", "RROP", "Calc_Rate", "Rate_Gp",
                        "Hrs_Wkd_Pay_Code", "Reg_Pay_Code", "OT_Pay_Code", "DT_Pay_Code",
                        "Meal_Pay_Code", "Rest_Pay_Code", "Sick_Pay_Code", "RROP_Pay_Code",
                        "pp_Hrs_Wkd", "pp_Reg_Hrs", "pp_OT_Hrs", "pp_DT_Hrs",
                        "pp_Straight_Time_Amt", "pp_OT_Amt", "pp_DT_Amt", "pp_Oth_RROP_Amt", "pp_Oth_Amt",
                        "Actual_Wages", "Calc_Tot_Wages",
                        "OT_Overpayment", "DT_Overpayment", "Meal_Overpayment", "Rest_Overpayment",
                        "Sick_Overpayment", "Gross_Overpayment", "Net_Overpayment",
                        "OT_rrop_dmgs", "DT_rrop_dmgs", "Meal_rrop_dmgs", "Rest_rrop_dmgs",
                        "Sick_rrop_dmgs", "Gross_rrop_dmgs", "Net_rrop_dmgs",
                        "Pay_Source")

      # Get available columns
      available_cols <- priority_cols[priority_cols %in% names(filtered)]

      # Add remaining columns
      remaining_cols <- setdiff(names(filtered), c(available_cols, "Pay_ID_Period_End"))
      final_cols <- c(available_cols, remaining_cols)

      display_data <- filtered[, ..final_cols]

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
        class = 'cell-border stripe hover compact',
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
        class = 'cell-border stripe hover compact',
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
        withProgress(message = 'Generating PDF Report', value = 0, {

          # Calculate total steps for progress tracking
          # Combine all four column selections
          sections <- c(input$pdf_sections_col1, input$pdf_sections_col2, input$pdf_sections_col3, input$pdf_sections_col4)
          total_sections <- length(sections) + 2  # +2 for setup and finalization
          current_step <- 0

          incProgress(1/total_sections, detail = "Initializing...")
          current_step <- current_step + 1

          data <- filtered_data()

        # Case name for PDF
        case_name <- "Wage & Hour Analysis"

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
    <p><strong>Report Generated:</strong> ', format(Sys.Date(), "%B %d, %Y"), '</p>
  </div>
')
        }

        # Overview Statistics
        if ("overview" %in% sections) {
          incProgress(1/total_sections, detail = "Overview Statistics")
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
      <div class="stat-label">Pay Periods (Time)</div>
      <div class="stat-value">', format(uniqueN(data$shift_data1$ID_Period_End), big.mark = ","), '</div>
    </div>
    <div class="stat-box">
      <div class="stat-label">Pay Periods (Pay)</div>
      <div class="stat-value">', format(uniqueN(data$pay1$Pay_ID_Period_End), big.mark = ","), '</div>
    </div>
    <div class="stat-box">
      <div class="stat-label">Weeks (Time)</div>
      <div class="stat-value">', format(uniqueN(data$shift_data1$ID_Week_End), big.mark = ","), '</div>
    </div>
  </div>
')
        }

        # Helper function to add table HTML
        add_table <- function(dt_table, title, icon = "📊") {
          if (nrow(dt_table) == 0) return("")

          # Update progress
          incProgress(1/total_sections, detail = title)

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
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          results <- calculate_group_metrics(data, metric_spec, time_summary_groups, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Time Analysis - Summary", "⏰"))
        }

        # Time Analysis - Shift Hours Analysis
        if ("time_shift_hours" %in% sections && length(time_shift_groups) > 0) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          results <- calculate_group_metrics(data, metric_spec, time_shift_groups, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Time Analysis - Shift Hours Analysis", "📊"))
        }

        # Time Analysis - Punch Rounding
        if ("time_rounding" %in% sections && length(time_rounding_groups) > 0) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          results <- calculate_group_metrics(data, metric_spec, time_rounding_groups, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Time Analysis - Punch Rounding", "🔄"))
        }

        # Meal Analysis
        if ("meal_analysis" %in% sections && length(time_meal_analysis) > 0) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          results <- calculate_group_metrics(data, metric_spec, time_meal_analysis, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Meal Period Analysis", "🍽️"))
        }

        # Meal Violations (no waivers)
        if ("meal_5hr" %in% sections && length(time_meal_violations_5_summary) > 0) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          results <- calculate_group_metrics(data, metric_spec, time_meal_violations_5_summary, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Meal Violations (no waivers)", "⚠️"))
        }

        # Meal Violations (waivers)
        if ("meal_6hr" %in% sections && length(time_meal_violations_6_summary) > 0) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          results <- calculate_group_metrics(data, metric_spec, time_meal_violations_6_summary, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Meal Violations (waivers)", "⚠️"))
        }

        # Rest Periods
        if ("rest_periods" %in% sections && length(time_rest) > 0) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          results <- calculate_group_metrics(data, metric_spec, time_rest, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Rest Periods", "☕"))
        }

        # Pay Analysis - Summary
        if ("pay_summary" %in% sections && length(pay_summary_groups) > 0) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          results <- calculate_group_metrics(data, metric_spec, pay_summary_groups, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Pay Analysis - Summary", "💰"))
        }

        # Pay Analysis - Regular Rate
        if ("pay_regular_rate" %in% sections && length(pay_regular_rate) > 0) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          results <- calculate_group_metrics(data, metric_spec, pay_regular_rate, current_filters(), extrap_factor())
          html_content <- paste0(html_content, add_table(results, "Pay Analysis - Regular Rate", "💵"))
        }

        # Pay Analysis - Pay Codes
        if ("pay_codes" %in% sections && !is.null(analysis_tables$pay_code_summary)) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(analysis_tables$pay_code_summary, "Pay Analysis - Pay Codes", "💳"))
        }

        # Pay Analysis - Rate Type Analysis
        if ("rate_type_analysis" %in% sections && !is.null(analysis_tables$rate_type_analysis)) {
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(analysis_tables$rate_type_analysis, "Pay Analysis - Rate Type Analysis", "📊"))
        }

        # Damages - Class/Individual Claims - No Waivers
        if ("damages_class_no_waivers" %in% sections) {
          incProgress(1/total_sections, detail = "Damages - Class/Individual (No Waivers)")

          # Split each metric group by waiver status
          meal_split <- split_by_waiver(damages_meal_groups)
          rest_split <- split_by_waiver(damages_rest_groups)
          rrop_split <- split_by_waiver(damages_rrop_groups)
          otc_split <- split_by_waiver(damages_otc_groups)
          rounding_split <- split_by_waiver(damages_rounding_groups)
          unpaid_ot_split <- split_by_waiver(damages_unpaid_ot_groups)
          expenses_split <- split_by_waiver(damages_expenses_groups)
          wsv_split <- split_by_waiver(damages_wsv_groups)
          wt_split <- split_by_waiver(damages_wt_groups)
          total_split <- split_by_waiver(damages_class_total_groups)

          damage_sections <- list()
          if (length(meal_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "MEAL PERIOD DAMAGES", groups = meal_split$no_waiver)
          if (length(rest_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "REST PERIOD DAMAGES", groups = rest_split$no_waiver)
          if (length(rrop_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "RROP DAMAGES", groups = rrop_split$no_waiver)
          if (length(otc_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "OFF-THE-CLOCK DAMAGES", groups = otc_split$no_waiver)
          if (length(rounding_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "CLOCK ROUNDING DAMAGES", groups = rounding_split$no_waiver)
          if (length(unpaid_ot_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "UNPAID OT/DT DAMAGES", groups = unpaid_ot_split$no_waiver)
          if (length(expenses_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "UNREIMBURSED EXPENSES DAMAGES", groups = expenses_split$no_waiver)
          if (length(wsv_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "WAGE STATEMENT PENALTIES", groups = wsv_split$no_waiver)
          if (length(wt_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "WAITING TIME PENALTIES", groups = wt_split$no_waiver)
          if (length(total_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "TOTAL DAMAGES", groups = total_split$no_waiver)

          damages_table <- combine_damages_with_headers(data, metric_spec, damage_sections, list(), 1.0)
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(damages_table, "Damages - Class/Individual Claims (No Waivers)", "⚖️"))
        }

        # Damages - Class/Individual Claims - Waivers
        if ("damages_class_waivers" %in% sections) {
          incProgress(1/total_sections, detail = "Damages - Class/Individual (Waivers)")

          meal_split <- split_by_waiver(damages_meal_groups)
          rest_split <- split_by_waiver(damages_rest_groups)
          rrop_split <- split_by_waiver(damages_rrop_groups)
          otc_split <- split_by_waiver(damages_otc_groups)
          rounding_split <- split_by_waiver(damages_rounding_groups)
          unpaid_ot_split <- split_by_waiver(damages_unpaid_ot_groups)
          expenses_split <- split_by_waiver(damages_expenses_groups)
          wsv_split <- split_by_waiver(damages_wsv_groups)
          wt_split <- split_by_waiver(damages_wt_groups)
          total_split <- split_by_waiver(damages_class_total_groups)

          damage_sections <- list()
          if (length(meal_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "MEAL PERIOD DAMAGES", groups = meal_split$waiver)
          if (length(rest_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "REST PERIOD DAMAGES", groups = rest_split$waiver)
          if (length(rrop_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "RROP DAMAGES", groups = rrop_split$waiver)
          if (length(otc_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "OFF-THE-CLOCK DAMAGES", groups = otc_split$waiver)
          if (length(rounding_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "CLOCK ROUNDING DAMAGES", groups = rounding_split$waiver)
          if (length(unpaid_ot_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "UNPAID OT/DT DAMAGES", groups = unpaid_ot_split$waiver)
          if (length(expenses_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "UNREIMBURSED EXPENSES DAMAGES", groups = expenses_split$waiver)
          if (length(wsv_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "WAGE STATEMENT PENALTIES", groups = wsv_split$waiver)
          if (length(wt_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "WAITING TIME PENALTIES", groups = wt_split$waiver)
          if (length(total_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "TOTAL DAMAGES", groups = total_split$waiver)

          damages_table <- combine_damages_with_headers(data, metric_spec, damage_sections, list(), 1.0)
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(damages_table, "Damages - Class/Individual Claims (Waivers)", "⚖️"))
        }

        # Damages - PAGA - No Waivers
        if ("damages_paga_no_waivers" %in% sections) {
          incProgress(1/total_sections, detail = "Damages - PAGA (No Waivers)")

          meal_split <- split_by_waiver(paga_meal_groups)
          rest_split <- split_by_waiver(paga_rest_groups)
          rrop_split <- split_by_waiver(paga_rrop_groups)
          s226_split <- split_by_waiver(paga_226_groups)
          s558_split <- split_by_waiver(paga_558_groups)
          min_wage_split <- split_by_waiver(paga_min_wage_groups)
          expenses_split <- split_by_waiver(paga_expenses_groups)
          recordkeeping_split <- split_by_waiver(paga_recordkeeping_groups)
          waiting_time_split <- split_by_waiver(paga_waiting_time_groups)
          total_split <- split_by_waiver(paga_total_groups)

          damage_sections <- list()
          if (length(meal_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - MEAL PERIODS", groups = meal_split$no_waiver)
          if (length(rest_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - REST PERIODS", groups = rest_split$no_waiver)
          if (length(rrop_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - REGULAR RATE (RROP)", groups = rrop_split$no_waiver)
          if (length(s226_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - WAGE STATEMENT (226)", groups = s226_split$no_waiver)
          if (length(s558_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - UNPAID WAGES (558)", groups = s558_split$no_waiver)
          if (length(min_wage_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - MIN WAGE (1197.1)", groups = min_wage_split$no_waiver)
          if (length(expenses_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - UNREIMBURSED EXPENSES (2802)", groups = expenses_split$no_waiver)
          if (length(recordkeeping_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - RECORDKEEPING (1174)", groups = recordkeeping_split$no_waiver)
          if (length(waiting_time_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - WAITING TIME (203)", groups = waiting_time_split$no_waiver)
          if (length(total_split$no_waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - TOTAL", groups = total_split$no_waiver)

          damages_table <- combine_damages_with_headers(data, metric_spec, damage_sections, list(), 1.0)
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(damages_table, "Damages - PAGA (No Waivers)", "⚖️"))
        }

        # Damages - PAGA - Waivers
        if ("damages_paga_waivers" %in% sections) {
          incProgress(1/total_sections, detail = "Damages - PAGA (Waivers)")

          meal_split <- split_by_waiver(paga_meal_groups)
          rest_split <- split_by_waiver(paga_rest_groups)
          rrop_split <- split_by_waiver(paga_rrop_groups)
          s226_split <- split_by_waiver(paga_226_groups)
          s558_split <- split_by_waiver(paga_558_groups)
          min_wage_split <- split_by_waiver(paga_min_wage_groups)
          expenses_split <- split_by_waiver(paga_expenses_groups)
          recordkeeping_split <- split_by_waiver(paga_recordkeeping_groups)
          waiting_time_split <- split_by_waiver(paga_waiting_time_groups)
          total_split <- split_by_waiver(paga_total_groups)

          damage_sections <- list()
          if (length(meal_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - MEAL PERIODS", groups = meal_split$waiver)
          if (length(rest_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - REST PERIODS", groups = rest_split$waiver)
          if (length(rrop_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - REGULAR RATE (RROP)", groups = rrop_split$waiver)
          if (length(s226_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - WAGE STATEMENT (226)", groups = s226_split$waiver)
          if (length(s558_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - UNPAID WAGES (558)", groups = s558_split$waiver)
          if (length(min_wage_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - MIN WAGE (1197.1)", groups = min_wage_split$waiver)
          if (length(expenses_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - UNREIMBURSED EXPENSES (2802)", groups = expenses_split$waiver)
          if (length(recordkeeping_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - RECORDKEEPING (1174)", groups = recordkeeping_split$waiver)
          if (length(waiting_time_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - WAITING TIME (203)", groups = waiting_time_split$waiver)
          if (length(total_split$waiver) > 0) damage_sections[[length(damage_sections) + 1]] <- list(section_name = "PAGA - TOTAL", groups = total_split$waiver)

          damages_table <- combine_damages_with_headers(data, metric_spec, damage_sections, list(), 1.0)
          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, add_table(damages_table, "Damages - PAGA (Waivers)", "⚖️"))
        }

        # Appendix Tables (All)
        if (input$pdf_include_appendix) {
          if (!is.null(analysis_tables$shift_hrs)) {
            html_content <- paste0(html_content, '<div class="page-break"></div>')
            html_content <- paste0(html_content, add_table(analysis_tables$shift_hrs, "Appendix - Shift Hours", "📑"))
          }

          if (!is.null(analysis_tables$non_wrk_hrs)) {
            html_content <- paste0(html_content, '<div class="page-break"></div>')
            html_content <- paste0(html_content, add_table(analysis_tables$non_wrk_hrs, "Appendix - Non-Work Hours", "📑"))
          }

          if (!is.null(analysis_tables$meal_period)) {
            html_content <- paste0(html_content, '<div class="page-break"></div>')
            html_content <- paste0(html_content, add_table(analysis_tables$meal_period, "Appendix - Meal Period Distribution", "📑"))
          }

          if (!is.null(analysis_tables$meal_start_time)) {
            html_content <- paste0(html_content, '<div class="page-break"></div>')
            html_content <- paste0(html_content, add_table(analysis_tables$meal_start_time, "Appendix - Meal Start Times", "📑"))
          }

          if (!is.null(analysis_tables$meal_quarter_hr)) {
            html_content <- paste0(html_content, '<div class="page-break"></div>')
            html_content <- paste0(html_content, add_table(analysis_tables$meal_quarter_hr, "Appendix - Meal Quarter Hour", "📑"))
          }
        }

        # Data Comparison Chart
        if (input$pdf_include_data_comparison) {
          incProgress(1/total_sections, detail = "Data Comparison")

          # Calculate overlap data (similar to venn_data reactive)
          time_ids <- unique(data$shift_data1$ID)
          pay_ids <- unique(data$pay1$Pay_ID)
          class_ids <- if (!is.null(data$class1) && "Class_ID" %in% names(data$class1)) {
            unique(data$class1$Class_ID)
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

          html_content <- paste0(html_content, '<div class="page-break"></div>')
          html_content <- paste0(html_content, '
  <h1>📊 Data Comparison - Employee Data Overlap Analysis</h1>

  <div style="margin: 15px 0; padding: 12px; background-color: #e8f4f8; border-left: 4px solid #0066cc;">
    <p style="margin: 0; font-size: 10pt;"><strong>💡 Interactive Chart:</strong> For a visual bar chart of employee overlap, please view the "Data Comparison" tab in the interactive dashboard. This PDF shows the summary statistics and detailed overlap counts.</p>
  </div>

  <h2>Summary Statistics</h2>
  <div style="margin: 20px 0;">
    <div class="stat-box">
      <div class="stat-label">Employees in Time Data</div>
      <div class="stat-value">', format(length(time_ids), big.mark = ","), '</div>
    </div>
    <div class="stat-box">
      <div class="stat-label">Employees in Pay Data</div>
      <div class="stat-value">', format(length(pay_ids), big.mark = ","), '</div>
    </div>')

          if (length(class_ids) > 0) {
            html_content <- paste0(html_content, '
    <div class="stat-box">
      <div class="stat-label">Employees in Class Data</div>
      <div class="stat-value">', format(length(class_ids), big.mark = ","), '</div>
    </div>')
          }

          html_content <- paste0(html_content, '
  </div>

  <h2>Overlap Analysis</h2>
  <table>
    <thead>
      <tr>
        <th>Category</th>
        <th class="value-col">Employee Count</th>
        <th class="value-col">Percentage</th>
      </tr>
    </thead>
    <tbody>')

          # Total unique employees
          all_unique_ids <- unique(c(time_ids, pay_ids, class_ids))
          total_unique <- length(all_unique_ids)

          # All three sources
          if (length(all_three) > 0 && length(class_ids) > 0) {
            pct <- sprintf("%.1f%%", (length(all_three) / total_unique) * 100)
            html_content <- paste0(html_content, '
      <tr style="background-color: #d4edda;">
        <td class="metric-col"><strong>All Three Sources</strong></td>
        <td class="value-col">', format(length(all_three), big.mark = ","), '</td>
        <td class="value-col">', pct, '</td>
      </tr>')
          }

          # Two-source overlaps
          if (length(time_pay) > 0) {
            pct <- sprintf("%.1f%%", (length(time_pay) / total_unique) * 100)
            html_content <- paste0(html_content, '
      <tr>
        <td class="metric-col">Time & Pay Only</td>
        <td class="value-col">', format(length(time_pay), big.mark = ","), '</td>
        <td class="value-col">', pct, '</td>
      </tr>')
          }

          if (length(time_class) > 0 && length(class_ids) > 0) {
            pct <- sprintf("%.1f%%", (length(time_class) / total_unique) * 100)
            html_content <- paste0(html_content, '
      <tr>
        <td class="metric-col">Time & Class Only</td>
        <td class="value-col">', format(length(time_class), big.mark = ","), '</td>
        <td class="value-col">', pct, '</td>
      </tr>')
          }

          if (length(pay_class) > 0 && length(class_ids) > 0) {
            pct <- sprintf("%.1f%%", (length(pay_class) / total_unique) * 100)
            html_content <- paste0(html_content, '
      <tr>
        <td class="metric-col">Pay & Class Only</td>
        <td class="value-col">', format(length(pay_class), big.mark = ","), '</td>
        <td class="value-col">', pct, '</td>
      </tr>')
          }

          # Single-source only
          if (length(time_only) > 0) {
            pct <- sprintf("%.1f%%", (length(time_only) / total_unique) * 100)
            html_content <- paste0(html_content, '
      <tr>
        <td class="metric-col">Time Data Only</td>
        <td class="value-col">', format(length(time_only), big.mark = ","), '</td>
        <td class="value-col">', pct, '</td>
      </tr>')
          }

          if (length(pay_only) > 0) {
            pct <- sprintf("%.1f%%", (length(pay_only) / total_unique) * 100)
            html_content <- paste0(html_content, '
      <tr>
        <td class="metric-col">Pay Data Only</td>
        <td class="value-col">', format(length(pay_only), big.mark = ","), '</td>
        <td class="value-col">', pct, '</td>
      </tr>')
          }

          if (length(class_only) > 0 && length(class_ids) > 0) {
            pct <- sprintf("%.1f%%", (length(class_only) / total_unique) * 100)
            html_content <- paste0(html_content, '
      <tr>
        <td class="metric-col">Class Data Only</td>
        <td class="value-col">', format(length(class_only), big.mark = ","), '</td>
        <td class="value-col">', pct, '</td>
      </tr>')
          }

          # Total row
          html_content <- paste0(html_content, '
      <tr style="background-color: #e9ecef; font-weight: bold;">
        <td class="metric-col">Total Unique Employees</td>
        <td class="value-col">', format(total_unique, big.mark = ","), '</td>
        <td class="value-col">100.0%</td>
      </tr>')

          html_content <- paste0(html_content, '
    </tbody>
  </table>

  <div style="margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-left: 4px solid #0066cc;">
    <p style="margin: 0;"><strong>Note:</strong> This analysis shows how employee data overlaps across different data sources (Time records, Pay records, and Class Action list). Employees appearing in multiple sources indicate good data matching, while single-source employees may require verification.</p>
  </div>
')
        }

          # Close HTML
          html_content <- paste0(html_content, '\n</body>\n</html>')

          # Write to file
          incProgress(1/total_sections, detail = "Finalizing report...")
          writeLines(html_content, file)

          showNotification("PDF report generated! Open the HTML file and use browser Print to PDF (Ctrl+P)",
                          type = "message", duration = 10)
        })
      }
    )
  }
}

# =============================================================================
# RUN APP
# =============================================================================

# Load data and spec
message("Loading data...")
data_list <- load_data()
metric_spec <- load_metric_spec()

message("Starting dashboard...")
shinyApp(
  ui = ui(data_list, metric_spec),
  server = server(data_list, metric_spec)
)
