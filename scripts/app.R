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

CASE_DIR <- Sys.getenv("ADS_CASE_DIR", unset = "")
DATA_DIR <- if (nzchar(CASE_DIR)) file.path(CASE_DIR, "output") else here("output")
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
  
  rbindlist(results, fill = TRUE)
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
