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

# =============================================================================
# DATA LOADING & METRIC SPEC PARSING
# =============================================================================

# Load processed data
load_data <- function() {
  list(
    shift_data1 = readRDS("data/processed/time_processed.rds"),
    pay1 = readRDS("data/processed/pay_processed.rds"),
    class1 = tryCatch(readRDS("data/processed/class_processed.rds"), error = function(e) NULL)
  )
}

# Load and parse metric spec
load_metric_spec <- function(path = "scripts/metrics_spec.csv") {
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

evaluate_metric <- function(data, expr_str, digits = NA) {
  if (is.na(expr_str) || expr_str == "") return(NA)
  
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
    NA
  })
  
  return(result)
}

# Calculate all metrics for a given group
calculate_group_metrics <- function(data_list, spec, group_name, filters = list()) {
  group_spec <- spec[metric_group == group_name]
  if (nrow(group_spec) == 0) return(data.table())
  
  results <- lapply(seq_len(nrow(group_spec)), function(i) {
    row <- group_spec[i]
    source <- row$source
    
    # Get appropriate data source
    src_data <- if (grepl("shift|time", source, ignore.case = TRUE)) {
      copy(data_list$shift_data1)
    } else if (grepl("pay", source, ignore.case = TRUE)) {
      copy(data_list$pay1)
    } else {
      NULL
    }
    
    if (is.null(src_data) || nrow(src_data) == 0) {
      return(data.table(
        metric_label = row$metric_label,
        value = NA,
        pct = NA,
        denom_value = NA
      ))
    }
    
    # Apply filters
    if (length(filters) > 0) {
      for (f in names(filters)) {
        if (!is.null(filters[[f]]) && f %in% names(src_data)) {
          src_data <- src_data[get(f) %in% filters[[f]]]
        }
      }
      
      # Date filters
      if (!is.null(filters$date_min)) {
        date_col <- if ("Date" %in% names(src_data)) "Date" else if ("Pay_Date" %in% names(src_data)) "Pay_Date" else NULL
        if (!is.null(date_col)) {
          src_data <- src_data[get(date_col) >= filters$date_min]
        }
      }
      if (!is.null(filters$date_max)) {
        date_col <- if ("Date" %in% names(src_data)) "Date" else if ("Pay_Date" %in% names(src_data)) "Pay_Date" else NULL
        if (!is.null(date_col)) {
          src_data <- src_data[get(date_col) <= filters$date_max]
        }
      }
    }
    
    # Calculate metric value
    value <- evaluate_metric(src_data, row$expr, row$digits)
    
    # Calculate denominator and percentage
    denom_value <- get_denominator_value(src_data, row$denom, source)
    pct <- if (!is.na(denom_value) && denom_value > 0 && is.numeric(value)) {
      round(value / denom_value * 100, 2)
    } else {
      NA_real_
    }
    
    data.table(
      metric_label = row$metric_label,
      value = value,
      pct = pct,
      denom_value = denom_value
    )
  })
  
  rbindlist(results, fill = TRUE)
}

# =============================================================================
# FORMAT HELPERS
# =============================================================================

format_value <- function(val, metric_type = "value", pct = NA) {
  if (is.na(val) || is.null(val)) return("-")
  
  formatted <- switch(metric_type,
                      "date" = as.character(as.Date(val, origin = "1970-01-01")),
                      "currency" = paste0("$", format(round(val, 2), big.mark = ",", nsmall = 2)),
                      "percent" = paste0(round(val, 2), "%"),
                      format(val, big.mark = ",")
  )
  
  # Add percentage if available
  if (!is.na(pct)) {
    formatted <- paste0(formatted, " (", pct, "%)")
  }
  
  formatted
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
  
  # Employee IDs
  all_ids <- unique(c(shift_data$ID, pay_data$Pay_ID))
  
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
      choices = c("All" = "", sort(all_ids)),
      multiple = TRUE,
      options = list(placeholder = "All Employees")
    ),
    
    # Location filter if available
    if ("Location" %in% names(shift_data) || "Pay_Location" %in% names(pay_data)) {
      locations <- unique(c(
        if ("Location" %in% names(shift_data)) shift_data$Location else NULL,
        if ("Pay_Location" %in% names(pay_data)) pay_data$Pay_Location else NULL
      ))
      selectizeInput(
        "location_filter",
        "Location(s)",
        choices = c("All" = "", sort(unique(na.omit(locations)))),
        multiple = TRUE,
        options = list(placeholder = "All Locations")
      )
    },
    
    hr(),
    
    actionButton("apply_filters", "Apply Filters", class = "btn-primary w-100"),
    actionButton("reset_filters", "Reset", class = "btn-outline-secondary w-100 mt-2"),
    
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
  
  page_navbar(
    title = "Wage & Hour Compliance Dashboard",
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2c3e50",
      "navbar-bg" = "#2c3e50"
    ),
    
    sidebar = filter_sidebar(data_list),
    
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
    
    # Time Summary Menu
    nav_menu(
      title = "Time Summary",
      icon = icon("clock"),
      lapply(time_summary, function(grp) {
        nav_panel(
          title = gsub("^Time ", "", grp),
          card(
            card_header(grp),
            card_body(DTOutput(paste0("table_", make.names(grp))))
          )
        )
      })
    ),
    
    # Time Meal Violations Menu
    nav_menu(
      title = "Meal Violations",
      icon = icon("utensils"),
      lapply(time_meal_violations, function(grp) {
        nav_panel(
          title = gsub("^Time Meal Violations ", "", grp),
          card(
            card_header(grp),
            card_body(DTOutput(paste0("table_", make.names(grp))))
          )
        )
      })
    ),
    
    # Time Rest Violations
    nav_menu(
      title = "Rest Violations",
      icon = icon("pause-circle"),
      lapply(c(time_rest_violations), function(grp) {
        nav_panel(
          title = gsub("^Time ", "", grp),
          card(
            card_header(grp),
            card_body(DTOutput(paste0("table_", make.names(grp))))
          )
        )
      })
    ),
    
    # Time Shift Hours Analysis Menu
    nav_menu(
      title = "Shift Hours",
      icon = icon("chart-bar"),
      lapply(time_shift_hours, function(grp) {
        nav_panel(
          title = gsub("^Time Shift Hours Analysis ", "", gsub("^Time Shift Hours Analysis", "Overview", grp)),
          card(
            card_header(grp),
            card_body(DTOutput(paste0("table_", make.names(grp))))
          )
        )
      })
    ),
    
    # Time Punch Rounding Menu
    nav_menu(
      title = "Punch Rounding",
      icon = icon("sync"),
      lapply(time_rounding, function(grp) {
        nav_panel(
          title = gsub("^Time Punch Rounding Analysis ", "", gsub("^Time Punch Rounding Analysis", "Overview", grp)),
          card(
            card_header(grp),
            card_body(DTOutput(paste0("table_", make.names(grp))))
          )
        )
      })
    ),
    
    # Pay Summary Menu
    nav_menu(
      title = "Pay Summary",
      icon = icon("dollar-sign"),
      lapply(pay_summary, function(grp) {
        nav_panel(
          title = gsub("^Pay ", "", grp),
          card(
            card_header(grp),
            card_body(DTOutput(paste0("table_", make.names(grp))))
          )
        )
      })
    ),
    
    # Pay Premiums Menu
    nav_menu(
      title = "Pay Premiums",
      icon = icon("coins"),
      lapply(pay_premiums, function(grp) {
        nav_panel(
          title = gsub("^Pay ", "", grp),
          card(
            card_header(grp),
            card_body(DTOutput(paste0("table_", make.names(grp))))
          )
        )
      })
    ),
    
    # Pay Bonuses/Differentials Menu
    nav_menu(
      title = "Bonuses & Diffs",
      icon = icon("gift"),
      lapply(pay_bonuses, function(grp) {
        nav_panel(
          title = gsub("^Pay ", "", grp),
          card(
            card_header(grp),
            card_body(DTOutput(paste0("table_", make.names(grp))))
          )
        )
      })
    ),
    
    # Pay Regular Rate Menu
    nav_menu(
      title = "Regular Rate",
      icon = icon("calculator"),
      lapply(pay_rrop, function(grp) {
        nav_panel(
          title = gsub("^Pay Regular Rate - ", "", grp),
          card(
            card_header(grp),
            card_body(DTOutput(paste0("table_", make.names(grp))))
          )
        )
      })
    ),
    
    # Full Report Tab
    nav_panel(
      title = "Full Report",
      icon = icon("file-alt"),
      
      card(
        card_header("Complete Metrics Report"),
        card_body(
          DTOutput("full_report_table")
        )
      )
    )
  )
}

server <- function(data_list, metric_spec) {
  function(input, output, session) {
    
    # Reactive: Current filters
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
    
    # Reactive: Filtered data
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
        pay_filtered <- pay_filtered[Pay_Date >= filters$date_min]
      }
      if (!is.null(filters$date_max)) {
        pay_filtered <- pay_filtered[Pay_Date <= filters$date_max]
      }
      if (!is.null(filters$Pay_ID)) {
        pay_filtered <- pay_filtered[Pay_ID %in% filters$Pay_ID]
      }
      if (!is.null(filters$Pay_Location) && "Pay_Location" %in% names(pay_filtered)) {
        pay_filtered <- pay_filtered[Pay_Location %in% filters$Pay_Location]
      }
      
      list(
        shift_data1 = shift_filtered,
        pay1 = pay_filtered,
        class1 = data_list$class1
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
        
        # Format for display
        display_df <- data.table(
          Metric = results$metric_label,
          Value = sapply(seq_len(nrow(results)), function(i) {
            val <- results$value[i]
            pct <- results$pct[i]
            
            if (is.na(val)) {
              "-"
            } else if (inherits(val, "Date")) {
              as.character(val)
            } else if (is.numeric(val)) {
              formatted <- format(val, big.mark = ",", nsmall = if(val %% 1 == 0) 0 else 2)
              if (!is.na(pct)) {
                paste0(formatted, " (", pct, "%)")
              } else {
                formatted
              }
            } else {
              as.character(val)
            }
          })
        )
        
        datatable(
          display_df,
          options = list(
            pageLength = 25,
            dom = 'frtip',
            scrollX = TRUE
          ),
          rownames = FALSE,
          class = 'cell-border stripe hover'
        )
      })
    })
    
    # Full report table
    output$full_report_table <- renderDT({
      data <- filtered_data()
      
      all_results <- lapply(metric_groups, function(grp) {
        results <- calculate_group_metrics(data, metric_spec, grp, current_filters())
        results[, metric_group := grp]
        results
      })
      
      combined <- rbindlist(all_results, fill = TRUE)
      
      display_df <- data.table(
        Group = combined$metric_group,
        Metric = combined$metric_label,
        Value = sapply(seq_len(nrow(combined)), function(i) {
          val <- combined$value[i]
          pct <- combined$pct[i]
          
          if (is.na(val)) {
            "-"
          } else if (inherits(val, "Date")) {
            as.character(val)
          } else if (is.numeric(val)) {
            formatted <- format(val, big.mark = ",", nsmall = if(val %% 1 == 0) 0 else 2)
            if (!is.na(pct)) {
              paste0(formatted, " (", pct, "%)")
            } else {
              formatted
            }
          } else {
            as.character(val)
          }
        })
      )
      
      datatable(
        display_df,
        options = list(
          pageLength = 50,
          dom = 'Bfrtip',
          scrollX = TRUE,
          buttons = c('csv', 'excel')
        ),
        rownames = FALSE,
        filter = 'top',
        class = 'cell-border stripe hover',
        extensions = 'Buttons'
      )
    })
    
    # Download handler
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("wage_hour_report_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- filtered_data()
        
        all_results <- lapply(metric_groups, function(grp) {
          results <- calculate_group_metrics(data, metric_spec, grp, current_filters())
          results[, metric_group := grp]
          results
        })
        
        combined <- rbindlist(all_results, fill = TRUE)
        setcolorder(combined, c("metric_group", "metric_label", "value", "pct", "denom_value"))
        
        fwrite(combined, file)
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