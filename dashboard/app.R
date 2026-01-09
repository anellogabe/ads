# Wage & Hour Dashboard - Cardona Case
# ==================================================

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(here)

# Load processed data
load_data <- function() {
  tryCatch({
    list(
      time = if(file.exists(here("data/processed/time_processed.rds"))) readRDS(here("data/processed/time_processed.rds")) else NULL,
      pay = if(file.exists(here("data/processed/pay_processed.rds"))) readRDS(here("data/processed/pay_processed.rds")) else NULL,
      class = if(file.exists(here("data/processed/class_processed.rds"))) readRDS(here("data/processed/class_processed.rds")) else NULL,
      shift = if(file.exists(here("output/Time Shift Data.csv"))) fread(here("output/Time Shift Data.csv")) else NULL,
      ee = if(file.exists(here("output/Time Employee Data.csv"))) fread(here("output/Time Employee Data.csv")) else NULL,
      analysis = if(file.exists(here("output/Analysis.csv"))) fread(here("output/Analysis.csv")) else NULL
    )
  }, error = function(e) {
    message("Error loading data: ", e$message)
    NULL
  })
}

data <- load_data()

# UI ===============================================
ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(title = "Wage & Hour Dashboard - Cardona"),

  # Sidebar
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem("1. Key Metrics Summary", tabName = "metrics", icon = icon("tachometer-alt")),
      menuItem("2. Time Summary", tabName = "time_summary", icon = icon("clock")),
      menuItem("3. Meal Period Analysis", tabName = "meal_analysis", icon = icon("utensils")),
      menuItem("4. Meal Violations >5 hrs", tabName = "meal_5hr", icon = icon("exclamation-triangle")),
      menuItem("5. Meal Violations >6 hrs", tabName = "meal_6hr", icon = icon("exclamation-circle")),
      menuItem("6. Rest Periods", tabName = "rest", icon = icon("pause")),
      menuItem("7. Shift Hours Analysis", tabName = "shift_hours", icon = icon("calendar-day")),
      menuItem("8. Rounding Analysis", tabName = "rounding", icon = icon("stopwatch")),
      menuItem("9. Pay Summary", tabName = "pay_summary", icon = icon("money-bill-wave")),
      menuItem("10. Bonuses & Diffs", tabName = "bonuses", icon = icon("gift")),
      menuItem("11. Regular Rate Groups", tabName = "rrop", icon = icon("calculator"))
    ),

    # Filters
    hr(),
    h4("Filters", style = "padding-left: 15px;"),

    dateRangeInput("date_range", "Date Range:",
                   start = if(!is.null(data$shift)) min(data$shift$Date, na.rm = TRUE) else Sys.Date() - 365,
                   end = if(!is.null(data$shift)) max(data$shift$Date, na.rm = TRUE) else Sys.Date()),

    actionButton("refresh", "Refresh Data", icon = icon("sync"),
                 style = "margin: 15px;")
  ),

  # Body
  dashboardBody(
    tabItems(
      # 1. Key Metrics Summary =====================================
      tabItem(tabName = "metrics",
              h2("Key Metrics Summary"),
              fluidRow(
                valueBoxOutput("total_employees", width = 3),
                valueBoxOutput("total_shifts", width = 3),
                valueBoxOutput("total_hours", width = 3),
                valueBoxOutput("total_violations", width = 3)
              ),
              fluidRow(
                valueBoxOutput("meal_violations", width = 3),
                valueBoxOutput("rest_violations", width = 3),
                valueBoxOutput("total_damages", width = 3),
                valueBoxOutput("avg_damages_per_ee", width = 3)
              ),
              fluidRow(
                box(
                  title = "Summary Statistics", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("metrics_table")
                )
              )
      ),

      # 2. Time Summary ============================================
      tabItem(tabName = "time_summary",
              h2("Time Summary"),
              fluidRow(
                box(
                  title = "Hours Worked by Category", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("hours_by_category")
                ),
                box(
                  title = "Hours Worked Over Time", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("hours_trend")
                )
              ),
              fluidRow(
                box(
                  title = "Time Data Summary", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("time_summary_table")
                )
              )
      ),

      # 3. Meal Period Analysis =====================================
      tabItem(tabName = "meal_analysis",
              h2("Meal Period Analysis"),
              fluidRow(
                infoBoxOutput("total_meal_periods", width = 4),
                infoBoxOutput("auto_deduct_meals", width = 4),
                infoBoxOutput("rounded_meals", width = 4)
              ),
              fluidRow(
                box(
                  title = "Meal Period Duration Distribution", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("meal_duration_dist")
                ),
                box(
                  title = "Meal Start Time Distribution", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("meal_start_time_dist")
                )
              ),
              fluidRow(
                box(
                  title = "Auto-Deduct & Rounded Meal Analysis", status = "warning", solidHeader = TRUE, width = 12,
                  DTOutput("meal_deduct_table")
                )
              )
      ),

      # 4. Meal Violations >5 hrs (no waivers) ======================
      tabItem(tabName = "meal_5hr",
              h2("Meal Period Violations - Shifts >5 Hours (No Waivers)"),
              fluidRow(
                valueBoxOutput("mpv_5hr_total", width = 3),
                valueBoxOutput("mpv_5hr_late", width = 3),
                valueBoxOutput("mpv_5hr_short", width = 3),
                valueBoxOutput("mpv_5hr_damages", width = 3)
              ),
              fluidRow(
                box(
                  title = "Violations Over Time", status = "danger", solidHeader = TRUE, width = 12,
                  plotlyOutput("mpv_5hr_trend")
                )
              ),
              fluidRow(
                box(
                  title = "Late Meal Period Detail", status = "warning", solidHeader = TRUE,
                  width = 12, collapsible = TRUE, collapsed = TRUE,
                  DTOutput("mpv_5hr_late_table")
                )
              ),
              fluidRow(
                box(
                  title = "Short Meal Period Detail", status = "warning", solidHeader = TRUE,
                  width = 12, collapsible = TRUE, collapsed = TRUE,
                  DTOutput("mpv_5hr_short_table")
                )
              )
      ),

      # 5. Meal Violations >6 hrs (waivers) =========================
      tabItem(tabName = "meal_6hr",
              h2("Meal Period Violations - Shifts >6 Hours (With Waivers)"),
              fluidRow(
                valueBoxOutput("mpv_6hr_total", width = 3),
                valueBoxOutput("mpv_6hr_late", width = 3),
                valueBoxOutput("mpv_6hr_short", width = 3),
                valueBoxOutput("mpv_6hr_damages", width = 3)
              ),
              fluidRow(
                box(
                  title = "Violations Over Time", status = "danger", solidHeader = TRUE, width = 12,
                  plotlyOutput("mpv_6hr_trend")
                )
              ),
              fluidRow(
                box(
                  title = "Late Meal Period Detail", status = "warning", solidHeader = TRUE,
                  width = 12, collapsible = TRUE, collapsed = TRUE,
                  DTOutput("mpv_6hr_late_table")
                )
              ),
              fluidRow(
                box(
                  title = "Short Meal Period Detail", status = "warning", solidHeader = TRUE,
                  width = 12, collapsible = TRUE, collapsed = TRUE,
                  DTOutput("mpv_6hr_short_table")
                )
              )
      ),

      # 6. Rest Periods & Violations ================================
      tabItem(tabName = "rest",
              h2("Rest Period Analysis & Violations"),
              fluidRow(
                valueBoxOutput("total_rest_periods", width = 3),
                valueBoxOutput("rest_violations_total", width = 3),
                valueBoxOutput("rest_violation_rate", width = 3),
                valueBoxOutput("rest_damages", width = 3)
              ),
              fluidRow(
                box(
                  title = "Rest Period Violations Over Time", status = "danger", solidHeader = TRUE, width = 12,
                  plotlyOutput("rest_violations_trend")
                )
              ),
              fluidRow(
                box(
                  title = "Rest Period Violations by Employee", status = "warning", solidHeader = TRUE, width = 12,
                  DTOutput("rest_violations_table")
                )
              )
      ),

      # 7. Shift Hours Analysis =====================================
      tabItem(tabName = "shift_hours",
              h2("Shift Hours Analysis"),
              fluidRow(
                infoBoxOutput("avg_shift_length", width = 4),
                infoBoxOutput("total_shifts_analyzed", width = 4),
                infoBoxOutput("shifts_over_8hrs", width = 4)
              ),
              fluidRow(
                box(
                  title = "Shift Length Distribution", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("shift_length_dist")
                ),
                box(
                  title = "Shifts by Day of Week", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("shift_dow_chart")
                )
              ),
              fluidRow(
                box(
                  title = "Shift Hours Detail", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("shift_hours_table")
                )
              )
      ),

      # 8. Rounding Analysis ========================================
      tabItem(tabName = "rounding",
              h2("Time Rounding Analysis"),
              fluidRow(
                valueBoxOutput("total_rounding_impact", width = 4),
                valueBoxOutput("shifts_rounded_up", width = 4),
                valueBoxOutput("shifts_rounded_down", width = 4)
              ),
              fluidRow(
                box(
                  title = "Rounding Impact Distribution", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("rounding_dist")
                ),
                box(
                  title = "Rounding Impact Over Time", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("rounding_trend")
                )
              ),
              fluidRow(
                box(
                  title = "Rounding Detail by Employee", status = "warning", solidHeader = TRUE, width = 12,
                  DTOutput("rounding_table")
                )
              )
      ),

      # 9. Pay Summary ==============================================
      tabItem(tabName = "pay_summary",
              h2("Pay Summary"),
              fluidRow(
                valueBoxOutput("total_pay_amount", width = 3),
                valueBoxOutput("total_pay_hours", width = 3),
                valueBoxOutput("avg_hourly_rate", width = 3),
                valueBoxOutput("total_pay_periods", width = 3)
              ),
              fluidRow(
                box(
                  title = "Pay by Category", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("pay_by_category")
                ),
                box(
                  title = "Pay Over Time", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("pay_trend")
                )
              ),
              fluidRow(
                box(
                  title = "Pay Summary by Code", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("pay_summary_table")
                )
              )
      ),

      # 10. Bonuses & Diffs =========================================
      tabItem(tabName = "bonuses",
              h2("Bonuses & Pay Differences"),
              fluidRow(
                valueBoxOutput("total_bonuses", width = 4),
                valueBoxOutput("total_diffs", width = 4),
                valueBoxOutput("employees_with_bonuses", width = 4)
              ),
              fluidRow(
                box(
                  title = "Bonus Payments Over Time", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("bonus_trend")
                ),
                box(
                  title = "Bonus Distribution by Type", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("bonus_type_dist")
                )
              ),
              fluidRow(
                box(
                  title = "Pay Differences Analysis", status = "warning", solidHeader = TRUE, width = 12,
                  DTOutput("diffs_table")
                )
              )
      ),

      # 11. Regular Rate Groups =====================================
      tabItem(tabName = "rrop",
              h2("Regular Rate of Pay (RROP) Analysis"),
              fluidRow(
                infoBoxOutput("rrop_categories", width = 4),
                infoBoxOutput("rrop_includable", width = 4),
                infoBoxOutput("rrop_excludable", width = 4)
              ),
              fluidRow(
                box(
                  title = "RROP Components by Category", status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("rrop_category_chart")
                )
              ),
              fluidRow(
                box(
                  title = "Regular Rate Groups Detail", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("rrop_table")
                )
              )
      )
    )
  )
)

# SERVER ===============================================
server <- function(input, output, session) {

  # Reactive filtered data
  filtered_data <- reactive({
    req(data$shift)
    df <- data$shift

    # Date filter
    if(!is.null(input$date_range)) {
      df <- df[Date >= input$date_range[1] & Date <= input$date_range[2]]
    }

    df
  })

  # === 1. KEY METRICS SUMMARY ===
  output$total_employees <- renderValueBox({
    df <- filtered_data()
    count <- if(!is.null(df) && "ID" %in% names(df)) uniqueN(df$ID) else 0
    valueBox(format(count, big.mark = ","), "Total Employees", icon = icon("users"), color = "blue")
  })

  output$total_shifts <- renderValueBox({
    df <- filtered_data()
    count <- if(!is.null(df)) nrow(df) else 0
    valueBox(format(count, big.mark = ","), "Total Shifts", icon = icon("calendar"), color = "light-blue")
  })

  output$total_hours <- renderValueBox({
    df <- filtered_data()
    hours <- if(!is.null(df) && "shift_hrs" %in% names(df)) sum(df$shift_hrs, na.rm = TRUE) else 0
    valueBox(format(round(hours, 0), big.mark = ","), "Total Hours", icon = icon("clock"), color = "aqua")
  })

  output$total_violations <- renderValueBox({
    df <- filtered_data()
    mpv <- if(!is.null(df) && "mpv_shift" %in% names(df)) sum(df$mpv_shift, na.rm = TRUE) else 0
    rpv <- if(!is.null(df) && "rpv_shift" %in% names(df)) sum(df$rpv_shift, na.rm = TRUE) else 0
    total <- mpv + rpv
    valueBox(format(round(total, 0), big.mark = ","), "Total Violations", icon = icon("exclamation-triangle"), color = "red")
  })

  output$meal_violations <- renderValueBox({
    df <- filtered_data()
    mpv <- if(!is.null(df) && "mpv_shift" %in% names(df)) sum(df$mpv_shift, na.rm = TRUE) else 0
    valueBox(format(round(mpv, 0), big.mark = ","), "Meal Violations", icon = icon("utensils"), color = "orange")
  })

  output$rest_violations <- renderValueBox({
    df <- filtered_data()
    rpv <- if(!is.null(df) && "rpv_shift" %in% names(df)) sum(df$rpv_shift, na.rm = TRUE) else 0
    valueBox(format(round(rpv, 0), big.mark = ","), "Rest Violations", icon = icon("pause"), color = "yellow")
  })

  output$total_damages <- renderValueBox({
    df <- filtered_data()
    mp_dmg <- if(!is.null(df) && "mp_tot_dmgs" %in% names(df)) sum(df$mp_tot_dmgs, na.rm = TRUE) else 0
    rp_dmg <- if(!is.null(df) && "rp_tot_dmgs" %in% names(df)) sum(df$rp_tot_dmgs, na.rm = TRUE) else 0
    total <- mp_dmg + rp_dmg
    valueBox(paste0("$", format(round(total, 0), big.mark = ",")), "Total Damages", icon = icon("dollar-sign"), color = "green")
  })

  output$avg_damages_per_ee <- renderValueBox({
    df <- filtered_data()
    mp_dmg <- if(!is.null(df) && "mp_tot_dmgs" %in% names(df)) sum(df$mp_tot_dmgs, na.rm = TRUE) else 0
    rp_dmg <- if(!is.null(df) && "rp_tot_dmgs" %in% names(df)) sum(df$rp_tot_dmgs, na.rm = TRUE) else 0
    total <- mp_dmg + rp_dmg
    n_ee <- if(!is.null(df) && "ID" %in% names(df)) uniqueN(df$ID) else 1
    avg <- total / n_ee
    valueBox(paste0("$", format(round(avg, 0), big.mark = ",")), "Avg Damages/EE", icon = icon("user-check"), color = "teal")
  })

  output$metrics_table <- renderDT({
    df <- filtered_data()
    if(is.null(df)) return(datatable(data.frame(Message = "No data available")))

    summary_df <- data.frame(
      Metric = c("Total Employees", "Total Shifts", "Total Hours Worked",
                 "Meal Period Violations", "Rest Period Violations",
                 "Total Violations", "Total Damages", "Avg Damages per Employee"),
      Value = c(
        format(uniqueN(df$ID), big.mark = ","),
        format(nrow(df), big.mark = ","),
        format(round(sum(df$shift_hrs, na.rm = TRUE), 0), big.mark = ","),
        format(round(sum(df$mpv_shift, na.rm = TRUE), 0), big.mark = ","),
        format(round(sum(df$rpv_shift, na.rm = TRUE), 0), big.mark = ","),
        format(round(sum(df$mpv_shift, na.rm = TRUE) + sum(df$rpv_shift, na.rm = TRUE), 0), big.mark = ","),
        paste0("$", format(round(sum(df$mp_tot_dmgs, na.rm = TRUE) + sum(df$rp_tot_dmgs, na.rm = TRUE), 0), big.mark = ",")),
        paste0("$", format(round((sum(df$mp_tot_dmgs, na.rm = TRUE) + sum(df$rp_tot_dmgs, na.rm = TRUE)) / uniqueN(df$ID), 0), big.mark = ","))
      )
    )

    datatable(summary_df, options = list(pageLength = 20, dom = 't'), rownames = FALSE)
  })

  # === 2. TIME SUMMARY ===
  output$hours_by_category <- renderPlotly({
    df <- filtered_data()
    if(is.null(df)) return(plotly_empty())

    plot_ly(type = "bar") %>%
      layout(title = "Coming Soon: Hours by Category",
             xaxis = list(title = ""),
             yaxis = list(title = "Hours"))
  })

  output$hours_trend <- renderPlotly({
    df <- filtered_data()
    if(is.null(df) || !"Date" %in% names(df)) return(plotly_empty())

    trend <- df[, .(Hours = sum(shift_hrs, na.rm = TRUE)), by = Date]

    plot_ly(trend, x = ~Date, y = ~Hours, type = "scatter", mode = "lines") %>%
      layout(title = "Hours Worked Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Total Hours"))
  })

  output$time_summary_table <- renderDT({
    df <- filtered_data()
    if(is.null(df)) return(datatable(data.frame(Message = "No data available")))

    datatable(head(df, 1000), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })

  # === 3. MEAL PERIOD ANALYSIS ===
  output$total_meal_periods <- renderInfoBox({
    df <- filtered_data()
    count <- if(!is.null(df) && "mp1_hrs" %in% names(df)) sum(!is.na(df$mp1_hrs)) else 0
    infoBox("Total Meal Periods", format(count, big.mark = ","), icon = icon("utensils"), color = "blue")
  })

  output$auto_deduct_meals <- renderInfoBox({
    infoBox("Auto-Deduct Meals", "Coming Soon", icon = icon("robot"), color = "yellow")
  })

  output$rounded_meals <- renderInfoBox({
    infoBox("Rounded Meals", "Coming Soon", icon = icon("stopwatch"), color = "orange")
  })

  output$meal_duration_dist <- renderPlotly({
    df <- filtered_data()
    if(is.null(df) || !"mp1_hrs" %in% names(df)) return(plotly_empty())

    meal_hrs <- df$mp1_hrs[!is.na(df$mp1_hrs)] * 60  # Convert to minutes

    plot_ly(x = meal_hrs, type = "histogram", nbinsx = 30) %>%
      layout(title = "Meal Period Duration (Minutes)",
             xaxis = list(title = "Duration (Minutes)"),
             yaxis = list(title = "Count"))
  })

  output$meal_start_time_dist <- renderPlotly({
    plot_ly(type = "bar") %>%
      layout(title = "Coming Soon: Meal Start Time Distribution")
  })

  output$meal_deduct_table <- renderDT({
    datatable(data.frame(Message = "Coming Soon: Auto-deduct & rounded meal analysis"),
              options = list(dom = 't'), rownames = FALSE)
  })

  # === 4 & 5. MEAL VIOLATIONS ===
  output$mpv_5hr_total <- renderValueBox({
    df <- filtered_data()
    count <- if(!is.null(df) && "mpv_shift" %in% names(df) && "shift_hrs" %in% names(df)) {
      sum(df$mpv_shift[df$shift_hrs > 5 & df$shift_hrs <= 6], na.rm = TRUE)
    } else 0
    valueBox(format(round(count, 0), big.mark = ","), "Total Violations", icon = icon("exclamation-triangle"), color = "red")
  })

  output$mpv_5hr_late <- renderValueBox({
    valueBox("Coming Soon", "Late Meals", icon = icon("clock"), color = "orange")
  })

  output$mpv_5hr_short <- renderValueBox({
    valueBox("Coming Soon", "Short Meals", icon = icon("hourglass-half"), color = "yellow")
  })

  output$mpv_5hr_damages <- renderValueBox({
    df <- filtered_data()
    damages <- if(!is.null(df) && "mp_tot_dmgs" %in% names(df) && "shift_hrs" %in% names(df)) {
      sum(df$mp_tot_dmgs[df$shift_hrs > 5 & df$shift_hrs <= 6], na.rm = TRUE)
    } else 0
    valueBox(paste0("$", format(round(damages, 0), big.mark = ",")), "Total Damages", icon = icon("dollar-sign"), color = "green")
  })

  output$mpv_5hr_trend <- renderPlotly({
    df <- filtered_data()
    if(is.null(df) || !"Date" %in% names(df)) return(plotly_empty())

    trend <- df[shift_hrs > 5 & shift_hrs <= 6, .(Violations = sum(mpv_shift, na.rm = TRUE)), by = Date]

    plot_ly(trend, x = ~Date, y = ~Violations, type = "scatter", mode = "lines") %>%
      layout(title = "Meal Violations >5hrs Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Violations"))
  })

  output$mpv_5hr_late_table <- renderDT({
    datatable(data.frame(Message = "Coming Soon: Late meal detail"),
              options = list(pageLength = 25), rownames = FALSE)
  })

  output$mpv_5hr_short_table <- renderDT({
    datatable(data.frame(Message = "Coming Soon: Short meal detail"),
              options = list(pageLength = 25), rownames = FALSE)
  })

  # Similar for 6hr violations
  output$mpv_6hr_total <- renderValueBox({
    df <- filtered_data()
    count <- if(!is.null(df) && "mpv_shift" %in% names(df) && "shift_hrs" %in% names(df)) {
      sum(df$mpv_shift[df$shift_hrs > 6], na.rm = TRUE)
    } else 0
    valueBox(format(round(count, 0), big.mark = ","), "Total Violations", icon = icon("exclamation-triangle"), color = "red")
  })

  output$mpv_6hr_late <- renderValueBox({
    valueBox("Coming Soon", "Late Meals", icon = icon("clock"), color = "orange")
  })

  output$mpv_6hr_short <- renderValueBox({
    valueBox("Coming Soon", "Short Meals", icon = icon("hourglass-half"), color = "yellow")
  })

  output$mpv_6hr_damages <- renderValueBox({
    df <- filtered_data()
    damages <- if(!is.null(df) && "mp_tot_dmgs" %in% names(df) && "shift_hrs" %in% names(df)) {
      sum(df$mp_tot_dmgs[df$shift_hrs > 6], na.rm = TRUE)
    } else 0
    valueBox(paste0("$", format(round(damages, 0), big.mark = ",")), "Total Damages", icon = icon("dollar-sign"), color = "green")
  })

  output$mpv_6hr_trend <- renderPlotly({
    df <- filtered_data()
    if(is.null(df) || !"Date" %in% names(df)) return(plotly_empty())

    trend <- df[shift_hrs > 6, .(Violations = sum(mpv_shift, na.rm = TRUE)), by = Date]

    plot_ly(trend, x = ~Date, y = ~Violations, type = "scatter", mode = "lines") %>%
      layout(title = "Meal Violations >6hrs Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Violations"))
  })

  output$mpv_6hr_late_table <- renderDT({
    datatable(data.frame(Message = "Coming Soon: Late meal detail"),
              options = list(pageLength = 25), rownames = FALSE)
  })

  output$mpv_6hr_short_table <- renderDT({
    datatable(data.frame(Message = "Coming Soon: Short meal detail"),
              options = list(pageLength = 25), rownames = FALSE)
  })

  # === 6. REST PERIODS ===
  output$total_rest_periods <- renderValueBox({
    valueBox("Coming Soon", "Total Rest Periods", icon = icon("pause"), color = "blue")
  })

  output$rest_violations_total <- renderValueBox({
    df <- filtered_data()
    count <- if(!is.null(df) && "rpv_shift" %in% names(df)) sum(df$rpv_shift, na.rm = TRUE) else 0
    valueBox(format(round(count, 0), big.mark = ","), "Rest Violations", icon = icon("exclamation-triangle"), color = "red")
  })

  output$rest_violation_rate <- renderValueBox({
    valueBox("Coming Soon", "Violation Rate", icon = icon("percent"), color = "orange")
  })

  output$rest_damages <- renderValueBox({
    df <- filtered_data()
    damages <- if(!is.null(df) && "rp_tot_dmgs" %in% names(df)) sum(df$rp_tot_dmgs, na.rm = TRUE) else 0
    valueBox(paste0("$", format(round(damages, 0), big.mark = ",")), "Rest Damages", icon = icon("dollar-sign"), color = "green")
  })

  output$rest_violations_trend <- renderPlotly({
    df <- filtered_data()
    if(is.null(df) || !"Date" %in% names(df)) return(plotly_empty())

    trend <- df[, .(Violations = sum(rpv_shift, na.rm = TRUE)), by = Date]

    plot_ly(trend, x = ~Date, y = ~Violations, type = "scatter", mode = "lines") %>%
      layout(title = "Rest Violations Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Violations"))
  })

  output$rest_violations_table <- renderDT({
    df <- filtered_data()
    if(is.null(df)) return(datatable(data.frame(Message = "No data available")))

    datatable(head(df[rpv_shift > 0], 1000), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })

  # === 7. SHIFT HOURS ANALYSIS ===
  output$avg_shift_length <- renderInfoBox({
    df <- filtered_data()
    avg <- if(!is.null(df) && "shift_hrs" %in% names(df)) mean(df$shift_hrs, na.rm = TRUE) else 0
    infoBox("Avg Shift Length", paste0(round(avg, 2), " hrs"), icon = icon("clock"), color = "blue")
  })

  output$total_shifts_analyzed <- renderInfoBox({
    df <- filtered_data()
    count <- if(!is.null(df)) nrow(df) else 0
    infoBox("Shifts Analyzed", format(count, big.mark = ","), icon = icon("calendar-check"), color = "green")
  })

  output$shifts_over_8hrs <- renderInfoBox({
    df <- filtered_data()
    count <- if(!is.null(df) && "shift_hrs" %in% names(df)) sum(df$shift_hrs > 8, na.rm = TRUE) else 0
    infoBox("Shifts >8 Hours", format(count, big.mark = ","), icon = icon("hourglass-end"), color = "orange")
  })

  output$shift_length_dist <- renderPlotly({
    df <- filtered_data()
    if(is.null(df) || !"shift_hrs" %in% names(df)) return(plotly_empty())

    plot_ly(x = df$shift_hrs, type = "histogram", nbinsx = 30) %>%
      layout(title = "Shift Length Distribution",
             xaxis = list(title = "Shift Hours"),
             yaxis = list(title = "Count"))
  })

  output$shift_dow_chart <- renderPlotly({
    df <- filtered_data()
    if(is.null(df) || !"Date" %in% names(df)) return(plotly_empty())

    df$dow <- weekdays(as.Date(df$Date))
    dow_counts <- table(df$dow)

    plot_ly(x = names(dow_counts), y = as.numeric(dow_counts), type = "bar") %>%
      layout(title = "Shifts by Day of Week",
             xaxis = list(title = "Day"),
             yaxis = list(title = "Count"))
  })

  output$shift_hours_table <- renderDT({
    df <- filtered_data()
    if(is.null(df)) return(datatable(data.frame(Message = "No data available")))

    datatable(head(df, 1000), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })

  # === 8. ROUNDING ANALYSIS ===
  output$total_rounding_impact <- renderValueBox({
    valueBox("Coming Soon", "Rounding Impact", icon = icon("balance-scale"), color = "blue")
  })

  output$shifts_rounded_up <- renderValueBox({
    valueBox("Coming Soon", "Rounded Up", icon = icon("arrow-up"), color = "green")
  })

  output$shifts_rounded_down <- renderValueBox({
    valueBox("Coming Soon", "Rounded Down", icon = icon("arrow-down"), color = "red")
  })

  output$rounding_dist <- renderPlotly({
    plot_ly(type = "histogram") %>%
      layout(title = "Coming Soon: Rounding Distribution")
  })

  output$rounding_trend <- renderPlotly({
    plot_ly(type = "scatter", mode = "lines") %>%
      layout(title = "Coming Soon: Rounding Trend")
  })

  output$rounding_table <- renderDT({
    datatable(data.frame(Message = "Coming Soon: Rounding detail by employee"),
              options = list(pageLength = 25), rownames = FALSE)
  })

  # === 9. PAY SUMMARY ===
  output$total_pay_amount <- renderValueBox({
    df <- data$pay
    amount <- if(!is.null(df) && "Pay_Amount" %in% names(df)) sum(df$Pay_Amount, na.rm = TRUE) else 0
    valueBox(paste0("$", format(round(amount, 0), big.mark = ",")), "Total Pay", icon = icon("money-bill"), color = "green")
  })

  output$total_pay_hours <- renderValueBox({
    df <- data$pay
    hours <- if(!is.null(df) && "Pay_Hours" %in% names(df)) sum(df$Pay_Hours, na.rm = TRUE) else 0
    valueBox(format(round(hours, 0), big.mark = ","), "Total Pay Hours", icon = icon("clock"), color = "blue")
  })

  output$avg_hourly_rate <- renderValueBox({
    df <- data$pay
    if(!is.null(df) && "Pay_Amount" %in% names(df) && "Pay_Hours" %in% names(df)) {
      avg <- sum(df$Pay_Amount, na.rm = TRUE) / sum(df$Pay_Hours, na.rm = TRUE)
    } else {
      avg <- 0
    }
    valueBox(paste0("$", round(avg, 2)), "Avg Rate", icon = icon("calculator"), color = "aqua")
  })

  output$total_pay_periods <- renderValueBox({
    df <- data$pay
    count <- if(!is.null(df) && "Pay_Period_End" %in% names(df)) uniqueN(df$Pay_Period_End) else 0
    valueBox(format(count, big.mark = ","), "Pay Periods", icon = icon("calendar-alt"), color = "purple")
  })

  output$pay_by_category <- renderPlotly({
    plot_ly(type = "bar") %>%
      layout(title = "Coming Soon: Pay by Category")
  })

  output$pay_trend <- renderPlotly({
    df <- data$pay
    if(is.null(df) || !"Pay_Period_End" %in% names(df)) return(plotly_empty())

    trend <- df[, .(Pay = sum(Pay_Amount, na.rm = TRUE)), by = Pay_Period_End]

    plot_ly(trend, x = ~Pay_Period_End, y = ~Pay, type = "scatter", mode = "lines") %>%
      layout(title = "Pay Over Time",
             xaxis = list(title = "Pay Period"),
             yaxis = list(title = "Total Pay ($)"))
  })

  output$pay_summary_table <- renderDT({
    df <- data$pay
    if(is.null(df)) return(datatable(data.frame(Message = "No data available")))

    datatable(head(df, 1000), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })

  # === 10. BONUSES & DIFFS ===
  output$total_bonuses <- renderValueBox({
    valueBox("Coming Soon", "Total Bonuses", icon = icon("gift"), color = "yellow")
  })

  output$total_diffs <- renderValueBox({
    valueBox("Coming Soon", "Total Diffs", icon = icon("not-equal"), color = "orange")
  })

  output$employees_with_bonuses <- renderValueBox({
    valueBox("Coming Soon", "EEs with Bonuses", icon = icon("users"), color = "blue")
  })

  output$bonus_trend <- renderPlotly({
    plot_ly(type = "scatter", mode = "lines") %>%
      layout(title = "Coming Soon: Bonus Trend")
  })

  output$bonus_type_dist <- renderPlotly({
    plot_ly(type = "bar") %>%
      layout(title = "Coming Soon: Bonus Type Distribution")
  })

  output$diffs_table <- renderDT({
    datatable(data.frame(Message = "Coming Soon: Pay differences analysis"),
              options = list(pageLength = 25), rownames = FALSE)
  })

  # === 11. REGULAR RATE GROUPS ===
  output$rrop_categories <- renderInfoBox({
    infoBox("RROP Categories", "Coming Soon", icon = icon("list"), color = "blue")
  })

  output$rrop_includable <- renderInfoBox({
    infoBox("Includable Pay", "Coming Soon", icon = icon("check-circle"), color = "green")
  })

  output$rrop_excludable <- renderInfoBox({
    infoBox("Excludable Pay", "Coming Soon", icon = icon("times-circle"), color = "red")
  })

  output$rrop_category_chart <- renderPlotly({
    plot_ly(type = "bar") %>%
      layout(title = "Coming Soon: RROP Components")
  })

  output$rrop_table <- renderDT({
    datatable(data.frame(Message = "Coming Soon: Regular rate groups detail"),
              options = list(pageLength = 25), rownames = FALSE)
  })

}

# Run the app
shinyApp(ui = ui, server = server)
