# Wage & Hour Dashboard
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
      time = readRDS(here("data/processed/time_processed.rds")),
      pay = readRDS(here("data/processed/pay_processed.rds")),
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
  dashboardHeader(title = "Wage & Hour Dashboard"),

  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Meal & Rest Periods", tabName = "meal_rest", icon = icon("utensils")),
      menuItem("Overtime Analysis", tabName = "overtime", icon = icon("clock")),
      menuItem("Regular Rate", tabName = "regular_rate", icon = icon("calculator")),
      menuItem("Time Rounding", tabName = "rounding", icon = icon("stopwatch")),
      menuItem("Damages Summary", tabName = "damages", icon = icon("dollar-sign")),
      menuItem("Employee Detail", tabName = "employee", icon = icon("user")),
      menuItem("Data Tables", tabName = "tables", icon = icon("table"))
    ),

    # Filters
    hr(),
    h4("Filters", style = "padding-left: 15px;"),

    dateRangeInput("date_range", "Date Range:",
                   start = if(!is.null(data$shift)) min(data$shift$Date, na.rm = TRUE) else Sys.Date() - 365,
                   end = if(!is.null(data$shift)) max(data$shift$Date, na.rm = TRUE) else Sys.Date()),

    selectInput("key_group", "Key Group:",
                choices = c("All" = "all",
                           if(!is.null(data$shift)) unique(data$shift$Key_Gps) else "All"),
                selected = "all"),

    actionButton("refresh", "Refresh Data", icon = icon("sync"),
                 style = "margin: 15px;")
  ),

  # Body
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_employees", width = 3),
                valueBoxOutput("total_shifts", width = 3),
                valueBoxOutput("total_violations", width = 3),
                valueBoxOutput("total_damages", width = 3)
              ),
              fluidRow(
                box(
                  title = "Violations by Type", status = "primary", solidHeader = TRUE,
                  plotlyOutput("violations_chart", height = 300)
                ),
                box(
                  title = "Violations Over Time", status = "primary", solidHeader = TRUE,
                  plotlyOutput("violations_trend", height = 300)
                )
              ),
              fluidRow(
                box(
                  title = "Key Metrics Summary", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("metrics_summary")
                )
              )
      ),

      # Meal & Rest Periods Tab
      tabItem(tabName = "meal_rest",
              h2("Meal & Rest Period Analysis"),
              fluidRow(
                valueBoxOutput("mp_violations", width = 4),
                valueBoxOutput("rp_violations", width = 4),
                valueBoxOutput("mp_rate", width = 4)
              ),
              fluidRow(
                box(
                  title = "Meal Period Violations by Type", status = "warning", solidHeader = TRUE,
                  plotlyOutput("mp_violations_chart")
                ),
                box(
                  title = "Meal Period Duration Distribution", status = "info", solidHeader = TRUE,
                  plotlyOutput("mp_duration_chart")
                )
              ),
              fluidRow(
                box(
                  title = "Shift Hours vs Meal Periods", status = "primary", solidHeader = TRUE,
                  plotlyOutput("shift_mp_scatter")
                ),
                box(
                  title = "Meal Period Start Times", status = "success", solidHeader = TRUE,
                  plotlyOutput("mp_start_times")
                )
              )
      ),

      # Overtime Tab
      tabItem(tabName = "overtime",
              h2("Overtime & Double Time Analysis"),
              fluidRow(
                valueBoxOutput("ot_hours", width = 4),
                valueBoxOutput("dt_hours", width = 4),
                valueBoxOutput("ot_employees", width = 4)
              ),
              fluidRow(
                box(
                  title = "OT/DT Hours by Month", status = "primary", solidHeader = TRUE,
                  plotlyOutput("ot_trend")
                ),
                box(
                  title = "Shift Length Distribution", status = "info", solidHeader = TRUE,
                  plotlyOutput("shift_length_dist")
                )
              ),
              fluidRow(
                box(
                  title = "Top OT Employees", status = "warning", solidHeader = TRUE,
                  DTOutput("top_ot_employees")
                )
              )
      ),

      # Regular Rate Tab
      tabItem(tabName = "regular_rate",
              h2("Regular Rate of Pay Analysis"),
              fluidRow(
                valueBoxOutput("rrop_violations", width = 4),
                valueBoxOutput("rrop_underpayment", width = 4),
                valueBoxOutput("rrop_avg", width = 4)
              ),
              fluidRow(
                box(
                  title = "RROP Underpayments Over Time", status = "danger", solidHeader = TRUE,
                  plotlyOutput("rrop_trend")
                ),
                box(
                  title = "RROP Violation Types", status = "warning", solidHeader = TRUE,
                  plotlyOutput("rrop_types")
                )
              )
      ),

      # Time Rounding Tab
      tabItem(tabName = "rounding",
              h2("Time Rounding Analysis"),
              fluidRow(
                valueBoxOutput("shifts_analyzed", width = 4),
                valueBoxOutput("net_time_diff", width = 4),
                valueBoxOutput("rounding_pattern", width = 4)
              ),
              fluidRow(
                box(
                  title = "Time Difference Distribution", status = "primary", solidHeader = TRUE,
                  plotlyOutput("rounding_dist")
                ),
                box(
                  title = "Rounding Impact by Employee", status = "info", solidHeader = TRUE,
                  plotlyOutput("rounding_employee")
                )
              )
      ),

      # Damages Tab
      tabItem(tabName = "damages",
              h2("Damages Summary"),
              fluidRow(
                valueBoxOutput("total_mp_damages", width = 3),
                valueBoxOutput("total_rp_damages", width = 3),
                valueBoxOutput("total_ot_damages", width = 3),
                valueBoxOutput("total_all_damages", width = 3)
              ),
              fluidRow(
                box(
                  title = "Damages by Category", status = "danger", solidHeader = TRUE,
                  plotlyOutput("damages_breakdown")
                ),
                box(
                  title = "Damages by Employee (Top 20)", status = "warning", solidHeader = TRUE,
                  plotlyOutput("damages_employee")
                )
              ),
              fluidRow(
                box(
                  title = "Cumulative Damages Over Time", status = "info", solidHeader = TRUE, width = 12,
                  plotlyOutput("damages_cumulative")
                )
              )
      ),

      # Employee Detail Tab
      tabItem(tabName = "employee",
              h2("Employee Detail View"),
              fluidRow(
                box(
                  title = "Select Employee", status = "primary", solidHeader = TRUE, width = 12,
                  selectInput("employee_id", "Employee ID:",
                             choices = if(!is.null(data$shift)) sort(unique(data$shift$ID)) else "None")
                )
              ),
              fluidRow(
                valueBoxOutput("emp_shifts", width = 3),
                valueBoxOutput("emp_violations", width = 3),
                valueBoxOutput("emp_ot_hours", width = 3),
                valueBoxOutput("emp_damages", width = 3)
              ),
              fluidRow(
                box(
                  title = "Employee Shift History", status = "primary", solidHeader = TRUE,
                  plotlyOutput("emp_shift_history")
                ),
                box(
                  title = "Employee Violations Timeline", status = "warning", solidHeader = TRUE,
                  plotlyOutput("emp_violations_timeline")
                )
              ),
              fluidRow(
                box(
                  title = "Employee Detail Table", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("emp_detail_table")
                )
              )
      ),

      # Data Tables Tab
      tabItem(tabName = "tables",
              h2("Data Tables"),
              tabsetPanel(
                tabPanel("Analysis Table",
                        DTOutput("analysis_table")),
                tabPanel("Shift Data",
                        DTOutput("shift_table")),
                tabPanel("Pay Data",
                        DTOutput("pay_table")),
                tabPanel("Employee Summary",
                        DTOutput("employee_table"))
              )
      )
    )
  )
)

# Server ==========================================
server <- function(input, output, session) {

  # Reactive filtered data
  filtered_data <- reactive({
    req(data$shift)

    dt <- data$shift[Date >= input$date_range[1] & Date <= input$date_range[2]]

    if(input$key_group != "all") {
      dt <- dt[Key_Gps == input$key_group]
    }

    dt
  })

  # Overview Value Boxes
  output$total_employees <- renderValueBox({
    dt <- filtered_data()
    valueBox(
      uniqueN(dt$ID),
      "Total Employees",
      icon = icon("users"),
      color = "aqua"
    )
  })

  output$total_shifts <- renderValueBox({
    dt <- filtered_data()
    valueBox(
      format(nrow(dt), big.mark = ","),
      "Total Shifts",
      icon = icon("calendar"),
      color = "green"
    )
  })

  output$total_violations <- renderValueBox({
    dt <- filtered_data()
    total_v <- sum(dt$mpv_shift, na.rm = TRUE) + sum(dt$rpv_shift, na.rm = TRUE)
    valueBox(
      format(total_v, big.mark = ","),
      "Total Violations",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })

  output$total_damages <- renderValueBox({
    dt <- filtered_data()
    damages <- sum(dt$mp_tot_dmgs, na.rm = TRUE) + sum(dt$rp_tot_dmgs, na.rm = TRUE)
    valueBox(
      paste0("$", format(round(damages, 0), big.mark = ",")),
      "Total Damages",
      icon = icon("dollar-sign"),
      color = "red"
    )
  })

  # Violations Chart
  output$violations_chart <- renderPlotly({
    dt <- filtered_data()

    violations <- data.table(
      Type = c("Missed MP1", "Late MP1", "Short MP1", "Missed MP2", "Late MP2", "Short MP2", "Rest Period"),
      Count = c(
        sum(dt$MissMP1, na.rm = TRUE),
        sum(dt$LateMP1, na.rm = TRUE),
        sum(dt$ShortMP1, na.rm = TRUE),
        sum(dt$MissMP2, na.rm = TRUE),
        sum(dt$LateMP2, na.rm = TRUE),
        sum(dt$ShortMP2, na.rm = TRUE),
        sum(dt$rpv_shift, na.rm = TRUE)
      )
    )

    plot_ly(violations, x = ~Type, y = ~Count, type = "bar",
            marker = list(color = "steelblue")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Count"))
  })

  # Violations Trend
  output$violations_trend <- renderPlotly({
    dt <- filtered_data()

    trend <- dt[, .(Violations = sum(mpv_shift + rpv_shift, na.rm = TRUE)),
                by = .(Date = as.Date(Date))]
    setorder(trend, Date)

    plot_ly(trend, x = ~Date, y = ~Violations, type = "scatter", mode = "lines+markers",
            line = list(color = "red")) %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Violations"))
  })

  # Metrics Summary Table
  output$metrics_summary <- renderDT({
    req(data$analysis)

    datatable(data$analysis,
              options = list(pageLength = 15, scrollX = TRUE),
              class = "display nowrap")
  })

  # Meal & Rest Period Tab
  output$mp_violations <- renderValueBox({
    dt <- filtered_data()
    valueBox(
      format(sum(dt$mpv_shift, na.rm = TRUE), big.mark = ","),
      "Meal Period Violations",
      icon = icon("utensils"),
      color = "orange"
    )
  })

  output$rp_violations <- renderValueBox({
    dt <- filtered_data()
    valueBox(
      format(sum(dt$rpv_shift, na.rm = TRUE), big.mark = ","),
      "Rest Period Violations",
      icon = icon("coffee"),
      color = "orange"
    )
  })

  output$mp_rate <- renderValueBox({
    dt <- filtered_data()
    rate <- sum(dt$mpv_shift, na.rm = TRUE) / sum(dt$Shifts_gt_5, na.rm = TRUE) * 100
    valueBox(
      paste0(round(rate, 1), "%"),
      "MP Violation Rate",
      icon = icon("percent"),
      color = "red"
    )
  })

  output$mp_violations_chart <- renderPlotly({
    dt <- filtered_data()

    mp_types <- data.table(
      Type = c("Missed MP1", "Late MP1", "Short MP1", "Missed MP2", "Late MP2", "Short MP2"),
      Count = c(
        sum(dt$MissMP1, na.rm = TRUE),
        sum(dt$LateMP1, na.rm = TRUE),
        sum(dt$ShortMP1, na.rm = TRUE),
        sum(dt$MissMP2, na.rm = TRUE),
        sum(dt$LateMP2, na.rm = TRUE),
        sum(dt$ShortMP2, na.rm = TRUE)
      )
    )

    plot_ly(mp_types, labels = ~Type, values = ~Count, type = "pie") %>%
      layout(title = "")
  })

  output$mp_duration_chart <- renderPlotly({
    dt <- filtered_data()

    plot_ly(dt[mp1_hrs > 0], x = ~mp1_hrs, type = "histogram",
            marker = list(color = "steelblue")) %>%
      layout(xaxis = list(title = "Meal Period Duration (hours)"),
             yaxis = list(title = "Count"))
  })

  output$shift_mp_scatter <- renderPlotly({
    dt <- filtered_data()

    plot_ly(dt, x = ~shift_hrs, y = ~shift_mps, type = "scatter", mode = "markers",
            marker = list(size = 5, color = "steelblue")) %>%
      layout(xaxis = list(title = "Shift Hours"),
             yaxis = list(title = "Number of Meal Periods"))
  })

  output$mp_start_times <- renderPlotly({
    dt <- filtered_data()

    # Create hour bins for meal start times based on hrs_to_mp1
    dt_mp <- dt[hrs_to_mp1 > 0]

    if(nrow(dt_mp) > 0) {
      dt_mp[, mp_start_hour := floor(hrs_to_mp1)]

      mp_times <- dt_mp[, .N, by = mp_start_hour]
      setorder(mp_times, mp_start_hour)

      plot_ly(mp_times, x = ~mp_start_hour, y = ~N, type = "bar",
              marker = list(color = "green")) %>%
        layout(xaxis = list(title = "Hours into Shift"),
               yaxis = list(title = "Count"))
    } else {
      plotly_empty()
    }
  })

  # Overtime Tab
  output$ot_hours <- renderValueBox({
    dt <- filtered_data()
    valueBox(
      format(round(sum(dt$pp_daily_ot, na.rm = TRUE), 0), big.mark = ","),
      "Total OT Hours",
      icon = icon("clock"),
      color = "yellow"
    )
  })

  output$dt_hours <- renderValueBox({
    dt <- filtered_data()
    valueBox(
      format(round(sum(dt$pp_daily_dt, na.rm = TRUE), 0), big.mark = ","),
      "Total DT Hours",
      icon = icon("clock"),
      color = "red"
    )
  })

  output$ot_employees <- renderValueBox({
    dt <- filtered_data()
    valueBox(
      uniqueN(dt[pp_daily_ot > 0 | pp_daily_dt > 0]$ID),
      "Employees with OT/DT",
      icon = icon("users"),
      color = "orange"
    )
  })

  output$ot_trend <- renderPlotly({
    dt <- filtered_data()

    dt[, month := floor_date(Date, "month")]
    ot_monthly <- dt[, .(
      OT_Hours = sum(pp_daily_ot, na.rm = TRUE),
      DT_Hours = sum(pp_daily_dt, na.rm = TRUE)
    ), by = month]
    setorder(ot_monthly, month)

    plot_ly(ot_monthly) %>%
      add_trace(x = ~month, y = ~OT_Hours, type = "scatter", mode = "lines+markers",
                name = "OT Hours", line = list(color = "orange")) %>%
      add_trace(x = ~month, y = ~DT_Hours, type = "scatter", mode = "lines+markers",
                name = "DT Hours", line = list(color = "red")) %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Hours"))
  })

  output$shift_length_dist <- renderPlotly({
    dt <- filtered_data()

    plot_ly(dt, x = ~shift_hrs, type = "histogram",
            marker = list(color = "steelblue")) %>%
      layout(xaxis = list(title = "Shift Hours"),
             yaxis = list(title = "Count"))
  })

  output$top_ot_employees <- renderDT({
    dt <- filtered_data()

    top_ot <- dt[, .(
      Total_OT_Hours = sum(pp_daily_ot, na.rm = TRUE),
      Total_DT_Hours = sum(pp_daily_dt, na.rm = TRUE),
      Shifts = .N
    ), by = ID]
    setorder(top_ot, -Total_OT_Hours)

    datatable(head(top_ot, 20),
              options = list(pageLength = 20),
              class = "display nowrap")
  })

  # Regular Rate Tab
  output$rrop_violations <- renderValueBox({
    req(data$pay)
    dt <- data$pay[Pay_Period_End >= input$date_range[1] & Pay_Period_End <= input$date_range[2]]

    valueBox(
      format(sum(dt$rrop_any_underpayment, na.rm = TRUE), big.mark = ","),
      "RROP Violations",
      icon = icon("calculator"),
      color = "red"
    )
  })

  output$rrop_underpayment <- renderValueBox({
    req(data$pay)
    dt <- data$pay[Pay_Period_End >= input$date_range[1] & Pay_Period_End <= input$date_range[2]]

    valueBox(
      paste0("$", format(round(sum(dt$Net_Underpayment, na.rm = TRUE), 0), big.mark = ",")),
      "Total Underpayment",
      icon = icon("dollar-sign"),
      color = "red"
    )
  })

  output$rrop_avg <- renderValueBox({
    req(data$pay)
    dt <- data$pay[Pay_Period_End >= input$date_range[1] & Pay_Period_End <= input$date_range[2]]

    valueBox(
      paste0("$", round(mean(dt$RROP, na.rm = TRUE), 2)),
      "Average RROP",
      icon = icon("calculator"),
      color = "blue"
    )
  })

  output$rrop_trend <- renderPlotly({
    req(data$pay)
    dt <- data$pay[Pay_Period_End >= input$date_range[1] & Pay_Period_End <= input$date_range[2]]

    rrop_trend <- dt[, .(Underpayment = sum(Net_Underpayment, na.rm = TRUE)),
                     by = .(Pay_Date = as.Date(Pay_Date))]
    setorder(rrop_trend, Pay_Date)

    plot_ly(rrop_trend, x = ~Pay_Date, y = ~Underpayment, type = "scatter",
            mode = "lines+markers", line = list(color = "red")) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Underpayment ($)"))
  })

  output$rrop_types <- renderPlotly({
    req(data$pay)
    dt <- data$pay[Pay_Period_End >= input$date_range[1] & Pay_Period_End <= input$date_range[2]]

    rrop_types <- data.table(
      Type = c("OT Underpayment", "DT Underpayment", "Meal Underpayment",
               "Rest Underpayment", "Sick Underpayment"),
      Amount = c(
        sum(dt$OT_Underpayment, na.rm = TRUE),
        sum(dt$DT_Underpayment, na.rm = TRUE),
        sum(dt$Meal_Underpayment, na.rm = TRUE),
        sum(dt$Rest_Underpayment, na.rm = TRUE),
        sum(dt$Sick_Underpayment, na.rm = TRUE)
      )
    )

    plot_ly(rrop_types, x = ~Type, y = ~Amount, type = "bar",
            marker = list(color = "red")) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Underpayment ($)"))
  })

  # Time Rounding Tab
  output$shifts_analyzed <- renderValueBox({
    dt <- filtered_data()
    valueBox(
      format(sum(dt$shifts_analyzed, na.rm = TRUE), big.mark = ","),
      "Shifts Analyzed",
      icon = icon("stopwatch"),
      color = "blue"
    )
  })

  output$net_time_diff <- renderValueBox({
    dt <- filtered_data()
    diff_hours <- sum(dt$diff, na.rm = TRUE)
    valueBox(
      paste0(round(diff_hours, 1), " hrs"),
      "Net Time Difference",
      icon = icon("clock"),
      color = if(diff_hours < 0) "red" else "green"
    )
  })

  output$rounding_pattern <- renderValueBox({
    dt <- filtered_data()
    lost <- sum(dt$pp_lost_time_shifts, na.rm = TRUE)
    gained <- sum(dt$pp_gain_time_shifts, na.rm = TRUE)
    pattern <- if(lost > gained) "Loss" else "Gain"

    valueBox(
      pattern,
      "Dominant Pattern",
      icon = icon("chart-line"),
      color = if(pattern == "Loss") "red" else "green"
    )
  })

  output$rounding_dist <- renderPlotly({
    dt <- filtered_data()

    plot_ly(dt[!is.na(diff)], x = ~diff, type = "histogram",
            marker = list(color = "steelblue")) %>%
      layout(xaxis = list(title = "Time Difference (hours)"),
             yaxis = list(title = "Count"))
  })

  output$rounding_employee <- renderPlotly({
    dt <- filtered_data()

    emp_round <- dt[, .(Net_Diff = sum(diff, na.rm = TRUE)), by = ID]
    setorder(emp_round, -abs(Net_Diff))

    plot_ly(head(emp_round, 20), x = ~ID, y = ~Net_Diff, type = "bar",
            marker = list(color = ~ifelse(Net_Diff < 0, "red", "green"))) %>%
      layout(xaxis = list(title = "Employee ID"),
             yaxis = list(title = "Net Time Difference (hours)"))
  })

  # Damages Tab
  output$total_mp_damages <- renderValueBox({
    dt <- filtered_data()
    valueBox(
      paste0("$", format(round(sum(dt$mp_tot_dmgs, na.rm = TRUE), 0), big.mark = ",")),
      "Meal Period Damages",
      icon = icon("dollar-sign"),
      color = "red"
    )
  })

  output$total_rp_damages <- renderValueBox({
    dt <- filtered_data()
    valueBox(
      paste0("$", format(round(sum(dt$rp_tot_dmgs, na.rm = TRUE), 0), big.mark = ",")),
      "Rest Period Damages",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })

  output$total_ot_damages <- renderValueBox({
    req(data$pay)
    dt <- data$pay[Pay_Period_End >= input$date_range[1] & Pay_Period_End <= input$date_range[2]]

    valueBox(
      paste0("$", format(round(sum(dt$Net_Underpayment, na.rm = TRUE), 0), big.mark = ",")),
      "RROP Damages",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })

  output$total_all_damages <- renderValueBox({
    dt_shift <- filtered_data()
    dt_pay <- if(!is.null(data$pay)) {
      data$pay[Pay_Period_End >= input$date_range[1] & Pay_Period_End <= input$date_range[2]]
    } else {
      data.table()
    }

    total <- sum(dt_shift$mp_tot_dmgs, na.rm = TRUE) +
             sum(dt_shift$rp_tot_dmgs, na.rm = TRUE) +
             sum(dt_pay$Net_Underpayment, na.rm = TRUE)

    valueBox(
      paste0("$", format(round(total, 0), big.mark = ",")),
      "Total Damages",
      icon = icon("dollar-sign"),
      color = "red"
    )
  })

  output$damages_breakdown <- renderPlotly({
    dt_shift <- filtered_data()
    dt_pay <- if(!is.null(data$pay)) {
      data$pay[Pay_Period_End >= input$date_range[1] & Pay_Period_End <= input$date_range[2]]
    } else {
      data.table()
    }

    damages <- data.table(
      Category = c("Meal Period", "Rest Period", "RROP"),
      Amount = c(
        sum(dt_shift$mp_tot_dmgs, na.rm = TRUE),
        sum(dt_shift$rp_tot_dmgs, na.rm = TRUE),
        sum(dt_pay$Net_Underpayment, na.rm = TRUE)
      )
    )

    plot_ly(damages, labels = ~Category, values = ~Amount, type = "pie") %>%
      layout(title = "")
  })

  output$damages_employee <- renderPlotly({
    dt <- filtered_data()

    emp_dmgs <- dt[, .(
      MP_Damages = sum(mp_tot_dmgs, na.rm = TRUE),
      RP_Damages = sum(rp_tot_dmgs, na.rm = TRUE)
    ), by = ID]
    emp_dmgs[, Total := MP_Damages + RP_Damages]
    setorder(emp_dmgs, -Total)

    plot_ly(head(emp_dmgs, 20)) %>%
      add_trace(x = ~ID, y = ~MP_Damages, type = "bar", name = "Meal Period",
                marker = list(color = "red")) %>%
      add_trace(x = ~ID, y = ~RP_Damages, type = "bar", name = "Rest Period",
                marker = list(color = "orange")) %>%
      layout(xaxis = list(title = "Employee ID"),
             yaxis = list(title = "Damages ($)"),
             barmode = "stack")
  })

  output$damages_cumulative <- renderPlotly({
    dt <- filtered_data()

    daily_dmgs <- dt[, .(Damages = sum(mp_tot_dmgs + rp_tot_dmgs, na.rm = TRUE)),
                     by = Date]
    setorder(daily_dmgs, Date)
    daily_dmgs[, Cumulative := cumsum(Damages)]

    plot_ly(daily_dmgs, x = ~Date, y = ~Cumulative, type = "scatter",
            mode = "lines", fill = "tozeroy",
            line = list(color = "red")) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Cumulative Damages ($)"))
  })

  # Employee Detail Tab
  output$emp_shifts <- renderValueBox({
    req(input$employee_id)
    dt <- filtered_data()
    emp_dt <- dt[ID == input$employee_id]

    valueBox(
      nrow(emp_dt),
      "Total Shifts",
      icon = icon("calendar"),
      color = "blue"
    )
  })

  output$emp_violations <- renderValueBox({
    req(input$employee_id)
    dt <- filtered_data()
    emp_dt <- dt[ID == input$employee_id]

    valueBox(
      sum(emp_dt$mpv_shift, na.rm = TRUE) + sum(emp_dt$rpv_shift, na.rm = TRUE),
      "Violations",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })

  output$emp_ot_hours <- renderValueBox({
    req(input$employee_id)
    dt <- filtered_data()
    emp_dt <- dt[ID == input$employee_id]

    valueBox(
      round(sum(emp_dt$pp_daily_ot, na.rm = TRUE) + sum(emp_dt$pp_daily_dt, na.rm = TRUE), 1),
      "OT/DT Hours",
      icon = icon("clock"),
      color = "yellow"
    )
  })

  output$emp_damages <- renderValueBox({
    req(input$employee_id)
    dt <- filtered_data()
    emp_dt <- dt[ID == input$employee_id]

    valueBox(
      paste0("$", format(round(sum(emp_dt$mp_tot_dmgs + emp_dt$rp_tot_dmgs, na.rm = TRUE), 0),
                        big.mark = ",")),
      "Total Damages",
      icon = icon("dollar-sign"),
      color = "red"
    )
  })

  output$emp_shift_history <- renderPlotly({
    req(input$employee_id)
    dt <- filtered_data()
    emp_dt <- dt[ID == input$employee_id]

    plot_ly(emp_dt, x = ~Date, y = ~shift_hrs, type = "scatter",
            mode = "lines+markers",
            line = list(color = "steelblue")) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Shift Hours"))
  })

  output$emp_violations_timeline <- renderPlotly({
    req(input$employee_id)
    dt <- filtered_data()
    emp_dt <- dt[ID == input$employee_id]

    plot_ly(emp_dt, x = ~Date, y = ~(mpv_shift + rpv_shift), type = "scatter",
            mode = "markers",
            marker = list(color = "red", size = 10)) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Violations"))
  })

  output$emp_detail_table <- renderDT({
    req(input$employee_id)
    dt <- filtered_data()
    emp_dt <- dt[ID == input$employee_id]

    emp_display <- emp_dt[, .(
      Date, shift_hrs, shift_mps, mp1_hrs, hrs_to_mp1,
      MissMP1, LateMP1, ShortMP1, rpv_shift,
      pp_daily_ot, pp_daily_dt,
      mp_tot_dmgs, rp_tot_dmgs
    )]

    datatable(emp_display,
              options = list(pageLength = 25, scrollX = TRUE),
              class = "display nowrap") %>%
      formatRound(c("shift_hrs", "mp1_hrs", "hrs_to_mp1", "pp_daily_ot", "pp_daily_dt"), 2) %>%
      formatCurrency(c("mp_tot_dmgs", "rp_tot_dmgs"), "$")
  })

  # Data Tables
  output$analysis_table <- renderDT({
    req(data$analysis)
    datatable(data$analysis,
              options = list(pageLength = 25, scrollX = TRUE),
              class = "display nowrap")
  })

  output$shift_table <- renderDT({
    dt <- filtered_data()
    datatable(dt,
              options = list(pageLength = 25, scrollX = TRUE),
              class = "display nowrap")
  })

  output$pay_table <- renderDT({
    req(data$pay)
    dt <- data$pay[Pay_Period_End >= input$date_range[1] & Pay_Period_End <= input$date_range[2]]

    datatable(dt,
              options = list(pageLength = 25, scrollX = TRUE),
              class = "display nowrap")
  })

  output$employee_table <- renderDT({
    req(data$ee)
    datatable(data$ee,
              options = list(pageLength = 25, scrollX = TRUE),
              class = "display nowrap")
  })

  # Refresh button
  observeEvent(input$refresh, {
    showNotification("Refreshing data...", type = "message")
    data <<- load_data()
    showNotification("Data refreshed successfully!", type = "message", duration = 3)
  })
}

# Run the app
shinyApp(ui, server)
