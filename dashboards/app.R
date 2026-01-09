# Wage & Hour Dashboard - Cardona Case
# ==================================================

library(shiny)
library(shinydashboard)
library(data.table)
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
      menuItem("1. Time Data Summary", tabName = "time_summary", icon = icon("clock")),
      menuItem("2. Meal Period Analysis", tabName = "meal_analysis", icon = icon("utensils")),
      menuItem("3. Meal Violations >5 hrs", tabName = "meal_5hr", icon = icon("exclamation-triangle")),
      menuItem("4. Meal Violations >6 hrs", tabName = "meal_6hr", icon = icon("exclamation-circle")),
      menuItem("5. Rest Periods", tabName = "rest", icon = icon("pause")),
      menuItem("6. Shift Hours Analysis", tabName = "shift_hours", icon = icon("calendar-day")),
      menuItem("7. Rounding Analysis", tabName = "rounding", icon = icon("stopwatch")),
      menuItem("8. Pay Summary", tabName = "pay_summary", icon = icon("money-bill-wave")),
      menuItem("9. Bonuses & Diffs", tabName = "bonuses", icon = icon("gift")),
      menuItem("10. Regular Rate Groups", tabName = "rrop", icon = icon("calculator"))
    ),

    # Filters
    hr(),
    h4("Filters", style = "padding-left: 15px;"),

    dateRangeInput("date_range", "Date Range:",
                   start = if(!is.null(data$shift)) min(data$shift$Date, na.rm = TRUE) else Sys.Date() - 365,
                   end = if(!is.null(data$shift)) max(data$shift$Date, na.rm = TRUE) else Sys.Date()),

    selectInput("year_filter", "Year:",
                choices = c("All Years" = "all",
                           if(!is.null(data$shift)) sort(unique(year(data$shift$Date)), decreasing = TRUE) else "All"),
                selected = "all"),

    selectInput("key_gp_filter", "Key Group:",
                choices = c("All Groups" = "all",
                           if(!is.null(data$shift) && "Key_Gps" %in% names(data$shift)) sort(unique(data$shift$Key_Gps)) else "All"),
                selected = "all"),

    actionButton("refresh", "Refresh Data", icon = icon("sync"),
                 style = "margin: 15px;")
  ),

  # Body
  dashboardBody(
    tabItems(
      # 1. Time Data Summary ========================================
      tabItem(tabName = "time_summary",
              h2("Time Data Summary"),
              fluidRow(
                box(
                  title = "Time Metrics by Year", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("time_summary_table")
                )
              )
      ),

      # 2. Meal Period Analysis =====================================
      tabItem(tabName = "meal_analysis",
              h2("Meal Period Analysis"),
              fluidRow(
                box(
                  title = "Meal Period Metrics by Year", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("meal_analysis_table")
                )
              )
      ),

      # 3. Meal Violations >5 hrs (no waivers) ======================
      tabItem(tabName = "meal_5hr",
              h2("Meal Period Violations - Shifts >5 Hours (No Waivers)"),
              fluidRow(
                box(
                  title = "Meal Violations >5hrs by Year", status = "danger", solidHeader = TRUE, width = 12,
                  DTOutput("meal_5hr_table")
                )
              ),
              fluidRow(
                box(
                  title = "Violation Detail", status = "warning", solidHeader = TRUE, width = 12,
                  DTOutput("meal_5hr_detail")
                )
              )
      ),

      # 4. Meal Violations >6 hrs (waivers) =========================
      tabItem(tabName = "meal_6hr",
              h2("Meal Period Violations - Shifts >6 Hours (With Waivers)"),
              fluidRow(
                box(
                  title = "Meal Violations >6hrs by Year", status = "danger", solidHeader = TRUE, width = 12,
                  DTOutput("meal_6hr_table")
                )
              ),
              fluidRow(
                box(
                  title = "Violation Detail", status = "warning", solidHeader = TRUE, width = 12,
                  DTOutput("meal_6hr_detail")
                )
              )
      ),

      # 5. Rest Periods & Violations ================================
      tabItem(tabName = "rest",
              h2("Rest Period Analysis & Violations"),
              fluidRow(
                box(
                  title = "Rest Period Metrics by Year", status = "danger", solidHeader = TRUE, width = 12,
                  DTOutput("rest_table")
                )
              ),
              fluidRow(
                box(
                  title = "Rest Violation Detail", status = "warning", solidHeader = TRUE, width = 12,
                  DTOutput("rest_detail")
                )
              )
      ),

      # 6. Shift Hours Analysis =====================================
      tabItem(tabName = "shift_hours",
              h2("Shift Hours Analysis"),
              fluidRow(
                box(
                  title = "Shift Hours Metrics by Year", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("shift_hours_table")
                )
              ),
              fluidRow(
                box(
                  title = "Shift Hours Detail", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("shift_hours_detail")
                )
              )
      ),

      # 7. Rounding Analysis ========================================
      tabItem(tabName = "rounding",
              h2("Time Rounding Analysis"),
              fluidRow(
                box(
                  title = "Rounding Metrics by Year", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("rounding_table")
                )
              ),
              fluidRow(
                box(
                  title = "Rounding Detail by Employee", status = "warning", solidHeader = TRUE, width = 12,
                  DTOutput("rounding_detail")
                )
              )
      ),

      # 8. Pay Summary ==============================================
      tabItem(tabName = "pay_summary",
              h2("Pay Summary"),
              fluidRow(
                box(
                  title = "Pay Metrics by Year", status = "success", solidHeader = TRUE, width = 12,
                  DTOutput("pay_summary_table")
                )
              ),
              fluidRow(
                box(
                  title = "Pay Detail by Code", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("pay_detail")
                )
              )
      ),

      # 9. Bonuses & Diffs =========================================
      tabItem(tabName = "bonuses",
              h2("Bonuses & Pay Differences"),
              fluidRow(
                box(
                  title = "Bonus Metrics by Year", status = "success", solidHeader = TRUE, width = 12,
                  DTOutput("bonuses_table")
                )
              ),
              fluidRow(
                box(
                  title = "Bonus & Diff Detail", status = "warning", solidHeader = TRUE, width = 12,
                  DTOutput("bonuses_detail")
                )
              )
      ),

      # 10. Regular Rate Groups =====================================
      tabItem(tabName = "rrop",
              h2("Regular Rate of Pay (RROP) Analysis"),
              fluidRow(
                box(
                  title = "RROP Metrics by Year", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("rrop_table")
                )
              ),
              fluidRow(
                box(
                  title = "RROP Detail by Category", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("rrop_detail")
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

    # Year filter
    if(!is.null(input$year_filter) && input$year_filter != "all") {
      df <- df[year(Date) == as.numeric(input$year_filter)]
    }

    # Key Group filter
    if(!is.null(input$key_gp_filter) && input$key_gp_filter != "all" && "Key_Gps" %in% names(df)) {
      df <- df[Key_Gps == input$key_gp_filter]
    }

    df
  })

  # Helper function to calculate metrics for a dataset
  calc_time_metrics <- function(df) {
    if(is.null(df) || nrow(df) == 0) {
      return(list(
        employees = 0, shifts = 0, hours = 0,
        meal_violations = 0, rest_violations = 0,
        total_violations = 0, total_damages = 0, avg_damages = 0
      ))
    }

    mpv <- if("mpv_shift" %in% names(df)) sum(df$mpv_shift, na.rm = TRUE) else 0
    rpv <- if("rpv_shift" %in% names(df)) sum(df$rpv_shift, na.rm = TRUE) else 0
    mp_dmg <- if("mp_tot_dmgs" %in% names(df)) sum(df$mp_tot_dmgs, na.rm = TRUE) else 0
    rp_dmg <- if("rp_tot_dmgs" %in% names(df)) sum(df$rp_tot_dmgs, na.rm = TRUE) else 0
    total_dmg <- mp_dmg + rp_dmg
    n_ee <- if("ID" %in% names(df)) uniqueN(df$ID) else 1

    list(
      employees = uniqueN(df$ID),
      shifts = nrow(df),
      hours = sum(df$shift_hrs, na.rm = TRUE),
      meal_violations = mpv,
      rest_violations = rpv,
      total_violations = mpv + rpv,
      meal_damages = mp_dmg,
      rest_damages = rp_dmg,
      total_damages = total_dmg,
      avg_damages = total_dmg / n_ee
    )
  }

  # === 1. TIME DATA SUMMARY ===
  output$time_summary_table <- renderDT({
    req(data$shift)

    # Calculate metrics for all data
    all_metrics <- calc_time_metrics(data$shift)

    # Get all available years
    years <- sort(unique(year(data$shift$Date)), decreasing = TRUE)

    # Calculate metrics for each year
    year_metrics <- lapply(years, function(y) {
      year_df <- data$shift[year(Date) == y]
      calc_time_metrics(year_df)
    })
    names(year_metrics) <- years

    # Build summary table
    summary_df <- data.frame(
      Metric = c("Total Employees", "Total Shifts", "Total Hours Worked",
                 "Meal Period Violations", "Rest Period Violations",
                 "Total Violations", "Total Damages", "Avg Damages per Employee"),
      `All Data` = c(
        format(all_metrics$employees, big.mark = ","),
        format(all_metrics$shifts, big.mark = ","),
        format(round(all_metrics$hours, 0), big.mark = ","),
        format(round(all_metrics$meal_violations, 0), big.mark = ","),
        format(round(all_metrics$rest_violations, 0), big.mark = ","),
        format(round(all_metrics$total_violations, 0), big.mark = ","),
        paste0("$", format(round(all_metrics$total_damages, 0), big.mark = ",")),
        paste0("$", format(round(all_metrics$avg_damages, 0), big.mark = ","))
      ),
      check.names = FALSE
    )

    # Add columns for each year
    for(y in years) {
      ym <- year_metrics[[as.character(y)]]
      summary_df[[as.character(y)]] <- c(
        format(ym$employees, big.mark = ","),
        format(ym$shifts, big.mark = ","),
        format(round(ym$hours, 0), big.mark = ","),
        format(round(ym$meal_violations, 0), big.mark = ","),
        format(round(ym$rest_violations, 0), big.mark = ","),
        format(round(ym$total_violations, 0), big.mark = ","),
        paste0("$", format(round(ym$total_damages, 0), big.mark = ",")),
        paste0("$", format(round(ym$avg_damages, 0), big.mark = ","))
      )
    }

    datatable(summary_df, options = list(pageLength = 20, dom = 't', scrollX = TRUE), rownames = FALSE)
  })

  # === 2. MEAL PERIOD ANALYSIS ===
  output$meal_analysis_table <- renderDT({
    req(data$shift)

    # Calculate meal metrics for all data
    all_df <- data$shift
    all_meal_periods <- sum(!is.na(all_df$mp1_hrs))
    all_avg_duration <- mean(all_df$mp1_hrs, na.rm = TRUE) * 60  # Convert to minutes

    years <- sort(unique(year(data$shift$Date)), decreasing = TRUE)

    summary_df <- data.frame(
      Metric = c("Total Meal Periods", "Avg Meal Duration (mins)", "Meal Violations", "Meal Damages"),
      `All Data` = c(
        format(all_meal_periods, big.mark = ","),
        format(round(all_avg_duration, 1), big.mark = ","),
        format(round(sum(all_df$mpv_shift, na.rm = TRUE), 0), big.mark = ","),
        paste0("$", format(round(sum(all_df$mp_tot_dmgs, na.rm = TRUE), 0), big.mark = ","))
      ),
      check.names = FALSE
    )

    for(y in years) {
      year_df <- data$shift[year(Date) == y]
      meal_periods <- sum(!is.na(year_df$mp1_hrs))
      avg_duration <- mean(year_df$mp1_hrs, na.rm = TRUE) * 60

      summary_df[[as.character(y)]] <- c(
        format(meal_periods, big.mark = ","),
        format(round(avg_duration, 1), big.mark = ","),
        format(round(sum(year_df$mpv_shift, na.rm = TRUE), 0), big.mark = ","),
        paste0("$", format(round(sum(year_df$mp_tot_dmgs, na.rm = TRUE), 0), big.mark = ","))
      )
    }

    datatable(summary_df, options = list(pageLength = 20, dom = 't', scrollX = TRUE), rownames = FALSE)
  })

  # === 3. MEAL VIOLATIONS >5 HRS ===
  output$meal_5hr_table <- renderDT({
    req(data$shift)

    years <- sort(unique(year(data$shift$Date)), decreasing = TRUE)

    # All data >5hrs
    all_df <- data$shift[shift_hrs > 5 & shift_hrs <= 6]
    all_viols <- sum(all_df$mpv_shift, na.rm = TRUE)
    all_dmg <- sum(all_df$mp_tot_dmgs, na.rm = TRUE)

    summary_df <- data.frame(
      Metric = c("Total Violations >5hrs", "Total Damages >5hrs"),
      `All Data` = c(
        format(round(all_viols, 0), big.mark = ","),
        paste0("$", format(round(all_dmg, 0), big.mark = ","))
      ),
      check.names = FALSE
    )

    for(y in years) {
      year_df <- data$shift[year(Date) == y & shift_hrs > 5 & shift_hrs <= 6]
      summary_df[[as.character(y)]] <- c(
        format(round(sum(year_df$mpv_shift, na.rm = TRUE), 0), big.mark = ","),
        paste0("$", format(round(sum(year_df$mp_tot_dmgs, na.rm = TRUE), 0), big.mark = ","))
      )
    }

    datatable(summary_df, options = list(pageLength = 20, dom = 't', scrollX = TRUE), rownames = FALSE)
  })

  output$meal_5hr_detail <- renderDT({
    df <- filtered_data()
    if(is.null(df)) return(datatable(data.frame(Message = "No data available")))

    detail_df <- df[shift_hrs > 5 & shift_hrs <= 6 & mpv_shift > 0]
    if(nrow(detail_df) == 0) return(datatable(data.frame(Message = "No violations in filtered data")))

    # Select relevant columns including Key_Gps
    cols_to_show <- c("ID", "Date", "Key_Gps", "shift_hrs", "mp1_hrs", "hrs_to_mp1",
                      "mp1_mins_late", "mp1_mins_short", "mpv_shift", "mp_tot_dmgs")
    cols_available <- intersect(cols_to_show, names(detail_df))

    datatable(head(detail_df[, ..cols_available], 1000),
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE)
  })

  # === 4. MEAL VIOLATIONS >6 HRS ===
  output$meal_6hr_table <- renderDT({
    req(data$shift)

    years <- sort(unique(year(data$shift$Date)), decreasing = TRUE)

    # All data >6hrs
    all_df <- data$shift[shift_hrs > 6]
    all_viols <- sum(all_df$mpv_shift, na.rm = TRUE)
    all_dmg <- sum(all_df$mp_tot_dmgs, na.rm = TRUE)

    summary_df <- data.frame(
      Metric = c("Total Violations >6hrs", "Total Damages >6hrs"),
      `All Data` = c(
        format(round(all_viols, 0), big.mark = ","),
        paste0("$", format(round(all_dmg, 0), big.mark = ","))
      ),
      check.names = FALSE
    )

    for(y in years) {
      year_df <- data$shift[year(Date) == y & shift_hrs > 6]
      summary_df[[as.character(y)]] <- c(
        format(round(sum(year_df$mpv_shift, na.rm = TRUE), 0), big.mark = ","),
        paste0("$", format(round(sum(year_df$mp_tot_dmgs, na.rm = TRUE), 0), big.mark = ","))
      )
    }

    datatable(summary_df, options = list(pageLength = 20, dom = 't', scrollX = TRUE), rownames = FALSE)
  })

  output$meal_6hr_detail <- renderDT({
    df <- filtered_data()
    if(is.null(df)) return(datatable(data.frame(Message = "No data available")))

    detail_df <- df[shift_hrs > 6 & mpv_shift > 0]
    if(nrow(detail_df) == 0) return(datatable(data.frame(Message = "No violations in filtered data")))

    cols_to_show <- c("ID", "Date", "Key_Gps", "shift_hrs", "mp1_hrs", "hrs_to_mp1",
                      "mp1_mins_late", "mp1_mins_short", "mpv_shift", "mp_tot_dmgs")
    cols_available <- intersect(cols_to_show, names(detail_df))

    datatable(head(detail_df[, ..cols_available], 1000),
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE)
  })

  # === 5. REST PERIODS ===
  output$rest_table <- renderDT({
    req(data$shift)

    years <- sort(unique(year(data$shift$Date)), decreasing = TRUE)

    all_df <- data$shift
    all_rest_viols <- sum(all_df$rpv_shift, na.rm = TRUE)
    all_rest_dmg <- sum(all_df$rp_tot_dmgs, na.rm = TRUE)

    summary_df <- data.frame(
      Metric = c("Total Rest Violations", "Total Rest Damages"),
      `All Data` = c(
        format(round(all_rest_viols, 0), big.mark = ","),
        paste0("$", format(round(all_rest_dmg, 0), big.mark = ","))
      ),
      check.names = FALSE
    )

    for(y in years) {
      year_df <- data$shift[year(Date) == y]
      summary_df[[as.character(y)]] <- c(
        format(round(sum(year_df$rpv_shift, na.rm = TRUE), 0), big.mark = ","),
        paste0("$", format(round(sum(year_df$rp_tot_dmgs, na.rm = TRUE), 0), big.mark = ","))
      )
    }

    datatable(summary_df, options = list(pageLength = 20, dom = 't', scrollX = TRUE), rownames = FALSE)
  })

  output$rest_detail <- renderDT({
    df <- filtered_data()
    if(is.null(df)) return(datatable(data.frame(Message = "No data available")))

    detail_df <- df[rpv_shift > 0]
    if(nrow(detail_df) == 0) return(datatable(data.frame(Message = "No violations in filtered data")))

    cols_to_show <- c("ID", "Date", "Key_Gps", "shift_hrs", "rpv_shift", "rp_tot_dmgs")
    cols_available <- intersect(cols_to_show, names(detail_df))

    datatable(head(detail_df[, ..cols_available], 1000),
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE)
  })

  # === 6. SHIFT HOURS ANALYSIS ===
  output$shift_hours_table <- renderDT({
    req(data$shift)

    years <- sort(unique(year(data$shift$Date)), decreasing = TRUE)

    all_df <- data$shift
    all_avg <- mean(all_df$shift_hrs, na.rm = TRUE)
    all_over_8 <- sum(all_df$shift_hrs > 8, na.rm = TRUE)

    summary_df <- data.frame(
      Metric = c("Total Shifts", "Avg Shift Length (hrs)", "Shifts >8 Hours"),
      `All Data` = c(
        format(nrow(all_df), big.mark = ","),
        format(round(all_avg, 2), big.mark = ","),
        format(all_over_8, big.mark = ",")
      ),
      check.names = FALSE
    )

    for(y in years) {
      year_df <- data$shift[year(Date) == y]
      summary_df[[as.character(y)]] <- c(
        format(nrow(year_df), big.mark = ","),
        format(round(mean(year_df$shift_hrs, na.rm = TRUE), 2), big.mark = ","),
        format(sum(year_df$shift_hrs > 8, na.rm = TRUE), big.mark = ",")
      )
    }

    datatable(summary_df, options = list(pageLength = 20, dom = 't', scrollX = TRUE), rownames = FALSE)
  })

  output$shift_hours_detail <- renderDT({
    df <- filtered_data()
    if(is.null(df)) return(datatable(data.frame(Message = "No data available")))

    cols_to_show <- c("ID", "Date", "Key_Gps", "shift_hrs", "Hours", "In", "Out")
    cols_available <- intersect(cols_to_show, names(df))

    datatable(head(df[, ..cols_available], 1000),
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE)
  })

  # === 7. ROUNDING ANALYSIS ===
  output$rounding_table <- renderDT({
    datatable(data.frame(Metric = "Coming Soon", `All Data` = "TBD", check.names = FALSE),
              options = list(dom = 't'), rownames = FALSE)
  })

  output$rounding_detail <- renderDT({
    datatable(data.frame(Message = "Coming Soon: Rounding detail by employee"),
              options = list(pageLength = 25), rownames = FALSE)
  })

  # === 8. PAY SUMMARY ===
  output$pay_summary_table <- renderDT({
    req(data$pay)

    years <- sort(unique(year(data$pay$Pay_Period_End)), decreasing = TRUE)

    all_df <- data$pay
    all_amount <- sum(all_df$Pay_Amount, na.rm = TRUE)
    all_hours <- sum(all_df$Pay_Hours, na.rm = TRUE)
    all_avg_rate <- all_amount / all_hours

    summary_df <- data.frame(
      Metric = c("Total Pay Amount", "Total Pay Hours", "Avg Hourly Rate"),
      `All Data` = c(
        paste0("$", format(round(all_amount, 0), big.mark = ",")),
        format(round(all_hours, 0), big.mark = ","),
        paste0("$", format(round(all_avg_rate, 2), big.mark = ","))
      ),
      check.names = FALSE
    )

    for(y in years) {
      year_df <- data$pay[year(Pay_Period_End) == y]
      y_amount <- sum(year_df$Pay_Amount, na.rm = TRUE)
      y_hours <- sum(year_df$Pay_Hours, na.rm = TRUE)
      y_rate <- y_amount / y_hours

      summary_df[[as.character(y)]] <- c(
        paste0("$", format(round(y_amount, 0), big.mark = ",")),
        format(round(y_hours, 0), big.mark = ","),
        paste0("$", format(round(y_rate, 2), big.mark = ","))
      )
    }

    datatable(summary_df, options = list(pageLength = 20, dom = 't', scrollX = TRUE), rownames = FALSE)
  })

  output$pay_detail <- renderDT({
    df <- data$pay
    if(is.null(df)) return(datatable(data.frame(Message = "No data available")))

    datatable(head(df, 1000),
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE)
  })

  # === 9. BONUSES & DIFFS ===
  output$bonuses_table <- renderDT({
    datatable(data.frame(Metric = "Coming Soon", `All Data` = "TBD", check.names = FALSE),
              options = list(dom = 't'), rownames = FALSE)
  })

  output$bonuses_detail <- renderDT({
    datatable(data.frame(Message = "Coming Soon: Bonus & diff detail"),
              options = list(pageLength = 25), rownames = FALSE)
  })

  # === 10. REGULAR RATE GROUPS ===
  output$rrop_table <- renderDT({
    datatable(data.frame(Metric = "Coming Soon", `All Data` = "TBD", check.names = FALSE),
              options = list(dom = 't'), rownames = FALSE)
  })

  output$rrop_detail <- renderDT({
    datatable(data.frame(Message = "Coming Soon: RROP detail by category"),
              options = list(pageLength = 25), rownames = FALSE)
  })

}

# Run the app
shinyApp(ui = ui, server = server)
