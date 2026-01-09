# Wage & Hour Dashboard - Cardona Case
# ==================================================

library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(here)

# Load analysis data
load_data <- function() {
  tryCatch({
    if(file.exists(here("output/Analysis.csv"))) {
      fread(here("output/Analysis.csv"))
    } else {
      NULL
    }
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

    selectInput("ee_id", "Employee ID:",
                choices = c("All Employees" = "all"),
                selected = "all"),

    selectInput("class_job", "Class/Job:",
                choices = c("All Jobs" = "all"),
                selected = "all"),

    actionButton("refresh", "Refresh Data", icon = icon("sync"),
                 style = "margin: 15px;")
  ),

  # Body
  dashboardBody(
    tabItems(
      # 1. Time Data Summary
      tabItem(tabName = "time_summary",
              h2("Time Data Summary"),
              fluidRow(
                box(title = "Time Metrics", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("time_summary_table"))
              )
      ),

      # 2. Meal Period Analysis
      tabItem(tabName = "meal_analysis",
              h2("Meal Period Analysis"),
              fluidRow(
                box(title = "Meal Period Metrics", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("meal_analysis_table"))
              )
      ),

      # 3. Meal Violations >5 hrs
      tabItem(tabName = "meal_5hr",
              h2("Meal Period Violations - Shifts >5 Hours (No Waivers)"),
              fluidRow(
                box(title = "Meal Violations >5hrs", status = "danger", solidHeader = TRUE, width = 12,
                    DTOutput("meal_5hr_table"))
              )
      ),

      # 4. Meal Violations >6 hrs
      tabItem(tabName = "meal_6hr",
              h2("Meal Period Violations - Shifts >6 Hours (With Waivers)"),
              fluidRow(
                box(title = "Meal Violations >6hrs", status = "danger", solidHeader = TRUE, width = 12,
                    DTOutput("meal_6hr_table"))
              )
      ),

      # 5. Rest Periods
      tabItem(tabName = "rest",
              h2("Rest Period Analysis & Violations"),
              fluidRow(
                box(title = "Rest Period Metrics", status = "danger", solidHeader = TRUE, width = 12,
                    DTOutput("rest_table"))
              )
      ),

      # 6. Shift Hours Analysis
      tabItem(tabName = "shift_hours",
              h2("Shift Hours Analysis"),
              fluidRow(
                box(title = "Shift Hours Metrics", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("shift_hours_table"))
              )
      ),

      # 7. Rounding Analysis
      tabItem(tabName = "rounding",
              h2("Time Rounding Analysis"),
              fluidRow(
                box(title = "Rounding Metrics", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("rounding_table"))
              )
      ),

      # 8. Pay Summary
      tabItem(tabName = "pay_summary",
              h2("Pay Summary"),
              fluidRow(
                box(title = "Pay Metrics", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("pay_summary_table"))
              )
      ),

      # 9. Bonuses & Diffs
      tabItem(tabName = "bonuses",
              h2("Bonuses & Pay Differences"),
              fluidRow(
                box(title = "Bonus Metrics", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("bonuses_table"))
              )
      ),

      # 10. Regular Rate Groups
      tabItem(tabName = "rrop",
              h2("Regular Rate of Pay (RROP) Analysis"),
              fluidRow(
                box(title = "RROP Metrics", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("rrop_table"))
              )
      )
    )
  )
)

# SERVER ===============================================
server <- function(input, output, session) {

  # Update filter choices when data loads
  observe({
    req(data)

    # Update EE ID choices
    if("ID" %in% names(data)) {
      ee_ids <- c("All Employees" = "all", sort(unique(data$ID)))
      updateSelectInput(session, "ee_id", choices = ee_ids)
    }

    # Update Class/Job choices
    if("Class_Job" %in% names(data)) {
      jobs <- c("All Jobs" = "all", sort(unique(data$Class_Job)))
      updateSelectInput(session, "class_job", choices = jobs)
    }
  })

  # Filtered data based on selections
  filtered_data <- reactive({
    req(data)
    df <- data

    # Filter by EE ID
    if(input$ee_id != "all" && "ID" %in% names(df)) {
      df <- df[ID == input$ee_id]
    }

    # Filter by Class/Job
    if(input$class_job != "all" && "Class_Job" %in% names(df)) {
      df <- df[Class_Job == input$class_job]
    }

    df
  })

  # Helper function to filter by metric_group and render table
  render_by_group <- function(group_filter) {
    renderDT({
      df <- filtered_data()
      if(is.null(df)) return(datatable(data.frame(Message = "No data available")))

      # Filter by metric_group if column exists
      if("metric_group" %in% names(df) && !is.null(group_filter)) {
        df <- df[metric_group %in% group_filter]
      }

      # Hide metric_group column in display (users don't need to see it)
      if("metric_group" %in% names(df)) {
        df <- df[, !"metric_group"]
      }

      datatable(df,
                options = list(pageLength = 25, scrollX = TRUE),
                rownames = FALSE)
    })
  }

  # Map each page to its metric_group value(s)
  # Adjust these values to match your actual metric_group names in final_table

  output$time_summary_table <- render_by_group(c("Time Summary", "Overview", "Time"))

  output$meal_analysis_table <- render_by_group(c("Meal Period Analysis", "Meal Periods"))

  output$meal_5hr_table <- render_by_group(c("Meal Violations >5hrs", "MP >5hrs"))

  output$meal_6hr_table <- render_by_group(c("Meal Violations >6hrs", "MP >6hrs"))

  output$rest_table <- render_by_group(c("Rest Periods", "Rest Period Violations"))

  output$shift_hours_table <- render_by_group(c("Shift Hours", "Shift Analysis"))

  output$rounding_table <- render_by_group(c("Rounding", "Time Rounding"))

  output$pay_summary_table <- render_by_group(c("Pay Summary", "Pay"))

  output$bonuses_table <- render_by_group(c("Bonuses", "Pay Differences", "Bonuses & Diffs"))

  output$rrop_table <- render_by_group(c("Regular Rate", "RROP", "Regular Rate Groups"))

  # Refresh button
  observeEvent(input$refresh, {
    showNotification("Refreshing data...", type = "message")
    data <<- load_data()
    showNotification("Data refreshed!", type = "message", duration = 2)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
