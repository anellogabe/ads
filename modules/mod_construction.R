# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Construction management module: budget vs actual, tasks, spend timeline ----

mod_construction_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Total Rehab Budget", value = textOutput(ns("budget"))),
      value_box(title = "Spent to Date", value = textOutput(ns("spent"))),
      value_box(title = "Open Tasks", value = textOutput(ns("open_tasks")))
    ),
    navset_card_tab(
      nav_panel("Budget vs Actual", plotlyOutput(ns("bva_chart"), height = 380),
                DTOutput(ns("bva_table"))),
      nav_panel("Tasks", DTOutput(ns("tasks"))),
      nav_panel("Spend Timeline", plotlyOutput(ns("timeline"), height = 380)),
      nav_panel("Spend Log", DTOutput(ns("spend_log")))
    )
  )
}

mod_construction_server <- function(id, p) {
  moduleServer(id, function(input, output, session) {
    spend <- reactive(read_spend())
    tasks <- reactive(read_tasks())
    bva   <- reactive(budget_vs_actual(p$rehab, spend()))

    output$budget <- renderText(fmt_dollar(sum(p$rehab$budget)))
    output$spent  <- renderText(fmt_dollar(sum(spend()$amount)))
    output$open_tasks <- renderText({
      t <- tasks()
      if (!nrow(t)) "0" else as.character(t[status != "done", .N])
    })

    output$bva_chart <- renderPlotly({
      dt <- bva()[, .(budget = sum(budget), actual = sum(actual)),
                  by = .(phase = paste("Phase", phase))]
      plot_ly(dt, x = ~phase) |>
        add_bars(y = ~budget, name = "Budget") |>
        add_bars(y = ~actual, name = "Actual") |>
        layout(barmode = "group",
               yaxis = list(title = "$", tickformat = "$,.0f"))
    })

    output$bva_table <- renderDT({
      dt <- copy(bva())
      dt[, `:=`(budget = fmt_dollar(budget), actual = fmt_dollar(actual),
                variance = fmt_dollar(variance),
                pct_spent = fifelse(is.na(pct_spent), "", fmt_pct(pct_spent)))]
      datatable(dt, rownames = FALSE, options = list(pageLength = 20))
    })

    output$tasks <- renderDT({
      t <- tasks()
      if (!nrow(t)) {
        return(datatable(
          data.table(note = "No tasks logged yet — use log_task() (see R/construction.R)"),
          rownames = FALSE, options = list(dom = "t")))
      }
      datatable(t, rownames = FALSE, filter = "top",
                options = list(pageLength = 20, scrollX = TRUE))
    })

    output$timeline <- renderPlotly({
      s <- spend()
      if (!nrow(s)) {
        return(plotly_empty(type = "scatter", mode = "markers") |>
                 layout(title = "No spend logged yet — use log_spend()"))
      }
      dt <- copy(s)[, date := as.IDate(date)][order(date)]
      dt[, cumulative := cumsum(amount)]
      plot_ly(dt, x = ~date, y = ~cumulative, type = "scatter",
              mode = "lines+markers") |>
        layout(yaxis = list(title = "Cumulative Spend", tickformat = "$,.0f"))
    })

    output$spend_log <- renderDT({
      datatable(spend(), rownames = FALSE, filter = "top",
                options = list(pageLength = 20, scrollX = TRUE))
    })
  })
}
