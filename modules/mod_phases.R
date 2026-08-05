# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Phases module: incremental development strategy and gate reviews ----

mod_phases_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Incremental Development Plan"),
      p("Each phase is self-funded and must pass its gate before the next begins.",
        "Update phase status in config/params.R as gates are passed."),
      DTOutput(ns("plan"))
    ),
    card(
      card_header("Phase Budget Detail"),
      DTOutput(ns("detail"))
    ),
    card(
      card_header("Funding Runway"),
      plotlyOutput(ns("runway"), height = 340)
    )
  )
}

mod_phases_server <- function(id, p) {
  moduleServer(id, function(input, output, session) {
    summ <- reactive(phase_summary(p))

    output$plan <- renderDT({
      s <- copy(summ())
      s[, `:=`(budget = fmt_dollar(budget), actual = fmt_dollar(actual),
               remaining = fmt_dollar(remaining))]
      datatable(s, rownames = FALSE, options = list(dom = "t", pageLength = 10))
    })

    output$detail <- renderDT({
      dt <- copy(p$build)[order(phase, category)]
      dt[, budget := fmt_dollar(budget)]
      datatable(dt, rownames = FALSE, filter = "top",
                options = list(pageLength = 20))
    })

    output$runway <- renderPlotly({
      s <- summ()[order(phase)]
      s[, cumulative_budget := cumsum(budget)]
      plot_ly(s, x = ~paste("Phase", phase)) |>
        add_bars(y = ~budget, name = "Phase Budget") |>
        add_trace(y = ~cumulative_budget, name = "Cumulative",
                  type = "scatter", mode = "lines+markers") |>
        layout(yaxis = list(title = "$", tickformat = "$,.0f"))
    })
  })
}
