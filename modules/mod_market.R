# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Market analytics module: comps, trends, rent estimate ----

mod_market_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Estimated Rent (comp-based)",
                value = textOutput(ns("rent_est")),
                showcase = NULL, theme = "primary"),
      value_box(title = "Basis $/sqft Percentile vs Sales",
                value = textOutput(ns("pct"))),
      value_box(title = "Comps Loaded", value = textOutput(ns("n_comps")))
    ),
    navset_card_tab(
      nav_panel("Rent Trend", plotlyOutput(ns("trend"), height = 380)),
      nav_panel("$/sqft Distribution", plotlyOutput(ns("ppsf"), height = 380)),
      nav_panel("Days on Market", plotlyOutput(ns("dom"), height = 380)),
      nav_panel("Comps Table", DTOutput(ns("table")))
    )
  )
}

mod_market_server <- function(id, p) {
  moduleServer(id, function(input, output, session) {
    comps <- reactive(load_comps("data/raw"))

    output$rent_est <- renderText({
      v <- estimate_rent(comps(), p$property$sqft)
      if (is.na(v)) "n/a" else paste0(fmt_dollar(v), "/mo")
    })
    output$pct <- renderText({
      v <- basis_percentile(comps(), total_basis(p), p$property$sqft)
      if (is.na(v)) "n/a" else fmt_pct(v)
    })
    output$n_comps <- renderText({
      dt <- comps()
      demo <- all(dt$source == "demo")
      paste0(nrow(dt), if (demo) " (demo data)" else "")
    })

    output$trend <- renderPlotly({
      tr <- market_trend(comps())
      plot_ly(tr, x = ~month, y = ~median_ppsf, color = ~type,
              type = "scatter", mode = "lines+markers") |>
        layout(xaxis = list(title = ""),
               yaxis = list(title = "Median $/sqft"))
    })

    output$ppsf <- renderPlotly({
      dt <- comps()[sqft > 0][, ppsf := price / sqft]
      plot_ly(dt, x = ~ppsf, color = ~type, type = "histogram", nbinsx = 25) |>
        layout(barmode = "overlay", xaxis = list(title = "$/sqft"),
               yaxis = list(title = "Count"))
    })

    output$dom <- renderPlotly({
      dt <- comps()[!is.na(dom)]
      plot_ly(dt, y = ~as.numeric(dom), color = ~type, type = "box") |>
        layout(yaxis = list(title = "Days on Market"))
    })

    output$table <- renderDT({
      datatable(comps(), rownames = FALSE, filter = "top",
                options = list(pageLength = 15, scrollX = TRUE))
    })
  })
}
