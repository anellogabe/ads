# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Market analytics module: comps, value estimate, trends, indices ----

mod_market_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Projected Value (comp-based)",
                value = textOutput(ns("value_est")), theme = "primary"),
      value_box(title = "Basis $/sqft Percentile vs Sales",
                value = textOutput(ns("pct"))),
      value_box(title = "Est. Rent (rental scenario)",
                value = textOutput(ns("rent_est"))),
      value_box(title = "Comps Loaded", value = textOutput(ns("n_comps")))
    ),
    navset_card_tab(
      nav_panel("Price Trend", plotlyOutput(ns("trend"), height = 380)),
      nav_panel("$/sqft Distribution", plotlyOutput(ns("ppsf"), height = 380)),
      nav_panel("Days on Market", plotlyOutput(ns("dom"), height = 380)),
      nav_panel("Market Indices", plotlyOutput(ns("indices"), height = 380),
                uiOutput(ns("indices_note"))),
      nav_panel("Comps Table", DTOutput(ns("table")))
    )
  )
}

mod_market_server <- function(id, p) {
  moduleServer(id, function(input, output, session) {
    comps   <- reactive(load_comps("data/raw"))
    indices <- reactive(load_market_indices())

    output$value_est <- renderText({
      v <- estimate_value(comps(), p)
      if (is.na(v)) "n/a" else fmt_dollar(v)
    })
    output$rent_est <- renderText({
      v <- estimate_rent(comps(), p$property$sqft_finished)
      if (is.na(v)) "n/a" else paste0(fmt_dollar(v), "/mo")
    })
    output$pct <- renderText({
      v <- basis_percentile(comps(), total_basis(p), p$property$sqft_finished)
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

    output$indices <- renderPlotly({
      idx <- indices()
      validate(need(!is.null(idx) && nrow(idx),
                    "No index data yet — run scripts/fetch_market_data.R"))
      plot_ly(idx, x = ~as.Date(date), y = ~value,
              color = ~paste(source, region, sep = " — "),
              type = "scatter", mode = "lines") |>
        layout(xaxis = list(title = ""), yaxis = list(title = "Index / $"))
    })
    output$indices_note <- renderUI({
      if (is.null(indices())) {
        p(class = "text-muted small p-3",
          "Run scripts/fetch_market_data.R to pull Zillow ZHVI/ZORI for the ",
          "Vestal area and the FRED Broome County house price index. ",
          "See docs/market_data.md for all comp data sources.")
      }
    })

    output$table <- renderDT({
      datatable(comps(), rownames = FALSE, filter = "top",
                options = list(pageLength = 15, scrollX = TRUE))
    })
  })
}
