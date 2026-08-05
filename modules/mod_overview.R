# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Overview module: headline KPIs and project status ----

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Total Basis", value = textOutput(ns("basis")),
                theme = "primary"),
      value_box(title = "Cash Invested (Equity)", value = textOutput(ns("equity")),
                theme = "secondary"),
      value_box(title = "Year-1 Cash Flow", value = textOutput(ns("cf")),
                theme = "success"),
      value_box(title = "Levered IRR", value = textOutput(ns("irr")),
                theme = "info")
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Cap Rate (on basis)", value = textOutput(ns("cap"))),
      value_box(title = "Cash-on-Cash", value = textOutput(ns("coc"))),
      value_box(title = "DSCR", value = textOutput(ns("dscr"))),
      value_box(title = "Rehab Spent / Budget", value = textOutput(ns("spend")))
    ),
    card(
      card_header("Phase Status"),
      DTOutput(ns("phases"))
    )
  )
}

mod_overview_server <- function(id, p) {
  moduleServer(id, function(input, output, session) {
    metrics <- reactive(model_metrics(p))
    ph <- reactive(phase_summary(p))

    output$basis  <- renderText(fmt_dollar(metrics()$total_basis))
    output$equity <- renderText(fmt_dollar(metrics()$equity))
    output$cf     <- renderText(fmt_dollar(metrics()$year1_cf))
    output$irr    <- renderText({
      v <- metrics()$irr
      if (is.na(v)) "n/a" else fmt_pct(v)
    })
    output$cap  <- renderText(fmt_pct(metrics()$cap_rate))
    output$coc  <- renderText(fmt_pct(metrics()$coc))
    output$dscr <- renderText(sprintf("%.2f", metrics()$dscr))
    output$spend <- renderText({
      s <- ph()
      paste(fmt_dollar(sum(s$actual)), "/", fmt_dollar(sum(s$budget)))
    })

    output$phases <- renderDT({
      s <- copy(ph())
      s[, `:=`(budget = fmt_dollar(budget), actual = fmt_dollar(actual),
               remaining = fmt_dollar(remaining))]
      datatable(s, rownames = FALSE, options = list(dom = "t", pageLength = 10))
    })
  })
}
