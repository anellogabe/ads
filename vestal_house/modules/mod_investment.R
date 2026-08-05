# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Investment model module: interactive pro forma with live levers ----

mod_investment_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Model Levers",
      # params is global by the time UI builds (sourced in app.R)
      sliderInput(ns("rent"), "Monthly Rent", min = 1000, max = 3000,
                  value = params$operating$rent_monthly, step = 25, pre = "$"),
      sliderInput(ns("rate"), "Interest Rate", min = 0.03, max = 0.10,
                  value = params$financing$rate_annual, step = 0.00125),
      sliderInput(ns("overrun"), "Rehab Overrun", min = -0.2, max = 0.5,
                  value = 0, step = 0.05),
      sliderInput(ns("ltv"), "LTV", min = 0, max = 0.85,
                  value = params$financing$ltv, step = 0.05)
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Year-1 Cash Flow", value = textOutput(ns("cf"))),
      value_box(title = "Levered IRR", value = textOutput(ns("irr"))),
      value_box(title = paste0("NPV @ hurdle"), value = textOutput(ns("npv")))
    ),
    navset_card_tab(
      nav_panel("Pro Forma", DTOutput(ns("pf"))),
      nav_panel("Cash Flow Chart", plotlyOutput(ns("cf_chart"), height = 380)),
      nav_panel("Sensitivity", plotlyOutput(ns("sens"), height = 380)),
      nav_panel("Amortization", DTOutput(ns("amort")))
    )
  )
}

mod_investment_server <- function(id, p) {
  moduleServer(id, function(input, output, session) {

    # Params with UI overrides applied
    p_live <- reactive({
      req(input$rent, input$rate, !is.null(input$ltv))
      q <- p
      q$operating$rent_monthly <- input$rent
      q$financing$rate_annual  <- input$rate
      q$financing$ltv          <- input$ltv
      q$rehab <- copy(p$rehab)[, budget := budget * (1 + input$overrun)]
      q
    })

    metrics <- reactive(model_metrics(p_live()))
    pf      <- reactive(pro_forma(p_live()))

    output$cf  <- renderText(fmt_dollar(metrics()$year1_cf))
    output$irr <- renderText({
      v <- metrics()$irr; if (is.na(v)) "n/a" else fmt_pct(v)
    })
    output$npv <- renderText(fmt_dollar(metrics()$npv))

    output$pf <- renderDT({
      dt <- copy(pf())
      money_cols <- setdiff(names(dt), "year")
      dt[, (money_cols) := lapply(.SD, function(x) fmt_dollar(x)), .SDcols = money_cols]
      datatable(dt, rownames = FALSE, options = list(dom = "t", pageLength = 15))
    })

    output$cf_chart <- renderPlotly({
      dt <- pf()
      plot_ly(dt, x = ~year) |>
        add_bars(y = ~noi, name = "NOI") |>
        add_bars(y = ~cash_flow, name = "Cash Flow (after debt)") |>
        layout(barmode = "group", xaxis = list(title = "Year"),
               yaxis = list(title = "$ / year", tickformat = "$,.0f"))
    })

    output$sens <- renderPlotly({
      q <- p_live()
      sens <- rbindlist(list(
        sensitivity_irr(q, "rent_monthly"),
        sensitivity_irr(q, "rate_annual"),
        sensitivity_irr(q, "rehab_overrun")
      ))
      plot_ly(sens, x = ~delta, y = ~irr, color = ~lever,
              type = "scatter", mode = "lines+markers") |>
        layout(xaxis = list(title = "Change in lever", tickformat = ".0%"),
               yaxis = list(title = "IRR", tickformat = ".1%"))
    })

    output$amort <- renderDT({
      q <- p_live()
      loan <- q$acquisition$purchase_price * q$financing$ltv
      sched <- amortization_schedule(loan, q$financing$rate_annual,
                                     q$financing$term_years)
      if (!nrow(sched)) return(datatable(data.table(note = "All-cash: no loan")))
      yearly <- sched[, .(payment = sum(payment), interest = sum(interest),
                          principal_paid = sum(principal_paid),
                          balance = last(balance)),
                      by = .(year = ceiling(month / 12))]
      money_cols <- setdiff(names(yearly), "year")
      yearly[, (money_cols) := lapply(.SD, fmt_dollar), .SDcols = money_cols]
      datatable(yearly, rownames = FALSE, options = list(pageLength = 15))
    })
  })
}
