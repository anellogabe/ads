# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Investment model module: interactive cost/value model with live levers ----

mod_investment_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Model Levers",
      # params is global by the time UI builds (sourced in app.R)
      sliderInput(ns("psf"), "Core Cost $/sqft", min = 180, max = 400,
                  value = params$cost_basis$cost_per_sqft_core, step = 5, pre = "$"),
      sliderInput(ns("rate"), "Interest Rate", min = 0.03, max = 0.10,
                  value = params$financing$rate_annual, step = 0.00125),
      sliderInput(ns("ltc"), "Loan-to-Cost", min = 0, max = 0.85,
                  value = params$financing$ltc, step = 0.05),
      sliderInput(ns("rent"), "Monthly Rent (rental scenario)", min = 2000,
                  max = 8000, value = params$operating$rent_monthly,
                  step = 100, pre = "$")
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Total Project Basis", value = textOutput(ns("basis"))),
      value_box(title = "Cash Required (Equity)", value = textOutput(ns("equity"))),
      value_box(title = "Monthly Carrying Cost", value = textOutput(ns("carry"))),
      value_box(title = "Loan (P&I payment)", value = textOutput(ns("pi")))
    ),
    navset_card_tab(
      nav_panel("Budget by Category", plotlyOutput(ns("budget_chart"), height = 380),
                DTOutput(ns("budget_table"))),
      nav_panel("Carrying Cost", DTOutput(ns("carry_table"))),
      nav_panel("Amortization", DTOutput(ns("amort"))),
      nav_panel("Rental Scenario", DTOutput(ns("pf")),
                plotlyOutput(ns("cf_chart"), height = 320)),
      nav_panel("Sensitivity", plotlyOutput(ns("sens"), height = 380))
    )
  )
}

mod_investment_server <- function(id, p) {
  moduleServer(id, function(input, output, session) {

    # Params with UI overrides applied; the budget reflows from $/sqft
    p_live <- reactive({
      req(input$psf, input$rate, !is.null(input$ltc), input$rent)
      q <- p
      q$operating$rent_monthly <- input$rent
      q$financing$rate_annual  <- input$rate
      q$financing$ltc          <- input$ltc
      scale <- input$psf / p$cost_basis$cost_per_sqft_core
      q$cost_basis$cost_per_sqft_core <- input$psf
      # Rescale only the core-house lines (adders are area-priced separately)
      q$build <- copy(p$build)
      core_cats <- c("Site Work", "Foundation", "Framing", "Exterior Finish",
                     "Systems Rough-In", "Interior Finish", "Final Steps",
                     "Other", "Contingency")
      q$build[category %in% core_cats, budget := budget * scale]
      q
    })

    metrics <- reactive(model_metrics(p_live()))
    pf      <- reactive(pro_forma(p_live()))

    output$basis  <- renderText(fmt_dollar(metrics()$total_basis))
    output$equity <- renderText(fmt_dollar(metrics()$equity))
    output$carry  <- renderText(paste0(fmt_dollar(metrics()$carrying_monthly), "/mo"))
    output$pi <- renderText({
      q <- p_live()
      sched <- amortization_schedule(loan_amount(q), q$financing$rate_annual,
                                     q$financing$term_years)
      if (!nrow(sched)) "All cash" else paste0(fmt_dollar(sched$payment[1]), "/mo")
    })

    output$budget_chart <- renderPlotly({
      dt <- p_live()$build[, .(budget = sum(budget)), by = category][order(-budget)]
      plot_ly(dt, x = ~reorder(category, budget), y = ~budget, type = "bar") |>
        layout(xaxis = list(title = ""),
               yaxis = list(title = "$", tickformat = "$,.0f"))
    })

    output$budget_table <- renderDT({
      dt <- copy(p_live()$build)[order(phase, category)]
      dt[, budget := fmt_dollar(budget)]
      datatable(dt, rownames = FALSE, options = list(pageLength = 20))
    })

    output$carry_table <- renderDT({
      cc <- carrying_cost(p_live())
      dt <- data.table(
        component = c("Principal & Interest", "Property Taxes", "Insurance",
                      "Utilities", "Maintenance Reserve", "TOTAL"),
        monthly = fmt_dollar(c(cc$principal_interest, cc$taxes, cc$insurance,
                               cc$utilities, cc$maintenance, cc$total)),
        annual = fmt_dollar(12 * c(cc$principal_interest, cc$taxes, cc$insurance,
                                   cc$utilities, cc$maintenance, cc$total))
      )
      datatable(dt, rownames = FALSE, options = list(dom = "t"))
    })

    output$amort <- renderDT({
      q <- p_live()
      sched <- amortization_schedule(loan_amount(q), q$financing$rate_annual,
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
        sensitivity_irr(q, "build_overrun")
      ))
      plot_ly(sens, x = ~delta, y = ~irr, color = ~lever,
              type = "scatter", mode = "lines+markers") |>
        layout(title = "Rental-scenario IRR sensitivity",
               xaxis = list(title = "Change in lever", tickformat = ".0%"),
               yaxis = list(title = "IRR", tickformat = ".1%"))
    })
  })
}
