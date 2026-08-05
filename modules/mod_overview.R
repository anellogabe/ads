# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Overview module: headline KPIs and project status ----

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Total Project Basis", value = textOutput(ns("basis")),
                theme = "primary"),
      value_box(title = "Construction Budget", value = textOutput(ns("build")),
                theme = "secondary"),
      value_box(title = "Projected Value (comps)", value = textOutput(ns("value")),
                theme = "success"),
      value_box(title = "Cost-to-Value", value = textOutput(ns("ctv")),
                theme = "info")
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Cash Required (Equity)", value = textOutput(ns("equity"))),
      value_box(title = "Monthly Carrying Cost", value = textOutput(ns("carry"))),
      value_box(title = "Spent / Budget", value = textOutput(ns("spend"))),
      value_box(title = "Above-Grade Finished", value = textOutput(ns("sqft")))
    ),
    card(
      card_header("Phase Status"),
      DTOutput(ns("phases"))
    ),
    card(
      card_header("Property"),
      uiOutput(ns("property"))
    )
  )
}

mod_overview_server <- function(id, p) {
  moduleServer(id, function(input, output, session) {
    comps   <- reactive(load_comps("data/raw"))
    value   <- reactive(estimate_value(comps(), p))
    metrics <- reactive(model_metrics(p, projected_value = value()))
    ph      <- reactive(phase_summary(p))

    output$basis <- renderText(fmt_dollar(metrics()$total_basis))
    output$build <- renderText(fmt_dollar(metrics()$construction))
    output$value <- renderText({
      v <- value()
      if (is.na(v)) "n/a — load comps" else fmt_dollar(v)
    })
    output$ctv <- renderText({
      v <- metrics()$cost_to_value
      if (is.na(v)) "n/a" else sprintf("%.2f", v)
    })
    output$equity <- renderText(fmt_dollar(metrics()$equity))
    output$carry  <- renderText(paste0(fmt_dollar(metrics()$carrying_monthly), "/mo"))
    output$spend <- renderText({
      s <- ph()
      paste(fmt_dollar(sum(s$actual)), "/", fmt_dollar(sum(s$budget)))
    })
    output$sqft <- renderText(
      paste0(format(p$property$sqft_finished, big.mark = ","), " SF"))

    output$phases <- renderDT({
      s <- copy(ph())
      s[, `:=`(budget = fmt_dollar(budget), actual = fmt_dollar(actual),
               remaining = fmt_dollar(remaining))]
      datatable(s, rownames = FALSE, options = list(dom = "t", pageLength = 10))
    })

    output$property <- renderUI({
      pr <- p$property
      tags$dl(class = "row mb-0",
        tags$dt(class = "col-sm-3", "Address"),
        tags$dd(class = "col-sm-9", pr$address),
        tags$dt(class = "col-sm-3", "Plan set"),
        tags$dd(class = "col-sm-9", paste0(pr$plan_set, " — ", pr$architect)),
        tags$dt(class = "col-sm-3", "Program"),
        tags$dd(class = "col-sm-9", sprintf(
          "%s beds / %s baths / %s stories; %s SF above grade, %s SF basement (10 ft), %s SF three-car garage, ~%s SF porches & decks, %s SF outside kitchen",
          pr$beds, pr$baths, pr$stories,
          format(pr$sqft_finished, big.mark = ","),
          format(pr$sqft_basement, big.mark = ","),
          format(pr$sqft_garage, big.mark = ","),
          format(pr$sqft_porches, big.mark = ","),
          pr$sqft_outdoor_kitchen)),
        tags$dt(class = "col-sm-3", "School district"),
        tags$dd(class = "col-sm-9", pr$school_dist)
      )
    })
  })
}
