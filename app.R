# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Vestal House Dashboard ----
# Run from the repo root: shiny::runApp(".")

library(shiny)
library(bslib)
library(data.table)
library(lubridate)
library(DT)
library(plotly)
library(scales)

source("config/params.R")
source("R/functions.R")
source("R/investment_model.R")
source("R/market_analytics.R")
source("R/construction.R")
source("modules/mod_overview.R")
source("modules/mod_investment.R")
source("modules/mod_market.R")
source("modules/mod_construction.R")
source("modules/mod_phases.R")

validate_params(params)

ui <- page_navbar(
  title = paste0("Vestal House — ", params$property$address),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  nav_panel("Overview",     mod_overview_ui("overview")),
  nav_panel("Investment",   mod_investment_ui("investment")),
  nav_panel("Market",       mod_market_ui("market")),
  nav_panel("Construction", mod_construction_ui("construction")),
  nav_panel("Phases",       mod_phases_ui("phases"))
)

server <- function(input, output, session) {
  mod_overview_server("overview", params)
  mod_investment_server("investment", params)
  mod_market_server("market", params)
  mod_construction_server("construction", params)
  mod_phases_server("phases", params)
}

shinyApp(ui, server)
