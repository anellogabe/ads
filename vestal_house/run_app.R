# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# Launcher — works from the repo root or from vestal_house/
if (basename(getwd()) != "vestal_house" && dir.exists("vestal_house")) {
  setwd("vestal_house")
}
shiny::runApp(".", launch.browser = TRUE)
