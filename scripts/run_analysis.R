# ==============================================================================
# PROPRIETARY AND CONFIDENTIAL
# Anello Data Solutions LLC
# 
# This file contains proprietary information and trade secrets.
# Unauthorized copying, distribution, or use is strictly prohibited.
# For authorized use by ANELLO DATA SOLUTIONS LLC contracted analysts only.
# ==============================================================================

# run_analysis.R (lives in ADS_REPO/scripts)
# ============================================================================
# CLEAN (case) -> ANALYSIS (case) -> DASHBOARD (repo)
# ============================================================================

cat("
╔══════════════════════════════════════════════════════════╗
║        WAGE & HOUR ANALYSIS DASHBOARD PIPELINE          ║
╚══════════════════════════════════════════════════════════╝
\n")

# ---- OPTIONS (TOGGLES) ------------------------------------------------------

RUN_CLEAN_DATA   <- FALSE
RUN_ANALYSIS     <- TRUE
VERIFY_OUTPUTS   <- TRUE
LAUNCH_DASHBOARD <- TRUE

# Set this per case (absolute path)
CASE_DIR_OVERRIDE <- "C:/Users/Gabe/OneDrive - anellodatasolutions.com/Documents/0. ADS/MWS/National Express/Analysis/MWS_NE"

# Dashboard location (in ADS repo)
# If your app.R is literally ADS_REPO/scripts/app.R, this is correct:
DASHBOARD_DIR_REL <- "scripts"
DASHBOARD_APP_REL <- file.path(DASHBOARD_DIR_REL, "app.R")

# ---- SETUP ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

# ---- LOAD ADS ENGINE (from ADS_REPO) ----------------------------------------

ADS_REPO <- Sys.getenv("ADS_REPO", unset = "")
if (!nzchar(ADS_REPO)) {
  stop("ADS_REPO env var not set. On Windows: setx ADS_REPO \"C:/Users/Gabe/Documents/GitHub/ads\"")
}
ADS_REPO <- normalizePath(ADS_REPO, winslash = "/", mustWork = TRUE)

engine_path <- file.path(ADS_REPO, "scripts", "functions.R")
if (!file.exists(engine_path)) stop("functions.R not found at: ", engine_path)

source(engine_path, local = FALSE, chdir = FALSE)

# ---- SET CASE DIR FOR THIS RUN ----------------------------------------------

if (!nzchar(CASE_DIR_OVERRIDE)) stop("CASE_DIR_OVERRIDE is blank. Set it at the top of run_analysis.R")

set_case_dir(CASE_DIR_OVERRIDE, set_globals = TRUE)   # sets ADS_CASE_DIR + CASE_DIR/RAW/PROCESSED/OUT globals
paths <- resolve_case_paths()

CASE_DIR      <- paths$CASE_DIR
RAW_DIR       <- paths$RAW_DIR
PROCESSED_DIR <- paths$PROCESSED_DIR
OUT_DIR       <- paths$OUT_DIR

SCRIPTS_DIR_CASE  <- file.path(CASE_DIR, "scripts")
clean_path_case   <- file.path(SCRIPTS_DIR_CASE, "clean_data.R")
analysis_path_case<- file.path(SCRIPTS_DIR_CASE, "analysis.R")
metrics_spec_case <- file.path(SCRIPTS_DIR_CASE, "metrics_spec.csv")

dashboard_dir_repo <- file.path(ADS_REPO, DASHBOARD_DIR_REL)
dashboard_app_repo <- file.path(ADS_REPO, DASHBOARD_APP_REL)

cat("\n[PATHS]\n")
cat("ADS_REPO        =", ADS_REPO, "\n")
cat("CASE_DIR        =", CASE_DIR, "\n")
cat("RAW_DIR         =", RAW_DIR, "\n")
cat("PROCESSED_DIR   =", PROCESSED_DIR, "\n")
cat("OUT_DIR         =", OUT_DIR, "\n")
cat("CASE scripts    =", SCRIPTS_DIR_CASE, "\n")
cat("Repo dashboard  =", dashboard_dir_repo, "\n")

# ---- STEP 1: CLEAN DATA (CASE) ----------------------------------------------

cat("\n[STEP 1] Data Cleaning & Processing (case)\n")
cat("═══════════════════════════════════════\n")

if (RUN_CLEAN_DATA) {
  if (!file.exists(clean_path_case)) stop("Missing clean_data.R at: ", clean_path_case)
  cat("Running:", clean_path_case, "\n")
  source(clean_path_case, local = FALSE, chdir = FALSE)
  cat("✓ Data cleaning complete\n")
} else {
  cat("Skipped (RUN_CLEAN_DATA = FALSE)\n")
}

# ---- STEP 2: ANALYSIS (CASE) ------------------------------------------------

cat("\n[STEP 2] Running Wage & Hour Analysis (case)\n")
cat("═══════════════════════════════════════\n")

if (RUN_ANALYSIS) {
  if (!file.exists(analysis_path_case)) stop("Missing analysis.R at: ", analysis_path_case)
  cat("Running:", analysis_path_case, "\n")
  source(analysis_path_case, local = FALSE, chdir = FALSE)
  cat("✓ Analysis complete\n")
} else {
  cat("Skipped (RUN_ANALYSIS = FALSE)\n")
}

# ---- STEP 3: VERIFY OUTPUTS -------------------------------------------------

cat("\n[STEP 3] Verifying Outputs\n")
cat("═══════════════════════════════════════\n")

if (VERIFY_OUTPUTS) {
  
  required_files <- c(
    file.path(PROCESSED_DIR, "time_processed.rds"),
    file.path(PROCESSED_DIR, "pay_processed.rds"),
    file.path(PROCESSED_DIR, "class_processed.rds"),
    file.path(OUT_DIR, "Time Shift Data.csv"),
    file.path(OUT_DIR, "Analysis.csv"),
    metrics_spec_case
  )
  
  missing <- required_files[!file.exists(required_files)]
  
  for (fp in required_files) {
    ok <- file.exists(fp)
    cat(if (ok) "✓" else "✗", fp, if (!ok) " (missing)" else "", "\n", sep = "")
  }
  
  if (length(missing)) {
    cat("\n⚠ Warning:", length(missing), "expected files are missing.\n")
  } else {
    cat("\n✓ All required files present\n")
  }
  
} else {
  cat("Skipped (VERIFY_OUTPUTS = FALSE)\n")
}

# ---- STEP 4: DASHBOARD (REPO) -----------------------------------------------

cat("\n[STEP 4] Dashboard (repo)\n")
cat("═══════════════════════════════════════\n")

if (LAUNCH_DASHBOARD) {
  
  if (!file.exists(dashboard_app_repo)) stop("Dashboard app.R not found at: ", dashboard_app_repo)
  cat("Launching dashboard from:", dashboard_dir_repo, "\n")
  shiny::runApp(dashboard_dir_repo)
  
} else {
  cat("Dashboard not launched (LAUNCH_DASHBOARD = FALSE)\n")
  cat("To launch manually:\n")
  cat("  shiny::runApp('", dashboard_dir_repo, "')\n", sep = "")
}

cat("\n✅ Pipeline complete!\n")
