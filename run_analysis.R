# run_analysis.R - Master Orchestration Script
# ============================================================================
# WAGE & HOUR ANALYSIS PIPELINE
# ============================================================================

cat("
╔══════════════════════════════════════════════════════════╗
║        WAGE & HOUR ANALYSIS DASHBOARD PIPELINE          ║
╚══════════════════════════════════════════════════════════╝
\n")

# Setup -----------------------------------------------------------------------
library(data.table)
library(lubridate)
library(here)

# Load configuration
if(file.exists(here("config/parameters.R"))) {
  source(here("config/parameters.R"))
  cat("✓ Configuration loaded\n")
} else {
  cat("⚠ No configuration file found, using defaults\n")
}

# STEP 1: DATA CLEANING -------------------------------------------------------
cat("\n[STEP 1] Data Cleaning & Processing\n")
cat("═══════════════════════════════════════\n")

if(file.exists(here("scripts/clean_data.R"))) {
  cat("Running clean_data.R...\n")
  source(here("scripts/clean_data.R"))
  cat("✓ Data cleaning complete\n")
} else {
  cat("✗ clean_data.R not found!\n")
  cat("  Please ensure scripts/clean_data.R exists\n")
  stop("Missing clean_data.R")
}

# STEP 2: ANALYSIS ------------------------------------------------------------
cat("\n[STEP 2] Running Wage & Hour Analysis\n")
cat("═══════════════════════════════════════\n")

if(file.exists(here("R/03_analysis/analysis.R"))) {
  cat("Running analysis.R...\n")
  source(here("R/03_analysis/analysis.R"))
  cat("✓ Analysis complete\n")
} else {
  cat("✗ analysis.R not found!\n")
  cat("  Please ensure R/03_analysis/analysis.R exists\n")
  stop("Missing analysis.R")
}

# STEP 3: VERIFY OUTPUTS ------------------------------------------------------
cat("\n[STEP 3] Verifying Outputs\n")
cat("═══════════════════════════════════════\n")

required_files <- c(
  "data/processed/time_processed.rds",
  "data/processed/pay_processed.rds",
  "output/Time Shift Data.csv",
  "output/Analysis.csv"
)

missing_files <- c()
for(file in required_files) {
  if(file.exists(here(file))) {
    cat("✓", file, "\n")
  } else {
    cat("✗", file, "(missing)\n")
    missing_files <- c(missing_files, file)
  }
}

if(length(missing_files) > 0) {
  cat("\n⚠ Warning:", length(missing_files), "expected files are missing\n")
  cat("  Dashboard may not function correctly\n")
} else {
  cat("\n✓ All required files present\n")
}

# STEP 4: SUMMARY -------------------------------------------------------------
cat("\n[STEP 4] Analysis Summary\n")
cat("═══════════════════════════════════════\n")

if(exists("shift_data1") && exists("pay1")) {
  cat("Employees analyzed:", uniqueN(shift_data1$ID), "\n")
  cat("Shifts analyzed:", format(nrow(shift_data1), big.mark = ","), "\n")
  cat("Pay periods analyzed:", uniqueN(pay1$Pay_ID_Period_End), "\n")

  if(exists("shift_data1") && "mpv_shift" %in% names(shift_data1)) {
    cat("\nViolations Summary:\n")
    cat("  Meal period violations:", format(sum(shift_data1$mpv_shift, na.rm = TRUE), big.mark = ","), "\n")
    cat("  Rest period violations:", format(sum(shift_data1$rpv_shift, na.rm = TRUE), big.mark = ","), "\n")
  }

  if(exists("shift_data1") && "mp_tot_dmgs" %in% names(shift_data1)) {
    cat("\nDamages Summary:\n")
    total_damages <- sum(shift_data1$mp_tot_dmgs, na.rm = TRUE) +
                     sum(shift_data1$rp_tot_dmgs, na.rm = TRUE)
    cat("  Total damages: $", format(round(total_damages, 0), big.mark = ","), "\n", sep = "")
  }
}

# STEP 5: LAUNCH DASHBOARD ----------------------------------------------------
cat("\n[STEP 5] Dashboard Ready\n")
cat("═══════════════════════════════════════\n")
cat("\n✅ Analysis pipeline complete!\n\n")

cat("To launch the dashboard, run:\n")
cat("  shiny::runApp('dashboard/app.R')\n\n")

cat("Or uncomment the line below to launch automatically:\n")
cat("# shiny::runApp(here('dashboard/app.R'))\n\n")

# Uncomment to auto-launch dashboard:
# shiny::runApp(here("dashboard/app.R"))
