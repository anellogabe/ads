# run_analysis.R - Master Run Script
# ============================================================================
# MASTER RUN SCRIPT - Complete Wage & Hour Analysis Pipeline
# ============================================================================

# Setup -----------------------------------------------------------------------
cat("Starting Wage & Hour Analysis Pipeline\n")
cat("=====================================\n\n")

# Load configuration
source("config/config.R")

# Check that data files exist
if(length(list.files("data/raw/payroll/")) == 0) {
  stop("No files found in data/raw/payroll/. Please add your payroll data files.")
}

if(length(list.files("data/raw/timekeeping/")) == 0) {
  warning("No files found in data/raw/timekeeping/. Time-based analyses will be skipped.")
}

# STEP 1: IMPORT DATA ---------------------------------------------------------
cat("\n[STEP 1] Importing Data\n")
cat("-----------------------\n")

# Load payroll data
source("R/01_data_import/load_payroll.R")

# Load time card data (if available)
if(file.exists("R/01_data_import/load_timecards.R")) {
  source("R/01_data_import/load_timecards.R")
}

cat("✓ Data import complete\n")

# STEP 2: DATA CLEANING -------------------------------------------------------
cat("\n[STEP 2] Cleaning Data\n")
cat("----------------------\n")

# Clean and standardize data
if(file.exists("R/02_data_cleaning/clean_payroll.R")) {
  source("R/02_data_cleaning/clean_payroll.R")
}

if(file.exists("R/02_data_cleaning/identify_shifts.R")) {
  source("R/02_data_cleaning/identify_shifts.R")
}

cat("✓ Data cleaning complete\n")

# STEP 3: ANALYSIS ------------------------------------------------------------
cat("\n[STEP 3] Running Analyses\n")
cat("-------------------------\n")

# Run analysis
cat("\nRunning Analysis...\n")
source("R/03_analysis/analysis.R")

# Save processed data for dashboard
saveRDS(time_dt, "data/processed/time_data.rds")
saveRDS(payroll_dt, "data/processed/payroll_data.rds")

cat("\n✓ All analyses complete\n")
cat("✓ Data saved for dashboard\n")

# STEP 4: LAUNCH DASHBOARD (Optional) ----------------------------------------
cat("\n[STEP 4] Dashboard Ready\n")
cat("-------------------------\n")
cat("To launch the dashboard, run:\n")
cat("  shiny::runApp('dashboard/app.R')\n")
cat("\nOr uncomment the line below to launch automatically:\n")
shiny::runApp("dashboard/app.R")
