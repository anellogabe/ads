# ==============================================================================
# PROPRIETARY AND CONFIDENTIAL
# Anello Data Solutions LLC
#
# This file contains proprietary information and trade secrets.
# Unauthorized copying, distribution, or use is strictly prohibited.
# For authorized use by ANELLO DATA SOLUTIONS LLC contracted analysts only.
# ==============================================================================

# ==============================================================================
# ANALYSIS SCRIPT
# ==============================================================================
# PREREQUISITE: You MUST run clean_data.R first!
#
# This script generates metrics and reports for a case.
# It depends on the environment set up by clean_data.R:
#   - ADS functions loaded
#   - Case paths initialized
#   - Processed data available
# ==============================================================================

start.time <- Sys.time()

# ----- Verify Prerequisites ---------------------------------------------------

cat("\n==============================================================================\n")
cat("                    ADS CASE ANALYSIS\n")
cat("==============================================================================\n\n")

# Check that clean_data.R was run (environment should be set up)
if (!exists("paths") || is.null(paths)) {
  stop("\n\n❌ ERROR: Environment not initialized!\n\n",
       "You must run clean_data.R first.\n",
       "That script loads ADS functions and sets up case paths.\n\n")
}

if (!exists("init_case_paths") || !is.function(init_case_paths)) {
  stop("\n\n❌ ERROR: ADS functions not loaded!\n\n",
       "You must run clean_data.R first.\n",
       "That script sources ADS functions from the shared OneDrive folder.\n\n")
}

# Verify required directories exist
if (!dir.exists(paths$PROCESSED_DIR)) {
  stop("\n\n❌ ERROR: Processed data directory not found:\n  ", paths$PROCESSED_DIR,
       "\n\nPlease run clean_data.R first to create directory structure.\n\n")
}

cat("✓ Prerequisites verified\n")
cat("✓ Case directory:", paths$CASE_DIR, "\n\n")

# ----- Load Additional Packages -----------------------------------------------

library(zoo)  # For time series functions (not loaded in clean_data.R)

# ----- Load Processed Data ----------------------------------------------------

cat("==============================================================================\n")
cat("LOADING PROCESSED DATA\n")
cat("==============================================================================\n\n")

# Load processed data files created by clean_data.R
time_rds  <- file.path(paths$PROCESSED_DIR, "time_processed.rds")
pay_rds   <- file.path(paths$PROCESSED_DIR, "pay_processed.rds")
class_rds <- file.path(paths$PROCESSED_DIR, "class_processed.rds")

if (!file.exists(time_rds))  stop("Missing: ", time_rds, "\nRun clean_data.R first!")
if (!file.exists(pay_rds))   stop("Missing: ", pay_rds, "\nRun clean_data.R first!")
if (!file.exists(class_rds)) stop("Missing: ", class_rds, "\nRun clean_data.R first!")

time1  <- readRDS(time_rds)
pay1   <- readRDS(pay_rds)
class1 <- readRDS(class_rds)

cat("✓ Loaded processed data from:", paths$PROCESSED_DIR, "\n")
cat("  - Time records:", prettyNum(nrow(time1), big.mark = ","), "\n")
cat("  - Pay records:", prettyNum(nrow(pay1), big.mark = ","), "\n")
cat("  - Class members:", prettyNum(nrow(class1), big.mark = ","), "\n\n")

# All ADS functions and paths are already available from clean_data.R environment
# Now proceed with your analysis...
