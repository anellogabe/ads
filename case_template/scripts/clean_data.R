# ==============================================================================
# PROPRIETARY AND CONFIDENTIAL
# Anello Data Solutions LLC
#
# This file contains proprietary information and trade secrets.
# Unauthorized copying, distribution, or use is strictly prohibited.
# For authorized use by ANELLO DATA SOLUTIONS LLC contracted analysts only.
# ==============================================================================

# ==============================================================================
# CLEAN DATA SCRIPT
# ==============================================================================
# This script MUST be run first for every case.
# It sets up the ADS environment, paths, and cleans raw data.
#
# After running this script, you can run analysis.R
# ==============================================================================

# ----- ALL DATA:   Load Packages & Repositories --------------------------

start.time <- Sys.time()

library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)
library(openxlsx)
library(stringr)
library(purrr)

# --- Source ADS Shared Functions (OneDrive) ---

# IMPORTANT: Update this path to match YOUR OneDrive location
# Default path:
# C:/Users/[USERNAME]/OneDrive - anellodatasolutions.com/Documents/0. ADS/ADS_Shared

ADS_SHARED <- Sys.getenv("ADS_SHARED",
                         unset = "C:/Users/Gabe/OneDrive - anellodatasolutions.com/Documents/0. ADS/ADS_Shared")

# Verify ADS_Shared folder exists
if (!dir.exists(ADS_SHARED)) {
  stop("\n\nERROR: Cannot find ADS_Shared folder at:\n  ", ADS_SHARED,
       "\n\nPlease either:",
       "\n  1. Update the ADS_SHARED path above to match your OneDrive location",
       "\n  2. Set the ADS_SHARED environment variable",
       "\n\nCheck OneDrive sync status if the folder should exist.\n")
}

# Source core ADS functions
cat("Loading ADS functions from:\n  ", ADS_SHARED, "\n")
source(file.path(ADS_SHARED, "scripts", "functions.R"), local = FALSE, chdir = FALSE)
cat("✓ ADS functions loaded successfully\n\n")

# --- Set Case Directory ---

# IMPORTANT: Update this path for EACH case
# This should be the root folder for THIS case (contains data/ and output/ folders)
#
# Examples:
#   set_case_dir("C:/Users/[USERNAME]/OneDrive/Cases/[CASE_NAME]/Analysis/CASE_R")

set_case_dir("C:/Users/[USERNAME]/OneDrive/Cases/[CASE_NAME]/Analysis/CASE_R")

# Initialize case paths (creates data/raw, data/processed, output folders if needed)
paths <- init_case_paths(set_globals = TRUE)

cat("\n✓ Case directory set:\n  ", paths$CASE_DIR, "\n")
cat("✓ Data directories:\n")
cat("    Raw:       ", paths$RAW_DIR, "\n")
cat("    Processed: ", paths$PROCESSED_DIR, "\n")
cat("    Output:    ", paths$OUT_DIR, "\n\n")

# ----- STEP 4: Load Raw Data --------------------------------------------------

cat("==============================================================================\n")
cat("LOADING RAW DATA\n")
cat("==============================================================================\n\n")

# TIME DATA
cat("Loading time data...\n")
time1 <- read_excel("")  # UPDATE: Add file path

cat("✓ Loaded", nrow(time1), "time records\n")
cat("\n=== Columns in Time Data ===\n")
for(col in names(time1)) {
  cat("  ", col, "\n")
}

# ----- STEP 5: Standardize Column Names ---------------------------------------

cat("\n==============================================================================\n")
cat("STANDARDIZING COLUMN NAMES\n")
cat("==============================================================================\n\n")

# UPDATE: Customize these mappings based on your data
standard_names <- c(
  "EEID" = "ID",
  "Shift Date" = "Date",
  "Punch_In" = "In",
  "Punch_Out" = "Out",
  "Hours_Worked" = "Hours",
  "Pay_Code" = "Code"
)

for(old_name in names(standard_names)) {
  if(old_name %in% names(time1)) {
    setnames(time1, old_name, standard_names[old_name])
    cat("✓ Renamed:", old_name, "→", standard_names[old_name], "\n")
  }
}

cat("\n=== Updated Columns in Time Data ===\n")
for(col in names(time1)) {
  cat("  ", col, "\n")
}

# ----- STEP 6: Convert Data Types ---------------------------------------------

cat("\n==============================================================================\n")
cat("CONVERTING DATA TYPES\n")
cat("==============================================================================\n\n")

setDT(time1)

# Convert date columns
date_cols <- c("Date")
for(col in date_cols) {
  if(col %in% names(time1)) {
    time1[, (col) := as.Date(get(col))]
    cat("✓ Converted", col, "to date format\n")
  }
}

# Convert numeric columns
numeric_cols <- c("Hours", "Rate", "Amount")
for(col in numeric_cols) {
  if(col %in% names(time1)) {
    time1[, (col) := as.numeric(get(col))]
    cat("✓ Converted", col, "to numeric format\n")
  }
}

# Convert time columns (HH:MM format to datetime)
time_cols <- c("In", "Out")
for(col in time_cols) {
  if(col %in% names(time1)) {
    if(inherits(time1[[col]], "POSIXct") || inherits(time1[[col]], "POSIXlt")) {
      cat("✓", col, "is already datetime format\n")
    } else if(is.character(time1[[col]])) {
      time1[, (col) := as.POSIXct(paste("1899-12-31", get(col)), format = "%Y-%m-%d %H:%M")]
      cat("✓ Converted", col, "from HH:MM to datetime\n")
    } else {
      cat("⚠", col, "has unexpected format:", class(time1[[col]]), "- skipping\n")
    }
  }
}

# ----- STEP 7: Clean and Validate Data ----------------------------------------

cat("\n==============================================================================\n")
cat("CLEANING AND VALIDATING DATA\n")
cat("==============================================================================\n\n")

# Remove duplicates
initial_rows <- nrow(time1)
time1 <- unique(time1)
duplicates_removed <- initial_rows - nrow(time1)
if(duplicates_removed > 0) {
  cat("✓ Removed", duplicates_removed, "duplicate rows\n")
} else {
  cat("✓ No duplicate rows found\n")
}

# Remove rows with missing critical fields
time1 <- time1[!is.na(ID) & !is.na(Date)]
cat("✓ Removed rows with missing ID or Date\n")

# Additional validation
cat("✓ Final time data:", nrow(time1), "records\n")
cat("✓ Date range:", min(time1$Date, na.rm = TRUE), "to", max(time1$Date, na.rm = TRUE), "\n")
cat("✓ Unique employees:", uniqueN(time1$ID), "\n")

# ----- STEP 8: Save Processed Data --------------------------------------------

cat("\n==============================================================================\n")
cat("SAVING PROCESSED DATA\n")
cat("==============================================================================\n\n")

# Save to processed directory
time_processed_file <- file.path(paths$PROCESSED_DIR, "time_processed.rds")
saveRDS(time1, time_processed_file)
cat("✓ Saved time data to:\n  ", time_processed_file, "\n")

# ----- STEP 9: Load Additional Data (PAY, CLASS) ------------------------------

# PAY DATA
cat("\nLoading pay data...\n")
# pay1 <- read_excel("")  # UPDATE: Add file path
# ... (similar cleaning steps)

# CLASS LIST DATA
cat("\nLoading class list...\n")
# class1 <- read_excel("")  # UPDATE: Add file path
# ... (similar cleaning steps)

# ----- COMPLETION -------------------------------------------------------------

end.time <- Sys.time()
elapsed <- round(difftime(end.time, start.time, units = "secs"), 1)

cat("\n==============================================================================\n")
cat("                    DATA CLEANING COMPLETE\n")
cat("==============================================================================\n")
cat("Time elapsed:", elapsed, "seconds\n")
cat("\nNext step: Run analysis.R to generate metrics and reports\n")
cat("==============================================================================\n\n")

# Environment is now ready for analysis.R:
# - paths (CASE_DIR, RAW_DIR, PROCESSED_DIR, OUT_DIR)
# - time1, pay1, class1 (cleaned data)
# - All ADS functions available
