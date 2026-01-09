# analysis.R - Main Analysis Script
# This script runs the comprehensive wage and hour analysis

# NOTE: This should be run AFTER clean_data.R has been executed
# It expects time_processed.rds and pay_processed.rds to exist

library(data.table)
library(lubridate)
library(here)
library(readr)
library(purrr)
library(tidyr)
library(dplyr)

# Load functions
source(here("scripts", "Functions.R"))

# Load processed data
cat("Loading processed data...\n")
time1 <- readRDS(here("data/processed/time_processed.rds"))
pay1 <- readRDS(here("data/processed/pay_processed.rds"))
class1 <- readRDS(here("data/processed/class_processed.rds"))

# Load or set parameters
if(file.exists(here("config/parameters.R"))) {
  source(here("config/parameters.R"))
} else {
  # Default parameters
  complaint_date <- as.Date("2025-09-15")
  mediation_date <- as.Date("2026-09-15")
  mode_days_btwn_pay_period_ends <- 14
}

cat("Analysis period:", format(complaint_date), "to", format(mediation_date), "\n")

# YOUR FULL ANALYSIS SCRIPT GOES HERE
# (Copy the content from your original analysis script starting from
# the "PAY DATA: CA Min Wage table" section through to the end)

# At the end, ensure you save the key output files:
cat("\nSaving output files...\n")

# The dashboard expects these files:
fwrite(time1, here("output", "Time Punch Data.csv"))
fwrite(shift_data1, here("output", "Time Shift Data.csv"))
fwrite(ee_data1, here("output", "Time Employee Data.csv"))
fwrite(pay1, here("output", "Pay Data.csv"))

# Final analysis table
if(exists("final_table")) {
  fwrite(final_table, here("output", "Analysis.csv"))
}

cat("\n✅ Analysis complete! Output files saved to output/\n")
cat("✅ Ready for dashboard\n")
