# ==============================================================================
# PROPRIETARY AND CONFIDENTIAL
# Anello Data Solutions LLC
# 
# This file contains proprietary information and trade secrets.
# Unauthorized copying, distribution, or use is strictly prohibited.
# For authorized use by ANELLO DATA SOLUTIONS LLC contracted analysts only.

# ==============================================================================
# CLEAN DATA SCRIPT
#
# This script MUST be run first for every case.
# It sets up the ADS environment, paths, and cleans raw data.
#
# After running this script, you can run analysis.R

# ----- ALL DATA:   Load packages, functions and directories --------------------------

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

CASE_DIR <- "YOUR_CASE_FOLDER_FILE_PATH"

CASE_DIR <- normalizePath(CASE_DIR, winslash = "/", mustWork = TRUE)

RAW_DIR       <- file.path(CASE_DIR, "data", "raw")
PROCESSED_DIR <- file.path(CASE_DIR, "data", "processed")
OUT_DIR       <- file.path(CASE_DIR, "output")
SCRIPTS_DIR   <- file.path(CASE_DIR, "scripts")

dir.create(RAW_DIR,       recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR,       recursive = TRUE, showWarnings = FALSE)
dir.create(SCRIPTS_DIR,   recursive = TRUE, showWarnings = FALSE)

message("CASE_DIR      = ", CASE_DIR)
message("RAW_DIR       = ", RAW_DIR)
message("PROCESSED_DIR = ", PROCESSED_DIR)
message("OUT_DIR       = ", OUT_DIR)
message("SCRIPTS_DIR   = ", SCRIPTS_DIR)


# Set ADS_Shared and load functions
ADS_SHARED <- Sys.getenv("ADS_SHARED",
                         unset = "C:/Users/Gabe/OneDrive - anellodatasolutions.com/Documents/0. ADS/ADS_Shared")


# Run core functions and PDF generator scripts
source(file.path(ADS_SHARED, "scripts", "functions.R"), local = FALSE, chdir = FALSE)
cat("✓ ADS functions loaded successfully\n\n")

source(file.path(ADS_SHARED, "scripts", "generate_pdf.R"), local = FALSE, chdir = FALSE)
cat("✓ ADS generate PDF loaded successfully\n\n")


# ----- ALL DATA:   Case configuration --------------------------

# --- Case info ---
contract_footer <- NA # e.g. use "Employment Research Corporation" for ERC projects else NA
case_name <- "Plaintiff v Defendant" 
case_no <- "4:49:cv-494949-NINERS"
date_filed <- as.Date("1981-09-15")
sample_size <- "100%" # Use as text field
sample_size_val <- 1  # e.g., 1 = 100%

# --- Key groups (named plaintiff(s)) ---
key_employees <- c("999999999" = "Chief, Chieify")
separate_key_gps <- FALSE # Set to TRUE if key groups should be separated from a anonymized random sample

# --- Key dates ---

complaint_date            <- date_filed # Date of ORIGINAL complaint (this is used to filter data; adjust if necessary)
mediation_date            <- Sys.Date() # if unknown or potentially not necessary, set to #Sys.Date()

# Class period
class_dmgs_start_date     <- (complaint_date %m-% years(4)) # Typically four years back from original Complaint.
class_dmgs_end_date       <- mediation_date

# PAGA period
paga_dmgs_start_date      <- (complaint_date %m-% years(1)) - days(65) # Typically 1 yr + 65 days from the filing of the Complaint. 
# Check for the particular Complaint with the PAGA claim.
paga_dmgs_end_date        <- mediation_date

# Waiting Time Penalties period (remember to filter out active employees)
wt_start_date             <- (complaint_date %m-% years(3)) # Typically three years back from original Complaint. 
wt_end_date               <- mediation_date

# Wage statement violations period
wsv_start_date            <- (complaint_date %m-% years(1)) # Typically one year back from original Complaint. 
wsv_end_date              <- mediation_date

# --- Analysis.R parameters ---

# Min and max allowable base_rate and RROP
min_rate <- 7.25 # default $7.25/hr
max_rate <- 5000 # default $5,000/hr

# Regular rate analysis de minimis under/overpayment buffer (< buffer forced to zero)
rrop_buffer <- 0.05 # 5 cents is default

# Shift duration / shift-level clock rounding analysis threshold
rounding_hrs_cutoff <- 0.25 # 0.25 hrs is default (values within threshold assumed acceptable, normal aberrations)

# Define unpaid overtime / double time analysis / damages model buffers 
min_ot_buffer <- 0.25 # 0.25 hrs is default min (values within threshold assumed acceptable, normal aberrations)
max_ot_buffer <- 20   # 20 hrs is default (over two week pay period normally)

# Interest (for damages) parameters
annual_interest_rate      <- 0.07  # 7% prejudgment interest rate default (10% occasionally argued here)
monthly_interest_rate     <- annual_interest_rate / 12 # "yr/12 mos" default due to most rates reported as annual rates
interest_thru_date        <- mediation_date # defaults to defined "mediation" date

# Wage statement penalties settings
wsv_initial_pp_penalty     <- 50   # $50 initial pay period penalty amount default
wsv_subsequent_pp_penalty  <- 100  # $100 subsequent pay period penalty amount default
wsv_cap                    <- 4000 # $4,000 cap per employee default

# Waiting time penalties settings
wt_active_days_threshold <- 30     # defaults to 30 days of waiting time penalties
wt_use_rrop <- TRUE                # default == TRUE (FALSE uses final_Base_Rate instead of RROP)

# PAGA penalties settings
initial_pp_penalty        <- 100   # $100 default (covers all penalty types other than below)
subsequent_pp_penalty     <- 100   # $100 default (covers all penalty types other than below)

initial_pp_penalty_226    <- 250   # $250 default
subsequent_pp_penalty_226 <- 250   # $250 default

initial_pp_penalty_558    <- 100   # $100 default
subsequent_pp_penalty_558 <- 100   # $100 default

penalty_1174 <- 500                # $500 default


# ----- TIME DATA:  Load data -------------------------------

time1 <- read_excel("")

cat("✓ Loaded", nrow(time1), "pay records\n")
cat("\n=== Columns in Time Data ===\n")
for(col in names(time1)) {
  cat(col, "\n")
}


# ----- TIME DATA:  Standardize column names -------------------------------

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
  }
}

cat("\n=== UPDATED columns in Time Data ===\n")
for(col in names(time1)) {
  cat(col, "\n")
}


# ----- TIME DATA:  Convert data types ------------------------------------------

setDT(time1)

date_cols <- c("Date")
for(col in date_cols) {
  if(col %in% names(time1)) {
    time1[, (col) := as.Date(get(col))]
    cat("Converted", col, "to date format\n")
  }
}

numeric_cols <- c("Hours", "Rate", "Amount")
for(col in numeric_cols) {
  if(col %in% names(time1)) {
    time1[, (col) := as.numeric(get(col))]
    cat("Converted", col, "to numeric format\n")
  }
}

# Convert HH:MM format to datetime (only if not already datetime)
time_cols <- c("In", "Out") # Adjust as needed

for(col in time_cols) {
  if(col %in% names(time1)) {
    if(inherits(time1[[col]], "POSIXct") || inherits(time1[[col]], "POSIXlt")) {
      cat(col, "is already datetime format - skipping conversion\n")
    } else if(is.character(time1[[col]])) {
      # Only convert if it's character format (HH:MM)
      time1[, (col) := as.POSIXct(paste("1899-12-31", get(col)), format = "%Y-%m-%d %H:%M")]
      cat("Converted", col, "from HH:MM to datetime\n")
    } else {
      cat(col, "has unexpected format:", class(time1[[col]]), "- skipping\n")
    }
  }
}


# ----- TIME DATA:  Basic data quality checks ---------------------------------------------------

cat("\n=== Data Quality Checks ===\n")

# Check for missing IDs
if("ID" %in% names(time1)) {
  missing_ids <- time1[is.na(ID), .N]
  if(missing_ids > 0) {
    cat("✗ Found", missing_ids, "rows with missing IDs\n")
  } else {
    cat("✓ No missing time data IDs\n")
  }
} else {
  cat("Note: ID column not found in time data\n")
}

# Check for missing Dates
if("Date" %in% names(time1)) {
  missing_dates <- time1[is.na(Date), .N]
  if(missing_dates > 0) {
    cat("✗ Found", missing_dates, "rows with missing Dates\n")
  } else {
    cat("✓ No missing time data Dates\n")
  }
} else {
  cat("Note: Date column not found in time data\n")
}

# Time data date range
date_cols <- c("Date")
existing_date_cols <- date_cols[date_cols %in% names(time1)]

if(length(existing_date_cols) > 0) {
  all_dates <- unlist(time1[, ..existing_date_cols], use.names = FALSE)
  all_dates <- as.Date(all_dates, origin = "1970-01-01")  # Convert back to Date
  all_dates <- all_dates[!is.na(all_dates)]
  
  if(length(all_dates) > 0) {
    cat("Time data date range:", 
        format(min(all_dates), "%Y-%m-%d"), "to",
        format(max(all_dates), "%Y-%m-%d"), "\n")
  } else {
    cat("Time data date range: All date values are NA\n")
  }
  
  # Note missing columns
  missing <- setdiff(date_cols, existing_date_cols)
  if(length(missing) > 0) {
    cat("  Note:", paste(missing, collapse=", "), "column(s) not found\n")
  }
} else {
  cat("Time data date range: Cannot determine - no date columns found\n")
}

cat("Unique time data employees:", uniqueN(time1$ID), "\n")
cat("Unique time data employee days:", uniqueN(paste(time1$ID, time1$Date)), "\n")


# ----- TIME DATA:  Duplicate review -----------------------------------------------

cat("\n=== Duplicate Check ===\n")

# Define columns to check for duplicates
dup_check_cols <- c("ID", "Date", "In", "Out")

# Check which columns exist
existing_cols <- dup_check_cols[dup_check_cols %in% names(time1)]
missing_cols <- setdiff(dup_check_cols, existing_cols)

# Report missing columns if any
if(length(missing_cols) > 0) {
  cat("Note: These columns don't exist for duplicate check:", paste(missing_cols, collapse=", "), "\n")
}

# Proceed with duplicate check using available columns
if(length(existing_cols) > 0) {
  cat("Checking duplicates based on:", paste(existing_cols, collapse=", "), "\n")
  
  # Count duplicates
  time1[, dup_count := .N, by = existing_cols]
  time_duplicates <- time1[dup_count > 1]
  
  if(nrow(time_duplicates) > 0) {
    cat("✗ Found", nrow(time_duplicates), "duplicate rows\n")
    cat("  Unique duplicate groups:", uniqueN(time_duplicates[, ..existing_cols]), "\n")
    
    # Option to remove duplicates (keep first occurrence)
    REMOVE_DUPLICATES <- TRUE  # Set to FALSE to keep duplicates
    
    if(REMOVE_DUPLICATES) {
      before_rows <- nrow(time1)
      time1 <- unique(time1, by = existing_cols)
      after_rows <- nrow(time1)
      cat("  ✓ Removed", before_rows - after_rows, "duplicate rows\n")
    } else {
      cat("  ⚠ Duplicates kept (set REMOVE_DUPLICATES = TRUE to remove)\n")
    }
  } else {
    cat("✓ No duplicate rows found\n")
  }
  
  # Clean up temp column
  time1[, dup_count := NULL]
  
} else {
  cat("⚠ Cannot check for duplicates - no specified columns exist\n")
}


# ----- PAY DATA:   Load payroll data --------------------------------------------

pay1 <- read_excel("")

cat("✓ Loaded", nrow(pay1), "pay records\n")
cat("\n=== Columns in Pay Data ===\n")
for(col in names(pay1)) {
  cat(col, "\n")
}


# ----- PAY DATA:   Transpose pay data (if necessary) -----------------------------------
# 
# # Select columns to keep the same (do not pivot)
# cols_to_keep <- c("POSITION ID", "PERIOD BEGINNING DATE", "PERIOD ENDING DATE")
# 
# # Add text strings to remove from final Pay_Code column (prefixes)
# strings_to_remove <- c("HOURS - ", "PAY - ")  
# 
# # Suffixes to remove for grouping (after identifying hours/amounts)
# suffixes_to_remove <- "( HOURS| EARNINGS|_HOURS|_EARNINGS| AMOUNT|_AMOUNT)$"
# 
# # Select strings that will find Pay_Hours and Pay_Amount values
# string_to_match_hrs <- c("HOURS")  
# string_to_match_amt <- c("^(?!.*HOURS).*$")
# 
# # Apply function with new parameter
# pay1_transposed <- transpose_pay_data(pay1, 
#                                       cols_to_keep, 
#                                       strings_to_remove, 
#                                       string_to_match_hrs, 
#                                       string_to_match_amt,
#                                       suffixes_to_remove)
# # Compute total sums
# total_transposed <- sum(pay1_transposed$Pay_Hours, na.rm = TRUE) + sum(pay1_transposed$Pay_Amount, na.rm = TRUE)
# 
# pay1 <- as.data.table(pay1)
# pay1_temp <- copy(pay1)
# cols_to_convert <- setdiff(names(pay1_temp), cols_to_keep)
# pay1_temp[, (cols_to_convert) := lapply(.SD, as.numeric), .SDcols = cols_to_convert]
# total_original <- sum(pay1_temp[, .SD, .SDcols = cols_to_convert], na.rm = TRUE)
# 
# # Compare and print result
# message(
#   sprintf("Total Transposed: %.2f | Total Original: %.2f | %s",
#           total_transposed, total_original,
#           ifelse(abs(total_transposed - total_original) < 1e-5, "MATCH ✅", "MISMATCH ❌"))
# )
# 
# # Return back to pay1
# pay1 <- pay1_transposed


# ----- PAY DATA:   Standardize column names --------------------------------------

standard_names <- c(
  "Employee Name" = "Pay_Name",
  "Employee ID" = "Pay_ID",
  "Pay Period Start" = "Pay_Period_Beg",
  "Pay Peirod End" = "Pay_Period_End",
  "Pay Code" = "Pay_Code",
  "Hours" = "Pay_Hours",
  "Rate" = "Pay_Rate",
  "Amount" = "Pay_Amount",
  "Department" = "Pay_Department",
  "Job Title" = "Pay_Job"
)

for(old_name in names(standard_names)) {
  if(old_name %in% names(pay1)) {
    setnames(pay1, old_name, standard_names[old_name])
  }
}

cat("\n=== UPDATED Columns in Pay Data ===\n")
for(col in names(pay1)) {
  cat(col, "\n")
}

# ----- PAY DATA:   Add Pay_Date if none exists -----------------------

# Configure pay date day of week (1=Mon, 2=Tue, 3=Wed, 4=Thu, 5=Fri, 6=Sat, 7=Sun)
pay_date_day_of_week <- 5  # Friday default (adjust as needed)

# Add Pay_Date if not exists
if (!"Pay_Date" %in% names(pay1)) {
  
  pay1[, Pay_Date := {
    end_dt <- as.Date(Pay_Period_End)
    current_dow <- wday(end_dt, week_start = 1)  # 1=Mon, 7=Sun
    
    # days forward to target pay date
    days_to_add <- (pay_date_day_of_week - current_dow + 7) %% 7
    days_to_add <- ifelse(days_to_add == 0, 7, days_to_add)
    
    end_dt + days_to_add
  }]
  
  day_names <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  cat("✓ Pay_Date added (", day_names[pay_date_day_of_week], " after Pay_Period_End)\n", sep="")
  
} else {
  
  cat("✓ Pay_Date already exists in pay1 (no action needed)\n")
}


# ----- PAY DATA:   Convert data types ----------------------------------------------------------

setDT(pay1)

date_cols <- c("Pay_Period_End", "Pay_Period_Beg", "Pay_Date")
for(col in date_cols) {
  if(col %in% names(pay1)) {
    pay1[, (col) := as.Date(get(col))]
    cat("Converted", col, "to date format\n")
  }
}

numeric_cols <- c("Pay_Rate", "Pay_Hours", "Pay_Amount")
for(col in numeric_cols) {
  if(col %in% names(pay1)) {
    pay1[, (col) := as.numeric(get(col))]
    cat("Converted", col, "to numeric format\n")
  }
}

# ----- PAY DATA:   Basic data quality checks ---------------------------------------------

cat("\n=== Data Quality Checks ===\n")

# Check for missing Pay_IDs
if("Pay_ID" %in% names(pay1)) {
  missing_ids <- pay1[is.na(Pay_ID), .N]
  if(missing_ids > 0) {
    cat("✗ Found", missing_ids, "rows with missing Pay_IDs\n")
  } else {
    cat("✓ No missing pay data Pay_IDs\n")
  }
} else {
  cat("Note: Pay_ID column not found in data\n")
}

# Check for missing Pay_Period_Ends
if("Pay_Period_End" %in% names(pay1)) {
  missing_period_ends <- pay1[is.na(Pay_Period_End), .N]
  if(missing_period_ends > 0) {
    cat("✗ Found", missing_period_ends, "rows with missing Pay_Period_Ends\n")
  } else {
    cat("✓ No missing pay data Pay_Period_Ends\n")
  }
} else {
  cat("Note: Pay_Period_End column not found in data\n")
}

# Pay data date range
date_cols <- c("Pay_Period_End")
existing_date_cols <- date_cols[date_cols %in% names(pay1)]

if(length(existing_date_cols) > 0) {
  all_dates <- unlist(pay1[, ..existing_date_cols], use.names = FALSE)
  all_dates <- as.Date(all_dates, origin = "1970-01-01")  # Convert back to Date
  all_dates <- all_dates[!is.na(all_dates)]
  
  if(length(all_dates) > 0) {
    cat("Pay data date range:", 
        format(min(all_dates), "%Y-%m-%d"), "to",
        format(max(all_dates), "%Y-%m-%d"), "\n")
  } else {
    cat("Pay data date range: All date values are NA\n")
  }
  
  # Note missing columns
  missing <- setdiff(date_cols, existing_date_cols)
  if(length(missing) > 0) {
    cat("  Note:", paste(missing, collapse=", "), "column(s) not found\n")
  }
} else {
  cat("Pay data date range: Cannot determine - no date columns found\n")
}

# Remove rows with zero hours and dollars
cat("Records before hours/amount filter:", nrow(pay1), "\n")
pay1 <- pay1[!(Pay_Hours == 0 & Pay_Amount == 0)]
cat("Records after hours/amount filter:", nrow(pay1), "\n")

cat("Unique pay data employees:", uniqueN(pay1$Pay_ID), "\n")
cat("Unique pay data employee pay periods:", uniqueN(paste(pay1$Pay_ID, pay1$Pay_Period_End)), "\n")


# ----- PAY DATA:   Duplicate review ------------------------------------------------------------

cat("\n=== Duplicate Check ===\n")

# Define columns to check for duplicates
dup_check_cols <- c("Pay_ID", "Pay_Period_End", "Pay_Amount", "Pay_Hours", "Pay_Code")

# Check which columns exist
existing_cols <- dup_check_cols[dup_check_cols %in% names(pay1)]
missing_cols <- setdiff(dup_check_cols, existing_cols)

# Report missing columns if any
if(length(missing_cols) > 0) {
  cat("Note: These columns don't exist for duplicate check:", paste(missing_cols, collapse=", "), "\n")
}

# Proceed with duplicate check using available columns
if(length(existing_cols) > 0) {
  cat("Checking duplicates based on:", paste(existing_cols, collapse=", "), "\n")
  
  # Count duplicates
  pay1[, dup_count := .N, by = existing_cols]
  pay_duplicates <- pay1[dup_count > 1]
  
  if(nrow(pay_duplicates) > 0) {
    cat("✗ Found", nrow(pay_duplicates), "duplicate rows\n")
    cat("  Unique duplicate groups:", uniqueN(pay_duplicates[, ..existing_cols]), "\n")
    
    # Option to remove duplicates (keep first occurrence)
    REMOVE_DUPLICATES <- FALSE  # Set to FALSE to keep duplicates
    
    if(REMOVE_DUPLICATES) {
      before_rows <- nrow(pay1)
      pay1 <- unique(pay1, by = existing_cols)
      after_rows <- nrow(pay1)
      cat("  ✓ Removed", before_rows - after_rows, "duplicate rows\n")
    } else {
      cat("  ⚠ Duplicates kept (set REMOVE_DUPLICATES = TRUE to remove)\n")
    }
  } else {
    cat("✓ No duplicate rows found\n")
  }
  
  # Clean up temp column
  pay1[, dup_count := NULL]
  
} else {
  cat("⚠ Cannot check for duplicates - no specified columns exist\n")
}


# ----- CLASS LIST: Load Class List or create one -------------------------------

# Load raw class data
#class1 <- class_list

### OR MAKE OUR OWN CLASS LIST ###

# Create a "Class List" from unique list of all time and pay data IDs (then add names)
setDT(time1); setDT(pay1)

# Ensure missing name columns exist
if (!"Name" %in% names(time1))   time1[,  Name := NA_character_]
if (!"Pay_Name" %in% names(pay1)) pay1[, Pay_Name := NA_character_]

# Unique ID list
class1 <- data.table(Class_ID = sort(unique(c(time1$ID, pay1$Pay_ID))))

# Extract first non-NA names
time_names <- time1[, .(Time_Name = first(na.omit(Name))), by = ID]
pay_names <- pay1[, .(Pay_Name  = first(na.omit(Pay_Name))), by = Pay_ID]

# Merge + flags
class1 <- merge(class1, time_names, by.x="Class_ID", by.y="ID",     all.x=TRUE)
class1 <- merge(class1, pay_names, by.x="Class_ID", by.y="Pay_ID", all.x=TRUE)

class1[, Missing_Time_Name := is.na(Time_Name)]
class1[, Missing_Pay_Name  := is.na(Pay_Name)]
class1[, Name_Mismatch := !is.na(Time_Name) & !is.na(Pay_Name) & Time_Name != Pay_Name]

### OR MAKE OUR OWN CLASS LIST  ###


# ----- CLASS LIST: Standardize column names ----------------------------------------------------

standard_names <- c(
  "Employee ID Number" = "Class_ID",
  "Employee Name" = "Class_Name",
  "Hire Date" = "Hire_Date",
  "Term Date" = "Term_Date",
  "Rehire Date" = "Rehire_Date"
)

for(old_name in names(standard_names)) {
  if(old_name %in% names(class1)) {
    setnames(class1, old_name, standard_names[old_name])
  }
}

cat("\n=== UPDATED columns in Class List ===\n")
for(col in names(class1)) {
  cat(col, "\n")
}

setDT(class1)


# ----- CLASS LIST: Convert data types ----------------------------------------------------------

date_cols <- c("Hire_Date", "Term_Date", "Hire_Date2", "Term_Date2", "Hire_Date3", "Term_Date3")
for(col in date_cols) {
  if(col %in% names(class1)) {
    class1[, (col) := as.Date(get(col))]
    cat("Converted", col, "to date format\n")
  }
}

numeric_cols <- c("Starting Hourly Wage", "Most Recent Hourly Wage", "Wage Rate", "Pay Rate", "Rate")
for(col in numeric_cols) {
  if(col %in% names(class1)) {
    class1[, (col) := as.numeric(get(col))]
    cat("Converted", col, "to numeric format\n")
  }
}


# ----- CLASS LIST: Basic data quality checks ---------------------------------------------------
cat("\n=== Data Quality Checks ===\n")

# Check for missing Names (handle column that may not exist)
if("Class_Name" %in% names(class1)) {
  missing_names <- class1[is.na(Class_Name), .N]
  if(missing_names > 0) {
    cat("✗ Found", missing_names, "rows with missing Employee Names\n")
  } else {
    cat("✓ No missing Class List Names\n")
  }
} else {
  cat("Note: Class_Name column not found in data\n")
}

# Check for missing IDs
if("Class_ID" %in% names(class1)) {
  missing_ids <- class1[is.na(Class_ID), .N]
  if(missing_ids > 0) {
    cat("✗ Found", missing_ids, "rows with missing Class IDs\n")
  } else {
    cat("✓ No missing Class List IDs\n")
  }
} else {
  cat("Note: Class_ID column not found in data\n")
}

# Class List date range
date_cols <- c("Term_Date")
existing_date_cols <- date_cols[date_cols %in% names(class1)]

if(length(existing_date_cols) > 0) {
  all_dates <- unlist(class1[, ..existing_date_cols], use.names = FALSE)
  all_dates <- as.Date(all_dates, origin = "1970-01-01")  # Convert back to Date
  all_dates <- all_dates[!is.na(all_dates)]
  
  if(length(all_dates) > 0) {
    cat("Class List date range:", 
        format(min(all_dates), "%Y-%m-%d"), "to",
        format(max(all_dates), "%Y-%m-%d"), "\n")
  } else {
    cat("Class List date range: All date values are NA\n")
  }
  
  # Note missing columns
  missing <- setdiff(date_cols, existing_date_cols)
  if(length(missing) > 0) {
    cat("  Note:", paste(missing, collapse=", "), "column(s) not found\n")
  }
} else {
  cat("Class List date range: Cannot determine - no date columns found\n")
}
cat("Unique Class List employees:", uniqueN(class1$Class_ID), "\n")


# ----- CLASS LIST: Duplicate review ------------------------------------------------------------

cat("\n=== Duplicate Check ===\n")

# Define columns to check for duplicates
dup_check_cols <- c("Class_ID")

# Check which columns exist
existing_cols <- dup_check_cols[dup_check_cols %in% names(class1)]
missing_cols <- setdiff(dup_check_cols, existing_cols)

# Report missing columns if any
if(length(missing_cols) > 0) {
  cat("Note: These columns don't exist for duplicate check:", paste(missing_cols, collapse=", "), "\n")
}

# Proceed with duplicate check using available columns
if(length(existing_cols) > 0) {
  cat("Checking duplicates based on:", paste(existing_cols, collapse=", "), "\n")
  
  # Count duplicates
  class1[, dup_count := .N, by = existing_cols]
  class_duplicates <- class1[dup_count > 1]
  
  if(nrow(class_duplicates) > 0) {
    cat("✗ Found", nrow(class_duplicates), "duplicate rows\n")
    cat("  Unique duplicate groups:", uniqueN(class_duplicates[, ..existing_cols]), "\n")
    
    # Option to remove duplicates (keep first occurrence)
    REMOVE_DUPLICATES <- FALSE  # Set to FALSE to keep duplicates
    
    if(REMOVE_DUPLICATES) {
      before_rows <- nrow(class1)
      class1 <- unique(class1, by = existing_cols)
      after_rows <- nrow(class1)
      cat("  ✓ Removed", before_rows - after_rows, "duplicate rows\n")
    } else {
      cat("  ⚠ Duplicates kept (set REMOVE_DUPLICATES = TRUE to remove)\n")
    }
  } else {
    cat("✓ No duplicate rows found\n")
  }
  
  # Clean up temp column
  class1[, dup_count := NULL]
  
} else {
  cat("⚠ Cannot check for duplicates - no specified columns exist\n")
}


# ----- ALL DATA:   Filter data based on class and date  --------------------------------------------------------------------------

# Filter based on Class Period (and summary of what was removed)

# Convert to data.table if not already
setDT(time1)
setDT(pay1)

# Original counts
base_summary <- data.table(
  table = c("time1", "pay1"),
  records_before = c(nrow(time1), nrow(pay1)),
  ids_before     = c(uniqueN(time1$ID), uniqueN(pay1$Pay_ID))
)

use_class_filter <- FALSE

# Optional class filter
if (use_class_filter) {
  
  class_ids <- unique(class1$Class_ID)
  pay_ids   <- unique(pay1$Pay_ID)
  time_ids  <- unique(time1$ID)
  
  class_ids_keep <- class_ids[class_ids %in% pay_ids | class_ids %in% time_ids]
  class_ids_drop <- setdiff(class_ids, class_ids_keep)
  
  time1_removed_class <- time1[!(ID %in% class_ids_keep)]
  pay1_removed_class  <- pay1[!(Pay_ID %in% class_ids_keep)]
  
  time1 <- time1[ID %in% class_ids_keep]
  pay1  <- pay1[Pay_ID %in% class_ids_keep]
  
  class_filter_summary <- data.table(
    table = c("time1", "pay1"),
    records_removed_class = c(nrow(time1_removed_class), nrow(pay1_removed_class)),
    ids_removed_class     = c(uniqueN(time1_removed_class$ID),
                              uniqueN(pay1_removed_class$Pay_ID))
  )
  
} else {
  class_filter_summary <- data.table(
    table = c("time1", "pay1"),
    records_removed_class = 0,
    ids_removed_class     = 0
  )
}

# Date filter
time1_removed_date <- time1[Date < class_dmgs_start_date]
pay1_removed_date  <- pay1[Pay_Period_End < class_dmgs_start_date]

time1 <- time1[Date >= class_dmgs_start_date]
pay1  <- pay1[Pay_Period_End >= class_dmgs_start_date]

date_filter_summary <- data.table(
  table = c("time1", "pay1"),
  records_removed_date = c(nrow(time1_removed_date), nrow(pay1_removed_date)),
  ids_removed_date     = c(uniqueN(time1_removed_date$ID),
                           uniqueN(pay1_removed_date$Pay_ID))
)

# Updated counts
final_summary <- data.table(
  table = c("time1", "pay1"),
  records_final = c(nrow(time1), nrow(pay1)),
  ids_final     = c(uniqueN(time1$ID), uniqueN(pay1$Pay_ID))
)

# Summary of records removed
summary_removed <- Reduce(
  function(x, y) merge(x, y, by = "table"),
  list(base_summary, class_filter_summary, date_filter_summary, final_summary)
)

summary_removed

write_csv_and_rds(
  summary_removed,
  file.path(OUT_DIR, "IDs & Records Removed.csv")
)

time1_prefiltered <- time1
pay1_prefiltered <- pay1

time1 <- time1_filtered
pay1 <- pay1_filtered


# ----- ALL DATA:   Pay calendar ---------------------------------------------------

# Convert data.frames to data.tables
time1 <- as.data.table(time1)
pay1 <- as.data.table(pay1)

# Convert relevant columns to Date format
pay1[, Pay_Period_End := as.Date(Pay_Period_End, format = "%Y-%m-%d")]
time1[, Date := as.Date(Date, format = "%Y-%m-%d")]

# Calculate minimum and maximum dates
min_pay_period_end <- min(as.Date(pay1$Pay_Period_End), na.rm = TRUE)
max_pay_period_end <- max(as.Date(pay1$Pay_Period_End), na.rm = TRUE)

min_time_date <- min(as.Date(time1$Date, na.rm = TRUE))
max_time_date <- max(as.Date(time1$Date, na.rm = TRUE))

# Calculate days between pay period ends and the weekday
pay1[, pay_period_end_weekday := weekdays(Pay_Period_End)]
pay1[, days_btwn_pay_period_ends := as.numeric(Pay_Period_End - shift(Pay_Period_End, type = "lag"))]

# Frequency table of days between pay period ends, excluding 0 and negative values
days_btwn_pay_period_ends_freq <- pay1[days_btwn_pay_period_ends > 0, 
                                       .N, by = days_btwn_pay_period_ends][order(-N)]
days_btwn_pay_period_ends_freq[, percent_of_total := round((N / sum(N)) * 100, 2)]

# Extract the mode (most frequent value)
mode_days_btwn_pay_period_ends <- days_btwn_pay_period_ends_freq[1, days_btwn_pay_period_ends]

# Find the most common weekday of Pay_Period_End
most_common_period_end_weekday <- pay1[, .N, by = pay_period_end_weekday][order(-N)][1, pay_period_end_weekday]

# Align start and end dates
earliest_date <- min(min_pay_period_end, min_time_date)
latest_date <- max(max_pay_period_end, max_time_date)

# Create a sequence of dates going backwards
pay_calendar <- data.table(Period_End = seq(max_pay_period_end, min_time_date, by = -mode_days_btwn_pay_period_ends))

# Sort in chronological order
setorder(pay_calendar, Period_End)

# Calculate Period_Beg
pay_calendar[, Period_Beg := Period_End - (mode_days_btwn_pay_period_ends - 1)]

# Reorder columns to have Period_Beg first
setcolorder(pay_calendar, c("Period_Beg", "Period_End"))

# Identify the last Period_End in pay_calendar
last_period_end <- max(pay_calendar$Period_End)

# Generate future Period_Ends from last Period_End to present
future_periods <- data.table(Period_End = seq(last_period_end + mode_days_btwn_pay_period_ends, Sys.Date() + mode_days_btwn_pay_period_ends, by = mode_days_btwn_pay_period_ends))

# Calculate Period_Beg for future periods
future_periods[, Period_Beg := Period_End - (mode_days_btwn_pay_period_ends - 1)]

# Reorder columns
setcolorder(future_periods, c("Period_Beg", "Period_End"))

# Append future periods to pay_calendar
pay_calendar <- rbind(pay_calendar, future_periods)

# Join pay_calendar to unique Time_Dates (from time1)
Time_Dates <- unique(as.data.table(time1)[, .(Date = Date)])

# Ensure all interval columns are of Date type
Time_Dates[, `:=`(Date_Start = as.Date(Date), Date_End = as.Date(Date))]

# Set keys for overlap join
setkey(Time_Dates, Date_Start, Date_End)
setkey(pay_calendar, Period_Beg, Period_End)

# Perform the overlap join
Time_Dates <- foverlaps(Time_Dates, pay_calendar, by.x = c("Date_Start", "Date_End"), type = "within")

# Drop Date_Start and Date_End columns from Time_Dates
Time_Dates[, `:=`(Date_Start = NULL, Date_End = NULL)]

# Join Time_Dates and time1
time1 <- safe_left_join(time1, Time_Dates, by = "Date")

# Create unique person pay period identifier (ID_Period_End)
time1[, ID_Period_End := paste(ID, Period_End, sep = "_")]


# ----- ALL DATA:   Key_Gps & Data comparison ------------------------------------------------

time1[, Key_Gps := key_employees[as.character(ID)]]
time1[is.na(Key_Gps), Key_Gps := "Everyone Else"]
unique(time1$Key_Gps)

pay1[, Pay_Key_Gps := key_employees[as.character(Pay_ID)]]
pay1[is.na(Pay_Key_Gps), Pay_Key_Gps := "Everyone Else"]
unique(pay1$Pay_Key_Gps)

class1[, Class_Key_Gps := key_employees[as.character(Class_ID)]]
class1[is.na(Class_Key_Gps), Class_Key_Gps := "Everyone Else"]
unique(class1$Class_Key_Gps)

run_data_comparison(time1, pay1)

employee_period_comparison(time1, pay1)

all_ids <- all_time_pay_class_ids(time1, pay1, class1)


# ----- ALL DATA:   Save processed data -----------------------------------------

write_csv_and_rds(
  time1,
  file.path(PROCESSED_DIR, "time_processed.csv")
)

write_csv_and_rds(
  pay1,
  file.path(PROCESSED_DIR, "pay_processed.csv")
)

write_csv_and_rds(
  class1,
  file.path(PROCESSED_DIR, "class_processed.csv")
)


# ----- ALL DATA:   Random sample generator (if needed) -----------------------------------------

# # pct <- 0.25                          # Change this for different sample size (0.25 = 25%)
# # seed_value <- 99999                  # Use case number for reproducibility
# #
# # NOTE:
# # • Files will now be written by default to OUT_DIR (absolute path)
# # • You can override with: output_dir = "C:/your/custom/folder"
# 
# # Generate random sample and output files
# # (matches Time + Pay + Class1 by default)
# sample1 <- generate_random_sample(
#   all_ids    = all_ids,
#   class1     = class1,
#   case_name  = case_name,   # already defined earlier in script
#   pct        = pct,
#   use_class1 = TRUE,        # TRUE = match all three sources
#   seed_num   = seed_value
#   # output_dir = NULL       # default -> OUT_DIR (absolute)
# )
# 
# # Extract sample_list from returned object
# sample_list <- sample1$sample_list


# ----- ALL DATA:   Anonymized sample production files (if needed) -----------------------------------------

# # Notes:
# # • If you define default_prod_fields_* vectors BEFORE calling the function,
# #   those get used (unless you explicitly override via prod_fields_* args).
# # • Time/Pay outputs are SAMPLE + DATE filtered.
# # • Class list output is FULL class1 (never filtered), but can include Class_Anon_ID.
# # • Excel headers are prettified:
# #     - underscores -> spaces
# #     - Proper Case
# #     - Pay_ removed from PAY output headers
# #     - Class_ removed from CLASS output headers
# 
# # --- DEFAULT FIELDS (define once per case) ---
# 
# default_prod_fields_time <- c(
#   "Anon_ID", "Date", "In", "Out", "Hours"
# )
# 
# default_prod_fields_pay <- c(
#   "Pay_Anon_ID", "Pay_Date", "Pay_Period_End",
#   "Pay_Code", "Pay_Hours", "Pay_Amount", "Pay_Rate"
# )
# 
# # >>>>> THIS IS THE KEY FIX FOR YOUR CLASS LIST <<<<<
# default_prod_fields_class <- c(
#   "Class_Anon_ID",   # optional (NA except sampled IDs)
#   "Class_ID",
#   "Class_Name",
#   "Hire_Date",
#   "Term_Date"
# )
# 

# --- RUN PRODUCTION EXPORT --- 
# 
# production_file_summary <- generate_production_files(
#   time1 = time1,
#   pay1  = pay1,
#   sample_list = sample_list,
#   class1 = class1,
#   case_name = case_name,
#   class_dmgs_start_date = class_dmgs_start_date,
#   
#   # Optional inline overrides (edit if you want different columns)
#   prod_fields_time  = c("Anon_ID", "Date", "In", "Out"),
#   prod_fields_pay   = c("Pay_Anon_ID", "Pay_Date", "Pay_Code"),
#   prod_fields_class = c("Class_Anon_ID", "Class_ID"),
#   
#   overwrite = FALSE
# )
# 
# # Returned object includes:
# # production_file_summary$time1_prod
# # production_file_summary$pay1_prod
# # production_file_summary$class1_prod
# # production_file_summary$time_file
# # production_file_summary$pay_file
# # production_file_summary$class_file

# ----- END  -----------------------------------------
end.time <- Sys.time()
end.time - start.time    