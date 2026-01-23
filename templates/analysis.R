# ----- ALL DATA:                Load Packages & Data ----------------------------------------

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

# DEV SWITCHES
RUN_CLEAN_DATA_FIRST <- TRUE
STOP_AFTER_CLEAN     <- FALSE

# Load ADS engine
ADS_REPO <- Sys.getenv("ADS_REPO", unset = "")
if (!nzchar(ADS_REPO)) stop("ADS_REPO not set. On Windows: setx ADS_REPO \"C:/Users/Gabe/Documents/GitHub/ads\"")
ADS_REPO <- normalizePath(ADS_REPO, winslash = "/", mustWork = TRUE)

source(file.path(ADS_REPO, "scripts", "functions.R"), local = FALSE, chdir = FALSE)

# --- Resolve case paths (ADS_CASE_DIR is the source of truth) ---
if (!nzchar(Sys.getenv("ADS_CASE_DIR", unset = ""))) {
  stop("ADS_CASE_DIR not set. Set it first (e.g., via run_analysis.R or set_case_dir()).")
}

paths <- resolve_case_paths()  # will validate globals vs env using your improved function

# Optional: print resolved dirs (super useful)
message("CASE_DIR      = ", paths$CASE_DIR)
message("PROCESSED_DIR = ", paths$PROCESSED_DIR)
message("OUT_DIR       = ", paths$OUT_DIR)

# Optional: run clean_data.R (dev convenience) using resolved CASE_DIR
clean_script <- file.path(paths$CASE_DIR, "scripts", "clean_data.R")

if (RUN_CLEAN_DATA_FIRST) {
  if (!file.exists(clean_script)) stop("clean_data.R not found at: ", clean_script)
  message("↻ RUN_CLEAN_DATA_FIRST=TRUE: sourcing ", clean_script)
  source(clean_script, local = FALSE, chdir = FALSE)
  
  if (STOP_AFTER_CLEAN) stop("Stopped after clean_data.R (STOP_AFTER_CLEAN=TRUE)")
}

# Load processed data (always load from disk for deterministic analysis)
time_rds  <- file.path(paths$PROCESSED_DIR, "time_processed.rds")
pay_rds   <- file.path(paths$PROCESSED_DIR, "pay_processed.rds")
class_rds <- file.path(paths$PROCESSED_DIR, "class_processed.rds")

if (!file.exists(time_rds))  stop("Missing: ", time_rds)
if (!file.exists(pay_rds))   stop("Missing: ", pay_rds)
if (!file.exists(class_rds)) stop("Missing: ", class_rds)

time1  <- readRDS(time_rds)
pay1   <- readRDS(pay_rds)
class1 <- readRDS(class_rds)

message("✓ loaded processed data from: ", paths$PROCESSED_DIR)


# # Merge time1 with class1 (if needed in order to get key information from Class List)
# time1 <- merge(
#   time1,
#   class1[, .(Class_ID, Class_Location = Location, Class_Job = Job)],
#   by.x = "ID",
#   by.y = "Class_ID",
#   all.x = TRUE
# )
# 
# # Merge pay1 with class1 (if needed in order to get key information from Class List) 
# pay1 <- merge(
#   pay1,
#   class1[, .(Class_ID, Class_Location = Pay_Location, Class_Job = Pay_Job)],
#   by.x = "Pay_ID",
#   by.y = "Class_ID",
#   all.x = TRUE
# )


# ----- ALL DATA:                DEV only - test subset of data (if needed) --------------------------
TEST_SUBSET <- FALSE
SEED_VAL    <- 99999
# Choose ONE of these (leave the others as NA/NULL)
# This prioritizes TEST_IDS if provided. To use random sampling, set TEST_IDS <- NULL.
# The intersect() ensures only filter to IDs that exist, with a warning if some are missing.
TEST_N      <- NA
TEST_PCT    <- .03
TEST_IDS    <- NULL # e.g NULL or c(561405, 421424) specific IDs to keep (numeric or character)

if (TEST_SUBSET) {
  
  if (!is.null(TEST_IDS) && length(TEST_IDS) > 0) {
    # Extract ID vector from data.table
    id_vec <- all_ids$ID
    
    # Coerce TEST_IDS to match the type
    if (is.numeric(id_vec)) {
      test_ids_typed <- as.numeric(TEST_IDS)
    } else {
      test_ids_typed <- as.character(TEST_IDS)
    }
    
    ids_to_keep <- intersect(test_ids_typed, id_vec)
    
    if (length(ids_to_keep) == 0) {
      stop("None of the specified TEST_IDS found in all_ids")
    }
    if (length(ids_to_keep) < length(TEST_IDS)) {
      warning(sprintf("%d of %d TEST_IDS not found in data", 
                      length(TEST_IDS) - length(ids_to_keep), length(TEST_IDS)))
    }
    
    pay1   <- pay1[Pay_ID %in% ids_to_keep]
    time1  <- time1[ID %in% ids_to_keep]

    message(sprintf("Filtered to %d specified ID(s): %s", 
                    length(ids_to_keep), 
                    paste(ids_to_keep, collapse = ", ")))
  } else {
    # Existing random sample logic
    tmp <- run_test_sample_from_all_ids(
      all_ids  = all_ids,
      pay1     = pay1,
      time1    = time1,
      test_n   = TEST_N,
      test_pct = TEST_PCT,
      seed_val = SEED_VAL
    )
    
    pay1   <- tmp$pay1
    time1  <- tmp$time1
  }
}


# ----- PAY DATA:                CA Min Wage table -------------------------

setDT(pay1)

# Create CA minimum wage table
CA_min_wage <- data.table(
  beg_date = as.Date(paste0(2015:2026, "-01-01")),
  end_date = as.Date(paste0(2015:2026, "-12-31")),
  CA_min_wage = c(9.00, 10.00, 10.50, 11.00, 12.00, 13.00, 14.00, 15.00, 15.50, 16.00, 16.50, 16.90)
)

# Join CA minimum wage using foverlaps
pay1[, `:=`(Pay_Date_Start = Pay_Date, Pay_Date_End = Pay_Date)]
setkey(CA_min_wage, beg_date, end_date)
setkey(pay1, Pay_Date_Start, Pay_Date_End)
pay1 <- foverlaps(pay1, CA_min_wage, by.x = c("Pay_Date_Start", "Pay_Date_End"), type = "within")
pay1[, c("Pay_Date_Start", "Pay_Date_End", "beg_date", "end_date") := NULL]

pay1[, double_CA_min_wage := CA_min_wage * 2]


# ----- PAY DATA:                Unique identifiers and calculated pay rate -------------------

# Base identifiers
pay1[, `:=`(
  Pay_ID_Period_End = paste(Pay_ID, Pay_Period_End, sep = "_"),
  Prior_Pay_Date = Pay_Date - days(mode_days_btwn_pay_period_ends),
  Prior_Pay_Period_End = Pay_Period_End - days(mode_days_btwn_pay_period_ends)
)]

pay1[, Pay_ID_Prior_Period_End := paste(Pay_ID, Prior_Pay_Period_End, sep = "_")]

# Time period identifiers (quarterly, semi-annual, annual)
pay1[, `:=`(
  # Quarterly
  Current_Qtr = paste(year(Pay_Date), paste0("Q", quarter(Pay_Date)), sep = "_"),
  Prior_Qtr = fifelse(
    quarter(Pay_Date) == 1, 
    paste(year(Pay_Date) - 1, "Q4", sep = "_"), 
    paste(year(Pay_Date), paste0("Q", quarter(Pay_Date) - 1), sep = "_")
  ),
  # Semi-annual
  Current_Semi_Ann = paste(year(Pay_Date), fifelse(quarter(Pay_Date) <= 2, "H1", "H2"), sep = "_"),
  Prior_Semi_Ann = fifelse(
    quarter(Pay_Date) <= 2,
    paste(year(Pay_Date) - 1, "H2", sep = "_"),
    paste(year(Pay_Date), "H1", sep = "_")
  ),
  # Annual
  Current_Yr = as.character(year(Pay_Date)),
  Prior_Yr = as.character(year(Pay_Date) - 1)
)]

# Create composite IDs for all periods
pay1[, `:=`(
  Pay_ID_Current_Qtr = paste(Pay_ID, Current_Qtr, sep = "_"),
  Pay_ID_Prior_Qtr = paste(Pay_ID, Prior_Qtr, sep = "_"),
  Pay_ID_Current_Semi_Ann = paste(Pay_ID, Current_Semi_Ann, sep = "_"),
  Pay_ID_Prior_Semi_Ann = paste(Pay_ID, Prior_Semi_Ann, sep = "_"),
  Pay_ID_Current_Yr = paste(Pay_ID, Current_Yr, sep = "_"),
  Pay_ID_Prior_Yr = paste(Pay_ID, Prior_Yr, sep = "_")
)]

# Sort and create period/employee flags
setorder(pay1, Pay_ID, Pay_ID_Period_End, Pay_Code, Pay_Hours, Pay_Amount)

pay1[, `:=`(
  Pay_pp = fifelse(shift(Pay_ID_Period_End, type = "lead") == Pay_ID_Period_End, 0, 1),
  Pay_ee = fifelse(shift(Pay_ID, type = "lead") == Pay_ID, 0, 1)
)]
pay1[is.na(Pay_pp), Pay_pp := 1]
pay1[is.na(Pay_ee), Pay_ee := 1]

# Calculate hourly rate
pay1[, Calc_Rate := fifelse(
  Pay_Hours > 0 & Pay_Amount != 0,
  round(Pay_Amount / Pay_Hours, 4),
  0
)]


# ----- PAY DATA:                Pay code categorization -------------------------

# Examine existing pay codes
sorted_pay_codes <- sort(unique(pay1$Pay_Code))
print(sorted_pay_codes)

# # NOTE: NORMALLY THIS IS NOT NEEDED
# # Create the mapping for standardizing pay codes (if needed)
# pay_code_mapping <- c(
#   # Bonus codes
#   "INCNT" = "INC",
#   "INCENTIV" = "INC",
#   "BON" = "BONUS",
#   "SPIFF" = "SPIFF",
#   "Spiff" = "SPIFF",
# 
#   # Time codes
#   "DT" = "DT",
#   "DOUBLE" = "DT",
#   "OT" = "OT",
#   "OVERTIM" = "OT",
#   "REG" = "REG",
#   "REGULAR" = "REG",
#   "TRAIN" = "TRAIN",
#   "TRAINING" = "TRAIN",
#   
#   # Meal/Rest codes
#   "MEAL" = "MEAL",
#   "Meal Premi" = "MEAL",
#   
#   # Leave codes
#   "VAC" = "VAC",
#   "VACATIO" = "VAC",
#   "HOL" = "HOL",
#   "HOLIDAY" = "HOL",
#   
#   # Other codes
#   "RR" = "REST_RECOVERY",
#   "Rest Recov" = "REST_RECOVERY"
# )
# 
# # Update Pay_Code column directly
# pay1[, Pay_Code := ifelse(Pay_Code %in% names(pay_code_mapping), 
#                           pay_code_mapping[Pay_Code], 
#                           Pay_Code)]
# 
# # Check the results
# unique(pay1$Pay_Code)
# 
# updated_sorted_pay_codes <- sort(unique(pay1$Pay_Code))
# print(updated_sorted_pay_codes)

setDT(pay1)

pay1[, Pay_Code_Orig := Pay_Code]

pay1[, Pay_Code := {
  x <- trimws(Pay_Code)
  x <- gsub("^E\\s+", "", x)
  x <- gsub("\\s+US$", "", x)
  x <- gsub("\\s+US\\s+WFM.*$", "", x)
  x <- gsub("\\s+WFM.*$", "", x)
  x <- gsub("\\s*\\(.*?Payroll.*?\\)", "", x, perl = TRUE)
  x <- gsub("\\s*\\(.*?PETERMAN.*?\\)", "", x, perl = TRUE)
  x <- gsub("\\s*-.*$", "", x)
  x <- gsub("\\s{2,}", " ", x)
  trimws(x)
}]

pay1[, Pay_Code := {
  x <- Pay_Code
  only_hours <- grepl("^\\s*(Hrs|Hours)\\s*$", x, ignore.case = TRUE)
  x[!only_hours] <- gsub("\\b(Hrs|Hours)\\b", "", x[!only_hours], ignore.case = TRUE)
  x <- gsub("\\s{2,}", " ", x)
  trimws(x)
}]

pay1[Pay_Code %in% c("SBus Aide"),           Pay_Code := "Bus Aide"]
pay1[Pay_Code %in% c("SBus Washing"),        Pay_Code := "Bus Washing"]
pay1[Pay_Code %in% c("SClassroom"),          Pay_Code := "Classroom"]
pay1[Pay_Code %in% c("SDry Run"),            Pay_Code := "Dry Run"]
pay1[Pay_Code %in% c("SField Trip"),         Pay_Code := "Field Trip"]
pay1[Pay_Code %in% c("SGuaranteed"),         Pay_Code := "Guaranteed"]
pay1[Pay_Code %in% c("SHoliday"),            Pay_Code := "Holiday"]
pay1[Pay_Code %in% c("SHome to School"),     Pay_Code := "Home to School"]
pay1[Pay_Code %in% c("SSafety Meeting"),     Pay_Code := "Safety and Accident Meeting"]
pay1[Pay_Code %in% c("SStandby or Cover"),   Pay_Code := "Standby or Cover"]
pay1[Pay_Code %in% c("SRegular"),            Pay_Code := "Regular"]
pay1[Pay_Code %in% c("SRegular US (WFM ONLY)"),            Pay_Code := "SRg US"]


pay1[Pay_Code %in% c("REG", "Regular Wages", "Regular Hours"), Pay_Code := "Regular"]
pay1[Pay_Code %in% c("Maint Support", "Maintenance Support", "Maintenance"), Pay_Code := "Maintenance"]
pay1[Pay_Code %in% c("Misc Ops Supp", "Misc Operations Support"), Pay_Code := "Misc Operations Support"]
pay1[Pay_Code %in% c("Misc Net Amt", "Misc Net Amount"), Pay_Code := "Misc Net Amount"]
pay1[Pay_Code %in% c("Home to Schl", "Home to School DOT"), Pay_Code := "Home to School"]
pay1[Pay_Code %in% c("Home to Schl Prime", "Home To School Prime"), Pay_Code := "Home to School Prime"]
pay1[Pay_Code %in% c("Rideshare", "Rideshare Calif", "Rideshare California"), Pay_Code := "Rideshare"]

pay1[Pay_Code %in% c("Overtime Straight"), Pay_Code := "Overtime"]

pay1[Pay_Code %in% c("OT HT Premium", "Overtime HT Premium"), Pay_Code := "OT Premium"]
pay1[Pay_Code == "OT Premium Double US California/Compass Only", Pay_Code := "OT Premium Double"]

pay1[Pay_Code == "California Meal Break", Pay_Code := "Meal Break"]
pay1[Pay_Code == "California Rest Break", Pay_Code := "Rest Break"]

pay1[Pay_Code == "Sick Taken US ONLY CODE TO BE USED FOR STATE SICK PLANS", Pay_Code := "Sick"]
pay1[Pay_Code == "CA COVID Pay", Pay_Code := "COVID Pay"]

pay1[Pay_Code == "DR Light Duty", Pay_Code := "Light Duty"]
pay1[Pay_Code == "Driver Eval", Pay_Code := "Driver Evaluation"]
pay1[Pay_Code == "Admin Driving", Pay_Code := "Administrator Driving"]

pay1[grepl("^Per Diem", Pay_Code),     Pay_Code := "Per Diem"]
pay1[grepl("^Workers Comp", Pay_Code), Pay_Code := "Workers Comp"]
pay1[grepl("^Van", Pay_Code),          Pay_Code := "Van"]

updated_sorted_pay_codes <- sort(unique(pay1$Pay_Code))
print(updated_sorted_pay_codes)

# Define pay code groups (use exact values or patterns - see "mode = " below)
# NOTE: Adapt these to match your actual pay codes
reg_pay_codes     <- c("home", "aid", "charter", "class", "safety", "shuttl", "stde", "stdr", "stib", "stmt", "stot", 
                       "strc", "train", "field", "washing", "fueling", "home2sch", "reg", "rop",
                       "1xq", "meeting", "clerical", "park out", "Wheelchair", "Dry Run", "Misc Operations Support",
                       "Maintenance", "Route Writer", "Transfer Driving", "Recruitment", "Light Duty", "Dispatching",
                       "Admin", "Driver Evaluation", "Administrator Driving", "Office", "Van"
)
ot_pay_codes      <- c("overtime", "otstraight", "ot straight", "ovt", "ot premium")
dt_pay_codes      <- c("double")
bon_pay_codes     <- c("Commission", "Bonus")
meal_pay_codes    <- c("meal", "brkm", "dhpb")
rest_pay_codes    <- c("rest")
diff_pay_codes    <- c("diff")
diff_ot_pay_codes <- c("diffot")
diff_dt_pay_codes <- c("diffdt")
sick_pay_codes    <- c("sick", "ccvd", "covid")

# Configure matching mode
pay_code_config <- list(
  OT      = list(codes = ot_pay_codes,      mode = "contains"),
  DT      = list(codes = dt_pay_codes,      mode = "contains"),
  Reg     = list(codes = reg_pay_codes,     mode = "contains"),
  Bon     = list(codes = bon_pay_codes,     mode = "contains"),
  Meal    = list(codes = meal_pay_codes,    mode = "contains"),
  Rest    = list(codes = rest_pay_codes,    mode = "contains"),
  Diff    = list(codes = diff_pay_codes,    mode = "contains"),
  Diff_OT = list(codes = diff_ot_pay_codes, mode = "exact"),
  Diff_DT = list(codes = diff_dt_pay_codes, mode = "exact"),
  Sick    = list(codes = sick_pay_codes,    mode = "contains")
)

# Helper function for flexible matching
match_pay_codes <- function(pay_code_col, code_list, mode = "contains") {
  if (mode == "exact") {
    return(as.integer(pay_code_col %in% code_list))
  } else if (mode == "contains") {
    pattern <- paste(tolower(code_list), collapse = "|")
    return(as.integer(grepl(pattern, tolower(pay_code_col))))
  } else {
    stop("Invalid mode. Use 'exact' or 'contains'")
  }
}

# Create indicator columns
for (type_name in names(pay_code_config)) {
  config <- pay_code_config[[type_name]]
  col_name <- paste0(type_name, "_Pay_Code")
  pay1[, (col_name) := match_pay_codes(Pay_Code, config$codes, config$mode)]
}

# Create composite indicators
pay1[, `:=`(
  Hrs_Wkd_Pay_Code = as.integer(Reg_Pay_Code == 1 | OT_Pay_Code == 1 | DT_Pay_Code == 1),
  RROP_Pay_Code = as.integer(
    Reg_Pay_Code == 1 | OT_Pay_Code == 1 | DT_Pay_Code == 1 | 
      Bon_Pay_Code == 1 | Diff_Pay_Code == 1 | Meal_Pay_Code == 1 | 
      Rest_Pay_Code == 1 | Sick_Pay_Code == 1
  )
)]

# Flag OT and DT differentials based on OT and Diff pay groups
pay1[, `:=`(
  Diff_OT_Pay_Code = as.integer(Diff_Pay_Code == 1 & OT_Pay_Code == 1),
  Diff_DT_Pay_Code = as.integer(Diff_Pay_Code == 1 & DT_Pay_Code == 1))
]

# Ensure Reg is not also flagged as OT or DT
pay1[OT_Pay_Code == 1 | DT_Pay_Code == 1, Reg_Pay_Code := 0]

# Pay code categories table
flag_cols <- c(
  "Hrs_Wkd_Pay_Code", "OT_Pay_Code", "DT_Pay_Code", "Reg_Pay_Code", "Bon_Pay_Code", 
  "Meal_Pay_Code", "Rest_Pay_Code", "Diff_Pay_Code", "Diff_OT_Pay_Code", 
  "Diff_DT_Pay_Code", "Sick_Pay_Code", "RROP_Pay_Code"
)

# Generate pay code summary
pay_code_summary_tbl <- pay_code_summary(
  pay1,
  file.path(OUT_DIR, "Pay_Code_Summary.csv"),
  separate_key_gps = separate_key_gps
)

# Generate pay code categories table
pay_code_categories_tbl <- categorize_pay_codes(
  df = pay1,
  flag_columns = flag_cols,
  output_csv = file.path(OUT_DIR, "Pay_Code_Categories.csv")
)

setDT(pay1)

# Group by Pay_ID_Period_End and calculate the flags
pay1[, `:=`(
  pp_Hrs_Wkd_Pay_Code = as.integer(any(Hrs_Wkd_Pay_Code == 1)),
  pp_Reg_Pay_Code     = as.integer(any(Reg_Pay_Code == 1)),
  pp_OT_Pay_Code      = as.integer(any(OT_Pay_Code == 1)),
  pp_DT_Pay_Code      = as.integer(any(DT_Pay_Code == 1)),
  pp_Bon_Pay_Code     = as.integer(any(Bon_Pay_Code == 1)),
  pp_Meal_Pay_Code    = as.integer(any(Meal_Pay_Code == 1)),
  pp_Rest_Pay_Code    = as.integer(any(Rest_Pay_Code == 1)),
  pp_Diff_Pay_Code    = as.integer(any(Diff_Pay_Code == 1)),
  pp_Diff_OT_Pay_Code = as.integer(any(Diff_OT_Pay_Code == 1)),
  pp_Diff_DT_Pay_Code = as.integer(any(Diff_DT_Pay_Code == 1)),
  pp_Sick_Pay_Code    = as.integer(any(Sick_Pay_Code == 1)),
  pp_RROP_Pay_Code    = as.integer(any(RROP_Pay_Code == 1))
), by = .(Pay_ID_Period_End)]


# ----- PAY DATA:                Base rate analysis -----

# Identify base rates from regular pay
pay1[, Base_Rate_Row := fifelse(
  Reg_Pay_Code == 1 & Diff_Pay_Code == 0 & Calc_Rate > 0 & is.finite(Calc_Rate), 
  round(Calc_Rate, 2), 
  NA_real_
)]

# Count unique valid rates per period
pay1[, Unique_Valid_Rates := uniqueN(Base_Rate_Row[!is.na(Base_Rate_Row)], na.rm = TRUE), by = Pay_ID_Period_End]

# Get top 3 base rates per period
rate_hours_summary <- pay1[
  !is.na(Base_Rate_Row),
  .(Total_Hours = sum(Pay_Hours, na.rm = TRUE)),
  by = .(Pay_ID_Period_End, Base_Rate_Row)
][
  order(Pay_ID_Period_End, -Total_Hours)
][
  , Rate_Rank := seq_len(.N), by = Pay_ID_Period_End
]

# Pivot to get Base_Rate1, Base_Rate2, Base_Rate3
base_rates_summary <- dcast(
  rate_hours_summary[Rate_Rank <= 3],
  Pay_ID_Period_End ~ Rate_Rank,
  value.var = "Base_Rate_Row"
)

# Ensure all three rate columns exist
for (col in c("1", "2", "3")) {
  if (!col %in% names(base_rates_summary)) {
    base_rates_summary[, (col) := NA_real_]
  }
}

# Rename columns
setnames(base_rates_summary, old = c("1", "2", "3"), new = c("Base_Rate1", "Base_Rate2", "Base_Rate3"))

# Merge base rates
pay1 <- safe_left_join(pay1, base_rates_summary, by = "Pay_ID_Period_End")

# Convert Base_Rate 0 values to NA
pay1[, Base_Rate1 := fifelse(Base_Rate1 == 0, NA_real_, Base_Rate1)]
pay1[, Base_Rate2 := fifelse(Base_Rate2 == 0, NA_real_, Base_Rate2)]
pay1[, Base_Rate3 := fifelse(Base_Rate3 == 0, NA_real_, Base_Rate3)]

# Fill Base_Rate1 forward/backward within employee
pay1[, Base_Rate1 := nafill(nafill(Base_Rate1, type = "locf"), type = "nocb"), by = Pay_ID]

# Calculate multipliers - create Pay_Rate if it doesn't exist
if(!"Pay_Rate" %in% names(pay1)) {
  pay1[, Pay_Rate := NA_real_]  # Create as NA if doesn't exist
}

# Calculate multipliers
pay1[, `:=`(
  Multiplier1 = fifelse(Calc_Rate == 0 | (is.na(Calc_Rate) & !is.na(Pay_Rate)), 
                        round(Pay_Rate / Base_Rate1, 2), 
                        round(Calc_Rate / Base_Rate1, 2)),
  Multiplier2 = round(Calc_Rate / Base_Rate2, 2),
  Multiplier3 = round(Calc_Rate / Base_Rate3, 2)
)]

# Clean up multipliers
for (col in c("Multiplier1", "Multiplier2", "Multiplier3")) {
  pay1[get(col) == 0, (col) := NA_real_]
}

# Categorize rate groups
pay1[, max_mult := pmax(Multiplier1, Multiplier2, Multiplier3, na.rm = TRUE)]
pay1[, Rate_Gp := fcase(
  Multiplier1 == 0.5 | Multiplier2 == 0.5 | Multiplier3 == 0.5, "0.5x",
  Multiplier1 == 1.0 | Multiplier2 == 1.0 | Multiplier3 == 1.0, "1x",
  Multiplier1 == 1.5 | Multiplier2 == 1.5 | Multiplier3 == 1.5, "1.5x",
  Multiplier1 == 2.0 | Multiplier2 == 2.0 | Multiplier3 == 2.0, "2x",
  max_mult < 0.5, "Less than 0.5x",
  max_mult < 1, "0.5x-1x",
  max_mult < 1.5, "1x-1.5x",
  max_mult < 2, "1.5x-2x",
  max_mult > 2, "Greater than 2x",
  default = "N/A"
)]

setDT(pay1)

# Below min wage check
pay1[, Below_CA_Min := as.integer(Calc_Rate < CA_min_wage & Calc_Rate > 0)]

# Handle off-cycle bonuses
pay1[, is_off_cycle := as.integer(all(Bon_Pay_Code == 1, na.rm = TRUE)), by = Pay_ID_Period_End]
pay1[, prev_Pay_Date := shift(Pay_Date, type = "lag"), by = Pay_ID]
pay1[, Pay_Date_Rev := fifelse(is_off_cycle == 1, prev_Pay_Date, Pay_Date)]
pay1[, Pay_ID_Pay_Date_Rev := paste(Pay_ID, Pay_Date_Rev, sep = "_")]

# Rate multiplier for cleaner logic
pay1[, Rate_Mult := fifelse(Base_Rate1 > 0, round(Calc_Rate / Base_Rate1, 2), NA_real_)]

# Rate type assignment
rate_type_threshold <- 0.2   # +/- buffer around target multipliers
thresh_max <- 5              # Max rate for "Premium Only"

pay1[, Rate_Mult := fifelse(Base_Rate1 > 0, round(Calc_Rate / Base_Rate1, 2), NA_real_)]

pay1[, Rate_Type := fcase(
  Calc_Rate == 0 | is.na(Calc_Rate), "No Rate",
  
  # OT
  OT_Pay_Code == 1 & Rate_Mult >= (1.5 - rate_type_threshold) & Rate_Mult <= (1.5 + rate_type_threshold), "Inclusive (1.5x)",
  OT_Pay_Code == 1 & Rate_Mult >= (0.5 - rate_type_threshold) & Rate_Mult <= (0.5 + rate_type_threshold), "Premium Only (0.5x)",
  OT_Pay_Code == 1 & Rate_Mult >= (1.0 - rate_type_threshold) & Rate_Mult <= (1.0 + rate_type_threshold), "Base Rate Inclusive (1x)",
  OT_Pay_Code == 1 & Rate_Mult >= 1.5 + rate_type_threshold, "Other Rate (Inclusive)",
  OT_Pay_Code == 1, "Other Rate (Premium Only)",
  
  # DT
  DT_Pay_Code == 1 & Rate_Mult >= (2.0 - rate_type_threshold) & Rate_Mult <= (2.0 + rate_type_threshold), "Inclusive (2x)",
  DT_Pay_Code == 1 & Rate_Mult >= (1.0 - rate_type_threshold) & Rate_Mult <= (1.0 + rate_type_threshold), "Premium Only (1x)",
  DT_Pay_Code == 1 & Rate_Mult >= 2.0 + rate_type_threshold, "Other Rate (Inclusive)",
  DT_Pay_Code == 1, "Other Rate (Premium Only)",
  
  # Regular
  Reg_Pay_Code == 1 & Below_CA_Min == 1, "Below Min Wage",
  Reg_Pay_Code == 1 & Rate_Mult < (1.0 - rate_type_threshold), "Below base, but above min wage",
  Reg_Pay_Code == 1 & Rate_Mult >= (1.0 - rate_type_threshold) & Rate_Mult <= (1.0 + rate_type_threshold), "Base Rate Inclusive (1x)",
  Reg_Pay_Code == 1 & Rate_Mult >= (1.0 + rate_type_threshold), "Other Rate (Inclusive)",
  
  # Bonus
  Bon_Pay_Code == 1 & is_off_cycle == 1, "Off Cycle",
  Bon_Pay_Code == 1, "On Cycle",
  
  # Meal
  Meal_Pay_Code == 1 & Below_CA_Min == 1, "Below Min Wage",
  Meal_Pay_Code == 1 & Rate_Mult >= (0.5 - rate_type_threshold) & Rate_Mult <= (0.5 + rate_type_threshold), "Half-time Premium",
  Meal_Pay_Code == 1 & Rate_Mult >= (1.0 - rate_type_threshold) & Rate_Mult <= (1.0 + rate_type_threshold), "Base Rate Inclusive (1x)",
  Meal_Pay_Code == 1 & Rate_Mult < (1.0 - rate_type_threshold), "Below Base Rate",
  Meal_Pay_Code == 1 & Rate_Mult > (1.0 + rate_type_threshold), "Above Base Rate",
  Meal_Pay_Code == 1, "Other Rate",
  
  # Rest
  Rest_Pay_Code == 1 & Below_CA_Min == 1, "Below Min Wage",
  Rest_Pay_Code == 1 & Rate_Mult >= (0.5 - rate_type_threshold) & Rate_Mult <= (0.5 + rate_type_threshold), "Half-time Premium",
  Rest_Pay_Code == 1 & Rate_Mult >= (1.0 - rate_type_threshold) & Rate_Mult <= (1.0 + rate_type_threshold), "Base Rate Inclusive (1x)",
  Rest_Pay_Code == 1 & Rate_Mult < (1.0 - rate_type_threshold), "Below Base Rate",
  Rest_Pay_Code == 1 & Rate_Mult > (1.0 + rate_type_threshold), "Above Base Rate",
  Rest_Pay_Code == 1, "Other Rate",
  
  # Sick
  Sick_Pay_Code == 1 & Below_CA_Min == 1, "Below Min Wage",
  Sick_Pay_Code == 1 & Rate_Mult >= (0.5 - rate_type_threshold) & Rate_Mult <= (0.5 + rate_type_threshold), "Half-time Premium",
  Sick_Pay_Code == 1 & Rate_Mult >= (1.0 - rate_type_threshold) & Rate_Mult <= (1.0 + rate_type_threshold), "Base Rate Inclusive (1x)",
  Sick_Pay_Code == 1 & Rate_Mult < (1.0 - rate_type_threshold), "Below Base Rate",
  Sick_Pay_Code == 1 & Rate_Mult > (1.0 + rate_type_threshold), "Above Base Rate",
  Sick_Pay_Code == 1, "Other Rate",
  
  # Differential
  Diff_Pay_Code == 1 & Calc_Rate < thresh_max, "Premium Only",
  Diff_Pay_Code == 1 & Rate_Mult >= (0.5 - rate_type_threshold) & Rate_Mult <= (0.5 + rate_type_threshold), "Half-time Premium",
  Diff_Pay_Code == 1 & Rate_Mult >= (1.0 - rate_type_threshold) & Rate_Mult < (1.5 - rate_type_threshold), "Includes Base",
  Diff_Pay_Code == 1 & Rate_Mult >= (1.5 - rate_type_threshold) & Rate_Mult < (2.0 - rate_type_threshold), "Includes Base & OT (1.5x)",
  Diff_Pay_Code == 1 & Rate_Mult >= (2.0 - rate_type_threshold) & Rate_Mult <= (2.0 + rate_type_threshold), "Includes Base & DT (2x)",
  Diff_Pay_Code == 1, "Other Rate",
  
  # Uncategorized
  RROP_Pay_Code == 0 & Rate_Mult >= (1.0 - rate_type_threshold) & Rate_Mult <= (1.0 + rate_type_threshold), "Uncat - At Base (1x)",
  RROP_Pay_Code == 0 & Rate_Mult >= (0.5 - rate_type_threshold) & Rate_Mult <= (0.5 + rate_type_threshold), "Uncat - Half-time",
  RROP_Pay_Code == 0, "Uncategorized",
  
  default = "Other"
)]

# Rate type summary table
rate_type_summary <- pay1[, .(
  Records = .N,
  Employees = uniqueN(Pay_ID, na.rm = TRUE),
  Hours = round(sum(Pay_Hours, na.rm = TRUE), 1),
  Amount = round(sum(Pay_Amount, na.rm = TRUE), 2),
  Avg_Rate = round(mean(Calc_Rate[Calc_Rate > 0], na.rm = TRUE), 2),
  Avg_Mult = round(mean(Rate_Mult[Rate_Mult > 0], na.rm = TRUE), 2),
  Rate_Type = names(sort(table(Rate_Type), decreasing = TRUE))[1],
  Rate_Type_Pct = round(max(table(Rate_Type)) / .N, 6)
), by = Pay_Code][order(-Records)]


# ----- PAY DATA:                Rate type adjustment (premium-only pay codes are NOT hours worked) -------------------

setDT(pay1)

pay_code_rate_type_map <- rate_type_summary[
  , .(Pay_Code, Pay_Code_Rate_Type = Rate_Type)
]

pay1 <- pay_code_rate_type_map[pay1, on = "Pay_Code"]

pay1[
  Hrs_Wkd_Pay_Code == 1 & !grepl("inclu", tolower(Pay_Code_Rate_Type)),
  Hrs_Wkd_Pay_Code := 0
]

# Reset set hours worked pay periods flags
pay1[, `:=`(
  pp_Hrs_Wkd_Pay_Code = as.integer(any(Hrs_Wkd_Pay_Code == 1))
), by = .(Pay_ID_Period_End)]

# Re-run pay code summary
pay_code_summary_tbl <- pay_code_summary(
  pay1,
  file.path(OUT_DIR, "Pay_Code_Summary.csv"),
  separate_key_gps = separate_key_gps
)

# Re-run pay code categories table
pay_code_categories_tbl <- categorize_pay_codes(
  df = pay1,
  flag_columns = flag_cols,
  output_csv = file.path(OUT_DIR, "Pay_Code_Categories.csv")
)

# Calculate straight time amounts (needed for group_pay_data function)
pay1[, Straight_Time_Amt := fifelse(
  Reg_Pay_Code == 1 & Hrs_Wkd_Pay_Code == 1 | (Diff_Pay_Code == 1 & Diff_OT_Pay_Code != 1 & Diff_DT_Pay_Code != 1), Pay_Amount,
  fifelse(
    OT_Pay_Code == 1 & Hrs_Wkd_Pay_Code == 1 | Diff_OT_Pay_Code == 1, Pay_Amount / 1.5,
    fifelse(DT_Pay_Code == 1 & Hrs_Wkd_Pay_Code == 1 | Diff_DT_Pay_Code == 1, Pay_Amount / 2, 0))
)]


# Join pay code category and move to last column
rate_type_summary <- pay_code_categories_tbl[rate_type_summary, on = "Pay_Code"]
setcolorder(rate_type_summary, c(setdiff(names(rate_type_summary), "Pay_Code_Category"), "Pay_Code_Category"))

print(rate_type_summary)

write_csv_and_rds(rate_type_summary, file.path(OUT_DIR, "Rate_Type_Analysis.csv"))


# ----- PAY DATA:                Separate earnings period aggregations -------------------

# Create period-level summaries using group_pay_data function (UPDATED GROUP PAY DATA!)
pay1_pp <- group_pay_data(pay1, group_by = "Pay_ID_Period_End")
pay1_qtr <- group_pay_data(pay1, group_by = "Pay_ID_Current_Qtr")

# Join summaries with appropriate prefixes
pay1 <- safe_left_join(pay1, pay1_pp, by = "Pay_ID_Period_End", prefix = "pp")
pay1 <- safe_left_join(pay1, pay1_pp, by = c("Pay_ID_Prior_Period_End" = "Pay_ID_Period_End"), prefix = "prior_pp")
pay1 <- safe_left_join(pay1, pay1_qtr, by = "Pay_ID_Current_Qtr", prefix = "qtr")
pay1 <- safe_left_join(pay1, pay1_qtr, by = c("Pay_ID_Prior_Qtr" = "Pay_ID_Current_Qtr"), prefix = "prior_qtr")

# Clean negative hours - find all columns with "prior_" prefix
negative_hour_cols <- names(pay1)[grepl("^prior_", names(pay1))]

# Set negative values to 0 for all prior_ columns
for (col in negative_hour_cols) {
  if (col %in% names(pay1)) {
    pay1[get(col) < 0 & !is.na(get(col)), (col) := 0]
  }
}


# ----- PAY DATA:                Regular Rate Analysis-----------------------------------------------------------------------------------

# Set multipliers and parameters
var_OT_multiplier <- 1.5
var_DT_multiplier <- 2
var_half_time_OT_multiplier <- 0.5
var_half_time1_multiplier <- 1
rrop_buffer <- 0.05

# --- BONUS-SPECIFIC REGULAR RATE ANALYSIS ---

# Get unique bonus pay codes
bonus_pay_code_values <- unique(pay1[Bon_Pay_Code == 1, Pay_Code])

# Create columns for each bonus type
for (pay_code in bonus_pay_code_values) {
  pay1[, (pay_code) := fifelse(
    Pay_Code == pay_code & Pay_Amount > 0, 
    Pay_Amount, 
    0
  )]
}

rrop_cols <- bonus_pay_code_values

# Calculate FLSA regular rates (divide by total hours worked) **** ADJUST AS NEEDED TO DIFFERENT EARNINGS PERIODS ****
for (rrop_col in rrop_cols) {
  rate_col <- paste0(rrop_col, "_FLSA_rrop")
  pay1[, (rate_col) := fifelse(pp_Hrs_Wkd > 0, get(rrop_col) / pp_Hrs_Wkd, 0)]
}

# Define multipliers if not already defined
if (!exists("var_half_time_OT_multiplier")) var_half_time_OT_multiplier <- 0.5
if (!exists("var_half_time1_multiplier")) var_half_time1_multiplier <- 1.0

# Calculate FLSA OT and DT wages
rate_types <- "FLSA_rrop"
for (val in rrop_cols) {
  for (rate in rate_types) {
    val_col <- paste0(val, "_", rate)
    
    if (val_col %in% names(pay1)) {
      # OT wage calculation
      ot_wage_col <- paste0(val_col, "_OT_wage")
      pay1[, (ot_wage_col) := get(val_col) * pp_OT_Hrs * var_half_time_OT_multiplier]
      
      # DT wage calculation
      dt_wage_col <- paste0(val_col, "_DT_wage")
      pay1[, (dt_wage_col) := get(val_col) * pp_DT_Hrs * var_half_time1_multiplier]
      
      # Meal wage calculation - use pp_Meal_Prem_Hrs
      meal_wage_col <- paste0(val_col, "_Meal_wage")
      pay1[, (meal_wage_col) := get(val_col) * pp_Meal_Prem_Hrs]
      
      # Rest wage calculation - use pp_Rest_Prem_Hrs
      rest_wage_col <- paste0(val_col, "_Rest_wage")
      pay1[, (rest_wage_col) := get(val_col) * pp_Rest_Prem_Hrs]
      
      # Sick wage calculation - pp_Sick_Hrs is correct
      sick_wage_col <- paste0(val_col, "_Sick_wage")
      pay1[, (sick_wage_col) := get(val_col) * pp_Sick_Hrs]
    }
  }
}

# Create underpayment indicator
bon_underpay_cols <- names(pay1)[grepl("FLSA_rrop_(OT|DT|Meal|Rest|Sick)_wage$", names(pay1))]

if(length(bon_underpay_cols) > 0) {
  pay1 <- pay1 %>%
    mutate(rrop_by_code_underpayment = as.integer(rowSums(select(., all_of(bon_underpay_cols)), na.rm = TRUE) > 0))
} else {
  pay1 <- pay1 %>%
    mutate(rrop_by_code_underpayment = 0)
}


# --- CURRENT PAY PERIOD RROP CALCULATIONS  (ACTUAL WAGES - TOTAL CALC WAGES) ---

setDT(pay1)

# Calculate overall RROP
pay1[, RROP := fifelse(
  pp_Hrs_Wkd > 0,
  (pp_Straight_Time_Amt + pp_Oth_RROP_Amt) / pp_Hrs_Wkd,
  Base_Rate1
)]

# Calculate expected vs actual wages
pay1[, `:=`(
  Calc_Tot_Wages = pp_Straight_Time_Amt + 
    (RROP * pp_OT_Hrs * var_half_time_OT_multiplier) +
    (RROP * pp_DT_Hrs) + 
    (RROP * pp_Meal_Prem_Hrs) + 
    (RROP * pp_Rest_Prem_Hrs) + 
    (RROP * pp_Sick_Hrs) +
    pp_Oth_RROP_Amt + pp_Oth_Amt,
  Actual_Wages = sum(Pay_Amount, na.rm = TRUE)
), by = .(Pay_ID, Pay_Date_Rev)]

# Calculate wage diff and underpayments
pay1[, `:=`(
  Wage_Diff = round(Actual_Wages - Calc_Tot_Wages, 2),
  OT_Diff = pp_OT_Amt - round((RROP * pp_OT_Hrs * var_half_time_OT_multiplier), 2),
  DT_Diff = pp_DT_Amt - round((RROP * pp_DT_Hrs), 2),
  Meal_Diff = pp_Meal_Amt - round((RROP * pp_Meal_Prem_Hrs), 2),
  Rest_Diff = pp_Rest_Amt - round((RROP * pp_Rest_Prem_Hrs), 2),
  Sick_Diff = pp_Sick_Amt - round((RROP * pp_Sick_Hrs), 2),
  
  Net_Overpayment = pmax(0, round(Actual_Wages - Calc_Tot_Wages, 2)),
  OT_Overpayment = pmax(0, pp_OT_Amt - round((RROP * pp_OT_Hrs * var_half_time_OT_multiplier), 2)),
  DT_Overpayment = pmax(0, pp_DT_Amt - round((RROP * pp_DT_Hrs), 2)),
  Meal_Overpayment = pmax(0, pp_Meal_Amt - round((RROP * pp_Meal_Prem_Hrs), 2)),
  Rest_Overpayment = pmax(0, pp_Rest_Amt - round((RROP * pp_Rest_Prem_Hrs), 2)),
  Sick_Overpayment = pmax(0, pp_Sick_Amt - round((RROP * pp_Sick_Hrs), 2)),
  
  Net_rrop_dmgs = pmax(0, round(Calc_Tot_Wages - Actual_Wages, 2)),
  OT_rrop_dmgs = pmax(0, round((RROP * pp_OT_Hrs * var_half_time_OT_multiplier) - pp_OT_Amt, 2)),
  DT_rrop_dmgs = pmax(0, round((RROP * pp_DT_Hrs) - pp_DT_Amt, 2)),
  Meal_rrop_dmgs = pmax(0, round((RROP * pp_Meal_Prem_Hrs) - pp_Meal_Amt, 2)),
  Rest_rrop_dmgs = pmax(0, round((RROP * pp_Rest_Prem_Hrs) - pp_Rest_Amt, 2)),
  Sick_rrop_dmgs = pmax(0, round((RROP * pp_Sick_Hrs) - pp_Sick_Amt, 2))
)]

pay1[, `:=`(
  Gross_Overpayment = (OT_Overpayment + DT_Overpayment + Meal_Overpayment + Rest_Overpayment + Sick_Overpayment),
  Gross_rrop_dmgs = (OT_rrop_dmgs + DT_rrop_dmgs + Meal_rrop_dmgs + Rest_rrop_dmgs + Sick_rrop_dmgs)
)]

# Apply buffer to totals
pay1[, Net_Overpayment := fifelse(Net_Overpayment < rrop_buffer | Net_Overpayment < Gross_Overpayment | Gross_Overpayment == 0, 0, Net_Overpayment)]
pay1[, Net_rrop_dmgs := fifelse(Net_rrop_dmgs < rrop_buffer | Net_rrop_dmgs > Gross_rrop_dmgs | Gross_rrop_dmgs == 0, 0, Net_rrop_dmgs)]
pay1[, Gross_Overpayment := fifelse(Gross_Overpayment < rrop_buffer, 0, Gross_Overpayment)]
pay1[, Gross_rrop_dmgs := fifelse(Gross_rrop_dmgs < rrop_buffer, 0, Gross_rrop_dmgs)]

# Create PP RROP underpayment indicator
pay1[, rrop_any_underpayment := as.integer(Gross_rrop_dmgs > 0)]
pay1[, rrop_net_underpayment := as.integer(Net_rrop_dmgs > 0)]


# ----- TIME DATA:               Key identifiers & workweeks --------

time1[, ID_Date := paste(ID, Date, sep = "_")]

# Week end day mapping for floor_date() and ceiling_date()
# week_end = 1  ->  Sunday
# week_end = 2  ->  Monday
# week_end = 3  ->  Tuesday
# week_end = 4  ->  Wednesday
# week_end = 5  ->  Thursday
# week_end = 6  ->  Friday
# week_end = 7  ->  Saturday
workweek_value = 7

# Add week ending date ADJUST AS NEEDED BASED ON THE PAY DATA
time1[, Week_End := floor_date(Date, "week", week_start = workweek_value) + days(6)]

# Add day of week name to verify
time1[, Week_End_Day := weekdays(Week_End)]

time1[, ID_Week_End := paste(ID, Week_End, sep = "-")]


# ----- TIME DATA:               Flag time off weeks, then filter out non work hrs records, split shifts, 5 hr guarantees --------

# Sort by ID and Date
setorder(time1, ID, Date)
setDT(time1)

# Split_Shift
time1[, split_shift_mark := fifelse(time1$Paycode_Name=="SPLT--SPLIT SHIFT", 1, 0)]
time1[, split_shift_mark := fifelse(is.na(split_shift_mark), 0, split_shift_mark)]
time1[, split_shift := cumsum(split_shift_mark), by = ID_Date]
time1[, split_shift := fifelse(split_shift>=1, 1, 0), by = ID_Date]

# DIHG 5 HOUR GUARANTEE
time1[, five_hour_guarantee_mark := fifelse(time1$Paycode_Name=="DIHG--GUARANTEE", 1, 0)]
time1[, five_hour_guarantee_mark := fifelse(is.na(five_hour_guarantee_mark), 0, five_hour_guarantee_mark)]
time1[, five_hour_guarantee:= cumsum(five_hour_guarantee_mark), by = ID_Date]
time1[, five_hour_guarantee := fifelse(five_hour_guarantee>=1, 1, 0), by = ID_Date]

# Ensure Hours is numeric (optional but usually wise)
time1[, Hours := as.numeric(Hours)]

# Split Shift hrs (sum Hours for ID_Date when any split shift exists in that day) ---
time1[
  , split_shift_hrs := sum(fifelse(Paycode_Name == "SPLT--SPLIT SHIFT", Hours, 0), na.rm = TRUE),
  by = ID_Date
]

# Guarantee hrs (sum Hours for ID_Date when any DIHG guarantee exists in that day) ---
time1[
  , guarantee_hrs := sum(fifelse(Paycode_Name == "DIHG--GUARANTEE", Hours, 0), na.rm = TRUE),
  by = ID_Date
]

# PTO hrs (sum Hours for ID_Date where Paycode_Name OR Paycode_Name2 contains "pto", case-insensitive) ---
time1[
  , pto_hrs := sum(fifelse(
    grepl("pto", Paycode_Name, ignore.case = TRUE) | grepl("pto", Paycode_Name2, ignore.case = TRUE), 
    Hours, 0), na.rm = TRUE),
  by = ID_Date
]

EXCLUDE_pay_codes <- c("pto", "dihg")

Time_code_config <- list(
  EXCLUDE = list(codes = EXCLUDE_pay_codes, mode = "contains")
)

for (type_name in names(Time_code_config)) {
  config <- Time_code_config[[type_name]]
  col_name <- paste0(type_name, "_Time_Code")
  time1[, (col_name) := as.integer(
    match_pay_codes(Paycode_Name, config$codes, config$mode) == 1 |
      match_pay_codes(Paycode_Name2, config$codes, config$mode) == 1
  )]
}

# Flag weeks where ANY record in that week has PTO/excluded time
time1[, wk_time_off := as.integer(sum(pto_hrs, na.rm = TRUE) > 0), by = ID_Week_End]

# Filter out time off records, but keep work code rows OR NA codes
nrow(time1) #_______
time1 <- time1[EXCLUDE_Time_Code != 1 ]
nrow(time1) #_______


# ----- TIME DATA:               Standardize and stack multiple In/Out pairs --------

# # Identify and rename all In/Out pairs to standard format
# rename_punch_pairs <- function(dt) {
#   # Define possible In/Out column patterns to look for
#   in_patterns <- c("In1", "In2", "In3", "In4", "In5", "In6")
#   out_patterns <- c("Out1", "Out2", "Out3", "Out4", "Out5", "Out6")
#   
#   # Find matching pairs
#   pair_count <- 1
#   for(i in seq_along(in_patterns)) {
#     if(in_patterns[i] %in% names(dt) && out_patterns[i] %in% names(dt)) {
#       setnames(dt,
#                old = c(in_patterns[i], out_patterns[i]),
#                new = c(paste0("In", pair_count), paste0("Out", pair_count)))
#       cat("✓ Renamed", in_patterns[i], "/", out_patterns[i],
#           "to In", pair_count, "/Out", pair_count, "\n")
#       pair_count <- pair_count + 1
#     }
#   }
#   return(dt)
# }
# 
# # Stack the standardized pairs
# stack_punch_pairs <- function(dt, max_pairs = 8, non_repeat_cols = c("Hours")) {
#   
#   # Add original sort order
#   dt[, orig_sort := .I]
#   
#   # Rename to standard format first
#   dt <- rename_punch_pairs(dt)
#   
#   # Stack all In/Out pairs
#   stacked_list <- list()
#   
#   for(i in 1:max_pairs) {
#     in_col <- paste0("In", i)
#     out_col <- paste0("Out", i)
#     
#     if(in_col %in% names(dt) && out_col %in% names(dt)) {
#       # Create a copy for this pair
#       temp <- copy(dt)
#       
#       # Standardize to "In" and "Out" for final output
#       setnames(temp, c(in_col, out_col), c("In", "Out"))
#       
#       # Add pair identifier
#       temp[, punch_pair := i]
#       
#       # For pairs after the first, zero out non-repeat columns
#       if(i > 1) {
#         for(col in non_repeat_cols) {
#           if(col %in% names(temp)) {
#             if(is.numeric(temp[[col]])) {
#               temp[, (col) := 0]
#             } else {
#               temp[, (col) := NA]
#             }
#           }
#         }
#       }
#       
#       # Keep only rows with at least one punch
#       temp <- temp[!is.na(In) | !is.na(Out)]
#       
#       if(nrow(temp) > 0) {
#         stacked_list[[i]] <- temp
#         cat("✓ Stacked In", i, "/Out", i, ": ", nrow(temp), " rows\n", sep="")
#       }
#     }
#   }
#   
#   if(length(stacked_list) > 0) {
#     # Combine all
#     result <- rbindlist(stacked_list, fill = TRUE)
#     
#     # Sort by original order
#     setorder(result, ID, Date, orig_sort, punch_pair)
#     
#     # Recalculate hours
#     result[, Hours_Calc := as.numeric(difftime(Out, In, units = "hours"))]
#     
#     cat("\n✓ Total stacked:", nrow(result), "rows from",
#         length(stacked_list), "punch pairs\n")
#     
#     return(result)
#   } else {
#     cat("No In/Out pairs found\n")
#     return(dt)
#   }
# }
# 
# time1 <- stack_punch_pairs(time1,
#                            max_pairs = 8,
#                            non_repeat_cols = c("Hours", "Pay_Amount", "Tips"))
# 
# 
# # Optional: Remove rows where both In and Out are NA (no punch recorded)
# time1 <- time1[!is.na(In) | !is.na(Out)]
# 
# # Check the result
# time1[1:10, .(ID, Date, In, Out, orig_sort)]


# ----- TIME DATA:               Merge dates with In and Out columns, accounting for those that cross midnight ----------

# Check for missing In or Out punches BEFORE datetime merge
missing_punches <- time1[is.na(In) | is.na(Out), .N]

if (missing_punches > 0) {
  cat("\n⚠ Found", missing_punches, "rows with missing In or Out punches\n")
  cat("  Missing In:", time1[is.na(In), .N], "\n")
  cat("  Missing Out:", time1[is.na(Out), .N], "\n")
  
  FILTER_MISSING_PUNCHES <- TRUE  # Set to FALSE to keep rows with missing punches
  
  if (FILTER_MISSING_PUNCHES) {
    time1_missing_punches <- time1[is.na(In) | is.na(Out)]
    time1 <- time1[!is.na(In) & !is.na(Out)]
    cat("✓ Filtered out", nrow(time1_missing_punches), "rows → saved to 'time1_missing_punches'\n")
    cat("  Remaining rows:", nrow(time1), "\n")
  } else {
    cat("  Keeping rows with missing punches (FILTER_MISSING_PUNCHES = FALSE)\n")
    time1_missing_punches <- data.table()  # Empty placeholder
  }
} else {
  cat("✓ No missing In/Out punches\n")
  time1_missing_punches <- data.table()  # Empty placeholder
}

# Flag rows where In/Out contain a real (non-1899) date
time1[, has_real_in  := !is.na(In)  & year(In)  != 1899L]
time1[, has_real_out := !is.na(Out) & year(Out) != 1899L]

has_real_dates <- time1[, any(has_real_in) || any(has_real_out)]

# Total records (use .N directly; this is the same as nrow(time1))
total_records <- time1[, .N]

# Helper for percent (safe)
pct <- function(n, d) if (is.na(d) || d == 0) NA_real_ else round(100 * n / d, 2)

if (has_real_dates) {
  cat("✓ Found existing dates in In/Out columns (not 1899 placeholder)\n")
  
  # HYBRID NOTE:
  # Some datasets contain a mix of true datetimes (real dates) and time-only values (1899 placeholders).
  # This script preserves real-dated rows and only merges Date + time-of-day for placeholder rows.
  
  # Only meaningful to compare when In has a real date
  time1[has_real_in == TRUE,  In_Date_Match := (as.Date(In) == Date)]
  time1[has_real_in == FALSE, In_Date_Match := NA]
  
  total_with_dates    <- time1[has_real_in == TRUE,  .N]
  total_without_dates <- time1[has_real_in == FALSE, .N]
  
  cat("\nDate Validation Summary:\n")
  
  cat("  Records with dates already included in the In/Out time punches: ",
      total_with_dates, " of ", total_records, " (", pct(total_with_dates, total_records), "%)\n", sep = "")
  
  cat("  Records without dates in the In/Out time punches (1899 placeholder): ",
      total_without_dates, " of ", total_records, " (", pct(total_without_dates, total_records), "%)\n", sep = "")
} else {
  cat("✓ All In/Out times have 1899 placeholder dates\n")
  time1[, In_Date_Match := NA]
  
  cat("\nDate Validation Summary:\n")
  cat("  Records without dates in the In/Out time punches (1899 placeholder): ",
      total_records, " of ", total_records, " (100%)\n", sep = "")
}

# Date from In/Out columns concatenated with In and Out punches
# HYBRID NOTE:
# - Preserve true datetimes (rows where year(In/Out) != 1899)
# - Only merge Date + time-of-day for placeholder rows (year == 1899)
time1[, In_time_str  := format(as.POSIXct(In),  "%H:%M:%S")]
time1[, Out_time_str := format(as.POSIXct(Out), "%H:%M:%S")]

time1[has_real_in  == FALSE, In  := as_datetime(paste(Date, In_time_str))]
time1[has_real_out == FALSE, Out := as_datetime(paste(Date, Out_time_str))]

# Check for NAs introduced during datetime merge
cat("  NA In:", time1[is.na(In), .N], "| NA Out:", time1[is.na(Out), .N], "\n")

# HYBRID NOTE:
# Rowwise midnight fix BEFORE shift creation so overnight rows don't distort hrs_since_last.
# Safe for both placeholder-merged rows and true datetime rows.
time1[!is.na(In) & !is.na(Out) & Out < In, Out := Out + days(1)]

new_shift_cutoff <- 7  #  *****  Hours between punches to consider separate shifts (ADJUST AS NEEDED)  ******** -----

# Sort by employee and time
setorder(time1, ID, Date, In, Out)

# Calculate hours since previous punch out
time1[, hrs_since_last := pmax(0, 
                               round(as.numeric(difftime(In, shift(Out, type = "lag"), units = "hours")), 2)
), by = ID]

# Mark new shifts
time1[, new_shift := fifelse(
  ID != shift(ID, type = "lag"), 1,
  fifelse(hrs_since_last > new_shift_cutoff, 1, 0)
)]
time1[is.na(new_shift), new_shift := 1]

# Create unique shift ID
time1[, shift_num := cumsum(new_shift), by = ID]
time1[, Shift_ID := paste(ID, shift_num, sep = "-")]

cat("✓ Created", uniqueN(time1$Shift_ID, na.rm = TRUE), "unique Shift IDs\n")
cat("  Avg records per shift:", round(nrow(time1) / uniqueN(time1$Shift_ID, na.rm = TRUE), 1), "\n")

time1[, duplicate_flag := duplicated(.SD), .SDcols = c("Shift_ID", "In", "Out")]

# Initialize overlap_flag (will be updated if NEEDS_MIDNIGHT_CHECK)
time1[, overlap_flag := FALSE]

# Determine if we need midnight crossing logic
NEEDS_MIDNIGHT_CHECK <- time1[!duplicate_flag & !is.na(In) & !is.na(Out), any(Out < In)]

if (NEEDS_MIDNIGHT_CHECK) {
  cat("\n=== Running midnight crossing adjustments ===\n")
  
  # Pre-sort (if not already)
  setorder(time1, Shift_ID, In)
  
  # Compute lags ONCE, vectorized (no by-group)
  time1[, `:=`(
    lag_Out      = shift(Out),
    lag_dup      = shift(duplicate_flag),
    lag_Shift_ID = shift(Shift_ID)
  )]
  
  # Midnight trigger - vectorized, only check cross-row within same shift
  time1[, midnight_trigger :=
          # Same-row midnight
          (!duplicate_flag & Out < In & as.Date(Out) == as.Date(In)) |
          # Cross-row midnight (must be same Shift_ID)
          (!duplicate_flag &
             !lag_dup %in% TRUE &
             Shift_ID == lag_Shift_ID &
             !is.na(lag_Out) &
             lag_Out > In &
             as.Date(lag_Out) == as.Date(In))]
  
  # Use cummax instead of if/any/which - much faster
  time1[, midnight_row := cummax(midnight_trigger %in% TRUE) == 1L, by = Shift_ID]
  
  midnight_shifts <- time1[(midnight_row), uniqueN(Shift_ID, na.rm = TRUE)]
  
  if (midnight_shifts > 0) {
    cat("Found", midnight_shifts, "shifts crossing midnight\n")
    
    time1[(midnight_row), `:=`(
      In  = In  + days(1),
      Out = Out + days(1)
    )]
    
    cat("✓ Adjusted times for midnight-crossing shifts\n")
  }
  
  # Cleanup temp columns
  time1[, c("midnight_trigger", "midnight_row", "lag_Out", "lag_dup", "lag_Shift_ID") := NULL]
  
  # Validation
  remaining_issues <- time1[!duplicate_flag & !is.na(In) & !is.na(Out) & Out < In, .N]
  
  if (remaining_issues > 0) {
    cat("⚠ NOTE:", remaining_issues,
        "single-row time anomalies remain (non-midnight, non-duplicate)\n")
  } else {
    cat("✓ Midnight validation passed\n")
  }
  
  # Overlap flag - also vectorized
  time1[, `:=`(
    lag_Out2      = shift(Out),
    lag_Shift_ID2 = shift(Shift_ID)
  )]
  time1[, overlap_flag := Shift_ID == lag_Shift_ID2 & !is.na(lag_Out2) & lag_Out2 > In & !duplicate_flag]
  time1[, c("lag_Out2", "lag_Shift_ID2") := NULL]
}

# Shifts spanning multiple days (possible issue)
time1[, shift_days := as.numeric(difftime(max(Out, na.rm = TRUE), min(In, na.rm = TRUE), units = "days")), by = Shift_ID]
cat("Shifts >1 day span:", time1[shift_days > 1, uniqueN(Shift_ID, na.rm = TRUE)], "\n")

# Summary counts
cat("\n=== Data Quality Summary ===\n")
cat("Duplicate rows:", time1[duplicate_flag == TRUE, .N], "\n")
cat("Out < In (non-dup):", time1[!duplicate_flag & Out < In, .N], "\n")
cat("Overlaps (non-dup):", time1[overlap_flag == TRUE, .N], "\n")

# Clean up temp columns before creating long format
if (NEEDS_MIDNIGHT_CHECK) {
  time1[, c("hrs_since_last", "new_shift", "shift_num", "shift_days", "In_Date_Match",
            "In_time_str", "Out_time_str", "has_real_in", "has_real_out") := NULL]
  cat("✓ Removed temporary columns (including midnight adjustment flags)\n")
} else {
  time1[, c("hrs_since_last", "new_shift", "shift_num", "shift_days", "In_Date_Match",
            "In_time_str", "Out_time_str", "has_real_in", "has_real_out") := NULL]
  cat("✓ Removed temporary columns\n")
  cat("  Info: 'shift_crosses_midnight' column was not created because:\n")
  cat("        - No midnight crossing adjustments were needed\n")
}

# # Rounded punches - concatenate dates from In and Out column if needed (confirm timezones are accurate after this step)
# time1[!is.na(r_In), 
#       r_In := as.POSIXct(paste(format(In, "%Y-%m-%d"), format(r_In, "%H:%M:%S")), 
#                          format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
# 
# time1[!is.na(r_Out), 
#       r_Out := as.POSIXct(paste(format(Out, "%Y-%m-%d"), format(r_Out, "%H:%M:%S")), 
#                           format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]


# ----- TIME DATA:               Create long format (punch_type and punch_time) ----------

# Create In punches
time1_in_punches <- copy(time1)
time1_in_punches[, `:=`(
  punch_time = In,
  #r_punch_time = r_In,
  punch_type = "in"
)]

# Create Out punches (zero out Hours to avoid double-counting; ADD ADDITIONAL COLUMNS AS NEEDED)
time1_out_punches <- copy(time1)
time1_out_punches[, `:=`(
  punch_time = Out,
  #r_punch_time = r_Out,
  punch_type = "out",
  Hours = 0  # Don't double-count hours
  #, r_Hours = 0
)]

# Combine and filter
time1 <- rbindlist(list(time1_in_punches, time1_out_punches), use.names = TRUE, fill = TRUE)
time1 <- time1[!is.na(punch_time)]

# Sort: ID, then punch_time, then "out" before "in" if same time
setorder(time1, ID, punch_time, -punch_type)

cat("✓ Created long format:", nrow(time1), "punch records from",
    nrow(time1_in_punches), "original records\n")


# ----- TIME DATA:               Shift, meal, rest, auto-meal analysis ----------

# Calculate hours from previous punch
setorder(time1, ID, punch_time, -punch_type)

# For new employees (or when ID changes), default to max(8 hrs, new_shift_cutoff) hours to ensure a new shift
time1[, hrs_from_prev := round(as.numeric(difftime(punch_time, shift(punch_time), units = "hours")), 2)]
time1[, hrs_from_prev := fifelse(is.na(hrs_from_prev) | ID != shift(ID), pmax(new_shift_cutoff, 8), hrs_from_prev)]

# Shift indicator logic
time1[, shift := fifelse(ID != shift(ID), 1,
                         fifelse(hrs_from_prev > new_shift_cutoff & punch_type == "in", 1, 0))]
time1[, shift := fifelse(is.na(shift), 1, shift)]

# Optional logic to split shifts with double punch_types (broken shifts split from non-broken shifts)
# Default is 16 hrs (adjust as necessary)
time1[, shift := fifelse(hrs_from_prev > 16, 1, shift)]

# Generate shift counter and ID-Shift combo
time1[, shift_ct := cumsum(shift), by = ID]
time1[, ID_Shift := paste(ID, shift_ct, sep = "-")]

# Meal period logic
rp_max_hrs <- 0 / 60  # currently set to 0 mins (no rest periods) -> ADJUST AS NEEDED AFTER REVIEWING MP TABLES

time1[, mp := fifelse((ID != shift(ID) & shift != 1) | 
                        (hrs_from_prev > rp_max_hrs & hrs_from_prev <= new_shift_cutoff & 
                           punch_type == "in" & shift != 1), 1, 0)]
time1[, mp := fifelse(is.na(mp), 1, mp)]
time1[, mp_ct := cumsum(mp), by = ID_Shift]
time1[, shift_mps := sum(mp), by = ID_Shift]

# # Rest period logic - Used if rest break punches are in data
# time1[, rp := fifelse((ID != shift(ID) & shift != 1) |
#                       (hrs_from_prev > 0 & hrs_from_prev <= rp_max_hrs &
#                        punch_type == "in" & shift != 1), 1, 0)]
# time1[, rp := fifelse(is.na(rp), 1, rp)]
# time1[, rp_ct := cumsum(rp), by = ID_Shift]
# time1[, shift_rps := sum(rp), by = ID_Shift]

# Calculate hours worked for each shift
time1[, hrs_wkd := fifelse(shift == 1 | mp == 1, 0, hrs_from_prev)]
time1[, cum_hrs_wkd := cumsum(hrs_wkd), by = ID_Shift]
time1[, shift_hrs := sum(hrs_wkd), by = ID_Shift]

# MP hours and hours to MP1 and MP2
time1[, mp_hrs := fifelse(mp == 1, hrs_from_prev, 0)]

# Meal period length flags
time1[, mp_one_min := fifelse(mp == 1 & mp_hrs > 0 & mp_hrs <= (2/60), 1L, 0L)]
sum(time1$mp_one_min, na.rm = TRUE)

time1[, mp_lt_twenty := fifelse(mp == 1 & mp_hrs > 0 & mp_hrs < (20/60), 1L, 0L)]
time1[, mp_lt_thirty := fifelse(mp == 1 & mp_hrs > 0 & mp_hrs < 0.5, 1L, 0L)]
time1[, mp_thirty     := fifelse(mp == 1 & mp_hrs == 0.5, 1L, 0L)]
time1[, mp_forty_five    := fifelse(mp == 1 & mp_hrs == 0.75, 1L, 0L)]
time1[, mp_gt_thirty := fifelse(mp == 1 & mp_hrs > 0.5, 1L, 0L)]
time1[, mp_gt_two_hrs := fifelse(mp == 1 & mp_hrs > 2, 1L, 0L)]
time1[, mp_gt_four_hrs := fifelse(mp == 1 & mp_hrs > 4, 1L, 0L)]

# Identify hours for meal period 1 and 2, per shift
time1[, mp1_hrs := fifelse(mp_ct == 1, mp_hrs, 0)]
time1[, mp2_hrs := fifelse(mp_ct == 2, mp_hrs, 0)]

# Shift hrs and non work hrs tables 
# (intended to show shift stats before filtering out zero or 20+ hr shifts or other bad data)
shift_tbl <- shift_hrs_tbl(time1) 
nonwrk_tbl <- non_wrk_hrs_tbl(time1)
meal_tbl <- meal_period_tbl(time1)
meal_start_tbl <- meal_start_time_tbl(time1, top_n = 25)
meal_qtr_tbl <- meal_quarter_hour_tbl(time1)

# # Auto meal deductions (if necessary)
# 
# min_auto_meal <- 0.25
# max_auto_meal <- 1.25
# 
# time1[, auto_mp_hrs :=
#         fifelse(
#           shift_mps == 0 &
#             (max(shift_hrs) - sum(Hours, na.rm = TRUE) >= min_auto_meal) &
#             (max(shift_hrs) - sum(Hours, na.rm = TRUE) <= max_auto_meal),
#           round(max(shift_hrs) - sum(Hours, na.rm = TRUE), 2),
#           0
#         ),
#       by = ID_Shift]
# 
# time1[, auto_mp := fifelse(auto_mp_hrs > 0 & punch_type == "in" & shift_mps == 0, 1, 0)]
# 
# # Validate shift_hrs adjustment by auto_mp_hrs, avoid duplicate application
# time1[, shift_hrs_adj := pmax(0, shift_hrs - auto_mp_hrs)]
# time1[, adjustment_ok := round((shift_hrs - shift_hrs_adj), 2) == auto_mp_hrs]
# 
# invalids <- time1[adjustment_ok == FALSE, .N]
# if (invalids > 0) {
#   message("❌ Error: ", invalids, " rows have shift_hrs adjusted by more than auto_mp_hrs.")
# } else {
#   message("✅ shift_hrs_adj values are valid. Proceeding with update...")
#   time1[, shift_hrs := shift_hrs_adj]
# }
# 
# # Clean up temp columns
# time1[, c("shift_hrs_adj", "adjustment_ok") := NULL]
# 
# # AUTO meal period length flags
# time1[, auto_mp_lt_twenty := fifelse(auto_mp == 1 & auto_mp_hrs > 0 & auto_mp_hrs < (20/60), 1L, 0L)]
# time1[, auto_mp_lt_thirty := fifelse(auto_mp == 1 & auto_mp_hrs > 0 & auto_mp_hrs < 0.5, 1L, 0L)]
# time1[, auto_mp_thirty     := fifelse(auto_mp == 1 & auto_mp_hrs == 0.5, 1L, 0L)]
# time1[, auto_mp_forty_five    := fifelse(auto_mp == 1 & auto_mp_hrs == 0.75, 1L, 0L)]
# time1[, auto_mp_gt_thirty := fifelse(auto_mp == 1 & auto_mp_hrs > 0.5, 1L, 0L)]
# time1[, auto_mp_gt_two_hrs := fifelse(auto_mp == 1 & auto_mp_hrs > 2, 1L, 0L)]
# time1[, auto_mp_gt_four_hrs := fifelse(auto_mp == 1 & auto_mp_hrs > 4, 1L, 0L)]

# Calculate cumulative hours worked up to each meal period
time1[, hrs_to_mp1 := fifelse(mp == 1 & mp_ct == 1, cum_hrs_wkd, 0)]
time1[, hrs_to_mp2 := fifelse(mp == 1 & mp_ct == 2, cum_hrs_wkd, 0)]

# Meal compliance checks: short or late meals
time1[, mp1_mins_short := fifelse(mp1_hrs < 0.5 & mp1_hrs > 0, round((0.5 - mp1_hrs) * 60), 0)]
time1[, mp2_mins_short := fifelse(mp2_hrs < 0.5 & mp2_hrs > 0, round((0.5 - mp2_hrs) * 60), 0)]

time1[, mp1_mins_late := fifelse(hrs_to_mp1 > 5, round((hrs_to_mp1 - 5) * 60), 0)]
time1[, mp2_mins_late := fifelse(hrs_to_mp2 > 10, round((hrs_to_mp2 - 10) * 60), 0)]
# 
# # Rest periods - ONLY for when rest breaks punches in time data.
# time1[, rp_hrs := fifelse(rp == 1, hrs_from_prev, 0)]
# time1[, rp_lt_ten := fifelse(rp_hrs < round(10 / 60, 2) & rp_hrs > 0, 1L, 0L)]
# time1[, rp_ten := fifelse(rp_hrs == round(10 / 60, 2), 1L, 0L)]
# time1[, rp_gt_ten := fifelse(rp_hrs > round(10 / 60, 2) & rp_hrs > 0, 1L, 0L)]
# time1[, rp_fifteen := fifelse(rp_hrs == round(15 / 60, 2), 1L, 0L)]
# 
# time1[, rp1_hrs := fifelse(rp_ct == 1, rp_hrs, 0)]
# time1[, rp2_hrs := fifelse(rp_ct == 2, rp_hrs, 0)]
# time1[, rp3_hrs := fifelse(rp_ct == 3, rp_hrs, 0)]
# time1[, rp4_hrs := fifelse(rp_ct == 4, rp_hrs, 0)]
# 
# time1[, hrs_to_rp1 := fifelse(rp == 1 & rp_ct == 1, cum_hrs_wkd, 0)]
# time1[, hrs_to_rp2 := fifelse(rp == 1 & rp_ct == 2, cum_hrs_wkd, 0)]
# time1[, hrs_to_rp3 := fifelse(rp == 1 & rp_ct == 3, cum_hrs_wkd, 0)]
# time1[, hrs_to_rp4 := fifelse(rp == 1 & rp_ct == 4, cum_hrs_wkd, 0)]


# ----- TIME DATA:               Punch-level rounding ------------------------------
# 
# # Time punch rounding analysis
# setDT(time1)
# 
# # Fix r_punch_time dates (replace 1899 with date from punch_time)
# time1[, r_punch_time := fifelse(
#   year(r_punch_time) == 1899,
#   as_datetime(paste(as.Date(punch_time), format(r_punch_time, "%H:%M:%S"))),
#   r_punch_time
# )]
# 
# # Now calculate the punch difference
# time1[, punch_diff := as.numeric(difftime(punch_time, r_punch_time, units = "hours"))]
# 
# # Check for any issues
# cat("✓ Fixed r_punch_time dates and calculated punch_diff\n")
# cat("  Punch differences range:", 
#     round(min(time1$punch_diff, na.rm = TRUE), 2), "to", 
#     round(max(time1$punch_diff, na.rm = TRUE), 2), "hours\n")
# 
# # Optional: Check if any large differences that might indicate issues
# large_diffs <- time1[abs(punch_diff) > 24, .N]
# if(large_diffs > 0) {
#   cat("⚠ Warning:", large_diffs, "records have punch differences > 24 hours\n")
# }
# 
# # Pre, post and mid shift lost and gained hours
# time1[, last_punch := .I == max(.I), by = ID_Shift]
# 
# time1[, `:=`(
#   pre_shift_hrs_lost = fifelse(punch_type == "in" & punch_diff < 0 & shift == 1, punch_diff, NA_real_),
#   pre_shift_hrs_gained = fifelse(punch_type == "in" & punch_diff > 0 & shift == 1, punch_diff, NA_real_),
# 
#   mid_shift_out_hrs_lost = fifelse(punch_type == "out" & punch_diff > 0 & shift != 1 & !last_punch & shift(hrs_from_prev, type = "lead") > 0.25, punch_diff * -1, NA_real_),
#   mid_shift_out_hrs_gained = fifelse(punch_type == "out" & punch_diff < 0 & shift != 1 & !last_punch & shift(hrs_from_prev, type = "lead") > 0.25, punch_diff * -1, NA_real_),
# 
#   mid_shift_in_hrs_lost = fifelse(punch_type == "in" & punch_diff < 0 & shift != 1 & !last_punch & hrs_from_prev > 0.25, punch_diff, NA_real_),
#   mid_shift_in_hrs_gained = fifelse(punch_type == "in" & punch_diff > 0 & shift != 1 & !last_punch & hrs_from_prev > 0.25, punch_diff, NA_real_),
# 
#   post_shift_hrs_lost = fifelse(punch_type == "out" & punch_diff > 0 & last_punch, punch_diff * -1, NA_real_),
#   post_shift_hrs_gained = fifelse(punch_type == "out" & punch_diff < 0 & last_punch, punch_diff * -1, NA_real_)
# ), by = ID_Shift]
# 
# # # Drop the temporary column after calculations
# time1[, last_punch := NULL]
# 
# # r_diff calculation with lead
# time1[, r_diff := fifelse(
#   (punch_type == "out" & shift(.SD$ID, type = "lead") == ID & shift(.SD$mp, type = "lead") == 1) |
#     (punch_type == "out" & shift(.SD$ID, type = "lead") == ID & shift(.SD$shift, type = "lead") == 1),
#   round(as.numeric(difftime(r_punch_time, punch_time, units = "hours")), 6),
#   round(as.numeric(difftime(punch_time, r_punch_time, units = "hours")), 6)
# )]
# 
# # r_diff fallback if NA
# time1[is.na(r_diff), r_diff := round(as.numeric(difftime(r_punch_time, punch_time, units = "hours")), 6)]
# 
# # r_hrs_from_prev calculation with lag
# time1[, r_hrs_from_prev := round(as.numeric(difftime(r_punch_time, shift(r_punch_time, type = "lag")), units = "hours"), 2)]
# time1[is.na(r_hrs_from_prev) | ID != shift(ID), r_hrs_from_prev := 8]
# 
# # r_hrs_wkd and r_cum_hrs_wkd by ID_Shift
# time1[, r_hrs_wkd := fifelse(shift == 1 | mp == 1, 0, r_hrs_from_prev), by = ID_Shift]
# time1[, r_cum_hrs_wkd := cumsum(r_hrs_wkd), by = ID_Shift]
# time1[, r_shift_hrs := sum(r_hrs_wkd), by = ID_Shift]
# 
# # r_mp_hrs
# time1[, r_mp_hrs := fifelse(mp == 1, r_hrs_from_prev, 0)]
# 
# # Meal period length flags
# time1[, r_mp_lt_twenty := fifelse(r_mp == 1 & r_mp_hrs > 0 & r_mp_hrs < (20/60), 1L, 0L)]
# time1[, r_mp_lt_thirty := fifelse(r_mp == 1 & r_mp_hrs > 0 & r_mp_hrs < 0.5, 1L, 0L)]
# time1[, r_mp_thirty     := fifelse(r_mp == 1 & r_mp_hrs == 0.5, 1L, 0L)]
# time1[, r_mp_forty_five    := fifelse(r_mp == 1 & r_mp_hrs == 0.75, 1L, 0L)]
# time1[, r_mp_gt_thirty := fifelse(r_mp == 1 & r_mp_hrs > 0.5, 1L, 0L)]
# time1[, r_mp_gt_two_hrs := fifelse(r_mp == 1 & r_mp_hrs > 2, 1L, 0L)]
# time1[, r_mp_gt_four_hrs := fifelse(r_mp == 1 & r_mp_hrs > 4, 1L, 0L)]
# 
# # Meal period 1 and 2 duration
# time1[, `:=`(
#   r_mp1_hrs = fifelse(mp_ct == 1, r_mp_hrs, 0),
#   r_mp2_hrs = fifelse(mp_ct == 2, r_mp_hrs, 0)
# ), by = ID_Shift]


# ----- TIME DATA:              Calculated unpaid short break (<20mins) analysis -----------------------------------------

time1[, short_break_hrs     := fifelse(mp_hrs < (20/60), mp_hrs, 0)]
time1[, short_break_reg_hrs := fifelse(mp_hrs < (20/60) & shift_hrs >= 8, mp_hrs, 0)]
time1[, short_break_ot_hrs  := fifelse(mp_hrs < (20/60) & shift_hrs <  8, mp_hrs, 0)]


# ----- TIME DATA:               Weekly summary and Alternative Workweek Analysis --------------

# Weekly summary without weeks with time off for clean AWW review
weekly_summary <- time1[wk_time_off != 1,
                        .(
                          ID = first(ID),
                          shifts_worked   = uniqueN(ID_Shift, na.rm = TRUE),
                          days_worked     = uniqueN(ID_Date, na.rm = TRUE),
                          shift_hrs     = sum(shift_hrs[shift == 1]),
                          avg_shift_hrs   = mean(shift_hrs),
                          min_shift_hrs   = min(shift_hrs),
                          max_shift_hrs   = max(shift_hrs),
                          shifts_near_8hrs  = sum(shift_hrs >= 7.5  & shift_hrs <= 8.5  & shift == 1),
                          shifts_near_10hrs = sum(shift_hrs >= 9.5  & shift_hrs <= 10.5 & shift == 1),
                          shifts_near_12hrs = sum(shift_hrs >= 11.5 & shift_hrs <= 12.5 & shift == 1)
                        ),
                        by = .(ID_Week_End)
]

# Identify potential AWW schedules

# 5/8 schedule  
weekly_summary[, is_5_8_candidate := (shifts_worked == 5 & shifts_near_8hrs == 5) |
                 (shifts_worked == shifts_near_8hrs)]

# 4/10 schedule
weekly_summary[, is_4_10_candidate := (shifts_worked == 4 & shifts_near_10hrs == 4) |
                 (shifts_worked == shifts_near_10hrs)]

# 3/12 schedule
weekly_summary[, is_3_12_candidate := (shifts_worked == 3 & shifts_near_8hrs == 3) |
                 (shifts_worked == shifts_near_12hrs)]

# 6+ shifts worked schedule detection 
weekly_summary[, is_6_plus_shifts := (shifts_worked >= 6)]

# Shifts & days worked mismatch 
weekly_summary[, days_gt_shifts_worked := 
                 shifts_worked < days_worked
]

weekly_summary[, days_lt_shifts_worked := 
                 shifts_worked > days_worked
]

weekly_summary[, days_e_shifts_worked := 
                 shifts_worked == days_worked
]

# Calculate consistency across multiple weeks
# An employee should show the pattern consistently to be on AWW

employee_aww_summary <- weekly_summary[,
                                       .(
                                         total_weeks = .N,
                                         weeks_5_8 = sum(is_5_8_candidate, na.rm = TRUE),
                                         weeks_4_10 = sum(is_4_10_candidate, na.rm = TRUE),
                                         weeks_3_12 = sum(is_3_12_candidate, na.rm = TRUE),
                                         weeks_6_plus_shifts = sum(is_6_plus_shifts, na.rm = TRUE),
                                         pct_5_8 = sum(is_5_8_candidate, na.rm = TRUE) / .N,
                                         pct_4_10 = sum(is_4_10_candidate, na.rm = TRUE) / .N,
                                         pct_3_12 = sum(is_3_12_candidate, na.rm = TRUE) / .N,
                                         pct_6_plus_shifts = sum(is_6_plus_shifts, na.rm = TRUE) / .N,
                                         avg_weekly_hours = mean(shift_hrs, na.rm = TRUE),
                                         avg_shifts_per_week = mean(shifts_worked, na.rm = TRUE),
                                         avg_days_per_week = mean(days_worked, na.rm = TRUE)
                                       ),
                                       by = ID
]

# Classify employees 
# Require 60% consistency to classify as AWW
employee_aww_summary[, AWW_schedule := case_when(
  pct_5_8 >= 0.6 ~ "5/8",
  pct_4_10 >= 0.6 ~ "4/10",
  pct_3_12 >= 0.6 ~ "3/12",
  pct_6_plus_shifts >= 0.6 ~ "6+ shifts",
  TRUE ~ "Other"
)]

# Show results 
cat("\n=== AWW DETECTION RESULTS ===\n")
cat("Total employees analyzed:", nrow(employee_aww_summary), "\n")

schedule_counts <- employee_aww_summary[, .N, by = AWW_schedule]
print(schedule_counts)

# Save outputs (absolute via OUT_DIR; writes both CSV + RDS)
write_csv_and_rds(
  employee_aww_summary,
  file.path(resolve_out_dir(), "employee_aww_classifications.csv")
)

# Create detailed report for employees on AWW
aww_employees <- employee_aww_summary[AWW_schedule != "Standard", ID]

aww_detail <- weekly_summary[ID %in% aww_employees]
aww_detail <- merge(
  aww_detail,
  employee_aww_summary[, .(ID, AWW_schedule)],
  by = "ID"
)


# ----- SHIFT (TIME) DATA:       Shift-level aggregation -----------------------------------------

names(time1)

# Define fields globally
first_fields_default <- c("Source", "Sheet", "Page", "Bates", "Key_Gps", "ID", "Name", "ID_Date", "Date", 
                          "Location_Name", "Executive_Area", "Area", "Region",
                          "ID_Period_End", "Week_End", "ID_Week_End", "wk_time_off", "Period_Beg", "Period_End")

#NOTE: Hours field is default "Sum" field but it could be a "Max" field depending on your time data format.
sum_fields_default <- c("mp", "mp_lt_twenty", "mp_lt_thirty", "mp_thirty", "mp_gt_thirty", "mp_forty_five", 
                        "mp_gt_two_hrs", "mp_gt_four_hrs", "Hours",
                        
                        "split_shift", "five_hour_guarantee", "split_shift_hrs", "guarantee_hrs", "pto_hrs"
                        
                        ,"short_break_hrs", "short_break_reg_hrs", "short_break_ot_hrs"
                        
                        # Rest period punches in data? Add:
                        #, "rp", "rp_lt_ten", "rp_ten", "rp_gt_ten", "rp_fifteen"
                        
                        # Auto-deducted meal periods in data? Add:
                        #, "auto_mp", "auto_mp_lt_twenty", "auto_mp_lt_thirty", "auto_mp_thirty", "auto_mp_gt_thirty", "auto_mp_forty_five", 
                        # "auto_mp_gt_two_hrs", "auto_mp_gt_four_hrs",
                        
                        # Rounded and actual punches analysis? Add:
                        #, "r_diff", "r_mp_lt_twenty", "r_mp_lt_thirty", "r_mp_thirty", "r_mp_gt_thirty", "r_mp_forty_five", 
                        # "r_mp_gt_two_hrs", "r_mp_gt_four_hrs", "r_Hours"
)

max_fields_default <- c("shift_hrs", "mp1_hrs", "mp2_hrs", "shift_mps", "hrs_to_mp1", "hrs_to_mp2", 
                        "mp1_mins_short", "mp2_mins_short", "mp1_mins_late", "mp2_mins_late"
                        
                        # Auto-deducted meal periods in data? Add:
                        #, "auto_mp_hrs", "auto_mp"
                        
                        # Rest period punches in data? Add:
                        #, "rp", "shift_rps", "rp1_hrs", "rp2_hrs", "rp3_hrs", "rp4_hrs", "hrs_to_rp1", "hrs_to_rp2", "hrs_to_rp3", "hrs_to_rp4"
                        
                        # Rounded and actual punches analysis? Add:
                        #, "r_shift_hrs", "r_mp_hrs", "r_mp1_hrs", "r_mp2_hrs", "pre_shift_hrs_lost", "pre_shift_hrs_gained", "mid_shift_out_hrs_lost",
                        #"mid_shift_out_hrs_gained", "mid_shift_in_hrs_lost", "mid_shift_in_hrs_gained", "post_shift_hrs_lost", "post_shift_hrs_gained"
)

min_fields_default <- c(character(0))
mean_fields_default <- c(character(0))
median_fields_default <- c(character(0))

# Aggregate punch-level data down to shift-level data using aggregate_data function
shift_data1 <- aggregate_data(time1, by = "ID_Shift")

# Suffixes to remove
suffixes_to_remove <- c("_sum", "_max")

# Remove suffixes (only keep for employee-level or other aggregations where it makes sense such as when same column needs have min AND max)
shift_data1 <- remove_suffixes(shift_data1, suffixes_to_remove)

# Convert shift_data1 to data.table
setDT(shift_data1)

# Sort by 'ID' and 'Date'
setorder(shift_data1, ID, Date)

# Filter out zero hour shifts and shifts > 20 hrs (adjust as needed)
nrow(shift_data1) #872428
shift_data1 <- shift_data1[shift_hrs > 0 & shift_hrs <= 20]
nrow(shift_data1) #872215

# Shift flag (each row = shift, so always "1" assuming it has hours (zero hour "shifts" were just filtered out))
shift_data1[, shift := as.integer(shift_hrs > 0)]


# ----- SHIFT (TIME) DATA:       Meal and rest analysis -----------------------------------------

# MP Analysis: No Waivers
shift_data1[, `:=`(
  MissMP1  = as.integer(shift_mps == 0 & shift_hrs > 5.01),
  LateMP1  = as.integer(shift_mps > 0 & shift_hrs > 5.01 & hrs_to_mp1 > 5.01),
  ShortMP1 = as.integer(shift_mps > 0 & shift_hrs > 5.01 & 
                          mp1_hrs > 0.01 & mp1_hrs < 0.49 &
                          !(shift_mps > 1 & hrs_to_mp2 < 6.01)),
  MissMP2  = as.integer(shift_mps < 2 & shift_hrs > 10.01),
  LateMP2  = as.integer(shift_mps > 1 & shift_hrs > 10.01 & hrs_to_mp2 > 10.01),
  ShortMP2 = as.integer(shift_mps > 1 & shift_hrs > 10.01 & mp2_hrs > 0.01 & mp2_hrs < 0.49)
)]

shift_data1[, `:=`(
  mpv_shift     = as.integer((MissMP1 + LateMP1 + ShortMP1 +
                                MissMP2 + LateMP2 + ShortMP2) > 0),
  
  mp1_violation = as.integer((MissMP1 + LateMP1 + ShortMP1) > 0),
  mp2_violation = as.integer((MissMP2 + LateMP2 + ShortMP2) > 0)
)]

# MP Analysis: With Waivers
shift_data1[, `:=`(
  MissMP1_w  = as.integer(shift_mps == 0 & shift_hrs > 6.01),
  LateMP1_w  = as.integer(shift_mps > 0 & shift_hrs > 6.01 & hrs_to_mp1 > 5.01),
  ShortMP1_w = as.integer(shift_mps > 0 & shift_hrs > 6.01 & 
                            mp1_hrs > 0.01 & mp1_hrs < 0.49 &
                            !(shift_mps > 1 & hrs_to_mp2 < 6.01)),
  
  MissMP2_w  = as.integer(shift_mps < 2 & shift_hrs > 12.01),
  LateMP2_w  = as.integer(shift_mps > 1 & shift_hrs > 12.01 & hrs_to_mp2 > 10.01),
  ShortMP2_w = as.integer(shift_mps > 1 & shift_hrs > 12.01 & mp2_hrs > 0.01 & mp2_hrs < 0.49)
)]

shift_data1[, `:=`(
  mpv_shift_w     = as.integer((MissMP1_w + LateMP1_w + ShortMP1_w + MissMP2_w + LateMP2_w + ShortMP2_w) > 0),
  mp1_violation_w = as.integer((MissMP1_w + LateMP1_w + ShortMP1_w) > 0),
  mp2_violation_w = as.integer((MissMP2_w + LateMP2_w + ShortMP2_w) > 0)
)]

# # AUTO MP Analysis: No Waivers
# shift_data1[, `:=`(
#   auto_MissMP1  = as.integer(MissMP1 == 1 & auto_mp_hrs == 0),
#   auto_LateMP1  = LateMP1,
#   auto_ShortMP1 = as.integer(
#     (ShortMP1 == 1) |
#       (ShortMP1 == 0 & auto_mp_hrs > 0 & auto_mp_hrs < 0.49)
#   ),
#   auto_MissMP2  = as.integer(MissMP2 == 1 & auto_mp_hrs < 0.99),
#   auto_LateMP2  = LateMP2,
#   auto_ShortMP2 = ShortMP2
# )]
#
# shift_data1[, `:=`(
#   auto_mpv_shift = as.integer(
#     auto_MissMP1 + auto_LateMP1 + auto_ShortMP1 +
#       auto_MissMP2 + auto_LateMP2 + auto_ShortMP2 > 0
#   )
# )]
# 
# # AUTO MP Analysis: With Waivers
# shift_data1[, `:=`(
#   auto_MissMP1_w  = as.integer(MissMP1_w == 1 & auto_mp_hrs == 0),
#   auto_LateMP1_w  = LateMP1_w,
#   auto_ShortMP1_w = as.integer(
#     (ShortMP1_w == 1) |
#       (ShortMP1_w == 0 & auto_mp_hrs > 0 & auto_mp_hrs < 0.49)
#   ),
#   auto_MissMP2_w  = as.integer(MissMP2_w == 1 & auto_mp_hrs < 0.99),
#   auto_LateMP2_w  = LateMP2_w,
#   auto_ShortMP2_w = ShortMP2_w
# )]
#   
# shift_data1[, `:=`(
#   auto_mpv_shift_w = as.integer(
#     auto_MissMP1_w + auto_LateMP1_w + auto_ShortMP1_w +
#       auto_MissMP2_w + auto_LateMP2_w + auto_ShortMP2_w > 0
#   )
# )]

# RP Analysis (no rest period punches)
#shift_data1[, rpv_shift := fifelse(shift_hrs > 3.5, 1L, 0L)]

# RP Analysis (match to mpv_shift -> meal period violations)
shift_data1[, rpv_shift := mpv_shift]

# shift_data1[, `:=`(
#   MissRP1  = as.integer(shift_rps == 0 & shift_hrs > 3.51),
#   ShortRP1 = as.integer(shift_rps > 0 & shift_hrs > 3.51 &
#                           rp1_hrs > 0.01 & rp1_hrs < (10/60)),
#   MissRP2  = as.integer(shift_rps < 2 & shift_hrs > 6.01),
#   MissRP3  = as.integer(shift_rps < 3 & shift_hrs > 10.01),
#   MissRP4  = as.integer(shift_rps < 4 & shift_hrs > 14.01)
#   )
# )]
#
# shift_data1[, `:=`(   
#   rpv_shift = as.integer(
#     MissRP1 + ShortRP1 + MissRP2 + MissRP3 + MissRP4 > 0
#   )
# )]

# ----- SHIFT (TIME) DATA:       Weekly aggregations -----------------------------------------

shift_data1[, `:=` (
  wk_shift_hrs = sum(shift_hrs, na.rm = TRUE),
  wk_Hours = sum(Hours, na.rm = TRUE),
  wk_short_break_hrs = sum(short_break_hrs, na.rm = TRUE),
  wk_short_break_reg_hrs = sum(short_break_reg_hrs, na.rm = TRUE),
  wk_short_break_ot_hrs = sum(short_break_ot_hrs, na.rm = TRUE)
), by = ID_Week_End]


shift_data1[, `:=`(
  week = fifelse(shift(ID_Week_End, type = "lead") == ID_Week_End, 0, 1)
)]


# ----- SHIFT (TIME) DATA:       Pay period aggregations -----------------------------------------

shift_data1[, ID_Period_End := paste(ID, Period_End, sep = "_")]

setorder(shift_data1, ID, Period_End)

shift_data1[, pp := fifelse(ID_Period_End != shift(ID_Period_End), 1L, 0L)]
shift_data1[is.na(pp), pp := 1L]

shift_data1[, `:=`(
  mpv_per_pp = sum(mpv_shift, na.rm = TRUE),
  mpv_per_pp_w = sum(mpv_shift_w, na.rm = TRUE),
  # # Auto-deducted meals
  # auto_mpv_per_pp = sum(auto_mpv_shift, na.rm = TRUE),
  # auto_mpv_per_pp_w = sum(auto_mpv_shift_w, na.rm = TRUE),
  rpv_per_pp = sum(rpv_shift, na.rm = TRUE),
  pp_shift_hrs = sum(shift_hrs, na.rm = TRUE),
  pp_Hours = sum(Hours, na.rm = TRUE),
  pp_short_break_hrs = sum(short_break_hrs, na.rm = TRUE),
  pp_short_break_reg_hrs = sum(short_break_reg_hrs, na.rm = TRUE),
  pp_short_break_ot_hrs = sum(short_break_ot_hrs, na.rm = TRUE)
), by = .(ID, Period_End)]


# ----- SHIFT (TIME) DATA:       Calculated CA & FLSA overtime and double time analysis  -----------------------------------------

setDT(shift_data1)

#  Order so "prior shift" is well-defined (within each ID_Week_End) 
ord_cols <- intersect(c("ID_Week_End", "ID", "Date"), names(shift_data1))
setorderv(shift_data1, ord_cols)

# assumes shift_data1 is already setDT + ordered by ID_Week_End, ID, Date (and ideally In)

# lag date
shift_data1[, prev_date :=  shift(Date, type = "lag"), by = ID_Week_End]

# define "new calendar day" vs "same day"
shift_data1[, `:=`(
  is_same_day =  fifelse(!is.na(prev_date) & Date == prev_date, 1L, 0L),
  is_next_day =  fifelse(!is.na(prev_date) & Date == (prev_date + 1L), 1L, 0L)
), by = ID_Week_End]

# consecutive-worked-day counter:
# - first row of the week = 1
# - same day as prior shift => keep same count
# - next day => +1
# - gap > 1 day => reset to 1
shift_data1[, cons_shift_ct := {
  out <- integer(.N)
  out[1L] <- 1L
  if (.N > 1L) {
    for (i in 2L:.N) {
      if (is_same_day[i] == 1L) {
        out[i] <- out[i-1L]
      } else if (is_next_day[i] == 1L) {
        out[i] <- out[i-1L] + 1L
      } else {
        out[i] <- 1L
      }
    }
  }
  out
}, by = ID_Week_End]

shift_data1[, c("prev_date","is_same_day","is_next_day") := NULL]

#  7th consecutive day OT/DT (CA) 
shift_data1[, `:=`(
  ot_seventh_day = fifelse(cons_shift_ct == 7L, pmin(shift_hrs, 8), 0),
  dt_seventh_day = fifelse(cons_shift_ct == 7L, pmax(shift_hrs - 8, 0), 0)
)]

#  Standard CA daily OT/DT (non-AWS baseline) 
shift_data1[, `:=`(
  calc_daily_ot = fifelse(shift_hrs > 12, 4,
                                      fifelse(shift_hrs > 8, shift_hrs - 8, 0)),
  calc_daily_dt = fifelse(shift_hrs > 12, shift_hrs - 12, 0)
)]

#  Final CA OT/DT per shift (avoid double counting on 7th consecutive day) 
shift_data1[, `:=`(
  ca_ot_hrs = fifelse(cons_shift_ct == 7L, ot_seventh_day, calc_daily_ot),
  ca_dt_hrs = fifelse(cons_shift_ct == 7L, dt_seventh_day, calc_daily_dt)
)]

# --- WEEKLY TOTALS (count week-level values once using week==1 marker) ---
# Weekly CA daily OT/DT totals
shift_data1[, `:=`(
  wk_ca_ot = sum(ca_ot_hrs, na.rm = TRUE),
  wk_ca_dt = sum(ca_dt_hrs, na.rm = TRUE)
), by = ID_Week_End]

# FLSA OT
shift_data1[, flsa_ot := pmax(wk_shift_hrs - 40, 0)]

# Weekly OT (max of weekly CA OT and FLSA OT)
shift_data1[, wk_ot := pmax(flsa_ot, wk_ca_ot)]

# Weekly OT/DT flag
shift_data1[, wk_has_ot := as.integer(wk_ot > 0 | wk_ca_dt > 0)]

# --- PAY PERIOD TOTALS (count week-level values once using week==1 marker) ---
shift_data1[, `:=`(
  pp_ot = sum(wk_ot * week, na.rm = TRUE),
  pp_dt = sum(wk_ca_dt  * week, na.rm = TRUE),
  
  # Optional reporting components
  pp_ca_ot_hrs = sum(wk_ca_ot   * week, na.rm = TRUE),
  pp_ca_dt_hrs = sum(wk_ca_dt   * week, na.rm = TRUE),
  pp_flsa_ot   = sum(flsa_ot * week, na.rm = TRUE),
  pp_ot_wks    = sum(wk_has_ot  * week, na.rm = TRUE)
), by = ID_Period_End]


# ----- SHIFT (TIME) DATA:       Shift duration & shift-level rounding review  -----------------------------------------

shift_data1[, qtr_hr := fifelse(
  is.na(Hours) | Hours == 0, NA_integer_,
  fifelse(
    (round(Hours, 2) - floor(Hours)) %in% c(0, 0.25, 0.5, 0.75), 1L, 0L
  )
)]

# Shift duration / shift-level clock rounding analysis
rounding_hrs_cutoff <- 0.25

# Calculate diff (rounded Hours - shift_hrs)
shift_data1[, diff := fifelse(
  Hours == 0 | is.na(Hours), 0,
  round(Hours - shift_hrs, 2)
)]

shift_data1[, shifts_analyzed := as.integer(!is.na(diff))]

# Invalidate diffs outside cutoff
shift_data1[, diff := fifelse(
  !is.na(rounding_hrs_cutoff) & abs(diff) > rounding_hrs_cutoff,
  NA_real_,
  diff
)]

# Weekly aggregations by ID_Week_End
shift_data1[, `:=`(
  wk_diff = if (all(is.na(diff))) NA_real_ else sum(diff, na.rm = TRUE),
  wk_lost_time_shifts = sum(diff < 0, na.rm = TRUE),
  wk_gain_time_shifts = sum(diff > 0, na.rm = TRUE)
), by = ID_Week_End]

# Pay period-level aggregations by ID_Period_End
shift_data1[, `:=`(
  pp_diff = if (all(is.na(diff))) NA_real_ else sum(diff, na.rm = TRUE),
  pp_lost_time_shifts = sum(diff < 0, na.rm = TRUE),
  pp_gain_time_shifts = sum(diff > 0, na.rm = TRUE)
), by = ID_Period_End]

# Employee-level net diff
shift_data1[, ee_diff := sum(diff, na.rm = TRUE), by = ID]


# ----- SHIFT (TIME) DATA:       Punch-level rounding analysis  -----------------------------------------
# setDT(shift_data1)
# 
# shift_data1[, r_shifts_analyzed := as.integer(!is.na(r_diff))]
# 
# # Zero out values beyond rounding cutoff
# shift_data1[, `:=`(
#   pre_shift_hrs_lost        = fifelse(abs(pre_shift_hrs_lost)        > rounding_hrs_cutoff, 0, pre_shift_hrs_lost),
#   pre_shift_hrs_gained      = fifelse(abs(pre_shift_hrs_gained)      > rounding_hrs_cutoff, 0, pre_shift_hrs_gained),
#   mid_shift_out_hrs_lost    = fifelse(abs(mid_shift_out_hrs_lost)    > rounding_hrs_cutoff, 0, mid_shift_out_hrs_lost),
#   mid_shift_out_hrs_gained  = fifelse(abs(mid_shift_out_hrs_gained)  > rounding_hrs_cutoff, 0, mid_shift_out_hrs_gained),
#   mid_shift_in_hrs_lost     = fifelse(abs(mid_shift_in_hrs_lost)     > rounding_hrs_cutoff, 0, mid_shift_in_hrs_lost),
#   mid_shift_in_hrs_gained   = fifelse(abs(mid_shift_in_hrs_gained)   > rounding_hrs_cutoff, 0, mid_shift_in_hrs_gained),
#   post_shift_hrs_lost       = fifelse(abs(post_shift_hrs_lost)       > rounding_hrs_cutoff, 0, post_shift_hrs_lost),
#   post_shift_hrs_gained     = fifelse(abs(post_shift_hrs_gained)     > rounding_hrs_cutoff, 0, post_shift_hrs_gained),
#   r_diff                    = fifelse(abs(r_diff)                    > rounding_hrs_cutoff, 0, r_diff)
# )]
# 
# # Net pre / mid / post
# shift_data1[, `:=`(
#   net_pre_shift_hrs      = fifelse(is.na(pre_shift_hrs_lost), 0, pre_shift_hrs_lost) +
#     fifelse(is.na(pre_shift_hrs_gained), 0, pre_shift_hrs_gained),
#   net_mid_shift_out_hrs  = fifelse(is.na(mid_shift_out_hrs_lost), 0, mid_shift_out_hrs_lost) +
#     fifelse(is.na(mid_shift_out_hrs_gained), 0, mid_shift_out_hrs_gained),
#   net_mid_shift_in_hrs   = fifelse(is.na(mid_shift_in_hrs_lost), 0, mid_shift_in_hrs_lost) +
#     fifelse(is.na(mid_shift_in_hrs_gained), 0, mid_shift_in_hrs_gained),
#   net_post_shift_hrs     = fifelse(is.na(post_shift_hrs_lost), 0, post_shift_hrs_lost) +
#     fifelse(is.na(post_shift_hrs_gained), 0, post_shift_hrs_gained)
# )]
# 
# # Weekly summaries
# shift_data1[, `:=`(
#   wk_r_diff              = if (all(is.na(r_diff))) NA_real_ else sum(r_diff, na.rm = TRUE),
#   wk_r_lost_time_shifts  = sum(r_diff < 0, na.rm = TRUE),
#   wk_r_gain_time_shifts  = sum(r_diff > 0, na.rm = TRUE)
# ), by = ID_Week_End]
# 
# # Pay period summaries
# shift_data1[, `:=`(
#   pp_r_diff              = if (all(is.na(r_diff))) NA_real_ else sum(r_diff, na.rm = TRUE),
#   pp_r_lost_time_shifts  = sum(r_diff < 0, na.rm = TRUE),
#   pp_r_gain_time_shifts  = sum(r_diff > 0, na.rm = TRUE)
# ), by = ID_Period_End]
# 
# # Employee-level net r_diff
# shift_data1[, ee_r_diff := sum(r_diff, na.rm = TRUE), by = ID]


# ----- SHIFT (TIME & PAY) DATA: Meal & Rest premiums adjustment-------------------------------------------------------------------------

setDT(pay1)
setDT(shift_data1)

# --- Filter + Prepare meal premium pay table ---
meal_prems_paid <- pay1[Meal_Pay_Code == 1 | Rest_Pay_Code == 1]

meal_prems_paid[, ID_Period_End := Pay_ID_Period_End]

# Fill missing Pay_Hours from Pay_Amount and Base_Rate1
meal_prems_paid[, Pay_Hours := fifelse(
  Pay_Hours == 0 | is.na(Pay_Hours),
  round(Pay_Amount / Base_Rate1, 0),
  Pay_Hours
)]

# Summarise meal and rest premiums per pay per period
meal_prems_paid <- meal_prems_paid[, .(
  Meal_Prem_Hrs = sum(Pay_Hours[Meal_Pay_Code == 1], na.rm = TRUE),
  Meal_Prem_Amt = sum(Pay_Amount[Meal_Pay_Code == 1], na.rm = TRUE),
  Rest_Prem_Hrs = sum(Pay_Hours[Rest_Pay_Code == 1], na.rm = TRUE),
  Rest_Prem_Amt = sum(Pay_Amount[Rest_Pay_Code == 1], na.rm = TRUE),
  Pay_Rate = sum(Pay_Rate, na.rm = TRUE)
), by = ID_Period_End]


# --- Join into shift_data1 ---
shift_data1 <- safe_left_join(shift_data1, meal_prems_paid, by = "ID_Period_End")

# --- Cumulative allocation of meal premiums to shifts ---
if ("Meal_Prem_Hrs" %in% names(shift_data1) & "mpv_shift" %in% names(shift_data1)) {
  # Sort by ID_Period_End and any shift identifier (adjust as needed)
  setorder(shift_data1, ID, Date)
  
  # Calculate cumulative meal violations
  shift_data1[, cum_mpv := cumsum(mpv_shift), by = ID_Period_End]
  
  # Create the lagged version
  shift_data1[, prev_cum_mpv := shift(cum_mpv, fill = 0, type = "lag"), by = ID_Period_End]
  
  # Allocate meal premiums to each shift - handle NA values
  shift_data1[, `:=`(
    meal_premium_allocated = fifelse(
      is.na(Meal_Prem_Hrs), 
      0, 
      pmin(mpv_shift, pmax(0, Meal_Prem_Hrs - prev_cum_mpv))
    ),
    mpv_shift_less_prems = fifelse(
      is.na(Meal_Prem_Hrs), 
      mpv_shift, 
      mpv_shift - pmin(mpv_shift, pmax(0, Meal_Prem_Hrs - prev_cum_mpv))
    )
  )]
  
  # Waivers
  if ("mpv_shift_w" %in% names(shift_data1)) {
    # Create cumulative for waived version
    shift_data1[, cum_mpv_w := cumsum(mpv_shift_w), by = ID_Period_End]
    
    # Create lagged version
    shift_data1[, prev_cum_mpv_w := shift(cum_mpv_w, fill = 0, type = "lag"), by = ID_Period_End]
    
    # Allocate premiums - handle NA values
    shift_data1[, `:=`(
      meal_premium_allocated_w = fifelse(
        is.na(Meal_Prem_Hrs), 
        0, 
        pmin(mpv_shift_w, pmax(0, Meal_Prem_Hrs - prev_cum_mpv_w))
      ),
      mpv_shift_less_prems_w = fifelse(
        is.na(Meal_Prem_Hrs), 
        mpv_shift_w, 
        mpv_shift_w - pmin(mpv_shift_w, pmax(0, Meal_Prem_Hrs - prev_cum_mpv_w))
      )
    )]
  }
  
  # Clean up intermediate columns (uncomment when ready)
  # shift_data1[, c("cum_mpv", "prev_cum_mpv", "cum_mpv_w", "prev_cum_mpv_w") := NULL]
}


# TESTING ONLY - USE THIS FOR AUTO DEDUCTED MEAL PERIODS
# if ("Meal_Prem_Hrs" %in% names(shift_data1) & "mpv_shift" %in% names(shift_data1)) {
#   # Sort by ID_Period_End and any shift identifier (adjust as needed)
#   setorder(shift_data1, ID, Date)
#   
#   # === Process mpv_shift ===
#   # Calculate cumulative meal violations
#   shift_data1[, cum_mpv := cumsum(mpv_shift), by = ID_Period_End]
#   
#   # Create the lagged version
#   shift_data1[, prev_cum_mpv := shift(cum_mpv, fill = 0, type = "lag"), by = ID_Period_End]
#   
#   # Allocate meal premiums to each shift - handle NA values
#   shift_data1[, `:=`(
#     meal_premium_allocated = fifelse(
#       is.na(Meal_Prem_Hrs), 
#       0, 
#       pmin(mpv_shift, pmax(0, Meal_Prem_Hrs - prev_cum_mpv))
#     ),
#     mpv_shift_less_prems = fifelse(
#       is.na(Meal_Prem_Hrs), 
#       mpv_shift, 
#       mpv_shift - pmin(mpv_shift, pmax(0, Meal_Prem_Hrs - prev_cum_mpv))
#     )
#   )]
#   
#   # === Process mpv_shift_w (waivers) ===
#   if ("mpv_shift_w" %in% names(shift_data1)) {
#     # Create cumulative for waived version
#     shift_data1[, cum_mpv_w := cumsum(mpv_shift_w), by = ID_Period_End]
#     
#     # Create lagged version
#     shift_data1[, prev_cum_mpv_w := shift(cum_mpv_w, fill = 0, type = "lag"), by = ID_Period_End]
#     
#     # Allocate premiums - handle NA values
#     shift_data1[, `:=`(
#       meal_premium_allocated_w = fifelse(
#         is.na(Meal_Prem_Hrs), 
#         0, 
#         pmin(mpv_shift_w, pmax(0, Meal_Prem_Hrs - prev_cum_mpv_w))
#       ),
#       mpv_shift_less_prems_w = fifelse(
#         is.na(Meal_Prem_Hrs), 
#         mpv_shift_w, 
#         mpv_shift_w - pmin(mpv_shift_w, pmax(0, Meal_Prem_Hrs - prev_cum_mpv_w))
#       )
#     )]
#   }
#   
#   # === Process auto_mpv_shift ===
#   if ("auto_mpv_shift" %in% names(shift_data1)) {
#     # Calculate cumulative automatic meal violations
#     shift_data1[, cum_auto_mpv := cumsum(auto_mpv_shift), by = ID_Period_End]
#     
#     # Create the lagged version
#     shift_data1[, prev_cum_auto_mpv := shift(cum_auto_mpv, fill = 0, type = "lag"), by = ID_Period_End]
#     
#     # Allocate meal premiums - handle NA values
#     shift_data1[, `:=`(
#       auto_meal_premium_allocated = fifelse(
#         is.na(Meal_Prem_Hrs), 
#         0, 
#         pmin(auto_mpv_shift, pmax(0, Meal_Prem_Hrs - prev_cum_auto_mpv))
#       ),
#       auto_mpv_shift_less_prems = fifelse(
#         is.na(Meal_Prem_Hrs), 
#         auto_mpv_shift, 
#         auto_mpv_shift - pmin(auto_mpv_shift, pmax(0, Meal_Prem_Hrs - prev_cum_auto_mpv))
#       )
#     )]
#   }
#   
#   # === Process auto_mpv_shift_w (waivers) ===
#   if ("auto_mpv_shift_w" %in% names(shift_data1)) {
#     # Create cumulative for automatic waived version
#     shift_data1[, cum_auto_mpv_w := cumsum(auto_mpv_shift_w), by = ID_Period_End]
#     
#     # Create lagged version
#     shift_data1[, prev_cum_auto_mpv_w := shift(cum_auto_mpv_w, fill = 0, type = "lag"), by = ID_Period_End]
#     
#     # Allocate premiums - handle NA values
#     shift_data1[, `:=`(
#       auto_meal_premium_allocated_w = fifelse(
#         is.na(Meal_Prem_Hrs), 
#         0, 
#         pmin(auto_mpv_shift_w, pmax(0, Meal_Prem_Hrs - prev_cum_auto_mpv_w))
#       ),
#       auto_mpv_shift_less_prems_w = fifelse(
#         is.na(Meal_Prem_Hrs), 
#         auto_mpv_shift_w, 
#         auto_mpv_shift_w - pmin(auto_mpv_shift_w, pmax(0, Meal_Prem_Hrs - prev_cum_auto_mpv_w))
#       )
#     )]
#   }
#   
#   # Clean up intermediate columns (uncomment when ready)
#   # shift_data1[, c("cum_mpv", "prev_cum_mpv", "cum_mpv_w", "prev_cum_mpv_w", 
#   #                 "cum_auto_mpv", "prev_cum_auto_mpv", "cum_auto_mpv_w", "prev_cum_auto_mpv_w") := NULL]
# }

# --- Cumulative allocation of rest premiums to shifts ---
if ("Rest_Prem_Hrs" %in% names(shift_data1) & "rpv_shift" %in% names(shift_data1)) {
  setorder(shift_data1, ID, Date)
  
  # Create cumulative sum first
  shift_data1[, cum_rpv := cumsum(rpv_shift), by = ID_Period_End]
  
  # Then create the lagged version
  shift_data1[, prev_cum_rpv := shift(cum_rpv, fill = 0, type = "lag"), by = ID_Period_End]
  
  # Allocate rest premiums - handle NA values
  shift_data1[, `:=`(
    rest_premium_allocated = fifelse(
      is.na(Rest_Prem_Hrs), 
      0, 
      pmin(rpv_shift, pmax(0, Rest_Prem_Hrs - prev_cum_rpv))
    ),
    rpv_shift_less_prems = fifelse(
      is.na(Rest_Prem_Hrs), 
      rpv_shift, 
      rpv_shift - pmin(rpv_shift, pmax(0, Rest_Prem_Hrs - prev_cum_rpv))
    )
  )]
  
  # Clean up intermediate columns
  # shift_data1[, c("cum_rpv", "prev_cum_rpv") := NULL]
}


# --- Pay period-level meal premiums adjustment ---

if ("Meal_Prem_Hrs" %in% names(shift_data1)) {
  shift_data1[is.na(Meal_Prem_Hrs), Meal_Prem_Hrs := 0]
} else {
  message("⚠️ Column 'Meal_Prem_Hrs' does not exist in shift_data1. CONFIRM NO MEAL PREMIUMS IN PAY DATA!")
}

if ("Rest_Prem_Hrs" %in% names(shift_data1)) {
  shift_data1[is.na(Rest_Prem_Hrs), Rest_Prem_Hrs := 0]
} else {
  message("⚠️ Column 'Rest_Prem_Hrs' does not exist in shift_data1. CONFIRM NO REST PREMIUMS IN PAY DATA!")
}

# Fallback logic if Meal_Prem_Hrs is missing
if ("Meal_Prem_Hrs" %in% names(shift_data1)) {
  shift_data1[, `:=`(
    mpv_per_pp_less_prems = fifelse(is.na(Meal_Prem_Hrs), mpv_per_pp, pmax(0, mpv_per_pp - Meal_Prem_Hrs)),
    mpv_per_pp_less_prems_w = fifelse(is.na(Meal_Prem_Hrs), mpv_per_pp_w, pmax(0, mpv_per_pp_w - Meal_Prem_Hrs))
  )]
} else {
  message("⚠️ 'Meal_Prem_Hrs' not found, setting mpv_per_pp_less_prems = mpv_per_pp (no adjustment)")
  shift_data1[, `:=`(
    mpv_per_pp_less_prems = mpv_per_pp,
    mpv_per_pp_less_prems_w = mpv_per_pp_w
  )]
}

if ("Rest_Prem_Hrs" %in% names(shift_data1)) {
  shift_data1[, `:=`(
    rpv_per_pp_less_prems = fifelse(is.na(Rest_Prem_Hrs), rpv_per_pp, pmax(0, rpv_per_pp - Rest_Prem_Hrs))
  )]
} else {
  message("⚠️ 'Rest_Prem_Hrs' not found, setting mpv_per_pp_less_prems = mpv_per_pp (no adjustment)")
  shift_data1[, `:=`(
    rpv_per_pp_less_prems = rpv_per_pp
  )]
}

# --- Employee-level meal / rest violations less premiums adjustment ---
viol_ee <- shift_data1[, .(
  mpv_ee   = sum(mpv_shift,   na.rm = TRUE),
  mpv_ee_w = sum(mpv_shift_w, na.rm = TRUE),
  rpv_ee   = sum(rpv_shift,   na.rm = TRUE),
  
  meal_prem_hrs_ee = sum(Meal_Prem_Hrs, na.rm = TRUE),
  rest_prem_hrs_ee = sum(Rest_Prem_Hrs, na.rm = TRUE),
  meal_prem_amt_ee = sum(Meal_Prem_Amt, na.rm = TRUE),
  rest_prem_amt_ee = sum(Rest_Prem_Amt, na.rm = TRUE)
), by = .(ID)]

# (Optional) guardrails if Meal/Rest prem cols could be all-NA but you want explicit zeros
viol_ee[is.na(meal_prem_hrs_ee), meal_prem_hrs_ee := 0]
viol_ee[is.na(rest_prem_hrs_ee), rest_prem_hrs_ee := 0]
viol_ee[is.na(meal_prem_amt_ee), meal_prem_amt_ee := 0]
viol_ee[is.na(rest_prem_amt_ee), rest_prem_amt_ee := 0]

# Subtract paid hours from violations owed; never negative
viol_ee[, `:=`(
  mpv_ee_less_prems   = pmax(0, mpv_ee   - meal_prem_hrs_ee),
  mpv_ee_less_prems_w = pmax(0, mpv_ee_w - meal_prem_hrs_ee),
  rpv_ee_less_prems   = pmax(0, rpv_ee   - rest_prem_hrs_ee)
)]

# Attach employee-level results back onto shift_data1 (every row gets the same employee totals)
shift_data1[viol_ee, `:=`(
  mpv_ee             = i.mpv_ee,
  mpv_ee_w           = i.mpv_ee_w,
  rpv_ee             = i.rpv_ee,
  
  meal_prem_hrs_ee   = i.meal_prem_hrs_ee,
  meal_prem_amt_ee   = i.meal_prem_amt_ee,
  rest_prem_hrs_ee   = i.rest_prem_hrs_ee,
  rest_prem_amt_ee   = i.rest_prem_amt_ee,
  
  mpv_ee_less_prems   = i.mpv_ee_less_prems,
  mpv_ee_less_prems_w = i.mpv_ee_less_prems_w,
  rpv_ee_less_prems   = i.rpv_ee_less_prems
), on = "ID"]


# ----- SHIFT (TIME & PAY) DATA: Merge and compare hours worked and OT/DT with pay data  -----------------------------------------

setDT(pay1)
setDT(shift_data1)

# Summarize pay data for hours worked code
pay_data_hrs_wkd <- pay1[Hrs_Wkd_Pay_Code == 1, .(
  Pay_Hrs_Wkd =        if (all(is.na(Pay_Hours))) NA_real_ else sum(Pay_Hours, na.rm = TRUE),
  Pay_Amount_Hrs_Wkd = if (all(is.na(Pay_Amount))) NA_real_ else sum(Pay_Amount, na.rm = TRUE),
  RROP =               if (all(is.na(RROP))) NA_real_ else mean(RROP, na.rm = TRUE),
  Base_Rate_Avg =      if (all(is.na(Base_Rate1))) NA_real_ else mean(Base_Rate1, na.rm = TRUE)
), by = Pay_ID_Period_End]

setnames(pay_data_hrs_wkd, "Pay_ID_Period_End", "ID_Period_End", skip_absent = TRUE)

shift_data1 <- safe_left_join(shift_data1, pay_data_hrs_wkd, by = "ID_Period_End")

# Calculate differences
if (all(c("Pay_Hrs_Wkd", "pp_shift_hrs") %in% names(shift_data1))) {
  shift_data1[, Pay_Hours_pp_shift_hrs_diff := fifelse(
    is.na(Pay_Hrs_Wkd) | is.na(pp_shift_hrs), NA_real_,
    round(Pay_Hrs_Wkd, 2) - pp_shift_hrs
  )]
}

if (all(c("Pay_Hrs_Wkd", "pp_Hours") %in% names(shift_data1))) {
  shift_data1[, Pay_Hours_pp_Hours_diff := fifelse(
    is.na(Pay_Hrs_Wkd) | is.na(pp_Hours), NA_real_,
    round(Pay_Hrs_Wkd, 2) - round(pp_Hours, 2)
  )]
}

# Assign avg base rate where missing or < 7.25
if ("Base_Rate_Avg" %in% names(shift_data1)) {
  avg_rate <- mean(shift_data1$Base_Rate_Avg, na.rm = TRUE)
  shift_data1[, Base_Rate_Avg := fifelse(
    is.na(Base_Rate_Avg) | Base_Rate_Avg < 7.25,
    avg_rate,
    Base_Rate_Avg
  )]
}

# RROP: Assign avg base rate where missing or < 7.25
if ("RROP" %in% names(shift_data1)) {
  avg_rate <- mean(shift_data1$Base_Rate_Avg, na.rm = TRUE)
  shift_data1[, RROP := fifelse(
    is.na(RROP) | RROP < 7.25,
    avg_rate,
    RROP
  )]
}


# ----- SHIFT (TIME & PAY) DATA: Summary aggregations for damages and other rate computations  -----------------------------------------

# Calculate shifts and shifts per workweek
shift_data1[, Shifts := 1]
shift_data1[, Shifts_per_workweek := .N / uniqueN(ID_Week_End, na.rm = TRUE), by = ID]

# Calculate shift length statistics and shift count thresholds
shift_data1[, `:=`(
  Avg_Shift_Length             = mean(shift_hrs, na.rm = TRUE),
  Median_Shift_Length          = median(shift_hrs, na.rm = TRUE),
  
  Shifts_gt_3_5                = fifelse(shift_hrs > 3.5, 1L, 0L),
  Shifts_gt_5                  = fifelse(shift_hrs > 5, 1L, 0L),
  Shifts_gt_5_lte_6            = fifelse(shift_hrs > 5 & shift_hrs <= 6, 1L, 0L),
  Shifts_gt_6                  = fifelse(shift_hrs > 6, 1L, 0L),
  Shifts_e_8                   = fifelse(shift_hrs == 8, 1L, 0L),
  Shifts_gt_8                  = fifelse(shift_hrs > 8, 1L, 0L),
  Shifts_gt_10                 = fifelse(shift_hrs > 10, 1L, 0L),
  Shifts_gt_10_lte_12          = fifelse(shift_hrs > 10 & shift_hrs <= 12, 1L, 0L),
  Shifts_gt_12                 = fifelse(shift_hrs > 12, 1L, 0L),
  Shifts_gt_14                 = fifelse(shift_hrs > 14, 1L, 0L)
), by = ID]

# Employee totals across pay periods: only count pp==1 rows so each pay period counts once
# Employee-level constants (mpv_ee_less_prems / rpv_ee_less_prems) just pass through
shift_data1[, `:=`(
  Meal_Prem_Hrs_per_ee = sum(fifelse(pp == 1, fcoalesce(as.numeric(Meal_Prem_Hrs), 0.0), 0.0)),
  Meal_Prem_Amt_per_ee = sum(fifelse(pp == 1, fcoalesce(as.numeric(Meal_Prem_Amt), 0.0), 0.0)),
  Rest_Prem_Hrs_per_ee = sum(fifelse(pp == 1, fcoalesce(as.numeric(Rest_Prem_Hrs), 0.0), 0.0)),
  Rest_Prem_Amt_per_ee = sum(fifelse(pp == 1, fcoalesce(as.numeric(Rest_Prem_Amt), 0.0), 0.0)),
  
  mpv_per_pp_per_ee    = sum(fifelse(pp == 1, fcoalesce(as.numeric(mpv_per_pp), 0.0), 0.0)),
  mpv_per_pp_per_ee_w  = sum(fifelse(pp == 1, fcoalesce(as.numeric(mpv_per_pp_w), 0.0), 0.0)),
  rpv_per_pp_per_ee    = sum(fifelse(pp == 1, fcoalesce(as.numeric(rpv_per_pp), 0.0), 0.0)),
  
  mpv_per_pp_less_prems_per_ee   = sum(fifelse(pp == 1, fcoalesce(as.numeric(mpv_per_pp_less_prems), 0.0), 0.0)),
  mpv_per_pp_less_prems_per_ee_w = sum(fifelse(pp == 1, fcoalesce(as.numeric(mpv_per_pp_less_prems_w), 0.0), 0.0)),
  rpv_per_pp_less_prems_per_ee   = sum(fifelse(pp == 1, fcoalesce(as.numeric(rpv_per_pp_less_prems), 0.0), 0.0)),
  
  # employee-level constants: pass through once per employee
  mpv_ee_less_prems_per_ee   = fifelse(all(is.na(mpv_ee_less_prems)),   NA_real_, max(as.numeric(mpv_ee_less_prems),   na.rm = TRUE)),
  mpv_ee_less_prems_per_ee_w = fifelse(all(is.na(mpv_ee_less_prems_w)), NA_real_, max(as.numeric(mpv_ee_less_prems_w), na.rm = TRUE)),
  rpv_ee_less_prems_per_ee   = fifelse(all(is.na(rpv_ee_less_prems)),   NA_real_, max(as.numeric(rpv_ee_less_prems),   na.rm = TRUE))
), by = ID]


# Employee-level rates (avoid double counting per_pp fields by only counting rows where pp == 1)

shift_data1[, c(
  "perc_shifts_gt_3_5", "perc_shifts_gt_5", "perc_shifts_gt_5_lte_6", "perc_shifts_gt_6",
  "perc_shifts_e_8", "perc_shifts_gt_8", "perc_shifts_gt_10", "perc_shifts_gt_10_lte_12",
  "perc_shifts_gt_12", "perc_shifts_gt_14",
  "perc_mp", "perc_mp_lt_thirty", "perc_mp_thirty", "perc_mp_gt_thirty",
  "mpv_rate", "mpv_rate_w", "mpv_rate_less_prems", "mpv_rate_less_prems_w",
  "mpv_ee_rate_less_prems", "mpv_ee_rate_less_prems_w",
  "rpv_rate", "rpv_rate_less_prems", "rpv_ee_rate_less_prems"
) := {
  n_shifts <- .N
  mp_n     <- sum(mp, na.rm = TRUE)
  
  n_gt3_5  <- sum(shift_hrs > 3.5, na.rm = TRUE)
  n_gt5    <- sum(shift_hrs > 5,   na.rm = TRUE)
  n_gt6    <- sum(shift_hrs > 6,   na.rm = TRUE)
  
  # pay-period-level rows only (pp == 1) to prevent double counting
  pp_rows <- (pp == 1)
  
  # Use PAY-PERIOD totals for thresholds (recommended)
  n_gt5_pp   <- sum(pp_rows & fcoalesce(pp_shift_hrs, 0) > 5,   na.rm = TRUE)
  n_gt6_pp   <- sum(pp_rows & fcoalesce(pp_shift_hrs, 0) > 6,   na.rm = TRUE)
  n_gt3_5_pp <- sum(pp_rows & fcoalesce(pp_shift_hrs, 0) > 3.5, na.rm = TRUE)
  
  # ee_less_prems are employee-level constants repeated on every row
  mpv_ee_lp   <- if (all(is.na(mpv_ee_less_prems)))   NA_real_ else max(mpv_ee_less_prems,   na.rm = TRUE)
  mpv_ee_lp_w <- if (all(is.na(mpv_ee_less_prems_w))) NA_real_ else max(mpv_ee_less_prems_w, na.rm = TRUE)
  rpv_ee_lp   <- if (all(is.na(rpv_ee_less_prems)))   NA_real_ else max(rpv_ee_less_prems,   na.rm = TRUE)
  
  list(
    round(fifelse(n_shifts > 0, sum(shift_hrs > 3.5, na.rm = TRUE) / n_shifts, NA_real_), 6),
    round(fifelse(n_shifts > 0, sum(shift_hrs > 5,   na.rm = TRUE) / n_shifts, NA_real_), 6),
    round(fifelse(n_shifts > 0, sum(shift_hrs > 5 & shift_hrs <= 6, na.rm = TRUE) / n_shifts, NA_real_), 6),
    round(fifelse(n_shifts > 0, sum(shift_hrs > 6,   na.rm = TRUE) / n_shifts, NA_real_), 6),
    round(fifelse(n_shifts > 0, sum(shift_hrs == 8,  na.rm = TRUE) / n_shifts, NA_real_), 6),
    round(fifelse(n_shifts > 0, sum(shift_hrs > 8,   na.rm = TRUE) / n_shifts, NA_real_), 6),
    round(fifelse(n_shifts > 0, sum(shift_hrs > 10,  na.rm = TRUE) / n_shifts, NA_real_), 6),
    round(fifelse(n_shifts > 0, sum(shift_hrs > 10 & shift_hrs <= 12, na.rm = TRUE) / n_shifts, NA_real_), 6),
    round(fifelse(n_shifts > 0, sum(shift_hrs > 12,  na.rm = TRUE) / n_shifts, NA_real_), 6),
    round(fifelse(n_shifts > 0, sum(shift_hrs > 14,  na.rm = TRUE) / n_shifts, NA_real_), 6),
    
    round(fifelse(n_shifts > 0, sum(mp, na.rm = TRUE) / n_shifts, NA_real_), 6),
    round(fifelse(mp_n > 0, sum(mp_lt_thirty, na.rm = TRUE) / mp_n, NA_real_), 6),
    round(fifelse(mp_n > 0, sum(mp_thirty,    na.rm = TRUE) / mp_n, NA_real_), 6),
    round(fifelse(mp_n > 0, sum(mp_gt_thirty, na.rm = TRUE) / mp_n, NA_real_), 6),
    
    round(fifelse(n_gt5 > 0, sum(mpv_shift,   na.rm = TRUE) / n_gt5, NA_real_), 6),
    round(fifelse(n_gt6 > 0, sum(mpv_shift_w, na.rm = TRUE) / n_gt6, NA_real_), 6),
    
    # per_pp fields: only count once per pay period (pp == 1)
    round(fifelse(n_gt5_pp > 0, sum(fifelse(pp_rows, fcoalesce(mpv_per_pp_less_prems, 0), 0), na.rm = TRUE) / n_gt5_pp, NA_real_), 6),
    round(fifelse(n_gt6_pp > 0, sum(fifelse(pp_rows, fcoalesce(mpv_per_pp_less_prems_w, 0), 0), na.rm = TRUE) / n_gt6_pp, NA_real_), 6),
    
    # ee_less_prems: employee-level constant; use once
    round(fifelse(n_gt5 > 0, mpv_ee_lp   / n_gt5, NA_real_), 6),
    round(fifelse(n_gt6 > 0, mpv_ee_lp_w / n_gt6, NA_real_), 6),
    
    round(fifelse(n_gt3_5 > 0, sum(rpv_shift, na.rm = TRUE) / n_gt3_5, NA_real_), 6),
    
    # per_pp rest: only count once per pay period (pp == 1)
    round(fifelse(n_gt3_5_pp > 0, sum(fifelse(pp_rows, fcoalesce(rpv_per_pp_less_prems, 0), 0), na.rm = TRUE) / n_gt3_5_pp, NA_real_), 6),
    
    # ee_less_prems rest: employee-level constant repeated; use once
    round(fifelse(n_gt3_5 > 0, rpv_ee_lp / n_gt3_5, NA_real_), 6)
  )
}, by = ID]


# ----- ALL DATA (BY PP):        Aggregate time and pay data by pay period and merge -----------------------------------------

# MERGE pay1 and shift_data1 after aggregating key statisitcs by pay period (one row per pp)

# --- pay1 pay period aggregation ----
setDT(pay1)

# Define fields globally
first_fields_default <- c("Pay_Source", "Pay_Name", "Pay_ID", "Pay_Key_Gps", "Pay_Date", "Pay_Period_End", "Pay_ID_Current_Qtr",
                          "Sample", "Class_Job", "Hire_Date", "Term_Date", "Subclass(es)")
sum_fields_default <- c("Pay_Hours", "Pay_Amount")
max_fields_default <- c("pp_Base_Rate", "RROP", "double_CA_min_wage", "CA_min_wage", 
                        "pp_Hrs_Wkd", "pp_Reg_Hrs", "pp_OT_Hrs", "pp_DT_Hrs", "pp_Meal_Prem_Hrs", 
                        "pp_Rest_Prem_Hrs", "pp_Sick_Hrs", "pp_Straight_Time_Amt", "pp_Reg_Amt", 
                        "pp_OT_Amt", "pp_DT_Amt", "pp_Meal_Amt", "pp_Rest_Amt", "pp_Sick_Amt",
                        "pp_Oth_RROP_Amt", "pp_Oth_Amt",
                        
                        # "prior_pp_Base_Rate", "prior_pp_Min_Pay_Date", "prior_pp_Max_Pay_Date",
                        # "prior_pp_Hrs_Wkd", "prior_pp_Reg_Hrs", "prior_pp_OT_Hrs", "prior_pp_DT_Hrs",
                        # "prior_pp_Meal_Prem_Hrs", "prior_pp_Rest_Prem_Hrs", "prior_pp_Sick_Hrs",
                        # "prior_pp_Straight_Time_Amt", "prior_pp_Reg_Amt", "prior_pp_OT_Amt", "prior_pp_DT_Amt",
                        # "prior_pp_Meal_Amt", "prior_pp_Rest_Amt", "prior_pp_Sick_Amt",
                        # "prior_pp_Oth_RROP_Amt", "prior_pp_Oth_Amt", "prior_pp_Rate_Rank",
                        # 
                        # "qtr_Base_Rate", "qtr_Min_Pay_Date", "qtr_Max_Pay_Date",
                        # "qtr_Hrs_Wkd", "qtr_Reg_Hrs", "qtr_OT_Hrs", "qtr_DT_Hrs",
                        # "qtr_Meal_Prem_Hrs", "qtr_Rest_Prem_Hrs", "qtr_Sick_Hrs",
                        # "qtr_Straight_Time_Amt", "qtr_Reg_Amt", "qtr_OT_Amt", "qtr_DT_Amt",
                        # "qtr_Meal_Amt", "qtr_Rest_Amt", "qtr_Sick_Amt",
                        # "qtr_Oth_RROP_Amt", "qtr_Oth_Amt", "qtr_Rate_Rank",
                        # 
                        # "prior_qtr_Base_Rate", "prior_qtr_Min_Pay_Date", "prior_qtr_Max_Pay_Date",
                        # "prior_qtr_Hrs_Wkd", "prior_qtr_Reg_Hrs", "prior_qtr_OT_Hrs", "prior_qtr_DT_Hrs",
                        # "prior_qtr_Meal_Prem_Hrs", "prior_qtr_Rest_Prem_Hrs", "prior_qtr_Sick_Hrs",
                        # "prior_qtr_Straight_Time_Amt", "prior_qtr_Reg_Amt", "prior_qtr_OT_Amt", "prior_qtr_DT_Amt",
                        # "prior_qtr_Meal_Amt", "prior_qtr_Rest_Amt", "prior_qtr_Sick_Amt",
                        # "prior_qtr_Oth_RROP_Amt", "prior_qtr_Oth_Amt", "prior_qtr_Rate_Rank",
                        
                        "rrop_by_code_underpayment", "Calc_Tot_Wages", "Actual_Wages",
                        "Wage_Diff", "OT_Diff", "DT_Diff", "Meal_Diff", "Rest_Diff", "Sick_Diff",
                        "OT_Overpayment", "DT_Overpayment", "Meal_Overpayment", "Rest_Overpayment", "Sick_Overpayment",
                        "OT_rrop_dmgs", "DT_rrop_dmgs", "Meal_rrop_dmgs", "Rest_rrop_dmgs", "Sick_rrop_dmgs",
                        "Gross_Overpayment", "Gross_rrop_dmgs", "Net_Overpayment",  "Net_rrop_dmgs", "rrop_any_underpayment", "rrop_net_underpayment"
)
min_fields_default <- c(character(0))
mean_fields_default <- c(character(0))
median_fields_default <- c(character(0))

pp_pay1 <- aggregate_data(pay1, by = "Pay_ID_Period_End")

pp_pay1 <- remove_suffixes(
  pp_pay1,
  suffixes = c("_sum", "_max", "_min", "_mean", "_median")
)

setnames(pp_pay1, "Pay_ID_Period_End", "ID_Period_End", skip_absent = TRUE)

# --- shift_data1 pay period aggregation ----

setDT(shift_data1)

# Fields
first_fields_default <- c(
  "Source", "Sheet", "Key_Gps", "ID", "Name", "ID_Date", "Period_Beg", "Period_End", 
  "Location_Name", "Executive_Area", "Area", "Region"
)

sum_fields_default <- c(
  "shift", "shift_hrs", "Hours", "mp",
  
  "mp_lt_twenty",
  "mp_lt_thirty", "mp_thirty", "mp_gt_thirty",
  "mp_gt_two_hrs", "mp_gt_four_hrs"
  
  ,"short_break_hrs", "short_break_reg_hrs", "short_break_ot_hrs",
  "split_shift", "five_hour_guarantee", "split_shift_hrs", "guarantee_hrs", "pto_hrs",
  
  "MissMP1", "LateMP1", "ShortMP1", "MissMP2", "LateMP2", "ShortMP2",
  "mp1_violation", "mp2_violation",
  
  "MissMP1_w", "LateMP1_w", "ShortMP1_w", "MissMP2_w", "LateMP2_w", "ShortMP2_w",
  "mp1_violation_w", "mp2_violation_w"
)

max_fields_default <- c(
  "pp_shift_hrs", "pp_Hours",
  "Base_Rate_Avg", "RROP",
  
  "pp_ot", "pp_dt", "pp_ca_ot_hrs", "pp_ca_dt_hrs", "pp_flsa_ot", "pp_ot_wks",
  
  "mpv_per_pp", "mpv_per_pp_w", "rpv_per_pp",
  "mpv_shift_less_prems", "mpv_shift_less_prems_w", "rpv_shift_less_prems",
  "mpv_per_pp_less_prems", "mpv_per_pp_less_prems_w", "rpv_per_pp_less_prems",
  
  "mpv_ee", "mpv_ee_w", "rpv_ee",
  "mpv_ee_less_prems", "mpv_ee_less_prems_w",
  "rpv_ee_less_prems",
  
  "Shifts", "Shifts_per_workweek", "Avg_Shift_Length", "Median_Shift_Length",
  "Shifts_gt_3_5", "Shifts_gt_5", "Shifts_gt_5_lte_6", "Shifts_gt_6",
  "Shifts_e_8", "Shifts_gt_8", "Shifts_gt_10", "Shifts_gt_10_lte_12",
  "Shifts_gt_12", "Shifts_gt_14",
  
  "perc_shifts_gt_3_5", "perc_shifts_gt_5", "perc_shifts_gt_5_lte_6", "perc_shifts_gt_6",
  "perc_shifts_e_8", "perc_shifts_gt_8", "perc_shifts_gt_10", "perc_shifts_gt_10_lte_12",
  "perc_shifts_gt_12", "perc_shifts_gt_14",
  "perc_mp", "perc_mp_lt_thirty", "perc_mp_thirty", "perc_mp_gt_thirty",
  
  "mpv_rate", "mpv_rate_w", "mpv_rate_less_prems", "mpv_rate_less_prems_w",
  "mpv_ee_rate_less_prems", "mpv_ee_rate_less_prems_w",
  "rpv_rate", "rpv_rate_less_prems",
  "rpv_ee_rate_less_prems"
)

min_fields_default <- character(0)
mean_fields_default <- character(0)
median_fields_default <- character(0)

pp_shift_data1 <- aggregate_data(shift_data1, by = "ID_Period_End")

pp_shift_data1 <- remove_suffixes(
  pp_shift_data1,
  suffixes = c("_sum", "_max", "_min", "_mean", "_median")
)

# --- Full outer join: keep all shift rows + all pay-period rows (1 row per ID_Period_End) ---
setDT(pp_shift_data1)
setDT(pp_pay1)

# Add row markers before join
pp_shift_data1[, shift_row_id := .I]
pp_pay1[, pay_row_id := .I]
n_shift0 <- nrow(pp_shift_data1)

setnames(pp_pay1, "Pay_ID_Period_End", "ID_Period_End", skip_absent = TRUE)
setkey(pp_shift_data1, ID_Period_End)
setkey(pp_pay1, ID_Period_End)

left_joined <- pp_pay1[pp_shift_data1]
right_only  <- pp_pay1[!pp_shift_data1, on = "ID_Period_End"]

pp_data1 <- rbindlist(list(left_joined, right_only), use.names = TRUE, fill = TRUE)

pp_data1[, has_shift := as.integer(!is.na(shift_row_id))]
pp_data1[, has_pay   := as.integer(!is.na(pay_row_id))]

stopifnot(pp_data1[has_shift == 1, .N] == n_shift0)
stopifnot(nrow(pp_data1) >= n_shift0)

pp_data1[, .(
  rows_total      = .N,
  rows_shift_data = sum(has_shift == 1, na.rm = TRUE),
  rows_pay_data   = sum(has_pay == 1, na.rm = TRUE),
  rows_shift_only = sum(has_shift == 1 & has_pay == 0, na.rm = TRUE),
  rows_pay_only   = sum(has_shift == 0 & has_pay == 1, na.rm = TRUE)
)]

# Coalesce matching columns (prefer non-NA), then drop redundant Pay_ versions
pp_data1[, `:=`(
  ID         = fcoalesce(ID, Pay_ID),
  Key_Gps    = fcoalesce(Key_Gps, Pay_Key_Gps),
  Period_End = fcoalesce(Period_End, Pay_Period_End),
  RROP       = fcoalesce(RROP, i.RROP),
  Base_Rate  = fcoalesce(Base_Rate_Avg, pp_Base_Rate)
)]

# Remove the now-redundant columns
pp_data1[, c("Pay_ID", "Pay_Key_Gps", "Pay_Period_End", "i.RROP", "Base_Rate_Avg", "pp_Base_Rate") := NULL]

# EE FLAG (one row per employee) ---
setDT(pp_data1)
setorder(pp_data1, ID, Period_End, Pay_Date, ID_Period_End)

pp_data1[, ee_flag := as.integer(rowid(ID) == 1L)]


# ----- ALL DATA (BY PP):        Principal damages -----------------------------------------

# Settings (>0 or TRUE means damages are "ON")
mp_dmgs_switch             <- TRUE
rp_dmgs_switch             <- TRUE
rrop_dmgs_switch           <- TRUE
otc_hrs_per_shift          <- 0 #e.g., (20/60) for 20 mins per shift
unreimb_exp_per_pp         <- 0
clock_rounding_dmgs_switch <- FALSE
unpaid_ot_dmgs_switch      <- TRUE
min_wage_dmgs_switch       <- TRUE

setDT(pp_data1)

# Employee-level RROP (average RROP across pay periods)
pp_data1[, RROP_ee := {
  if (all(is.na(RROP))) NA_real_ else mean(RROP, na.rm = TRUE)
}, by = ID]

# Principal damages by claim 
pp_data1[, `:=`(
# Meal 
  mp_dmgs                 = fifelse(mp_dmgs_switch == FALSE | is.na(mpv_per_pp)   | is.na(RROP), NA_real_, mpv_per_pp   * RROP),
  mp_dmgs_w               = fifelse(mp_dmgs_switch == FALSE | is.na(mpv_per_pp_w) | is.na(RROP), NA_real_, mpv_per_pp_w * RROP),
  mp_dmgs_less_prems      = fifelse(mp_dmgs_switch == FALSE | is.na(mpv_per_pp_less_prems)   | is.na(RROP), NA_real_, mpv_per_pp_less_prems   * RROP),
  mp_dmgs_less_prems_w    = fifelse(mp_dmgs_switch == FALSE | is.na(mpv_per_pp_less_prems_w) | is.na(RROP), NA_real_, mpv_per_pp_less_prems_w * RROP),
# Rest 
  rp_dmgs                 = fifelse(rp_dmgs_switch == FALSE | is.na(rpv_per_pp) | is.na(RROP), NA_real_, rpv_per_pp * RROP),
  rp_dmgs_less_prems      = fifelse(rp_dmgs_switch == FALSE | is.na(rpv_per_pp_less_prems) | is.na(RROP), NA_real_, rpv_per_pp_less_prems * RROP),
# RROP (pulled in from pay1, already at pp level) 
  Net_rrop_dmgs           = fifelse(rep_len(!rrop_dmgs_switch, .N), 0, fcoalesce(Net_rrop_dmgs, 0)),
  Gross_rrop_dmgs         = fifelse(rep_len(!rrop_dmgs_switch, .N), 0, fcoalesce(Gross_rrop_dmgs, 0)),                   
# Off-the-clock
  otc_dmgs                = fifelse(is.na(RROP), 0, otc_hrs_per_shift * Shifts * RROP),                                                   # MUST BE UPDATED
# Unreimbursed expenses
  unreimb_exp_dmgs        = unreimb_exp_per_pp,
# Clock rounding
  clock_rounding_dmgs     = fifelse(!clock_rounding_dmgs_switch | is.na(RROP), 0, 0 * RROP),                                              # MUST BE UPDATED
# Unpaid wages (min wage)
  min_wage_dmgs           = fifelse(min_wage_dmgs_switch == FALSE | is.na(RROP) | is.na(short_break_hrs), 0, 
                                    (short_break_reg_hrs * CA_min_wage) + (short_break_ot_hrs * RROP))                                                     
)]

# --- Unpaid overtime and double time ---

# Define buffers (in hours)
min_ot_buffer <- 0.25
max_ot_buffer <- 20

# Calculate intermediate columns
pp_data1[, `:=`(
  unpaid_ot_hrs    = pmax(0, pp_ot - pp_OT_Hrs),
  unpaid_dt_hrs    = pmax(0, pp_dt - pp_DT_Hrs)
)]

pp_data1[, unpaid_hrs_total := unpaid_ot_hrs + unpaid_dt_hrs]

pp_data1[, unpaid_ot_dmgs_raw := (unpaid_ot_hrs * RROP * var_half_time_OT_multiplier) + 
           (unpaid_dt_hrs * RROP)]

# Final calculation with buffer logic
pp_data1[, unpaid_ot_dmgs := fifelse(
  unpaid_ot_dmgs_switch == FALSE | is.na(RROP), 0,
  fifelse(
    unpaid_hrs_total <= min_ot_buffer | unpaid_hrs_total >= max_ot_buffer, 0,
    fifelse(is.na(unpaid_ot_dmgs_raw), NA_real_, round(unpaid_ot_dmgs_raw, 2))
  )
)]

# Meal/Rest - employee-level premium adjustment 
pp_data1[, `:=`(
  mp_ee_dmgs_less_prems   = fifelse(is.na(mpv_ee_less_prems)   | is.na(RROP_ee), NA_real_, mpv_ee_less_prems   * RROP_ee),
  mp_ee_dmgs_less_prems_w = fifelse(is.na(mpv_ee_less_prems_w) | is.na(RROP_ee), NA_real_, mpv_ee_less_prems_w * RROP_ee),
  rp_ee_dmgs_less_prems   = fifelse(is.na(rpv_ee_less_prems)   | is.na(RROP_ee), NA_real_, rpv_ee_less_prems   * RROP_ee)
), by = ID]

# Total principal damages (no waivers)
pp_data1[, tot_principal_dmgs :=
           fcoalesce(mp_dmgs, 0) +
           fcoalesce(rp_dmgs, 0) +
           fcoalesce(Net_rrop_dmgs, 0) +
           fcoalesce(otc_dmgs, 0) +
           fcoalesce(unreimb_exp_dmgs, 0) +
           fcoalesce(clock_rounding_dmgs, 0) +
           fcoalesce(unpaid_ot_dmgs, 0) +
           fcoalesce(min_wage_dmgs, 0)
]

# Total principal damages (less premiums and no waivers)
pp_data1[, tot_principal_dmgs_less_prems :=
           fcoalesce(mp_dmgs_less_prems, 0) +
           fcoalesce(rp_dmgs_less_prems, 0) +
           fcoalesce(Net_rrop_dmgs, 0) +
           fcoalesce(otc_dmgs, 0) +
           fcoalesce(unreimb_exp_dmgs, 0) +
           fcoalesce(clock_rounding_dmgs, 0) +
           fcoalesce(unpaid_ot_dmgs, 0) +
           fcoalesce(min_wage_dmgs, 0)
]

# Total principal damages (waivers)
pp_data1[, tot_principal_dmgs_w :=
           fcoalesce(mp_dmgs_w, 0) +
           fcoalesce(rp_dmgs, 0) +
           fcoalesce(Net_rrop_dmgs, 0) +
           fcoalesce(otc_dmgs, 0) +
           fcoalesce(unreimb_exp_dmgs, 0) +
           fcoalesce(clock_rounding_dmgs, 0) +
           fcoalesce(unpaid_ot_dmgs, 0) +
           fcoalesce(min_wage_dmgs, 0)
]

# Total principal damages (less premiums & waivers)
pp_data1[, tot_principal_dmgs_less_prems_w :=
           fcoalesce(mp_dmgs_less_prems_w, 0) +
           fcoalesce(rp_dmgs_less_prems, 0) +
           fcoalesce(Net_rrop_dmgs, 0) +
           fcoalesce(otc_dmgs, 0) +
           fcoalesce(unreimb_exp_dmgs, 0) +
           fcoalesce(clock_rounding_dmgs, 0) +
           fcoalesce(unpaid_ot_dmgs, 0) +
           fcoalesce(min_wage_dmgs, 0)
]


# ----- ALL DATA (BY PP):        Credit to total principal damages (if any credits apply)

# Credit amount - SET TO TOTAL GUARANTEED WAGES IN GIVEN PAY PERIOD
pay1_credits <- pay1[
  grepl("guar", Pay_Code, ignore.case = TRUE),
  .(Credit_Amount = sum(Pay_Amount, na.rm = TRUE)),
  by = .(Pay_ID_Period_End)
]

# Join credit values to pp_data1
 setDT(pp_data1)
 setDT(pay1_credits)
pp_data1 <- safe_left_join(pp_data1, pay1_credits, by = c("ID_Period_End" = "Pay_ID_Period_End"))

# Default missing credits to 0
pp_data1[, Credit_Amount :=  fcoalesce(Credit_Amount, 0)]

# Total principal damages, less credits (no waivers)
pp_data1[, tot_principal_dmgs_less_credits :=
           pmax(0, tot_principal_dmgs - Credit_Amount)
]

# Total principal damages, less credits (less premiums, no waivers)
pp_data1[, tot_principal_dmgs_less_prems_and_credits :=
           pmax(0, tot_principal_dmgs_less_prems - Credit_Amount)
]

# Total principal damages, less credits (waivers)
pp_data1[, tot_principal_dmgs_less_credits_w :=
           pmax(0, tot_principal_dmgs_w - Credit_Amount)
]

# Total principal damages, less credits (less premiums, waivers)
pp_data1[, tot_principal_dmgs_less_prems_and_credits_w :=
           pmax(0, tot_principal_dmgs_less_prems_w - Credit_Amount)
]


# ----- ALL DATA (BY PP):        TOTAL INTEREST (scenario-specific but aggregates all claims' damages) ----------------------

annual_interest_rate      <- 0.07 # 7% prejudgment interest rate
monthly_interest_rate     <- annual_interest_rate / 12
interest_thru_date        <- mediation_date

pp_data1[, interest_months := fifelse(
  !is.na(Period_End) & Period_End <= interest_thru_date,
  as.integer((year(interest_thru_date) - year(Period_End)) * 12 +
               (month(interest_thru_date) - month(Period_End))),
  0L
)]

# INTEREST
pp_data1[, `:=`(
  interest =
    pmax(0, fcoalesce(tot_principal_dmgs, 0)) * interest_months * monthly_interest_rate,
  
  interest_less_prems =
    pmax(0, fcoalesce(tot_principal_dmgs_less_prems, 0)) * interest_months * monthly_interest_rate,
  
  interest_w =
    pmax(0, fcoalesce(tot_principal_dmgs_w, 0)) * interest_months * monthly_interest_rate,
  
  interest_less_prems_w =
    pmax(0, fcoalesce(tot_principal_dmgs_less_prems_w, 0)) * interest_months * monthly_interest_rate,
  
  interest_less_credits =
    pmax(0, fcoalesce(tot_principal_dmgs_less_credits, 0)) * interest_months * monthly_interest_rate,
  
  interest_less_prems_and_credits =
    pmax(0, fcoalesce(tot_principal_dmgs_less_prems_and_credits, 0)) * interest_months * monthly_interest_rate,
  
  interest_less_credits_w =
    pmax(0, fcoalesce(tot_principal_dmgs_less_credits_w, 0)) * interest_months * monthly_interest_rate,
  
  interest_less_prems_and_credits_w =
    pmax(0, fcoalesce(tot_principal_dmgs_less_prems_and_credits_w, 0)) * interest_months * monthly_interest_rate
)]

# TOTAL DAMAGES (principal + interest)
# (NOTE: this does not yet include wage statement or waiting time penalties!)
pp_data1[, `:=`(
  tot_dmgs =
    pmax(0, fcoalesce(tot_principal_dmgs, 0)) + fcoalesce(interest, 0),
  
  tot_dmgs_less_prems =
    pmax(0, fcoalesce(tot_principal_dmgs_less_prems, 0)) + fcoalesce(interest_less_prems, 0),
  
  tot_dmgs_w =
    pmax(0, fcoalesce(tot_principal_dmgs_w, 0)) + fcoalesce(interest_w, 0),
  
  tot_dmgs_less_prems_w =
    pmax(0, fcoalesce(tot_principal_dmgs_less_prems_w, 0)) + fcoalesce(interest_less_prems_w, 0),
  
  tot_dmgs_less_credits =
    pmax(0, fcoalesce(tot_principal_dmgs_less_credits, 0)) + fcoalesce(interest_less_credits, 0),
  
  tot_dmgs_less_prems_and_credits =
    pmax(0, fcoalesce(tot_principal_dmgs_less_prems_and_credits, 0)) + fcoalesce(interest_less_prems_and_credits, 0),
  
  tot_dmgs_less_credits_w =
    pmax(0, fcoalesce(tot_principal_dmgs_less_credits_w, 0)) + fcoalesce(interest_less_credits_w, 0),
  
  tot_dmgs_less_prems_and_credits_w =
    pmax(0, fcoalesce(tot_principal_dmgs_less_prems_and_credits_w, 0)) + fcoalesce(interest_less_prems_and_credits_w, 0)
)]


# ----- ALL DATA (BY PP):        Flag pay periods with damages -----------------------------------------

flag_map <- list(
  has_dmgs                          = "tot_principal_dmgs",
  has_dmgs_less_prems               = "tot_principal_dmgs_less_prems",
  has_dmgs_w                        = "tot_principal_dmgs_w",
  has_dmgs_less_prems_w             = "tot_principal_dmgs_less_prems_w",
  has_dmgs_less_credits             = "tot_principal_dmgs_less_credits",
  has_dmgs_less_prems_and_credits   = "tot_principal_dmgs_less_prems_and_credits",
  has_dmgs_less_credits_w           = "tot_principal_dmgs_less_credits_w",
  has_dmgs_less_prems_and_credits_w = "tot_principal_dmgs_less_prems_and_credits_w"
)

# Create all flags (no filtering)
for (flag in names(flag_map)) {
  
  col <- flag_map[[flag]]
  
  pp_data1[, (flag) :=
             as.integer(fcoalesce(get(col), 0) > 0)]
}


# ----- ALL DATA (BY PP):        Wage statement penalties (class) -----------------------------------------

# Settings
wsv_initial_pp_penalty     <- 50
wsv_subsequent_pp_penalty  <- 100
wsv_cap                    <- 4000

# Order by ID and Period_End
setorder(pp_data1, ID, Period_End)

# Pay period counter per employee
pp_data1[, pp_count := seq_len(.N), by = ID]

# WSV pay period counter (only counts pay periods after wsv_start_date)
pp_data1[, wsv_pp_count := cumsum(Period_End > wsv_start_date), by = ID]

# Map scenarios to your has_* flags
wsv_scenarios <- list(
  base                    = "has_dmgs",
  less_prems              = "has_dmgs_less_prems",
  w                       = "has_dmgs_w",
  less_prems_w            = "has_dmgs_less_prems_w",
  less_credits            = "has_dmgs_less_credits",
  less_prems_and_credits  = "has_dmgs_less_prems_and_credits",
  less_credits_w          = "has_dmgs_less_credits_w",
  less_prems_and_credits_w= "has_dmgs_less_prems_and_credits_w"
)

# Helper: compute WSV flags/counts/penalty for one scenario
calc_wsv <- function(suffix, has_flag_col) {
  
  flag_col   <- paste0("wsv_penalty_flag_", suffix)
  count_col  <- paste0("wsv_flag_count_", suffix)
  pen_col    <- paste0("wsv_penalty_", suffix)
  
  # 1) Flag this pay period
  pp_data1[, (flag_col) :=
             as.integer(Period_End > wsv_start_date & fcoalesce(get(has_flag_col), 0L) == 1L)
  ]
  
  # 2) Total flagged pay periods per employee (same value repeated on each row of that ID)
  pp_data1[, (count_col) := sum(get(flag_col), na.rm = TRUE), by = ID]
  
  # 3) Penalty per pay period row (based on that employee's count), capped
  pp_data1[, (pen_col) := fcase(
    get(count_col) == 0L, 0,
    get(count_col) == 1L, wsv_initial_pp_penalty,
    get(count_col) >  1L, wsv_initial_pp_penalty + (get(count_col) - 1L) * wsv_subsequent_pp_penalty,
    default = 0
  )]
  
  pp_data1[, (pen_col) := pmin(get(pen_col), wsv_cap)]
}

# Run all scenarios
for (nm in names(wsv_scenarios)) {
  calc_wsv(nm, wsv_scenarios[[nm]])
}

setnames(
  pp_data1,
  old = c("wsv_penalty_flag_base",
          "wsv_flag_count_base",
          "wsv_penalty_base"),
  new = c("wsv_penalty_flag",
          "wsv_flag_count",
          "wsv_penalty")
)


# ----- ALL DATA (BY PP):        Waiting time penalties (class) -----------------------------------------

# Settings
wt_active_days_threshold <- 30
wt_use_rrop <- TRUE  # FALSE uses final_Base_Rate instead of RROP

# Max Period_End across all data
max_period_end <- max(pp_data1$Period_End, na.rm = TRUE)

# Flag active employees (their max Period_End within threshold of overall max)
pp_data1[, active :=
           as.integer(max(Period_End, na.rm = TRUE) >= max_period_end - wt_active_days_threshold),
         by = ID]

# Order by ID and Period_End to ensure last record is correct
setorder(pp_data1, ID, Period_End)

# Calculate mean rates per employee (use only >0 values)
pp_data1[, `:=`(
  mean_Base_Rate = mean(Base_Rate[Base_Rate > 0], na.rm = TRUE),
  mean_RROP      = mean(RROP[RROP > 0], na.rm = TRUE)
), by = ID]

# Get final record values
pp_data1[, `:=`(
  last_Base_Rate = last(Base_Rate),
  last_RROP      = last(RROP)
), by = ID]

# Final rate: use last if non-NA and > mean, else mean
pp_data1[, `:=`(
  final_Base_Rate = fifelse(!is.na(last_Base_Rate) & last_Base_Rate > mean_Base_Rate, last_Base_Rate, mean_Base_Rate),
  final_RROP      = fifelse(!is.na(last_RROP)      & last_RROP      > mean_RROP,      last_RROP,      mean_RROP)
)]

# Clean up intermediates
pp_data1[, c("mean_Base_Rate", "mean_RROP", "last_Base_Rate", "last_RROP") := NULL]

# Rate used for WT
pp_data1[, wt_rate := if (isTRUE(wt_use_rrop)) final_RROP else final_Base_Rate]

# Flag if employee has any records after wt_start_date
pp_data1[, has_wt_period := as.integer(any(Period_End > wt_start_date, na.rm = TRUE)), by = ID]

# Map scenarios to has_* flags
wt_scenarios <- list(
  base                    = "has_dmgs",
  less_prems              = "has_dmgs_less_prems",
  w                       = "has_dmgs_w",
  less_prems_w            = "has_dmgs_less_prems_w",
  less_credits            = "has_dmgs_less_credits",
  less_prems_and_credits  = "has_dmgs_less_prems_and_credits",
  less_credits_w          = "has_dmgs_less_credits_w",
  less_prems_and_credits_w= "has_dmgs_less_prems_and_credits_w"
)

# Helper: compute WT penalty for one scenario
calc_wt <- function(suffix, has_flag_col) {
  
  pen_col <- paste0("wt_penalty_", suffix)
  
  pp_data1[, (pen_col) := fifelse(
    has_wt_period == 1L &
      active == 0L &
      fcoalesce(get(has_flag_col), 0L) == 1L,
    fcoalesce(wt_rate, 0) * fcoalesce(Avg_Shift_Length, 0) * 30,
    0
  )]
}

# Run all scenarios
for (nm in names(wt_scenarios)) {
  calc_wt(nm, wt_scenarios[[nm]])
}

setnames(
  pp_data1,
  old = c("wt_penalty_base"),
  new = c("wt_penalty")
)


# ----- ALL DATA (BY PP):        Grand total damages by pay period (principal + interest + wsv and wt penalties) -----------------------------------
# (NOTE: this does NOT and SHOULD NOT include PAGA)

# Sum of total damages (already includes interest) and penalties (ee_flag only due to repeating on each row)
pp_data1[, `:=`(
  tot_dmgs_w_penalties =
    fcoalesce(tot_dmgs, 0) +
    fifelse(ee_flag == 1L, fcoalesce(wsv_penalty, 0), 0) +
    fifelse(ee_flag == 1L, fcoalesce(wt_penalty, 0), 0),
  
  tot_dmgs_w_penalties_less_prems =
    fcoalesce(tot_dmgs_less_prems, 0) +
    fifelse(ee_flag == 1L, fcoalesce(wsv_penalty_less_prems, 0), 0) +
    fifelse(ee_flag == 1L, fcoalesce(wt_penalty_less_prems, 0), 0),
  
  tot_dmgs_w_penalties_w =
    fcoalesce(tot_dmgs_w, 0) +
    fifelse(ee_flag == 1L, fcoalesce(wsv_penalty_w, 0), 0) +
    fifelse(ee_flag == 1L, fcoalesce(wt_penalty_w, 0), 0),
  
  tot_dmgs_w_penalties_less_prems_w =
    fcoalesce(tot_dmgs_less_prems_w, 0) +
    fifelse(ee_flag == 1L, fcoalesce(wsv_penalty_less_prems_w, 0), 0) +
    fifelse(ee_flag == 1L, fcoalesce(wt_penalty_less_prems_w, 0), 0),
  
  tot_dmgs_w_penalties_less_credits =
    fcoalesce(tot_dmgs_less_credits, 0) +
    fifelse(ee_flag == 1L, fcoalesce(wsv_penalty_less_credits, 0), 0) +
    fifelse(ee_flag == 1L, fcoalesce(wt_penalty_less_credits, 0), 0),
  
  tot_dmgs_w_penalties_less_prems_and_credits =
    fcoalesce(tot_dmgs_less_prems_and_credits, 0) +
    fifelse(ee_flag == 1L, fcoalesce(wsv_penalty_less_prems_and_credits, 0), 0) +
    fifelse(ee_flag == 1L, fcoalesce(wt_penalty_less_prems_and_credits, 0), 0),
  
  tot_dmgs_w_penalties_less_credits_w =
    fcoalesce(tot_dmgs_less_credits_w, 0) +
    fifelse(ee_flag == 1L, fcoalesce(wsv_penalty_less_credits_w, 0), 0) +
    fifelse(ee_flag == 1L, fcoalesce(wt_penalty_less_credits_w, 0), 0),
  
  tot_dmgs_w_penalties_less_prems_and_credits_w =
    fcoalesce(tot_dmgs_less_prems_and_credits_w, 0) +
    fifelse(ee_flag == 1L, fcoalesce(wsv_penalty_less_prems_and_credits_w, 0), 0) +
    fifelse(ee_flag == 1L, fcoalesce(wt_penalty_less_prems_and_credits_w, 0), 0)
)]


# ----- ALL DATA (BY PP):        PAGA analysis -----------------------------------------

# --- Settings ---
# paga_dmgs_start_date must exist (Date)
# pp_data1 must have: ID, Period_End, active, and your damages cols used below

setorder(pp_data1, ID, Period_End)  # or whatever your PP date column is

pp_data1[, paga_ee_flag := 0L]
pp_data1[, in_PAGA_period := fifelse(Period_End > paga_dmgs_start_date, 1, 0)]
pp_data1[in_PAGA_period == 1, paga_ee_flag := as.integer(seq_len(.N) == 1L), by = ID]

initial_pp_penalty        <- 100
subsequent_pp_penalty     <- 100

initial_pp_penalty_226    <- 250
subsequent_pp_penalty_226 <- 250

initial_pp_penalty_558    <- 100
subsequent_pp_penalty_558 <- 100

penalty_1174 <- 500

# --- Helper function (vectorized) ---
calc_paga_penalty <- function(flag_count, initial, subsequent) {
  fcase(
    flag_count == 0, 0,
    flag_count == 1, initial,
    flag_count > 1,  initial + (flag_count - 1) * subsequent,
    default = 0
  )
}

# --- PAGA SCENARIOS (8 output suffixes) ---
# Credits scenarios require: paga damages AND has_dmgs_less_credits{suf} > 0
# Unreimb exp always requires: unreimb_exp_dmgs > 0 AND has_col > 0

paga_scenarios <- list(
  
  # 1) Base
  base = list(
    suf = "",
    has = "has_dmgs",
    mp  = "mp_dmgs",
    rp  = "rp_dmgs",
    is_credits = FALSE
  ),
  
  # 2) Waivers
  w = list(
    suf = "_w",
    has = "has_dmgs_w",
    mp  = "mp_dmgs_w",
    rp  = "rp_dmgs",
    is_credits = FALSE
  ),
  
  # 3) Less premiums
  less_prems = list(
    suf = "_less_prems",
    has = "has_dmgs_less_prems",
    mp  = "mp_dmgs_less_prems",
    rp  = "rp_dmgs_less_prems",
    is_credits = FALSE
  ),
  
  # 4) Less premiums + waivers
  less_prems_w = list(
    suf = "_less_prems_w",
    has = "has_dmgs_less_prems_w",
    mp  = "mp_dmgs_less_prems_w",
    rp  = "rp_dmgs_less_prems",
    is_credits = FALSE
  ),
  
  # 5) Less credits
  less_credits = list(
    suf = "_less_credits",
    has = "has_dmgs_less_credits",
    mp  = "mp_dmgs",
    rp  = "rp_dmgs",
    is_credits = TRUE
  ),
  
  # 6) Less premiums + credits
  less_prems_and_credits = list(
    suf = "_less_prems_and_credits",
    has = "has_dmgs_less_prems_and_credits",
    mp  = "mp_dmgs_less_prems",
    rp  = "rp_dmgs_less_prems",
    is_credits = TRUE
  ),
  
  # 7) Less credits + waivers
  less_credits_w = list(
    suf = "_less_credits_w",
    has = "has_dmgs_less_credits_w",
    mp  = "mp_dmgs_w",
    rp  = "rp_dmgs",
    is_credits = TRUE
  ),
  
  # 8) Less premiums + credits + waivers
  less_prems_and_credits_w = list(
    suf = "_less_prems_and_credits_w",
    has = "has_dmgs_less_prems_and_credits_w",
    mp  = "mp_dmgs_less_prems_w",
    rp  = "rp_dmgs_less_prems",
    is_credits = TRUE
  )
)


# ---- PAGA BUILDER FUNCTION 

build_paga_for_scenario <- function(sc) {
  
  suf        <- sc$suf
  has_col    <- sc$has
  mp_col     <- sc$mp
  rp_col     <- sc$rp
  is_credits <- sc$is_credits
  
  # ---------------- MEAL PERIODS ----------------
  flag_meal <- paste0("PAGA_meal_flag", suf)
  cnt_meal  <- paste0("PAGA_meal_flag_count", suf)
  dmg_meal  <- paste0("PAGA_meal_penalties", suf)
  
  pp_data1[, (flag_meal) :=
             as.integer(
               in_PAGA_period == 1 &
                 fcoalesce(get(mp_col), 0) > 0 &
                 if (is_credits) fcoalesce(get(has_col), 0L) == 1 else TRUE
             )]
  
  pp_data1[, (cnt_meal) :=
             fifelse(in_PAGA_period == 1,
                     sum(get(flag_meal), na.rm = TRUE),
                     0L),
           by = ID]
  
  pp_data1[, (dmg_meal) :=
             fifelse(paga_ee_flag == 1L,
                     calc_paga_penalty(get(cnt_meal),
                                       initial_pp_penalty,
                                       subsequent_pp_penalty),
                     0)]
  
  # ---------------- REST PERIODS ----------------
  flag_rest <- paste0("PAGA_rest_flag", suf)
  cnt_rest  <- paste0("PAGA_rest_flag_count", suf)
  dmg_rest  <- paste0("PAGA_rest_penalties", suf)
  
  pp_data1[, (flag_rest) :=
             as.integer(
               in_PAGA_period == 1 &
                 fcoalesce(get(rp_col), 0) > 0 &
                 if (is_credits) fcoalesce(get(has_col), 0L) == 1 else TRUE
             )]
  
  pp_data1[, (cnt_rest) :=
             fifelse(in_PAGA_period == 1,
                     sum(get(flag_rest), na.rm = TRUE),
                     0L),
           by = ID]
  
  pp_data1[, (dmg_rest) :=
             fifelse(paga_ee_flag == 1L,
                     calc_paga_penalty(get(cnt_rest),
                                       initial_pp_penalty,
                                       subsequent_pp_penalty),
                     0)]
  
  # ---------------- RROP ----------------
  flag_rrop <- paste0("PAGA_rrop_flag", suf)
  cnt_rrop  <- paste0("PAGA_rrop_flag_count", suf)
  dmg_rrop  <- paste0("PAGA_rrop_penalties", suf)
  
  pp_data1[, (flag_rrop) :=
             as.integer(
               in_PAGA_period == 1 &
                 fcoalesce(Net_rrop_dmgs, 0) > 0 &
                 if (is_credits) fcoalesce(get(has_col), 0L) == 1 else TRUE
             )]
  
  pp_data1[, (cnt_rrop) :=
             fifelse(in_PAGA_period == 1,
                     sum(get(flag_rrop), na.rm = TRUE),
                     0L),
           by = ID]
  
  pp_data1[, (dmg_rrop) :=
             fifelse(paga_ee_flag == 1L,
                     calc_paga_penalty(get(cnt_rrop),
                                       initial_pp_penalty,
                                       subsequent_pp_penalty),
                     0)]
  
  # ---------------- 226 WAGE STATEMENT ----------------
  flag_226 <- paste0("PAGA_226_flag", suf)
  cnt_226  <- paste0("PAGA_226_flag_count", suf)
  dmg_226  <- paste0("PAGA_226_penalties", suf)
  
  pp_data1[, (flag_226) :=
             as.integer(in_PAGA_period == 1 &
                          fcoalesce(get(has_col), 0L) == 1)]
  
  pp_data1[, (cnt_226) :=
             fifelse(in_PAGA_period == 1,
                     sum(get(flag_226), na.rm = TRUE),
                     0L),
           by = ID]
  
  pp_data1[, (dmg_226) :=
             fifelse(paga_ee_flag == 1L,
                     calc_paga_penalty(get(cnt_226),
                                       initial_pp_penalty_226,
                                       subsequent_pp_penalty_226),
                     0)]
  
  # ---------------- 558 UNPAID WAGES ----------------
  flag_558 <- paste0("PAGA_558_flag", suf)
  cnt_558  <- paste0("PAGA_558_flag_count", suf)
  dmg_558  <- paste0("PAGA_558_penalties", suf)
  
  pp_data1[, (flag_558) :=
             as.integer(in_PAGA_period == 1 &
                          fcoalesce(get(has_col), 0L) == 1)]
  
  pp_data1[, (cnt_558) :=
             fifelse(in_PAGA_period == 1,
                     sum(get(flag_558), na.rm = TRUE),
                     0L),
           by = ID]
  
  pp_data1[, (dmg_558) :=
             fifelse(paga_ee_flag == 1L,
                     calc_paga_penalty(get(cnt_558),
                                       initial_pp_penalty_558,
                                       subsequent_pp_penalty_558),
                     0)]
  
  # ---------------- 1197.1 MIN WAGE ----------------
  flag_1197 <- paste0("PAGA_1197_1_flag", suf)
  cnt_1197  <- paste0("PAGA_1197_1_flag_count", suf)
  dmg_1197  <- paste0("PAGA_1197_1_penalties", suf)
  
  pp_data1[, (flag_1197) :=
             as.integer(
               in_PAGA_period == 1 &
                 fcoalesce(min_wage_dmgs, 0) > 0 &
                 get(flag_558) == 0
             )]
  
  pp_data1[, (cnt_1197) :=
             fifelse(in_PAGA_period == 1,
                     sum(get(flag_1197), na.rm = TRUE),
                     0L),
           by = ID]
  
  pp_data1[, (dmg_1197) :=
             fifelse(paga_ee_flag == 1L,
                     calc_paga_penalty(get(cnt_1197),
                                       initial_pp_penalty,
                                       subsequent_pp_penalty),
                     0)]
  
  # ---------------- 1174 RECORDKEEPING (EMPLOYEE LEVEL) ----------------
  flag_1174 <- paste0("PAGA_1174_flag", suf)
  dmg_1174  <- paste0("PAGA_1174_penalties", suf)
  
  pp_data1[, (flag_1174) :=
             as.integer(any(in_PAGA_period == 1 &
                              fcoalesce(get(has_col), 0L) == 1)),
           by = ID]
  
  pp_data1[, (dmg_1174) :=
             fifelse(paga_ee_flag == 1L,
                     get(flag_1174) * penalty_1174,
                     0)]
  
  # ---------------- 2802 UNREIMBURSED EXPENSES ----------------
  flag_2802 <- paste0("PAGA_2802_flag", suf)
  cnt_2802  <- paste0("PAGA_2802_flag_count", suf)
  dmg_2802  <- paste0("PAGA_2802_penalties", suf)
  
  pp_data1[, (flag_2802) :=
             as.integer(
               in_PAGA_period == 1 &
                 fcoalesce(unreimb_exp_dmgs, 0) > 0 &
                 fcoalesce(get(has_col), 0L) == 1
             )]
  
  pp_data1[, (cnt_2802) :=
             fifelse(in_PAGA_period == 1,
                     sum(get(flag_2802), na.rm = TRUE),
                     0L),
           by = ID]
  
  pp_data1[, (dmg_2802) :=
             fifelse(paga_ee_flag == 1L,
                     calc_paga_penalty(get(cnt_2802),
                                       initial_pp_penalty,
                                       subsequent_pp_penalty),
                     0)]
  
  # ---------------- 203 WAITING TIME (EMPLOYEE LEVEL) ----------------
  flag_203 <- paste0("PAGA_203_flag", suf)
  dmg_203  <- paste0("PAGA_203_penalties", suf)
  
  pp_data1[, (flag_203) :=
             as.integer(any(in_PAGA_period == 1 &
                              fcoalesce(get(has_col), 0L) == 1) &
                          active == 0),
           by = ID]
  
  pp_data1[, (dmg_203) :=
             fifelse(paga_ee_flag == 1L,
                     get(flag_203) * initial_pp_penalty,
                     0)]
  
  # ---------------- HAS ANY PAGA (PAY PERIOD LEVEL) ----------------
  
  paga_flag_cols <- c(
    "PAGA_meal_flag",
    "PAGA_rest_flag",
    "PAGA_rrop_flag",
    "PAGA_226_flag",
    "PAGA_558_flag",
    "PAGA_1197_1_flag",
    "PAGA_2802_flag"
  )
  
  pp_data1[, has_paga_penalties :=
             as.integer(
               in_PAGA_period == 1L &
                 rowSums(.SD, na.rm = TRUE) > 0
             ),
           .SDcols = paga_flag_cols
  ]
  
  # ---------------- PAGA TOTAL ----------------
  paga_tot <- paste0("PAGA_tot", suf)
  
  pp_data1[, (paga_tot) :=
             fifelse(
               paga_ee_flag == 1L,
               fcoalesce(get(dmg_meal), 0) +
                 fcoalesce(get(dmg_rest), 0) +
                 fcoalesce(get(dmg_rrop), 0) +
                 fcoalesce(get(dmg_226), 0) +
                 fcoalesce(get(dmg_558), 0) +
                 fcoalesce(get(dmg_1197), 0) +
                 fcoalesce(get(dmg_1174), 0) +
                 fcoalesce(get(dmg_2802), 0) +
                 fcoalesce(get(dmg_203), 0),
               0
             )]
}


# --- BUILD FOR ALL 8 PAGA SCENARIOS ---
for (nm in names(paga_scenarios)) {
  build_paga_for_scenario(paga_scenarios[[nm]])
}


# ----- ALL DATA (BY PP):        Damages Flags (principal -> class penalties -> PAGA) -------------------------

setDT(pp_data1)

# Helpers (row-level + ee-level)
add_has_flag <- function(dt, src_col, out_col) {
  if (!src_col %in% names(dt)) return(invisible(NULL))
  dt[, (out_col) := as.integer(fcoalesce(as.numeric(get(src_col)), 0) > 0)]
  invisible(NULL)
}

add_ee_has_flag <- function(dt, src_col, out_col) {
  if (!src_col %in% names(dt)) return(invisible(NULL))
  dt[, (out_col) := as.integer(any(fcoalesce(as.numeric(get(src_col)), 0) > 0)), by = ID]
  invisible(NULL)
}

# 1) PRINCIPAL FLAGS (trigger ONLY on principal scenario totals / has_dmgs*)
#    - Row-level:    has_dmgs* already exist (built off tot_principal_dmgs* above)
#    - EE-level:     ee_has_dmgs* derived from row-level has_dmgs* flags
principal_map <- list(
  base                     = "has_dmgs",
  less_prems               = "has_dmgs_less_prems",
  w                        = "has_dmgs_w",
  less_prems_w             = "has_dmgs_less_prems_w",
  less_credits             = "has_dmgs_less_credits",
  less_prems_and_credits   = "has_dmgs_less_prems_and_credits",
  less_credits_w           = "has_dmgs_less_credits_w",
  less_prems_and_credits_w = "has_dmgs_less_prems_and_credits_w"
)

principal_map <- principal_map[vapply(principal_map, \(x) x %in% names(pp_data1), logical(1))]

for (nm in names(principal_map)) {
  src <- principal_map[[nm]]
  suf <- if (nm == "base") "" else paste0("_", nm)
  add_ee_has_flag(pp_data1, src, paste0("ee_has_dmgs", suf))
}

# 2) CLASS PENALTIES FLAGS (trigger on penalty $ columns)
#    - WSV + WT only (PAGA is separate and comes next)

# --- WSV (wage statement penalties) ---
wsv_pen_map <- list(
  base                     = "wsv_penalty",
  less_prems               = "wsv_penalty_less_prems",
  w                        = "wsv_penalty_w",
  less_prems_w             = "wsv_penalty_less_prems_w",
  less_credits             = "wsv_penalty_less_credits",
  less_prems_and_credits   = "wsv_penalty_less_prems_and_credits",
  less_credits_w           = "wsv_penalty_less_credits_w",
  less_prems_and_credits_w = "wsv_penalty_less_prems_and_credits_w"
)

wsv_pen_map <- wsv_pen_map[vapply(wsv_pen_map, \(x) x %in% names(pp_data1), logical(1))]

for (nm in names(wsv_pen_map)) {
  pen_col <- wsv_pen_map[[nm]]
  suf     <- if (nm == "base") "" else paste0("_", nm)
  
  add_has_flag(pp_data1,    pen_col, paste0("has_wsv_penalties", suf))
  add_ee_has_flag(pp_data1, pen_col, paste0("ee_has_wsv_penalties", suf))
}

# --- WT (waiting time penalties) ---
wt_pen_map <- list(
  base                     = "wt_penalty",
  less_prems               = "wt_penalty_less_prems",
  w                        = "wt_penalty_w",
  less_prems_w             = "wt_penalty_less_prems_w",
  less_credits             = "wt_penalty_less_credits",
  less_prems_and_credits   = "wt_penalty_less_prems_and_credits",
  less_credits_w           = "wt_penalty_less_credits_w",
  less_prems_and_credits_w = "wt_penalty_less_prems_and_credits_w"
)

wt_pen_map <- wt_pen_map[vapply(wt_pen_map, \(x) x %in% names(pp_data1), logical(1))]

for (nm in names(wt_pen_map)) {
  pen_col <- wt_pen_map[[nm]]
  suf     <- if (nm == "base") "" else paste0("_", nm)
  
  add_has_flag(pp_data1,    pen_col, paste0("has_wt_penalties", suf))
  add_ee_has_flag(pp_data1, pen_col, paste0("ee_has_wt_penalties", suf))
}

# PAGA FLAGS (separate)
#    - Employee-level flags for overall + each bucket, per scenario suffix

paga_penalty_types <- c(
  meal     = "PAGA_meal_penalties",
  rest     = "PAGA_rest_penalties",
  rrop     = "PAGA_rrop_penalties",
  `226`    = "PAGA_226_penalties",
  `558`    = "PAGA_558_penalties",
  `1197_1` = "PAGA_1197_1_penalties",
  `1174`   = "PAGA_1174_penalties",
  `2802`   = "PAGA_2802_penalties",
  `203`    = "PAGA_203_penalties"
)

for (nm in names(paga_scenarios)) {
  suf <- paga_scenarios[[nm]]$suf
  
  # overall ee_has_paga_penalties{suf} (prefers has_PAGA_penalties{suf})
  has_paga_col <- paste0("has_PAGA_penalties", suf)
  ee_has_paga  <- paste0("ee_has_paga_penalties", suf)
  
  if (has_paga_col %in% names(pp_data1)) {
    pp_data1[, (ee_has_paga) := as.integer(any(fcoalesce(as.numeric(get(has_paga_col)), 0) > 0)), by = ID]
  } else {
    cols <- paste0(unname(paga_penalty_types), suf)
    cols <- cols[cols %in% names(pp_data1)]
    if (length(cols)) {
      pp_data1[, (ee_has_paga) :=
                 as.integer(any(Reduce(`+`, lapply(.SD, \(x) fcoalesce(as.numeric(x), 0))) > 0)),
               by = ID, .SDcols = cols]
    }
  }
  
  # per-bucket ee_has_paga_<bucket>{suf}
  for (typ in names(paga_penalty_types)) {
    pen_col <- paste0(paga_penalty_types[[typ]], suf)
    out_col <- paste0("ee_has_paga_", typ, suf)
    add_ee_has_flag(pp_data1, pen_col, out_col)
  }
}


# ----- ALL DATA (BY EE):        Employee-level Summary Table (no further calculations)  -----------------------------------------

names(pp_data1)

setDT(pp_data1)

# FIRST fields
first_fields_default <- c(
  "Name", "Pay_Name", "Source", "Pay_Source", "Sheet", "Key_Gps", "active",
  "Sample", "Class_Job", "Hire_Date", "Term_Date", "Subclass(es)",
  "Location_Name", "Executive_Area", "Area", "Region"
)

# SUM fields
sum_fields_default <- c(
  
  # --- Shift counts ---
  "shift", "mp",
  "mp_lt_twenty", "mp_lt_thirty", "mp_thirty", "mp_gt_thirty",
  "mp_gt_two_hrs", "mp_gt_four_hrs",
  "split_shift", "five_hour_guarantee",
  
  # --- Meal violations ---
  "MissMP1", "LateMP1", "ShortMP1", "MissMP2", "LateMP2", "ShortMP2",
  "mp1_violation", "mp2_violation",
  "MissMP1_w", "LateMP1_w", "ShortMP1_w", "MissMP2_w", "LateMP2_w", "ShortMP2_w",
  "mp1_violation_w", "mp2_violation_w",
  
  # --- Meal/rest violations per pp ---
  "mpv_per_pp", "mpv_per_pp_w", "rpv_per_pp",
  "mpv_shift_less_prems", "mpv_shift_less_prems_w", "rpv_shift_less_prems",
  "mpv_per_pp_less_prems", "mpv_per_pp_less_prems_w", "rpv_per_pp_less_prems",
  
  # --- Meal damages (pp-level) ---
  "mp_dmgs", "mp_dmgs_w", "mp_dmgs_less_prems", "mp_dmgs_less_prems_w",

  # --- Rest damages (pp-level) ---
  "rp_dmgs", "rp_dmgs_less_prems",

  # --- RROP ---
  "rrop_by_code_underpayment", "Wage_Diff", "OT_Diff", "DT_Diff", "Meal_Diff", "Rest_Diff", "Sick_Diff",
  "OT_Overpayment", "DT_Overpayment", "Meal_Overpayment", "Rest_Overpayment", "Sick_Overpayment",
  "OT_rrop_dmgs", "DT_rrop_dmgs", "Meal_rrop_dmgs", "Rest_rrop_dmgs", "Sick_rrop_dmgs",
  "Gross_Overpayment", "Gross_rrop_dmgs", "Net_Overpayment", "Net_rrop_dmgs",
  "rrop_any_underpayment", "rrop_net_underpayment",

  # --- Other damages ---
  "otc_dmgs", 
  "unreimb_exp_dmgs",
  "clock_rounding_dmgs",
  "unpaid_ot_dmgs",
  "min_wage_dmgs",
  
  # --- Total principal damages / credits scenarios (pp-level dollars) ---
  "tot_principal_dmgs",
  "tot_principal_dmgs_less_prems",
  "tot_principal_dmgs_w",
  "tot_principal_dmgs_less_prems_w",
  
  "Credit_Amount",
  "tot_principal_dmgs_less_credits",
  "tot_principal_dmgs_less_prems_and_credits",
  "tot_principal_dmgs_less_credits_w",
  "tot_principal_dmgs_less_prems_and_credits_w",
  
  # --- Damage flags (pp-level, typically 0/1 per PP row) ---
  "has_dmgs",
  "has_dmgs_less_prems",
  "has_dmgs_w",
  "has_dmgs_less_prems_w",
  "has_dmgs_less_credits",
  "has_dmgs_less_prems_and_credits",
  "has_dmgs_less_credits_w",
  "has_dmgs_less_prems_and_credits_w",
  
  # --- WSV flags (pp-level) ---
  "has_wsv_penalties",
  "has_wsv_penalties_less_prems",
  "has_wsv_penalties_w",
  "has_wsv_penalties_less_prems_w",
  "has_wsv_penalties_less_credits",
  "has_wsv_penalties_less_prems_and_credits",
  "has_wsv_penalties_less_credits_w",
  "has_wsv_penalties_less_prems_and_credits_w",
  
  # --- WT flags (pp-level) ---
  "has_wt_penalties",
  "has_wt_penalties_less_prems",
  "has_wt_penalties_w",
  "has_wt_penalties_less_prems_w",
  "has_wt_penalties_less_credits",
  "has_wt_penalties_less_prems_and_credits",
  "has_wt_penalties_less_credits_w",
  "has_wt_penalties_less_prems_and_credits_w",
  
  # --- PAGA pp-level flags (per PP row; these get summed to count flagged PP rows) ---
  # Base
  "PAGA_meal_flag",
  "PAGA_rest_flag",
  "PAGA_rrop_flag",
  "PAGA_226_flag",
  "PAGA_558_flag",
  "PAGA_1197_1_flag",
  "PAGA_2802_flag",
  
  # Waivers
  "PAGA_meal_flag_w",
  "PAGA_226_flag_w",
  "PAGA_558_flag_w",
  
  # Less premiums
  "PAGA_meal_flag_less_prems",
  "PAGA_rest_flag_less_prems",
  "PAGA_226_flag_less_prems",
  "PAGA_558_flag_less_prems",
  
  # Less premiums + waivers
  "PAGA_meal_flag_less_prems_w",
  "PAGA_226_flag_less_prems_w",
  "PAGA_558_flag_less_prems_w",
  
  # Less credits
  "PAGA_meal_flag_less_credits",
  "PAGA_rest_flag_less_credits",
  "PAGA_rrop_flag_less_credits",
  "PAGA_226_flag_less_credits",
  "PAGA_558_flag_less_credits",
  "PAGA_1197_1_flag_less_credits",
  "PAGA_2802_flag_less_credits",
  
  # Less premiums + credits
  "PAGA_meal_flag_less_prems_and_credits",
  "PAGA_rest_flag_less_prems_and_credits",
  "PAGA_rrop_flag_less_prems_and_credits",
  "PAGA_226_flag_less_prems_and_credits",
  "PAGA_558_flag_less_prems_and_credits",
  "PAGA_1197_1_flag_less_prems_and_credits",
  "PAGA_2802_flag_less_prems_and_credits",
  
  # Less credits + waivers
  "PAGA_meal_flag_less_credits_w",
  "PAGA_rest_flag_less_credits_w",
  "PAGA_rrop_flag_less_credits_w",
  "PAGA_226_flag_less_credits_w",
  "PAGA_558_flag_less_credits_w",
  "PAGA_1197_1_flag_less_credits_w",
  "PAGA_2802_flag_less_credits_w",
  
  # Less premiums + credits + waivers
  "PAGA_meal_flag_less_prems_and_credits_w",
  "PAGA_rest_flag_less_prems_and_credits_w",
  "PAGA_rrop_flag_less_prems_and_credits_w",
  "PAGA_226_flag_less_prems_and_credits_w",
  "PAGA_558_flag_less_prems_and_credits_w",
  "PAGA_1197_1_flag_less_prems_and_credits_w",
  "PAGA_2802_flag_less_prems_and_credits_w",
  
  # --- Scenario total interest (PP-level) ---
  "interest",
  "interest_less_prems",
  "interest_w",
  "interest_less_prems_w",
  "interest_less_credits",
  "interest_less_prems_and_credits",
  "interest_less_credits_w",
  "interest_less_prems_and_credits_w",
  
  # --- Total damages = principal + interest (PP-level) ---
  "tot_dmgs",
  "tot_dmgs_less_prems",
  "tot_dmgs_w",
  "tot_dmgs_less_prems_w",
  "tot_dmgs_less_credits",
  "tot_dmgs_less_prems_and_credits",
  "tot_dmgs_less_credits_w",
  "tot_dmgs_less_prems_and_credits_w",
  
  # --- Total damages INCLUDING class penalties (WSV + WT, no PAGA) ---
  "tot_dmgs_w_penalties",
  "tot_dmgs_w_penalties_less_prems",
  "tot_dmgs_w_penalties_w",
  "tot_dmgs_w_penalties_less_prems_w",
  "tot_dmgs_w_penalties_less_credits",
  "tot_dmgs_w_penalties_less_prems_and_credits",
  "tot_dmgs_w_penalties_less_credits_w",
  "tot_dmgs_w_penalties_less_prems_and_credits_w"
)

# MIN fields
min_fields_default <- c("Pay_Date", "Period_Beg", "Period_End")

# MAX fields 
max_fields_default <- c(
  "Pay_Date", "Period_Beg", "Period_End",
  "Shifts_per_workweek", "Avg_Shift_Length", "Median_Shift_Length",
  
  # --- Shift percentages ---
  "perc_shifts_gt_3_5", "perc_shifts_gt_5", "perc_shifts_gt_5_lte_6", "perc_shifts_gt_6",
  "perc_shifts_e_8", "perc_shifts_gt_8",
  "perc_shifts_gt_10", "perc_shifts_gt_10_lte_12", "perc_shifts_gt_12", "perc_shifts_gt_14",
  "perc_mp", "perc_mp_lt_thirty", "perc_mp_thirty", "perc_mp_gt_thirty",
  
  # --- Violation rates ---
  "mpv_rate", "mpv_rate_w", "mpv_rate_less_prems", "mpv_rate_less_prems_w",
  "mpv_ee_rate_less_prems", "mpv_ee_rate_less_prems_w",
  "rpv_rate", "rpv_rate_less_prems", "rpv_ee_rate_less_prems",
  
  # --- Employee-level violations ---
  "mpv_ee_less_prems", "mpv_ee_less_prems_w", "rpv_ee_less_prems",
  
  # --- Employee-level damages ---
  "mp_ee_dmgs_less_prems", "mp_ee_dmgs_less_prems_w",
  "rp_ee_dmgs_less_prems", 
  
  # --- Final rates ---
  "final_Base_Rate", "final_RROP",
  
  # --- WSV penalties (EE-level constants) ---
  "wsv_flag_count",
  "wsv_penalty",
  "wsv_flag_count_w",
  "wsv_penalty_w",
  
  # --- WT penalties (EE-level constants) ---
  "has_wt_period",
  "wt_penalty",
  "wt_penalty_w",
  
  # --- PAGA: EE-level flag counts ---
  
  "in_PAGA_period",
  
  # Base
  "PAGA_meal_flag_count",
  "PAGA_rest_flag_count",
  "PAGA_rrop_flag_count",
  "PAGA_226_flag_count",
  "PAGA_558_flag_count",
  "PAGA_1197_1_flag_count",
  "PAGA_2802_flag_count",
  
  # Waivers
  "PAGA_meal_flag_count_w",
  "PAGA_226_flag_count_w",
  "PAGA_558_flag_count_w",
  
  # Less premiums
  "PAGA_meal_flag_count_less_prems",
  "PAGA_rest_flag_count_less_prems",
  "PAGA_rrop_flag_count_less_prems",
  "PAGA_226_flag_count_less_prems",
  "PAGA_558_flag_count_less_prems",
  "PAGA_1197_1_flag_count_less_prems",
  "PAGA_2802_flag_count_less_prems",
  
  # Less premiums + waivers
  "PAGA_meal_flag_count_less_prems_w",
  "PAGA_226_flag_count_less_prems_w",
  "PAGA_558_flag_count_less_prems_w",
  
  # Less credits
  "PAGA_meal_flag_count_less_credits",
  "PAGA_rest_flag_count_less_credits",
  "PAGA_rrop_flag_count_less_credits",
  "PAGA_226_flag_count_less_credits",
  "PAGA_558_flag_count_less_credits",
  "PAGA_1197_1_flag_count_less_credits",
  "PAGA_2802_flag_count_less_credits",
  
  # Less premiums + credits
  "PAGA_meal_flag_count_less_prems_and_credits",
  "PAGA_rest_flag_count_less_prems_and_credits",
  "PAGA_rrop_flag_count_less_prems_and_credits",
  "PAGA_226_flag_count_less_prems_and_credits",
  "PAGA_558_flag_count_less_prems_and_credits",
  "PAGA_1197_1_flag_count_less_prems_and_credits",
  "PAGA_2802_flag_count_less_prems_and_credits",
  
  # Less credits + waivers
  "PAGA_meal_flag_count_less_credits_w",
  "PAGA_rest_flag_count_less_credits_w",
  "PAGA_rrop_flag_count_less_credits_w",
  "PAGA_226_flag_count_less_credits_w",
  "PAGA_558_flag_count_less_credits_w",
  "PAGA_1197_1_flag_count_less_credits_w",
  "PAGA_2802_flag_count_less_credits_w",
  
  # Less premiums + credits + waivers
  "PAGA_meal_flag_count_less_prems_and_credits_w",
  "PAGA_rest_flag_count_less_prems_and_credits_w",
  "PAGA_rrop_flag_count_less_prems_and_credits_w",
  "PAGA_226_flag_count_less_prems_and_credits_w",
  "PAGA_558_flag_count_less_prems_and_credits_w",
  "PAGA_1197_1_flag_count_less_prems_and_credits_w",
  "PAGA_2802_flag_count_less_prems_and_credits_w",
  
  # --- PAGA: EE-level penalties (THIS IS THE IMPORTANT "TOTAL PENALTIES IN MAX") ---
  # Base
  "PAGA_meal_penalties",
  "PAGA_rest_penalties",
  "PAGA_rrop_penalties",
  "PAGA_226_penalties",
  "PAGA_558_penalties",
  "PAGA_1197_1_penalties",
  "PAGA_1174_penalties",
  "PAGA_2802_penalties",
  "PAGA_203_penalties",
  
  # Waivers
  "PAGA_meal_penalties_w",
  "PAGA_226_penalties_w",
  "PAGA_558_penalties_w",
  
  # Less premiums
  "PAGA_meal_penalties_less_prems",
  "PAGA_rest_penalties_less_prems",
  "PAGA_rrop_penalties_less_prems",
  "PAGA_226_penalties_less_prems",
  "PAGA_558_penalties_less_prems",
  "PAGA_1197_1_penalties_less_prems",
  "PAGA_1174_penalties_less_prems",
  "PAGA_2802_penalties_less_prems",
  "PAGA_203_penalties_less_prems",
  
  # Less premiums + waivers
  "PAGA_meal_penalties_less_prems_w",
  "PAGA_226_penalties_less_prems_w",
  "PAGA_558_penalties_less_prems_w",
  
  # Less credits
  "PAGA_meal_penalties_less_credits",
  "PAGA_rest_penalties_less_credits",
  "PAGA_rrop_penalties_less_credits",
  "PAGA_226_penalties_less_credits",
  "PAGA_558_penalties_less_credits",
  "PAGA_1197_1_penalties_less_credits",
  "PAGA_1174_penalties_less_credits",
  "PAGA_2802_penalties_less_credits",
  "PAGA_203_penalties_less_credits",
  
  # Less premiums + credits
  "PAGA_meal_penalties_less_prems_and_credits",
  "PAGA_rest_penalties_less_prems_and_credits",
  "PAGA_rrop_penalties_less_prems_and_credits",
  "PAGA_226_penalties_less_prems_and_credits",
  "PAGA_558_penalties_less_prems_and_credits",
  "PAGA_1197_1_penalties_less_prems_and_credits",
  "PAGA_1174_penalties_less_prems_and_credits",
  "PAGA_2802_penalties_less_prems_and_credits",
  "PAGA_203_penalties_less_prems_and_credits",
  
  # Less credits + waivers
  "PAGA_meal_penalties_less_credits_w",
  "PAGA_rest_penalties_less_credits_w",
  "PAGA_rrop_penalties_less_credits_w",
  "PAGA_226_penalties_less_credits_w",
  "PAGA_558_penalties_less_credits_w",
  "PAGA_1197_1_penalties_less_credits_w",
  "PAGA_1174_penalties_less_credits_w",
  "PAGA_2802_penalties_less_credits_w",
  "PAGA_203_penalties_less_credits_w",
  
  # Less premiums + credits + waivers
  "PAGA_meal_penalties_less_prems_and_credits_w",
  "PAGA_rest_penalties_less_prems_and_credits_w",
  "PAGA_rrop_penalties_less_prems_and_credits_w",
  "PAGA_226_penalties_less_prems_and_credits_w",
  "PAGA_558_penalties_less_prems_and_credits_w",
  "PAGA_1197_1_penalties_less_prems_and_credits_w",
  "PAGA_1174_penalties_less_prems_and_credits_w",
  "PAGA_2802_penalties_less_prems_and_credits_w",
  "PAGA_203_penalties_less_prems_and_credits_w",
  
  
  # --- PAGA totals ---
  "PAGA_tot", 
  "PAGA_tot_w",                                     
  "PAGA_tot_less_prems",                            
  "PAGA_tot_less_prems_w",                          
  "PAGA_tot_less_credits",                          
  "PAGA_tot_less_prems_and_credits",                
  "PAGA_tot_less_credits_w",                        
  "PAGA_tot_less_prems_and_credits_w",  
  
  # --- PAGA: scenario-level overall indicator columns (EE-level, repeating) ---
  "has_PAGA_penalties",
  "has_PAGA_penalties_w",
  "has_PAGA_penalties_less_prems",
  "has_PAGA_penalties_less_prems_w",
  "has_PAGA_penalties_less_credits",
  "has_PAGA_penalties_less_prems_and_credits",
  "has_PAGA_penalties_less_credits_w",
  "has_PAGA_penalties_less_prems_and_credits_w"
)

# MEAN / MEDIAN
mean_fields_default <- c("Base_Rate", "RROP", "CA_min_wage", "double_CA_min_wage")
median_fields_default <- c("Base_Rate", "RROP", "CA_min_wage", "double_CA_min_wage")

# Aggregate
ee_data1 <- aggregate_data(pp_data1, by = "ID")

ee_data1 <- remove_suffixes(
  ee_data1,
  suffixes = c("_sum", "_max")
)


# ----- ALL DATA:                Extrapolation  -----------------------------------------

sample_size # Reminder to set the sample_size_val

# Sample size (1 = 100%, 0.5 = 50%, etc.)
sample_size_val <- 1
class_extrap_factor <- 1 / sample_size_val
message(sprintf("Class extrap factor = %.2f", class_extrap_factor))

# Set extrapolated employee number from class list or data
# Default is total unique class members from class1
extrap_class_ees <- uniqueN(class1$Class_ID, na.rm = TRUE)

# Alternatively, could be:
#extrap_class_ees <- uniqueN(pp_data1$ID, na.rm = TRUE) * class_extrap_factor

# Calculated actual employees in data, by period
# Based on EVERYONE in the combined time and pay data analysis
class_data_ees        <- uniqueN(pp_data1$ID, na.rm = TRUE) 
class_data_former_ees <- uniqueN(pp_data1$ID[pp_data1$active == 0], na.rm = TRUE)
wsv_data_ees          <- uniqueN(pp_data1$ID[pp_data1$Period_End > wsv_start_date], na.rm = TRUE)
wt_data_ees           <- uniqueN(pp_data1$ID[pp_data1$Period_End > wt_start_date], na.rm = TRUE) 
paga_data_ees         <- uniqueN(pp_data1$ID[pp_data1$Period_End > paga_dmgs_start_date], na.rm = TRUE) 

# Calculated extrapolation factor based on employees missing from data that are on class list
# Essentially treats subset like a sample extrapolation
missing_ee_extrap_factor       <- fifelse(class_extrap_factor == 1, extrap_class_ees / class_data_ees, 1) 
missing_wsv_ee_extrap_factor   <- fifelse(class_extrap_factor == 1, wsv_data_ees / class_data_ees, 1) 
missing_wt_ee_extrap_factor    <- fifelse(class_extrap_factor == 1, wt_data_ees / class_data_ees, 1) 
missing_paga_ee_extrap_factor  <- fifelse(class_extrap_factor == 1, paga_data_ees / class_data_ees, 1) 

# Estimate sub-group employee counts 
# Based on ratio within each sub-period compared to whole class population in the data then overlaid to extrapolated population.
extrap_wsv_ees  <- round(missing_wsv_ee_extrap_factor * extrap_class_ees, 0)
extrap_wt_ees   <- round(missing_wt_ee_extrap_factor * extrap_class_ees, 0)
extrap_paga_ees <- round(missing_paga_ee_extrap_factor * extrap_class_ees, 0)

# Set class_extrap_factor, etc to missing_ee_extrap_factor if it is a 100% sample AND there are more employees on class1 than pp_data1
class_extrap_factor <- fifelse(missing_ee_extrap_factor > 1, missing_ee_extrap_factor, class_extrap_factor)
wsv_extrap_factor   <- fifelse(missing_wsv_ee_extrap_factor > 1, missing_wsv_ee_extrap_factor, class_extrap_factor)
wt_extrap_factor    <- fifelse(missing_wt_ee_extrap_factor > 1, missing_wt_ee_extrap_factor, class_extrap_factor)
paga_extrap_factor  <- fifelse(missing_paga_ee_extrap_factor > 1, missing_paga_ee_extrap_factor, class_extrap_factor)

# Get end dates from time and pay data
max_time_end <- max(shift_data1$Period_End)
max_pay_end  <- max(pay1$Pay_Period_End)
message(sprintf("Time data ends = %s | Pay data ends = %s", 
                format(max_time_end, "%m/%d/%Y"), 
                format(max_pay_end, "%m/%d/%Y")))

# Warn if time vs pay end dates differ by >30 days
date_diff <- as.numeric(difftime(max_pay_end, max_time_end, units = "days"))
if (abs(date_diff) > 30) {
  direction <- if (date_diff < 0) "before" else "after"
  message(sprintf("WARNING: Pay end is %d days %s time end", abs(date_diff), direction))
}

# Time extrapolation: data coverage / full class period
data_coverage      <- as.numeric(difftime(max_time_end, class_dmgs_start_date, units = "days"))
wsv_data_coverage  <- as.numeric(difftime(max_time_end, wsv_start_date, units = "days"))
wt_data_coverage   <- as.numeric(difftime(max_time_end, wt_start_date, units = "days"))
paga_data_coverage <- as.numeric(difftime(max_time_end, paga_dmgs_start_date, units = "days"))

class_period <- as.numeric(difftime(mediation_date, class_dmgs_start_date, units = "days"))
wsv_period   <- as.numeric(difftime(mediation_date, wsv_start_date, units = "days"))
wt_period    <- as.numeric(difftime(mediation_date, wt_start_date, units = "days"))
paga_period  <- as.numeric(difftime(mediation_date, paga_dmgs_start_date, units = "days"))

time_extrap_factor      <- data_coverage / class_period
wsv_time_extrap_factor  <- wsv_data_coverage / wsv_period
wt_time_extrap_factor   <- wt_data_coverage / wt_period
paga_time_extrap_factor <- paga_data_coverage / paga_period

message(sprintf("Class Period extrap factor = %.4f (%.1f%%)", 
                time_extrap_factor, time_extrap_factor * 100))
message(sprintf("WSV Period extrap factor = %.4f (%.1f%%)", 
                wsv_time_extrap_factor, wsv_time_extrap_factor * 100))
message(sprintf("WT Period extrap factor = %.4f (%.1f%%)", 
                wt_time_extrap_factor, wt_time_extrap_factor * 100))
message(sprintf("PAGA Period extrap factor = %.4f (%.1f%%)", 
                paga_time_extrap_factor, paga_time_extrap_factor * 100))

# Extrapolated counts
extrap_class_pps      <- round(((uniqueN(pp_data1$ID_Period_End, na.rm = TRUE) / class_data_ees) * extrap_class_ees) / time_extrap_factor, 0)
extrap_class_wks      <- round((sum(shift_data1$week, na.rm = TRUE) / sum(shift_data1$pp, na.rm = TRUE)) * extrap_class_pps, 0)
extrap_class_shifts   <- round(mean(shift_data1$Shifts_per_workweek, na.rm = TRUE) * extrap_class_wks, 0)

extrap_wsv_pps        <- round(((uniqueN(pp_data1$ID_Period_End[pp_data1$Period_End > wsv_start_date], na.rm = TRUE) / wsv_data_ees) * extrap_wsv_ees) / wsv_time_extrap_factor, 0)

extrap_wt_former_ees  <- round(((class_data_former_ees / class_data_ees) * extrap_wt_ees) * wt_time_extrap_factor, 0)

extrap_paga_pps       <- round(((uniqueN(pp_data1$ID_Period_End[pp_data1$Period_End > paga_dmgs_start_date], na.rm = TRUE) / paga_data_ees) * extrap_paga_ees) / paga_time_extrap_factor, 0)

message(sprintf(" | Extrap EEs = %s | Extrap PPs = %s | Extrap Wks = %s | Extrap Shifts = %s | 
 | Extrap WSV EEs = %s | Extrap WSV PPs = %s | 
 | Extrap WT EEs = %s | Extrap WT Former EEs = %s |
 | Extrap PAGA EEs = %s | Extrap PAGA PPs = %s |",
                format(extrap_class_ees, big.mark = ","),
                format(extrap_class_pps, big.mark = ","),
                format(extrap_class_wks, big.mark = ","),
                format(extrap_class_shifts, big.mark = ","),
                format(extrap_wsv_ees, big.mark = ","),
                format(extrap_wsv_pps, big.mark = ","),
                format(extrap_wt_ees, big.mark = ","),
                format(extrap_wt_former_ees, big.mark = ","),
                format(extrap_paga_ees, big.mark = ","),
                format(extrap_paga_pps, big.mark = ",")))




# ----- ALL DATA:                Write CSVs & Metadata Files  -----------------------------------------

setDT(pp_data1)

# Generate metadata csv files used to map column types from R to PowerQuery Editor (see Functions.R)
generate_metadata(ee_data1, "ee_metadata.csv")
generate_metadata(pp_data1, "pp_metadata.csv")
generate_metadata(shift_data1, "time_shift_metadata.csv")
generate_metadata(time1, "time_punch_metadata.csv")
generate_metadata(pay1, "pay_metadata.csv")

OUT_DIR <- resolve_out_dir()

# Toggle per table: TRUE = write Key_Gps filtered, FALSE = write full
write_key_gps_time  <- TRUE
write_key_gps_shift <- FALSE
write_key_gps_pay   <- FALSE

# Helper: build *_p subset (returns NULL if Key_Gps missing)
make_keygps_p <- function(dt, key_col = "Key_Gps", everyone_else = "Everyone Else") {
  if (!is.data.table(dt)) dt <- as.data.table(dt)
  if (!key_col %in% names(dt)) return(NULL)
  dt[!is.na(get(key_col)) & get(key_col) != everyone_else]
}

# Always create *_p objects (so they're available regardless of writing choice)
time1_p       <- make_keygps_p(time1)
pay1_p        <- make_keygps_p(pay1)
shift_data1_p <- make_keygps_p(shift_data1)

# Always write these full outputs
write_csv_and_rds(ee_data1, file.path(OUT_DIR, "Employee Level Data.csv"))
write_csv_and_rds(pp_data1, file.path(OUT_DIR, "Pay Period Level Data.csv"))

# Time Punch Data
if (isTRUE(write_key_gps_time) && !is.null(time1_p) && nrow(time1_p) > 0) {
  write_csv_and_rds(time1_p, file.path(OUT_DIR, "Time Punch Data.csv"))
  message("Time Punch Data: Key_Gps filtered")
} else if (isTRUE(write_key_gps_time)) {
  message("Time Punch Data: Key_Gps filter requested but missing/empty; writing full")
  write_csv_and_rds(time1, file.path(OUT_DIR, "Time Punch Data.csv"))
} else {
  write_csv_and_rds(time1, file.path(OUT_DIR, "Time Punch Data.csv"))
}

# Time Shift Data
if (isTRUE(write_key_gps_shift) && !is.null(shift_data1_p) && nrow(shift_data1_p) > 0) {
  write_csv_and_rds(shift_data1_p, file.path(OUT_DIR, "Time Shift Data.csv"))
  message("Time Shift Data: Key_Gps filtered")
} else if (isTRUE(write_key_gps_shift)) {
  message("Time Shift Data: Key_Gps filter requested but missing/empty; writing full")
  write_csv_and_rds(shift_data1, file.path(OUT_DIR, "Time Shift Data.csv"))
} else {
  write_csv_and_rds(shift_data1, file.path(OUT_DIR, "Time Shift Data.csv"))
}

# Pay Data
if (isTRUE(write_key_gps_pay) && !is.null(pay1_p) && nrow(pay1_p) > 0) {
  write_csv_and_rds(pay1_p, file.path(OUT_DIR, "Pay Data.csv"))
  message("Pay Data: Key_Gps filtered")
} else if (isTRUE(write_key_gps_pay)) {
  message("Pay Data: Key_Gps filter requested but missing/empty; writing full")
  write_csv_and_rds(pay1, file.path(OUT_DIR, "Pay Data.csv"))
} else {
  write_csv_and_rds(pay1, file.path(OUT_DIR, "Pay Data.csv"))
}

# ----- ALL DATA:                Final Analysis Table-------------------------------------------------------------

metrics_spec_path <- file.path(CASE_DIR, "scripts", "metrics_spec.csv")
if (!file.exists(metrics_spec_path)) stop("Missing metrics_spec.csv at: ", metrics_spec_path)

metrics_spec <- fread(metrics_spec_path)
setDT(metrics_spec)

metrics_spec[, metric_type := fcase(
  grepl("date", metric_label, ignore.case = TRUE), "date",
  grepl("percent", metric_label, ignore.case = TRUE), "percent",
  default = "value"
)]

# Rows that should NOT be broken out by year
metrics_spec_no_year <- metrics_spec[no_year_breakdown == FALSE]

# Rows that CAN be broken out by year
metrics_spec_year_ok <- metrics_spec[no_year_breakdown == TRUE]

# Build Key_Gps filters dynamically (exclude Everyone Else + NA)
tg <- unique(shift_data1$Key_Gps)
pg <- unique(pay1$Pay_Key_Gps)
unique_groups <- union(tg, pg)
unique_groups <- unique_groups[!is.na(unique_groups)]
unique_groups <- unique_groups[!grepl("Everyone Else", unique_groups, ignore.case = TRUE)]

custom_filters <- setNames(lapply(unique_groups, function(g) {
  list(
    time_filter = bquote(Key_Gps == .(g)),
    pay_filter  = bquote(Pay_Key_Gps == .(g)),
    pp_filter   = bquote(Key_Gps == .(g))
    )
}), unique_groups)

extrap_env <- list(
  class_ees = extrap_class_ees,
  class_pps = extrap_class_pps,
  wsv_ees   = extrap_wsv_ees,
  wsv_pps   = extrap_wsv_pps,
  class_dmgs_start_date = class_dmgs_start_date
)

raw_results <- run_metrics_pipeline(
  time_dt = shift_data1,
  pay_dt  = pay1,
  pp_dt   = pp_data1,
  ee_dt   = ee_data1,
  spec    = metrics_spec,
  custom_filters = custom_filters,
  extrap_env = extrap_env,
  globals_env = .GlobalEnv
)

final_table <- format_metrics_table(raw_results)
export_metrics(final_table, base_name = "Analysis")


# ----- END  -----------------------------------------
end.time <- Sys.time()
end.time - start.time    
