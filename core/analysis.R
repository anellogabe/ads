# ----- ALL DATA:                Load Packages & Data --------------------------
library(data.table)
library(lubridate)
library(here)
library(readr)
library(purrr)
library(tidyr)
library(dplyr)

# Run accompanying functions script
source(here("scripts", "Functions.R"))
source(here("scripts", "clean_data.R")) # Only run if you need to re-run cleaning script

# # *************** RUN IF "clean_data.R" is not already in work space *************** 
# # Frequency table of days between pay period ends, excluding 0 and negative values
# days_btwn_pay_period_ends_freq <- pay1[days_btwn_pay_period_ends > 0, 
#                                        .N, by = days_btwn_pay_period_ends][order(-N)]
# days_btwn_pay_period_ends_freq[, percent_of_total := round((N / sum(N)) * 100, 2)]
# 
# mode_days_btwn_pay_period_ends <- days_btwn_pay_period_ends_freq[1, days_btwn_pay_period_ends]
# # *************** RUN IF "clean_data.R" is not already in work space *************** 

start.time <- Sys.time()

time1 <- readRDS("data/processed/time_processed.rds")
pay1 <- readRDS("data/processed/pay_processed.rds")
class1 <- readRDS("data/processed/class_processed.rds")

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


# ----- PAY DATA:                CA Min Wage table -------------------------

setDT(pay1)

# Create CA minimum wage table
CA_min_wage <- data.table(
  beg_date = as.Date(paste0(2015:2025, "-01-01")),
  end_date = as.Date(paste0(2015:2025, "-12-31")),
  CA_min_wage = c(9.00, 10.00, 10.50, 11.00, 12.00, 13.00, 14.00, 15.00, 15.50, 16.00, 16.50)
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

# Define pay code groups (use exact values or patterns - see "mode = " below)
# NOTE: Adapt these to match your actual pay codes
reg_pay_codes     <- c("reg", "train", "called in", "retro")
ot_pay_codes      <- c("overtime")
dt_pay_codes      <- c("dt", "double")
bon_pay_codes     <- c("bon", "comm", "inc", "spiff", "awd", "award")
meal_pay_codes    <- c("meal")
rest_pay_codes    <- c("rest")
diff_pay_codes    <- c("diff", "2nd", "3rd", "night", "weekend", "evening")
diff_ot_pay_codes <- c("SEE BELOW")
diff_dt_pay_codes <- c("SEE BELOW")
sick_pay_codes    <- c("sick")

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

# Pay code categories function
pay_code_categories <- categorize_pay_codes(pay1, flag_cols, output_csv = here("output", "Pay Code Categories.csv"))

# Optional: Generate pay code summary
pay_code_summary_tbl <- pay_code_summary(pay1, output_path = here("output", "Pay_Code_Summary.csv"))

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

# Calculate hourly rate
pay1[, Calc_Rate := fifelse(
  Pay_Hours > 0 & Pay_Amount != 0,
  round(Pay_Amount / Pay_Hours, 4),
  0
)]

# Identify base rates from regular pay
pay1[, Base_Rate_Row := fifelse(
  Reg_Pay_Code == 1 & Diff_Pay_Code == 0 & Calc_Rate > 0 & is.finite(Calc_Rate), 
  round(Calc_Rate, 2), 
  NA_real_
)]

# Count unique valid rates per period
pay1[, Unique_Valid_Rates := uniqueN(Base_Rate_Row[!is.na(Base_Rate_Row)]), by = Pay_ID_Period_End]

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
pay1[, is_off_cycle := as.integer(sum(Reg_Pay_Code) == 0 & sum(Bon_Pay_Code) == 1), by = .(Pay_ID, Pay_Date)]
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
  Employees = uniqueN(Pay_ID),
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

# Re-run pay code categories function
pay_code_categories <- categorize_pay_codes(pay1, flag_cols, output_csv = here("output", "Pay Code Categories.csv"))

# Re-run (optional) generate pay code summary
pay_code_summary_tbl <- pay_code_summary(pay1, output_path = here("output", "Pay_Code_Summary.csv"))

# Calculate straight time amounts (needed for group_pay_data function)
pay1[, Straight_Time_Amt := fifelse(
  Reg_Pay_Code == 1 & Hrs_Wkd_Pay_Code == 1 | (Diff_Pay_Code == 1 & Diff_OT_Pay_Code != 1 & Diff_DT_Pay_Code != 1), Pay_Amount,
  fifelse(
    OT_Pay_Code == 1 & Hrs_Wkd_Pay_Code == 1 | Diff_OT_Pay_Code == 1, Pay_Amount / 1.5,
    fifelse(DT_Pay_Code == 1 & Hrs_Wkd_Pay_Code == 1 | Diff_DT_Pay_Code == 1, Pay_Amount / 2, 0))
)]


# Join pay code category and move to last column
rate_type_summary <- pay_code_categories[rate_type_summary, on = "Pay_Code"]
setcolorder(rate_type_summary, c(setdiff(names(rate_type_summary), "Pay_Code_Category"), "Pay_Code_Category"))

print(rate_type_summary)

# Save
fwrite(rate_type_summary, here("output", "Rate_Type_Analysis.csv"))
cat("\n✓ Rate_Type assigned to pay1 and summary analysis saved\n")


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
  NA_real_
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
  Net_Underpayment = pmax(0, round(Calc_Tot_Wages - Actual_Wages, 2)),
  OT_Underpayment = pmax(0, round((RROP * pp_OT_Hrs * var_half_time_OT_multiplier) - pp_OT_Amt, 2)),
  DT_Underpayment = pmax(0, round((RROP * pp_DT_Hrs) - pp_DT_Amt, 2)),
  Meal_Underpayment = pmax(0, round((RROP * pp_Meal_Prem_Hrs) - pp_Meal_Amt, 2)),
  Rest_Underpayment = pmax(0, round((RROP * pp_Rest_Prem_Hrs) - pp_Rest_Amt, 2)),
  Sick_Underpayment = pmax(0, round((RROP * pp_Sick_Hrs) - pp_Sick_Amt, 2))
)]

# Calculate wage diff and underpayments
pay1[, `:=`(
  Gross_Overpayment = (OT_Overpayment + DT_Overpayment + Meal_Overpayment + Rest_Overpayment + Sick_Overpayment),
  Gross_Underpayment = (OT_Underpayment + DT_Underpayment + Meal_Underpayment + Rest_Underpayment + Sick_Underpayment)
)]

# Apply buffer to totals
pay1[, Net_Overpayment := fifelse(Net_Overpayment < rrop_buffer, 0, Net_Overpayment)]
pay1[, Net_Underpayment := fifelse(Net_Underpayment < rrop_buffer, 0, Net_Underpayment)]
pay1[, Gross_Overpayment := fifelse(Gross_Overpayment < rrop_buffer, 0, Gross_Overpayment)]
pay1[, Gross_Underpayment := fifelse(Gross_Underpayment < rrop_buffer, 0, Gross_Underpayment)]

# If total is zero, zero out all sub-groups
pay1[Net_Underpayment == 0, `:=`(
  OT_Underpayment = 0,
  DT_Underpayment = 0,
  Meal_Underpayment = 0,
  Rest_Underpayment = 0,
  Sick_Underpayment = 0
)]

pay1[Net_Overpayment == 0, `:=`(
  OT_Overpayment = 0,
  DT_Overpayment = 0,
  Meal_Overpayment = 0,
  Rest_Overpayment = 0,
  Sick_Overpayment = 0
)]

# Create PP RROP underpayment indicator
pay1[, rrop_any_underpayment := as.integer(Gross_Underpayment > 0)]
pay1[, rrop_net_underpayment := as.integer(Net_Underpayment > 0)]


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

# Sort by ID and Date
setorder(time1, ID, Date)
setDT(time1)

# wk_codes <- c("reg", "wkd", "ovt", "dbl", "admin", "edu")
# 
# wk_pattern <- paste(wk_codes, collapse = "|")

# Flag weeks where ANY record in that week is NOT one of the "work" codes (i.e., time off / other)
time1[, wk_time_off := NA]
      #   any(!is.na(PAYCODE) &
      #         !grepl(wk_pattern, PAYCODE, ignore.case = TRUE)),
      # by = ID_Week_End]
# 
# # Filter out time off records, but keep work code rows OR NA codes
# time1 <- time1[is.na(PAYCODE) | grepl(wk_pattern, PAYCODE, ignore.case = TRUE)]


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

new_shift_cutoff <- 4  #  *****  Hours between punches to consider separate shifts (ADJUST AS NEEDED)  ******** -----

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

cat("✓ Created", uniqueN(time1$Shift_ID), "unique Shift IDs\n")
cat("  Avg records per shift:", round(nrow(time1) / uniqueN(time1$Shift_ID), 1), "\n")

time1[, duplicate_flag := duplicated(.SD), .SDcols = c("Shift_ID", "In", "Out")]

# Initialize overlap_flag (will be updated if NEEDS_MIDNIGHT_CHECK)
time1[, overlap_flag := FALSE]

# Determine if we need midnight crossing logic
# HYBRID FIX:
# Do NOT gate this on "has_real_dates" or "In_Date_Match" because a single real-dated row can exist.
# Instead, run cross-row midnight logic only if issues remain after the rowwise fix above.
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
  
  midnight_shifts <- time1[(midnight_row), uniqueN(Shift_ID)]
  
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
cat("Shifts >1 day span:", time1[shift_days > 1, uniqueN(Shift_ID)], "\n")

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
time1[, mp_lt_thirty := fifelse(mp == 1 & mp_hrs > 0 & mp_hrs < 0.5, 1L, 0L)]
time1[, mp_thirty     := fifelse(mp == 1 & mp_hrs == 0.5, 1L, 0L)]
time1[, mp_forty_five    := fifelse(mp == 1 & mp_hrs == 0.75, 1L, 0L)]
time1[, mp_gt_thirty := fifelse(mp == 1 & mp_hrs > 0.5, 1L, 0L)]

# Identify hours for meal period 1 and 2, per shift
time1[, mp1_hrs := fifelse(mp_ct == 1, mp_hrs, 0)]
time1[, mp2_hrs := fifelse(mp_ct == 2, mp_hrs, 0)]

# Shift hrs and non work hrs tables (intended to show shift stats before filtering out zero or 20+ hr shifts or other bad data)
shift_hrs_tbl(time1)
non_wrk_hrs_tbl(time1) 
meal_period_tbl(time1)
meal_start_time_tbl(time1, top_n = 20) # Top N bins then "rest"
meal_quarter_hour_tbl(time1)

# # Auto meal deductions (IF NEEDED)
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
# time1[, auto_mp_lt_thirty := fifelse(auto_mp == 1 & auto_mp_hrs > 0 & auto_mp_hrs < 0.5, 1L, 0L)]
# time1[, auto_mp_thirty     := fifelse(auto_mp == 1 & auto_mp_hrs == 0.5, 1L, 0L)]
# time1[, auto_mp_gt_thirty := fifelse(auto_mp == 1 & auto_mp_hrs > 0.5, 1L, 0L)]

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
# time1[, r_mp_lt_thirty := fifelse(mp == 1 & r_mp_hrs > 0 & r_mp_hrs < 0.5, 1L, 0L)]
# time1[, r_mp_thirty     := fifelse(mp == 1 & r_mp_hrs > 0 & r_mp_hrs == 0.5, 1L, 0L)]
# time1[, r_mp_gt_thirty  := fifelse(mp == 1 & r_mp_hrs > 0 & r_mp_hrs > 0.5, 1L, 0L)]
# time1[, r_mp_forty_five    := fifelse(mp == 1 & r_mp_hrs == 0.75, 1L, 0L)]
# 
# # Meal period 1 and 2 durations
# time1[, `:=`(
#   r_mp1_hrs = fifelse(mp_ct == 1, r_mp_hrs, 0),
#   r_mp2_hrs = fifelse(mp_ct == 2, r_mp_hrs, 0)
# ), by = ID_Shift]


# ----- TIME DATA:               Weekly summary and Alternative Workweek Analysis --------------

# Weekly summary without weeks with time off for clean AWW review
weekly_summary <- time1[wk_time_off != 1,
                        .(
                          ID = first(ID),
                          shifts_worked   = uniqueN(ID_Shift),
                          days_worked     = uniqueN(ID_Date),
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

saveRDS(employee_aww_summary, "output/employee_aww_classifications.rds")

fwrite(employee_aww_summary, here("output", "employee_aww_classifications.csv"))

# Create detailed report for employees on AWW
aww_employees <- employee_aww_summary[AWW_schedule != "Standard", ID]

aww_detail <- weekly_summary[ID %in% aww_employees]
aww_detail <- merge(
  aww_detail,
  employee_aww_summary[, .(ID, AWW_schedule)],
  by = "ID"
)

fwrite(aww_detail, here("output", "aww_detail.csv"))
cat("\n✓ AWW analysis complete. Files saved to output/\n")


# ----- SHIFT (TIME) DATA:       Shift-level aggregation -----------------------------------------

names(time1)

# Define fields globally
first_fields_default <- c("Source", "Sheet", "Page", "Bates", "Key_Gps", "ID", "Name", "ID_Date", "Date", "ID_Period_End", 
                          "Week_End", "ID_Week_End", "wk_time_off", "Period_Beg", "Period_End")

#NOTE: Hours field is default "Sum" field but it could be a "Max" field depending on your time data format.
sum_fields_default <- c("mp", "mp_lt_thirty", "mp_thirty", "mp_gt_thirty", "mp_forty_five", "Hours"
                        
                        # Rest period punches in data? Add:
                        #, "rp_lt_ten", "rp_ten", "rp_gt_ten"
                        
                        # Auto-deducted meal periods in data? Add:
                        #, "auto_mp_lt_thirty", "auto_mp_thirty", "auto_mp_gt_thirty"
                        
                        # Rounded and actual punches analysis? Add:
                        #, "r_diff", "r_mp_lt_thirty", "r_mp_thirty", "r_mp_gt_thirty", "r_mp_forty_five", "r_Hours"
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
nrow(shift_data1) #_____
shift_data1 <- shift_data1[shift_hrs > 0 & shift_hrs <= 20]
nrow(shift_data1) #_____

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
shift_data1[, rpv_shift := fifelse(shift_hrs > 3.5, 1L, 0L)]

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
  wk_Hours = sum(Hours, na.rm = TRUE)
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
  pp_Hours = sum(Hours, na.rm = TRUE)
), by = .(ID, Period_End)]


# ----- SHIFT (TIME) DATA:       Calculated overtime analysis  -----------------------------------------

shift_data1[, `:=`(
  calc_daily_ot = fifelse(shift_hrs > 12, 4,
                          fifelse(shift_hrs > 8, shift_hrs - 8, 0)),
  calc_daily_dt = fifelse(shift_hrs > 12, shift_hrs - 12, 0)
)]

shift_data1[, flsa_ot := shift_hrs - 40]
shift_data1[flsa_ot < 0, flsa_ot := 0]

shift_data1[, ot_wk := fifelse(flsa_ot > 0 | calc_daily_ot > 0 | calc_daily_dt > 0, 1L, 0L)]

shift_data1[, `:=`(
  pp_daily_ot = sum(calc_daily_ot, na.rm = TRUE),
  pp_daily_dt = sum(calc_daily_dt, na.rm = TRUE),
  pp_flsa_ot = sum(flsa_ot[week == 1], na.rm = TRUE),
  pp_ot_wks = sum(ot_wk[week == 1], na.rm = TRUE)
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



# ----- SHIFT (TIME & PAY) DATA: Merge and compare hours worked and OT/DT with pay data  -----------------------------------------

setDT(pay1)
setDT(shift_data1)

# Summarize pay data for hours worked code
pay_data_hrs_wkd <- pay1[Hrs_Wkd_Pay_Code == 1, .(
  Pay_Hrs_Wkd = if (all(is.na(Pay_Hours))) NA_real_ else sum(Pay_Hours, na.rm = TRUE),
  Pay_Amount_Hrs_Wkd = if (all(is.na(Pay_Amount))) NA_real_ else sum(Pay_Amount, na.rm = TRUE),
  Base_Rate_Avg = if (all(is.na(Base_Rate1))) NA_real_ else mean(Base_Rate1, na.rm = TRUE)
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


# ----- SHIFT (TIME & PAY) DATA: Summary aggregations for damages and other aggregations  -----------------------------------------

# Calculate shifts and shifts per workweek (assuming you have a Date or Week field)
shift_data1[, Shifts := 1]
shift_data1[, Shifts_per_workweek := .N / uniqueN(ID_Week_End), by = ID]

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
  Shifts_gt_14                 = fifelse(shift_hrs > 14, 1L, 0L),
  
  meal_prem_hrs                = fifelse(pp == 1, Meal_Prem_Hrs, 0),
  meal_prem_amt                = fifelse(pp == 1, Meal_Prem_Amt, 0),
  mpv_per_pp                   = fifelse(pp == 1, mpv_per_pp, 0),
  mpv_per_pp_w                 = fifelse(pp == 1, mpv_per_pp_w, 0),
  rpv_per_pp                   = fifelse(pp == 1, rpv_per_pp, 0),
  mpv_per_pp_less_prems   = fifelse(pp == 1, mpv_per_pp_less_prems, 0),
  mpv_per_pp_less_prems_w = fifelse(pp == 1, mpv_per_pp_less_prems_w, 0)
), by = ID]


# Calculate employee-level rates
shift_data1[, `:=`(
  perc_shifts_gt_3_5        = round(sum(shift_hrs > 3.5, na.rm = TRUE) / .N, 6),
  perc_shifts_gt_5          = round(sum(shift_hrs > 5, na.rm = TRUE) / .N, 6),
  perc_shifts_gt_5_lte_6    = round(sum(shift_hrs > 5 & shift_hrs <= 6, na.rm = TRUE) / .N, 6),
  perc_shifts_gt_6          = round(sum(shift_hrs > 6, na.rm = TRUE) / .N, 6),
  perc_shifts_e_8           = round(sum(shift_hrs == 8, na.rm = TRUE) / .N, 6),
  perc_shifts_gt_8          = round(sum(shift_hrs > 8, na.rm = TRUE) / .N, 6),
  perc_shifts_gt_10         = round(sum(shift_hrs > 10, na.rm = TRUE) / .N, 6),
  perc_shifts_gt_10_lte_12  = round(sum(shift_hrs > 10 & shift_hrs <= 12, na.rm = TRUE) / .N, 6),
  perc_shifts_gt_12         = round(sum(shift_hrs > 12, na.rm = TRUE) / .N, 6),
  perc_shifts_gt_14         = round(sum(shift_hrs > 14, na.rm = TRUE) / .N, 6),
  
  perc_mp                   = round(sum(mp, na.rm = TRUE) / .N, 6),
  perc_mp_lt_thirty         = round(sum(mp_lt_thirty, na.rm = TRUE) / sum(mp, na.rm = TRUE), 6),
  perc_mp_thirty            = round(sum(mp_thirty, na.rm = TRUE) / sum(mp, na.rm = TRUE), 6),
  perc_mp_gt_thirty         = round(sum(mp_gt_thirty, na.rm = TRUE) / sum(mp, na.rm = TRUE), 6),
  
  mpv_rate                  = round(sum(mpv_shift, na.rm = TRUE) / sum(shift_hrs > 5, na.rm = TRUE), 6),
  mpv_rate_w                = round(sum(mpv_shift_w, na.rm = TRUE) / sum(shift_hrs > 6, na.rm = TRUE), 6),
  mpv_rate_less_prems       = round(sum(mpv_per_pp_less_prems, na.rm = TRUE) / sum(shift_hrs > 5, na.rm = TRUE), 6),
  mpv_rate_less_prems_w     = round(sum(mpv_per_pp_less_prems_w, na.rm = TRUE) / sum(shift_hrs > 6, na.rm = TRUE), 6),
  
  rpv_rate                  = round(sum(rpv_shift, na.rm = TRUE) / sum(shift_hrs > 3.5, na.rm = TRUE), 6)
), by = ID]


# ----- SHIFT (TIME & PAY) DATA: Damages analysis  -----------------------------------------

annual_interest_rate      <- 0.07 # 7% prejudgment interest rate
monthly_interest_rate     <- annual_interest_rate / 12
interest_thru_date        <- mediation_date

# Calculate principal damages (WILL NEED TO ADJUST TO RROP)
shift_data1[, mp_damages := mpv_shift * Base_Rate_Avg]
shift_data1[, mp_damages_w := mpv_shift_w * Base_Rate_Avg]
shift_data1[, mp_damages_less_prems := mpv_per_pp_less_prems * Base_Rate_Avg]
shift_data1[, mp_damages_less_prems_w := mpv_per_pp_less_prems_w * Base_Rate_Avg]

shift_data1[, rp_damages := rpv_shift * Base_Rate_Avg]
shift_data1[, rp_damages_less_prems := rpv_per_pp_less_prems * Base_Rate_Avg]

# Calculate interest
shift_data1[, mp_interest := mp_damages * floor(interval(Date, interest_thru_date) / months(1)) * monthly_interest_rate]
shift_data1[, mp_interest_w := mp_damages_w * floor(interval(Date, interest_thru_date) / months(1)) * monthly_interest_rate]
shift_data1[, mp_interest_less_prems := mp_damages_less_prems * floor(interval(Date, interest_thru_date) / months(1)) * monthly_interest_rate]
shift_data1[, mp_interest_less_prems_w := mp_damages_less_prems_w * floor(interval(Date, interest_thru_date) / months(1)) * monthly_interest_rate]

shift_data1[, rp_interest := rp_damages * floor(interval(Date, interest_thru_date) / months(1)) * monthly_interest_rate]
shift_data1[, rp_interest_less_prems := rp_damages_less_prems * floor(interval(Date, interest_thru_date) / months(1)) * monthly_interest_rate]

# Totals (principal + interest)
shift_data1[, mp_tot_dmgs := mp_damages + mp_interest]
shift_data1[, mp_tot_dmgs_w := mp_damages_w + mp_interest_w]
shift_data1[, mp_tot_dmgs_less_prems := mp_damages_less_prems + mp_interest_less_prems]
shift_data1[, mp_tot_dmgs_less_prems_w := mp_damages_less_prems_w + mp_interest_less_prems_w]

shift_data1[, rp_tot_dmgs := rp_damages + rp_interest]
shift_data1[, rp_tot_dmgs_less_prems := rp_damages_less_prems + rp_interest_less_prems]


# ----- SHIFT (TIME & PAY) DATA: Employee-level analysis  -----------------------------------------

names(shift_data1)

setDT(shift_data1)

# Define fields globally
first_fields_default <- c("Source", "Key_Gps", "Name", "Shifts_per_workweek", "Avg_Shift_Length", "Median_Shift_Length", "perc_shifts_gt_3_5", "perc_shifts_gt_5", "perc_shifts_gt_5_lte_6", "perc_shifts_gt_6",
                          "perc_shifts_e_8", "perc_shifts_gt_8", "perc_shifts_gt_10", "perc_shifts_gt_10_lte_12", "perc_shifts_gt_12", "perc_shifts_gt_14",
                          "perc_mp", "perc_mp_lt_thirty", "perc_mp_thirty", "perc_mp_gt_thirty",
                          "mpv_rate", "mpv_rate_w", "mpv_rate_less_prems", "mpv_rate_less_prems_w",
                          "rpv_rate")
sum_fields_default <- c("Shifts", "Shifts_gt_3_5", "Shifts_gt_5", "Shifts_gt_5_lte_6",
                        "Shifts_gt_6", "Shifts_e_8", "Shifts_gt_8", "Shifts_gt_10", "Shifts_gt_10_lte_12", "Shifts_gt_12", "Shifts_gt_14",
                        "mp", "mp_lt_thirty", "mp_thirty", "mp_gt_thirty", "MissMP1", "LateMP1", "ShortMP1", "MissMP2", "LateMP2", "ShortMP2", "mpv_shift",
                        "MissMP1_w", "LateMP1_w", "ShortMP1_w", "MissMP2_w", "LateMP2_w", "ShortMP2_w", "mpv_shift_w", 
                        "rpv_shift", 
                        "week", "ot_wk",
                        "pp", "Meal_Prem_Hrs", "Meal_Prem_Amt", "Rest_Prem_Hrs", "Rest_Prem_Amt", "mpv_per_pp_less_prems", "mpv_per_pp_less_prems_w",
                        "mp_damages", "mp_damages_w", "mp_damages_less_prems", "mp_damages_less_prems_w",
                        "mp_interest", "mp_interest_w", "mp_interest_less_prems", "mp_interest_less_prems_w",
                        "mp_tot_dmgs", "mp_tot_dmgs_w", "mp_tot_dmgs_less_prems", "mp_tot_dmgs_less_prems_w",
                        "rp_damages", "rp_interest", "rp_tot_dmgs")
min_fields_default <- c("Date", "Period_Beg", "Period_End", "year")
max_fields_default <- c("Date", "Period_Beg", "Period_End", "year")
mean_fields_default <- c("Base_Rate_Avg")
median_fields_default <- c("Base_Rate_Avg")

ee_data1 <- aggregate_data(shift_data1, by = "ID")

# Add "ee_" prefix to all columns
setnames(ee_data1, names(ee_data1), paste0("ee_", names(ee_data1)))


# ----- ALL DATA:                Write CSVs & Metadata Files for PowerQuery  -----------------------------------------

# Generate metadata csv files used to map column types from R to PowerQuery Editor (see Functions.R)
generate_metadata(ee_data1, "time_ee_metadata.csv")
generate_metadata(shift_data1, "time_shift_metadata.csv")
generate_metadata(time1, "time_punch_metadata.csv")
generate_metadata(pay1, "pay_metadata.csv")

write.csv(ee_data1, here("output", "Time Employee Data.csv"), row.names = FALSE)

write.csv(time1, here("output", "Time Punch Data.csv"), row.names = FALSE)
write.csv(shift_data1, here("output", "Time Shift Data.csv"), row.names = FALSE)
write.csv(pay1, here("output", "Pay Data.csv"), row.names = FALSE)

# #### OR RUN THIS FOR CASES WITH A LOT OF DATA AND USE FINAL DATA TABLE BELOW ####
# time1_P <- time1[!is.na(Key_Gps) & Key_Gps != "Everyone Else"] 
# shift_data_P <- shift_data1[!is.na(Key_Gps) & Key_Gps != "Everyone Else"]
# pay1_P <- pay1[!is.na(Pay_Key_Gps) & Pay_Key_Gps != "Everyone Else"]

# write.csv(time1_P, here("output", "Time Punch Data.csv"), row.names = FALSE)
# write.csv(shift_data_P, here("output", "Time Shift Data.csv"), row.names = FALSE)
# write.csv(pay1_P, here("output", "Pay Data.csv"), row.names = FALSE)

# Convert your time data to RDS
# saveRDS(time1, "output/time_data.rds")


# ----- ALL DATA:                Final Analysis Table-------------------------------------------------------------

# Load spec
metric_spec <- fread("scripts/metrics_spec.csv")
metric_spec[, metric_type := fcase(
  grepl("date", metric_label, ignore.case = TRUE), "date",
  grepl("percent", metric_label, ignore.case = TRUE), "percent",
  default = "value"
)]
metric_spec[, metric_order := .I]

# Define custom filters (optional)
custom_filters <- list(
  "Erik Brown" = list(
    time_filter = quote(ID == "21003"),
    pay_filter  = quote(Pay_ID == "21003")
  )
)

# Run pipeline
raw_results <- run_metrics_pipeline(shift_data1, pay1, metric_spec, custom_filters)

# Format and export
final_table <- format_metrics_table(raw_results)
fwrite(final_table, "output/Analysis.csv")
cat("\n✓ Analysis final table saved\n")

# ----- END  -----------------------------------------
end.time <- Sys.time()
end.time - start.time    


