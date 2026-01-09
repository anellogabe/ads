# parameters.R - Case Parameters Configuration
# =============================================================================

# Case Information
case_name <- "Wage and Hour Analysis"
case_number <- "ABC-12345"

# Key Dates
complaint_date <- as.Date("2025-09-15")
mediation_date <- as.Date("2026-09-15")  # Or use Sys.Date() for current date

# Damage Periods
class_dmgs_start_date <- complaint_date %m-% years(4)
class_dmgs_end_date   <- mediation_date
paga_dmgs_start_date  <- (complaint_date %m-% years(1)) - days(65)
paga_dmgs_end_date    <- mediation_date
wT_start_date         <- complaint_date %m-% years(3)
wT_end_date           <- mediation_date
wsv_start_date        <- complaint_date %m-% years(1)
wsv_end_date          <- mediation_date

# Pay Calendar
mode_days_btwn_pay_period_ends <- 14  # Bi-weekly = 14, Weekly = 7, Semi-monthly = 15

# Workweek
workweek_value <- 7  # 1=Monday, 7=Saturday (for week start)

# Interest Rates
annual_interest_rate <- 0.07  # 7% prejudgment interest rate
monthly_interest_rate <- annual_interest_rate / 12
interest_thru_date <- mediation_date

# Regular Rate Multipliers
var_OT_multiplier <- 1.5
var_DT_multiplier <- 2
var_half_time_OT_multiplier <- 0.5
var_half_time1_multiplier <- 1
rrop_buffer <- 0.05

# Class Filtering
use_class_filter <- TRUE  # Set FALSE to skip class list filtering

# Pay Date Configuration
pay_date_day_of_week <- 5  # 1=Mon, 2=Tue, 3=Wed, 4=Thu, 5=Fri, 6=Sat, 7=Sun

# Key Employees (Named Plaintiffs)
key_employees <- c(
  "09012012" = "Gato, Chief"
  # Add more as needed:
  # "12345" = "Smith, John",
  # "67890" = "Doe, Jane"
)

# Shift Parameters
new_shift_cutoff <- 4  # Hours between punches to consider separate shifts
rp_max_hrs <- 0 / 60   # Maximum rest period hours (0 = no rest breaks in time data)

# Meal Period Thresholds
mp_5_hour_threshold <- 5.01   # For first meal period
mp_10_hour_threshold <- 10.01 # For second meal period

# With Waivers
mp_6_hour_threshold <- 6.01   # First MP with waiver
mp_12_hour_threshold <- 12.01 # Second MP with waiver

# Time Rounding
rounding_hrs_cutoff <- 0.25  # Threshold for valid rounding differences

# Rate Type Classification
rate_type_threshold <- 0.2  # +/- buffer around target multipliers
thresh_max <- 5             # Max rate for "Premium Only"

cat("\n")
cat("═══════════════════════════════════════\n")
cat("Configuration Loaded:\n")
cat("═══════════════════════════════════════\n")
cat("Case:", case_name, "\n")
cat("Analysis Period:", format(class_dmgs_start_date), "to", format(class_dmgs_end_date), "\n")
cat("Pay Period:", mode_days_btwn_pay_period_ends, "days\n")
cat("Interest Rate:", annual_interest_rate * 100, "%\n")
cat("═══════════════════════════════════════\n\n")
