# Create Sample Data for Testing
# Run this script to create sample test data files

library(data.table)
library(lubridate)
library(openxlsx)
library(here)

cat("Creating sample test data files...\n")

# Sample Time Data
sample_time <- data.table(
  ID = rep(c("001", "002", "003"), each = 20),
  Date = rep(seq(as.Date("2024-01-01"), as.Date("2024-01-20"), by = "day"), 3),
  In = as.POSIXct(rep(seq(as.Date("2024-01-01"), as.Date("2024-01-20"), by = "day"), 3)) + hours(8),
  Out = as.POSIXct(rep(seq(as.Date("2024-01-01"), as.Date("2024-01-20"), by = "day"), 3)) + hours(17),
  Hours = 8,
  Code = "REG"
)

# Sample Pay Data
sample_pay <- data.table(
  Pay_ID = rep(c("001", "002", "003"), each = 10),
  Pay_Date = rep(seq(as.Date("2024-01-15"), as.Date("2024-03-15"), by = "14 days"), 3),
  Pay_Period_End = rep(seq(as.Date("2024-01-14"), as.Date("2024-03-14"), by = "14 days"), 3),
  Pay_Period_Beg = rep(seq(as.Date("2024-01-01"), as.Date("2024-03-01"), by = "14 days"), 3),
  Pay_Code = "REG",
  Pay_Hours = 80,
  Pay_Rate = 20.00,
  Pay_Amount = 1600.00
)

# Sample Employee List
sample_employees <- data.table(
  Class_ID = c("001", "002", "003"),
  Class_Name = c("Employee One", "Employee Two", "Employee Three"),
  Hire_Date = as.Date(c("2023-01-01", "2023-06-01", "2023-09-01")),
  Term_Date = as.Date(NA)
)

# Save to Excel files
write.xlsx(sample_time, here("data/raw/SAMPLE_time_data.xlsx"))
write.xlsx(sample_pay, here("data/raw/SAMPLE_pay_data.xlsx"))
write.xlsx(sample_employees, here("data/raw/SAMPLE_employee_list.xlsx"))

cat("\n✅ Sample data files created in data/raw/:\n")
cat("  - SAMPLE_time_data.xlsx\n")
cat("  - SAMPLE_pay_data.xlsx\n")
cat("  - SAMPLE_employee_list.xlsx\n\n")

cat("To use these sample files, update your clean_data.R:\n")
cat('  Line 11:  time1 <- read_excel(here("data/raw/SAMPLE_time_data.xlsx"))\n')
cat('  Line 191: pay1 <- read_excel(here("data/raw/SAMPLE_pay_data.xlsx"))\n')
cat('  Line 433: class1 <- read_excel(here("data/raw/SAMPLE_employee_list.xlsx"))\n')
