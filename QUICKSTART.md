# Quick Start Guide - Wage & Hour Dashboard

Get your dashboard up and running in minutes!

## ⚡ Quick Setup (5 Steps)

### 1. Copy Your Code
```r
# Copy your full analysis script content to:
scripts/analysis.R

# Copy your full functions content to:
scripts/functions.R

# Copy your clean_data.R content (includes parameters):
scripts/clean_data.R
```

### 2. Place Your Data
```
data/raw/
├── your_time_data.xlsx
└── your_pay_data.xlsx
```

Update file paths in `scripts/clean_data.R`:
```r
time1 <- read_excel("data/raw/your_time_data.xlsx")
pay1 <- read_excel("data/raw/your_pay_data.xlsx")
```

### 3. Run Analysis
```r
source("run_analysis.R")
```

### 4. Launch Dashboard
```r
shiny::runApp("dashboard/app.R")
```

## 🎯 What You Get

The pipeline will automatically:
- ✅ Clean and standardize your data
- ✅ Calculate meal/rest violations
- ✅ Compute overtime and RROP
- ✅ Calculate damages with interest
- ✅ Generate interactive dashboard
- ✅ Create exportable reports

## 📊 Dashboard Tabs

1. **Overview** - Summary metrics and trends
2. **Meal & Rest** - Violation analysis
3. **Overtime** - OT/DT patterns
4. **Regular Rate** - RROP underpayments
5. **Time Rounding** - Rounding analysis
6. **Damages** - Total damages breakdown
7. **Employee Detail** - Drill down by employee
8. **Data Tables** - Raw data access

## 🔍 Key Features

### Filters (Left Sidebar)
- **Date Range**: Filter entire dashboard
- **Key Group**: Focus on specific employees
- **Refresh**: Reload data after re-running analysis

### Value Boxes
All tabs show key metrics at the top with color coding:
- 🔵 Blue = Informational
- 🟢 Green = Good/neutral
- 🟡 Yellow = Caution
- 🔴 Red = Violations/damages

### Interactive Charts
- **Click & drag** to zoom
- **Double-click** to reset
- **Hover** for details
- **Download** icon to export as PNG

### Data Tables
- **Search** any column
- **Sort** by clicking headers
- **Page** through results
- **Export** to CSV (future feature)

## 💡 Pro Tips

### For Best Performance
```r
# If you have >100K rows, consider filtering first:
time1 <- time1[Date >= "2023-01-01"]
pay1 <- pay1[Pay_Period_End >= "2023-01-01"]
```

### To Add Named Plaintiffs
Add to your `scripts/clean_data.R`:
```r
key_employees <- c(
  "12345" = "Smith, John",
  "67890" = "Doe, Jane"
)
```

### To Customize Metrics
Edit `scripts/metrics_spec.csv` to add rows:
```csv
metric_group,metric_label,source,expr,denom,digits
Custom,My Metric,shift_data1,sum(my_column),NA,0
```

## 🐛 Troubleshooting

### "Object not found" errors
```r
# Ensure all data is loaded:
ls()  # Should show time1, pay1, shift_data1, etc.

# Re-run analysis:
source("scripts/analysis.R")
```

### Dashboard shows no data
```r
# Check output files exist:
list.files("output")
list.files("data/processed")

# Verify RDS files:
file.exists("data/processed/time_processed.rds")
```

### Memory issues
```r
# Increase memory limit (Windows):
memory.limit(size = 16000)

# Clear workspace:
rm(list = ls())
gc()
```

## 📁 Expected File Structure

After running, you should have:

```
ads/
├── data/
│   ├── processed/
│   │   ├── time_processed.rds ✓
│   │   ├── pay_processed.rds ✓
│   │   └── class_processed.rds ✓
│
├── output/
│   ├── Analysis.csv ✓
│   ├── Time Shift Data.csv ✓
│   ├── Time Employee Data.csv ✓
│   ├── Pay Data.csv ✓
│   └── [various analysis tables] ✓
│
└── dashboard/
    └── app.R ✓ (running)
```

## 🎬 Example Session

```r
# 1. Configure
# Edit scripts/clean_data.R with your case details and parameters

# 2. Run everything
source("run_analysis.R")

# Output shows:
# ✓ Data cleaning complete
# ✓ Analysis complete
# ✓ Employees analyzed: 1,234
# ✓ Shifts analyzed: 45,678
# ✓ Meal period violations: 5,432
# ✓ Total damages: $234,567

# 3. Launch dashboard
shiny::runApp("dashboard/app.R")

# 4. Explore!
# - Select date range in sidebar
# - Click through tabs
# - Drill down to specific employees
# - Export data as needed
```

## ⏭️ Next Steps

1. Review the full [README.md](README.md) for detailed documentation
2. Customize parameters in `scripts/clean_data.R`
3. Add your own metrics to `scripts/metrics_spec.csv`
4. Adjust dashboard layout in `dashboard/app.R`
5. Share the dashboard URL with your team (when deployed)

## 🆘 Need Help?

1. **Console errors**: Check that all required packages are installed
2. **Missing data**: Verify file paths in `scripts/clean_data.R`
3. **Wrong calculations**: Review parameters in `scripts/clean_data.R`
4. **Dashboard issues**: Check browser console (F12) for errors

Happy analyzing! 🎉
