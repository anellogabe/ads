# Wage & Hour Analysis Dashboard

A comprehensive R-based dashboard for analyzing wage and hour compliance, meal/rest periods, overtime, regular rate of pay, and damages calculations.

## 📋 Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Project Structure](#project-structure)
- [Setup](#setup)
- [Usage](#usage)
- [Dashboard Features](#dashboard-features)
- [Customization](#customization)
- [Troubleshooting](#troubleshooting)

## 🎯 Overview

This dashboard provides interactive visualizations and analysis for California wage and hour compliance, including:
- Meal and rest period violations
- Overtime and double time analysis
- Regular rate of pay (RROP) calculations
- Time rounding analysis
- Damages calculations with prejudgment interest

## ✨ Features

### Analysis Capabilities
- **Meal & Rest Periods**: Detect missed, late, and short meal periods; rest period violations
- **Overtime**: Daily OT/DT, FLSA overtime, workweek analysis
- **Regular Rate**: Bonus allocations, RROP underpayments, violation tracking
- **Time Rounding**: Shift-level and punch-level rounding analysis
- **Damages**: Automated damages calculations with interest

### Dashboard Features
- Interactive filtering by date range and employee groups
- Real-time data visualizations with Plotly
- Detailed employee drill-down views
- Exportable data tables
- Summary metrics and KPIs

## 📁 Project Structure

```
ads/
├── README.md                   # This file
├── run_analysis.R              # Master orchestration script
│
├── scripts/
│   ├── functions.R             # Helper functions
│   ├── clean_data.R            # Data cleaning script (USER PROVIDED)
│   ├── analysis.R              # Main analysis script (USER PROVIDED)
│   └── metrics_spec.csv        # Metrics specification
│
├── data/
│   ├── raw/                    # Raw data files (USER PROVIDED)
│   └── processed/              # Processed RDS files
│       ├── time_processed.rds
│       ├── pay_processed.rds
│       └── class_processed.rds
│
├── output/                     # Analysis outputs
│   ├── Analysis.csv
│   ├── Time Shift Data.csv
│   ├── Time Employee Data.csv
│   └── Pay Data.csv
│
├── prod/                       # Production files (if using random samples)
│
└── dashboard/
    └── app.R                   # Shiny dashboard application
```

## 🚀 Setup

### Prerequisites

Install required R packages:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "data.table",
  "ggplot2",
  "plotly",
  "DT",
  "lubridate",
  "here",
  "readr",
  "readxl",
  "purrr",
  "tidyr",
  "dplyr",
  "openxlsx"
))
```

### Initial Setup

1. **Place your data files**:
   - Put raw time/pay data in `data/raw/`
   - Update file paths in `scripts/clean_data.R`

2. **Add your analysis code**:
   - Copy your full analysis script content into `scripts/analysis.R`
   - Copy your full functions into `scripts/functions.R`
   - Ensure `scripts/clean_data.R` is complete (includes parameters)

## 📊 Usage

### Basic Workflow

1. **Run the full pipeline**:
   ```r
   source("run_analysis.R")
   ```

   This will:
   - Load and clean your data
   - Run the full wage & hour analysis
   - Generate all output files
   - Prepare data for the dashboard

2. **Launch the dashboard**:
   ```r
   shiny::runApp("dashboard/app.R")
   ```

   Or uncomment the auto-launch line in `run_analysis.R`

### Step-by-Step (Manual)

If you prefer to run steps individually:

```r
# Step 1: Clean data
source("scripts/clean_data.R")

# Step 2: Run analysis
source("scripts/analysis.R")

# Step 3: Launch dashboard
shiny::runApp("dashboard/app.R")
```

## 📈 Dashboard Features

### Overview Tab
- Total employees, shifts, violations, and damages
- Violation trends over time
- Key metrics summary table

### Meal & Rest Periods Tab
- Violation counts and rates
- Duration distributions
- Start time analysis
- Shift hours vs meal periods correlation

### Overtime Analysis Tab
- OT/DT hours by month
- Shift length distribution
- Top OT employees

### Regular Rate Tab
- RROP violation tracking
- Underpayment analysis by type
- Trend analysis

### Time Rounding Tab
- Net time differences
- Rounding pattern analysis
- Employee-level impact

### Damages Summary Tab
- Total damages by category
- Employee-level damages
- Cumulative damages over time

### Employee Detail Tab
- Select any employee for detailed analysis
- Shift history
- Violations timeline
- Complete data table export

### Data Tables Tab
- Full access to all underlying data
- Sortable and filterable tables
- Export capabilities

## 🎨 Customization

### Adding New Metrics

Edit `scripts/metrics_spec.csv`:

```csv
metric_group,metric_label,source,expr,denom,digits
Your Group,Your Metric,shift_data1,sum(your_column),denominator_name,2
```

### Modifying Filters

In `dashboard/app.R`, update the sidebar filters:

```r
selectInput("your_filter", "Label:",
           choices = c(...),
           selected = "default")
```

### Changing Colors/Themes

Modify the dashboard skin in `app.R`:

```r
dashboardPage(
  skin = "blue"  # Options: blue, black, purple, green, red, yellow
```

## 🔧 Troubleshooting

### Common Issues

**Dashboard shows "No data"**:
- Ensure `run_analysis.R` completed successfully
- Check that output files exist in `output/` folder
- Verify RDS files exist in `data/processed/`

**Analysis fails**:
- Check your data file paths in `clean_data.R`
- Ensure all required columns exist
- Review console for specific error messages

**Missing functions error**:
- Ensure `scripts/functions.R` contains all helper functions
- Source functions.R before running analysis

**Date format errors**:
- Verify date columns are properly formatted in clean_data.R
- Check that date parameters in scripts/clean_data.R are valid

### Performance Tips

For large datasets (>100K rows):
- Consider filtering to key employees or date ranges
- Use random sampling (see `generate_random_sample()` function)
- Increase R memory limit if needed

### Getting Help

Check these files for detailed implementations:
- `scripts/functions.R` - All helper functions with examples
- `scripts/clean_data.R` - All configurable parameters
- `scripts/metrics_spec.csv` - Metric definitions

## 📝 Notes

- The dashboard uses reactive filtering - changes to filters update all visualizations automatically
- All monetary values round to nearest dollar in summaries
- Percentage metrics show 1 decimal place
- Time differences round to 2 decimal places (hours)
- The "Refresh Data" button reloads all data from disk

## 🔄 Workflow Example

```r
# 1. Configure your case
# Edit scripts/clean_data.R (includes all parameters)

# 2. Run the pipeline
source("run_analysis.R")

# 3. Review console output for summary statistics

# 4. Launch dashboard (auto-launches or run manually)
shiny::runApp("dashboard/app.R")

# 5. Explore data:
#    - Filter by date range
#    - Select key employee groups
#    - Drill down to individual employees
#    - Export tables as needed

# 6. Make adjustments:
#    - Update parameters
#    - Re-run analysis
#    - Click "Refresh Data" in dashboard
```

## 📄 License

This is a proprietary wage and hour analysis tool. All rights reserved.

## 👥 Support

For questions or issues:
1. Check the Troubleshooting section above
2. Review function documentation in `scripts/functions.R`
3. Consult the metrics specification in `scripts/metrics_spec.csv`
