# ADS Case Analysis - Quick Start Guide for Analysts

## Mandatory Workflow

**ALWAYS run scripts in this order for every case:**

1. **`clean_data.R`** - MUST run first (sets up environment, cleans data)
2. **`analysis.R`** - Run after clean_data.R completes
3. **`app.R`** (optional) - Launch interactive dashboard to explore results

## Setting Up a New Case

### Step 1: Update Your OneDrive Path (One-Time Setup)

Open `clean_data.R` and verify **line 28** matches YOUR OneDrive location:

```r
ADS_SHARED <- Sys.getenv("ADS_SHARED",
  unset = "C:/Users/[YOUR_USERNAME]/OneDrive - anellodatasolutions.com/Documents/0. ADS/ADS_Shared")
```

Replace `[YOUR_USERNAME]` with your actual Windows username. This path gives you read-only access to shared ADS functions.

### Step 2: Set Case Directory (Required for Each Case)

In `clean_data.R` around **line 53**, update the case directory path:

```r
set_case_dir("C:/Users/[USERNAME]/OneDrive/Cases/[CASE_NAME]/Analysis/[CASE]_R")
```

This should point to the root folder for THIS specific case (which will contain `data/` and `output/` subfolders).

### Step 3: Configure Case Parameters (Required for Each Case)

Update the case configuration section in `clean_data.R` (starts around **line 70**):

**Required fields:**
- `case_name` - Case title (e.g., "Ulloa v Securitas")
- `case_no` - Court case number
- `date_filed` - Filing date
- `complaint_date` - Usually same as date_filed
- `mediation_date` - Target mediation/settlement date

**Date ranges:**
- `class_dmgs_start_date` / `class_dmgs_end_date` - Class action period
- `paga_dmgs_start_date` / `paga_dmgs_end_date` - PAGA period
- `wt_start_date` / `wt_end_date` - Waiting time penalties period
- `wsv_start_date` / `wsv_end_date` - Wage statement violations period

**Optional parameters** (review with supervisor):
- `key_employees` - Named plaintiffs (Employee ID = "Name")
- `sample_size` - Sample description (e.g., "100%", "Random 10%")
- Analysis thresholds (`rrop_buffer`, `min_ot_buffer`, etc.)
- Penalty amounts (wage statement, waiting time, PAGA)
- Interest rate (`annual_interest_rate`)

### Step 4: Place Raw Data Files

Ensure raw data files (time, pay, employee data) are in the `data/raw/` folder created by `init_case_paths()`.

### Step 5: Run the Workflow

Execute in order:
1. Source `clean_data.R` - Creates processed data files in `data/processed/`
2. Source `analysis.R` - Generates analysis tables and outputs in `output/`
3. (Optional) Run `shiny::runApp("scripts")` from case root to launch dashboard

## Important Notes

- **Never edit files in ADS_Shared** - You have read-only access to shared functions
- **Always run clean_data.R first** - Other scripts depend on the environment it creates
- **Check OneDrive sync status** - If you get path errors, ensure OneDrive is synced
- **Case directory structure** - The workflow automatically creates required folders (data/raw, data/processed, output)

## Troubleshooting

**"Cannot find ADS_Shared folder"** → Update line 28 in clean_data.R with your correct OneDrive path

**"ADS functions not loaded"** → You didn't run clean_data.R first - start there

**"Processed data not found"** → clean_data.R didn't complete successfully - check for errors

## Questions?

Contact your supervisor for case-specific configuration guidance or if you encounter errors during setup.
