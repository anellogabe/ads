# ==============================================================================
# PROPRIETARY AND CONFIDENTIAL
# Anello Data Solutions LLC
# 
# This file contains proprietary information and trade secrets.
# Unauthorized copying, distribution, or use is strictly prohibited.
# For authorized use by ANELLO DATA SOLUTIONS LLC contracted analysts only.
# ==============================================================================

# ADS ENGINE: PATH HELPERS ------------------------------------------------------------------------------

ads_repo <- function() {
  ADS_REPO <- Sys.getenv("ADS_REPO", unset = "")
  if (!nzchar(ADS_REPO)) stop("ADS_REPO env var not set. On Windows: setx ADS_REPO \"C:/.../GitHub/ads\"")
  normalizePath(ADS_REPO, winslash = "/", mustWork = TRUE)
}

ads_path <- function(...) file.path(ads_repo(), ...)

# CASE PATH INITIALIZER ------------------------------------------------------------------------------

init_case_paths <- function(case_dir = Sys.getenv("ADS_CASE_DIR", unset = ""),
                            set_globals = TRUE) {
  if (!nzchar(case_dir)) stop("ADS_CASE_DIR not set. Set it or call set_case_dir(\"C:/.../CaseFolder\").")
  case_dir <- normalizePath(case_dir, winslash = "/", mustWork = TRUE)
  
  raw_dir       <- file.path(case_dir, "data", "raw")
  processed_dir <- file.path(case_dir, "data", "processed")
  out_dir       <- file.path(case_dir, "output")
  
  dir.create(raw_dir,       recursive = TRUE, showWarnings = FALSE)
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_dir,       recursive = TRUE, showWarnings = FALSE)
  
  paths <- list(
    CASE_DIR      = case_dir,
    RAW_DIR       = raw_dir,
    PROCESSED_DIR = processed_dir,
    OUT_DIR       = out_dir
  )
  
  if (isTRUE(set_globals)) {
    assign("CASE_DIR",      paths$CASE_DIR,      envir = .GlobalEnv)
    assign("RAW_DIR",       paths$RAW_DIR,       envir = .GlobalEnv)
    assign("PROCESSED_DIR", paths$PROCESSED_DIR, envir = .GlobalEnv)
    assign("OUT_DIR",       paths$OUT_DIR,       envir = .GlobalEnv)
  }
  
  paths
}

# One-liner you can call at the top of scripts (absolute, deterministic)
set_case_dir <- function(case_dir, set_globals = TRUE) {
  case_dir <- normalizePath(case_dir, winslash = "/", mustWork = TRUE)
  Sys.setenv(ADS_CASE_DIR = case_dir)
  init_case_paths(case_dir = case_dir, set_globals = set_globals)
}

resolve_case_paths <- function() {
  
  env_case <- Sys.getenv("ADS_CASE_DIR", unset = "")
  env_case <- if (nzchar(env_case)) normalizePath(env_case, winslash = "/", mustWork = FALSE) else ""
  
  globals_ok <- (
    exists("CASE_DIR", inherits = TRUE) &&
      exists("RAW_DIR", inherits = TRUE) &&
      exists("PROCESSED_DIR", inherits = TRUE) &&
      exists("OUT_DIR", inherits = TRUE)
  )
  
  if (globals_ok) {
    g_case <- normalizePath(get("CASE_DIR", inherits = TRUE), winslash = "/", mustWork = FALSE)
    
    # Trust globals only if:
    # 1) ADS_CASE_DIR is blank OR matches CASE_DIR
    # AND
    # 2) directory structure exists under CASE_DIR
    structure_ok <- dir.exists(file.path(g_case, "data")) && dir.exists(file.path(g_case, "output"))
    
    same_as_env <- (!nzchar(env_case)) || identical(g_case, env_case)
    
    if (structure_ok && same_as_env) {
      return(list(
        CASE_DIR      = get("CASE_DIR", inherits = TRUE),
        RAW_DIR       = get("RAW_DIR", inherits = TRUE),
        PROCESSED_DIR = get("PROCESSED_DIR", inherits = TRUE),
        OUT_DIR       = get("OUT_DIR", inherits = TRUE)
      ))
    }
  }
  
  # Fall back to ADS_CASE_DIR (strict)
  init_case_paths()
}

resolve_out_dir <- function(out_dir = NULL, subdir = NULL) {
  base <- out_dir
  if (is.null(base) || !nzchar(base)) base <- resolve_case_paths()$OUT_DIR
  base <- normalizePath(base, winslash = "/", mustWork = FALSE)
  if (!is.null(subdir) && nzchar(subdir)) base <- file.path(base, subdir)
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  base
}

resolve_processed_dir <- function(processed_dir = NULL, subdir = NULL) {
  base <- processed_dir
  if (is.null(base) || !nzchar(base)) base <- resolve_case_paths()$PROCESSED_DIR
  base <- normalizePath(base, winslash = "/", mustWork = FALSE)
  if (!is.null(subdir) && nzchar(subdir)) base <- file.path(base, subdir)
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  base
}


# ADS ENGINE LOADER ------------------------------------------------------------------------------

load_ads_engine <- function() {
  repo <- Sys.getenv("ADS_REPO", unset = "")
  if (!nzchar(repo)) stop("ADS_REPO is not set. On Windows: setx ADS_REPO \"C:/.../GitHub/ads\"")
  repo <- normalizePath(repo, winslash = "/", mustWork = TRUE)
  
  fn_path <- file.path(repo, "scripts", "functions.R")
  if (!file.exists(fn_path)) stop("functions.R not found at: ", fn_path)
  
  source(fn_path, local = FALSE, chdir = FALSE)
  invisible(TRUE)
}


# SAFE CSV & RDS FILE WRITER ------------------------------------------------------------------------------

write_csv_and_rds <- function(dt, out_path_csv) {
  if (!grepl("\\.csv$", out_path_csv, ignore.case = TRUE)) {
    out_path_csv <- paste0(out_path_csv, ".csv")
  }
  
  out_dir <- dirname(out_path_csv)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_path_rds <- sub("\\.csv$", ".rds", out_path_csv, ignore.case = TRUE)
  
  csv_ok <- tryCatch(
    { fwrite(dt, out_path_csv); TRUE },
    error = function(e) { warning("❌ FAILED CSV: ", out_path_csv, "\n   ", e$message); FALSE }
  )
  
  rds_ok <- tryCatch(
    { saveRDS(dt, out_path_rds); TRUE },
    error = function(e) { warning("❌ FAILED RDS: ", out_path_rds, "\n   ", e$message); FALSE }
  )
  
  if (csv_ok || rds_ok) {
    message("✔ Output status:")
    if (csv_ok) message("  • CSV: ", out_path_csv)
    if (rds_ok) message("  • RDS: ", out_path_rds)
  }
  
  invisible(list(csv = out_path_csv, rds = out_path_rds, csv_ok = csv_ok, rds_ok = rds_ok))
}


# DEV / TEST SUBSET ------------------------------------------------------------------------------

run_test_sample_from_all_ids <- function(all_ids, pay1, time1,
                                         test_n = NA, test_pct = NA,
                                         seed_val = 99999,
                                         id_col_all  = "ID",
                                         id_col_time = "ID",
                                         id_col_pay  = "Pay_ID",
                                         require_cols_all = c("Time_Present","Pay_Present","Class_Present"),
                                         verbose = TRUE) {
  
  data.table::setDT(all_ids)
  data.table::setDT(pay1)
  data.table::setDT(time1)
  
  # Validate selector params
  if (!is.na(test_n) && !is.na(test_pct)) stop("Set ONLY one of test_n or test_pct, not both.")
  if (is.na(test_n) && is.na(test_pct))   stop("Set either test_n or test_pct (one must be non-NA).")
  
  # Required cols exist
  if (!id_col_all %in% names(all_ids)) stop("all_ids missing id column: ", id_col_all)
  missing_req <- setdiff(require_cols_all, names(all_ids))
  if (length(missing_req) > 0) stop("all_ids missing required columns: ", paste(missing_req, collapse = ", "))
  
  # Eligible universe: present in Time + Pay + Class (as recorded in all_ids)
  eligible <- all_ids[Time_Present == 1L & Pay_Present == 1L & Class_Present == 1L, unique(get(id_col_all))]
  eligible <- eligible[!is.na(eligible)]
  if (length(eligible) == 0) stop("No eligible IDs found in all_ids with Time+Pay+Class present.")
  
  # Sample size
  n_keep <- if (!is.na(test_n)) {
    min(as.integer(test_n), length(eligible))
  } else {
    ceiling(as.numeric(test_pct) * length(eligible))
  }
  n_keep <- max(1L, n_keep)
  
  set.seed(seed_val)
  test_ids <- sample(eligible, n_keep)
  
  # Filter only time + pay
  time1_sub <- time1[get(id_col_time) %in% test_ids]
  pay1_sub  <- pay1[get(id_col_pay)  %in% test_ids]
  
  if (verbose) {
    message("⚠ DEV MODE: Random test subset (eligible = Time+Pay+Class present per all_ids)")
    message("  • Seed: ", seed_val)
    message("  • Eligible IDs: ", format(length(eligible), big.mark = ","))
    message("  • Sample IDs kept: ", format(n_keep, big.mark = ","))
    message("  • time1 rows:  ", format(nrow(time1_sub), big.mark = ","))
    message("  • pay1 rows:   ", format(nrow(pay1_sub), big.mark = ","))
  }
  
  invisible(list(
    pay1 = pay1_sub,
    time1 = time1_sub,
    test_ids = test_ids,
    eligible_ids = eligible,
    n_keep = n_keep,
    seed_val = seed_val
  ))
}


# SAFE LEFT REJOIN / JOIN ------------------------------------------------------------------------------

safe_left_join <- function(dt_left, dt_right, by, prefix = NULL) {
  tryCatch({
    # Check that both inputs are data.tables
    if (!is.data.table(dt_left) || !is.data.table(dt_right)) {
      stop("Both dt_left and dt_right must be data.tables.")
    }
    
    # Make a copy of dt_right to avoid modifying original
    dt_right <- copy(dt_right)
    
    # Handle different column names in by
    if (is.null(names(by))) {
      # Simple case: same column names in both tables
      by_left <- by
      by_right <- by
    } else {
      # Different column names: by = c("left_col" = "right_col")
      by_left <- names(by)
      by_right <- unname(by)
    }
    
    # Determine which columns would be added by the join (non-key columns from dt_right)
    right_nonkeys <- setdiff(names(dt_right), by_right)
    
    # Apply prefix to dt_right columns if specified
    if (!is.null(prefix)) {
      new_names <- paste0(prefix, "_", right_nonkeys)
      setnames(dt_right, old = right_nonkeys, new = new_names)
      right_nonkeys <- new_names  # Update to reflect new names
    }
    
    # Check which columns already exist in dt_left
    overlapping_cols <- intersect(names(dt_left), right_nonkeys)
    
    # Remove overlapping columns from dt_right (not dt_left)
    if (length(overlapping_cols) > 0) {
      dt_right[, (overlapping_cols) := NULL]
      message("✅ Columns already exist in target table and will be skipped: ", paste(overlapping_cols, collapse = ", "))
    } else {
      message("✅ No overlapping columns found. Proceeding with join.")
    }
    
    # Perform the safe left join
    if (is.null(names(by))) {
      result <- merge(dt_left, dt_right, by = by, all.x = TRUE)
    } else {
      result <- merge(dt_left, dt_right, by.x = by_left, by.y = by_right, all.x = TRUE)
    }
    
    message("✅ Left join successfully completed using keys: ", 
            if(is.null(names(by))) paste(by, collapse = ", ") 
            else paste(by_left, "=", by_right, collapse = ", "))
    if (!is.null(prefix)) {
      message("✅ Prefix '", prefix, "_' added to new columns")
    }
    invisible(result)  # suppress output in console
  },
  error = function(e) {
    message("❌ Error during Safe_Left_Join: ", e$message)
    return(NULL)
  })
}


# TRANSPOSE PAY DATA ------------------------------------------------------------------------------------------------

transpose_pay_data <- function(pay_data, 
                               cols_to_keep, 
                               strings_to_remove = NULL, 
                               string_to_match_hrs, 
                               string_to_match_amt,
                               suffixes_to_remove = NULL) {
  
  # Convert to data.table
  dt <- as.data.table(pay_data)
  
  # Check for duplicate rows before processing
  dup_check <- dt[, .N, by = cols_to_keep]
  if(any(dup_check$N > 1)) {
    warning(paste("Warning: Found", sum(dup_check$N > 1), 
                  "duplicate rows based on key columns. Aggregating duplicates."))
    
    # Get numeric columns only
    numeric_cols <- names(dt)[sapply(dt, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, cols_to_keep)
    
    dt <- dt[, lapply(.SD, sum, na.rm = TRUE), 
             by = cols_to_keep, 
             .SDcols = numeric_cols]
  }
  
  # Ensure all non-key columns are numeric before melting
  non_key_cols <- setdiff(names(dt), cols_to_keep)
  dt[, (non_key_cols) := lapply(.SD, function(x) {
    if(is.character(x)) as.numeric(x) else as.numeric(x)
  }), .SDcols = non_key_cols]
  
  # Melt from wide to long
  dt_long <- melt(dt, 
                  id.vars = cols_to_keep,
                  variable.name = "Pay_Code",
                  value.name = "Value",
                  variable.factor = FALSE)
  
  # Filter out zeros and NAs
  dt_long <- dt_long[!is.na(Value) & Value != 0]
  
  # Convert Pay_Code to character
  dt_long[, Pay_Code := as.character(Pay_Code)]
  
  # Apply strings_to_remove if provided
  if(!is.null(strings_to_remove) && length(strings_to_remove) > 0) {
    remove_pattern <- paste0("(", paste(strings_to_remove, collapse = "|"), ")")
    dt_long[, Pay_Code := trimws(gsub(remove_pattern, "", Pay_Code))]
  } else {
    dt_long[, Pay_Code := trimws(Pay_Code)]
  }
  
  # Create patterns for matching
  hrs_pattern <- paste0(string_to_match_hrs, collapse = "|")
  amt_pattern <- paste0(string_to_match_amt, collapse = "|")
  
  # Identify hours vs amounts
  if(grepl("\\(\\?", amt_pattern)) {
    dt_long[, `:=`(
      Is_Hours = grepl(hrs_pattern, Pay_Code, ignore.case = TRUE),
      Is_Amount = grepl(amt_pattern, Pay_Code, perl = TRUE)
    )]
  } else {
    dt_long[, `:=`(
      Is_Hours = grepl(hrs_pattern, Pay_Code, ignore.case = TRUE),
      Is_Amount = grepl(amt_pattern, Pay_Code)
    )]
  }
  
  # Clean Pay_Code for grouping (use the user-defined suffixes)
  if(!is.null(suffixes_to_remove)) {
    dt_long[, Pay_Code_Clean := gsub(suffixes_to_remove, "", Pay_Code, ignore.case = TRUE)]
  } else {
    dt_long[, Pay_Code_Clean := Pay_Code]  # No cleaning if not specified
  }
  
  # Assign values to appropriate columns
  dt_long[, `:=`(
    Pay_Hours = fifelse(Is_Hours, Value, 0),
    Pay_Amount = fifelse(Is_Amount, Value, 0)
  )]
  
  # Group and summarize
  result <- dt_long[, .(
    Pay_Hours = sum(Pay_Hours, na.rm = TRUE),
    Pay_Amount = sum(Pay_Amount, na.rm = TRUE)
  ), by = c(cols_to_keep, "Pay_Code_Clean")]
  
  # Rename Pay_Code_Clean back to Pay_Code
  setnames(result, "Pay_Code_Clean", "Pay_Code")
  
  # Sort by key columns, then by Pay_Hours descending, then Pay_Code
  setkeyv(result, cols_to_keep)
  result <- result[order(get(cols_to_keep[1]), get(cols_to_keep[2]), -Pay_Hours, Pay_Code)]
  
  return(result)
}


# PAY CODES SUMMARY TABLE ------------------------------------------------------------------------------------------------

pay_code_summary <- function(
    data,
    output_path,
    separate_key_gps = TRUE
) {
  # Packages
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table required")
  
  # Require your writer
  if (!exists("write_csv_and_rds", mode = "function", inherits = TRUE)) {
    stop("write_csv_and_rds() not found. Source functions.R first.")
  }
  
  # Required columns
  required_cols <- c(
    "Pay_Code","Pay_Hours","Pay_Amount","Pay_Period_End","Pay_ID_Period_End",
    "Hrs_Wkd_Pay_Code","RROP_Pay_Code",
    "OT_Pay_Code","DT_Pay_Code","Reg_Pay_Code",
    "Bon_Pay_Code","Meal_Pay_Code","Rest_Pay_Code","Diff_Pay_Code",
    "Diff_OT_Pay_Code","Diff_DT_Pay_Code","Sick_Pay_Code",
    "Pay_Key_Gps","Pay_ID"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Normalize date
  data$Pay_Period_End <- as.Date(data$Pay_Period_End)
  
  safe_max <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  }
  
  # Helper to summarize one group
  summarize_one_group <- function(df, key_label) {
    unique_pay_codes <- unique(df$Pay_Code)
    summary_list <- list()
    
    for (pay_code in unique_pay_codes) {
      subset_data <- df %>% filter(Pay_Code == pay_code)
      if (nrow(subset_data) == 0) next
      
      summary_list[[paste(key_label, pay_code, sep = "_")]] <-
        data.frame(
          Pay_Key_Gps = key_label,
          Pay_Code = pay_code,
          Min_Period_End = min(subset_data$Pay_Period_End, na.rm = TRUE),
          Max_Period_End = max(subset_data$Pay_Period_End, na.rm = TRUE),
          Employees = uniqueN(subset_data$Pay_ID, na.rm = TRUE),
          Pay_Periods = uniqueN(subset_data$Pay_ID_Period_End, na.rm = TRUE),
          Total_Hours = sum(subset_data$Pay_Hours, na.rm = TRUE),
          Total_Amount = sum(subset_data$Pay_Amount, na.rm = TRUE),
          Avg_Amount = mean(subset_data$Pay_Amount, na.rm = TRUE),
          
          Hrs_Wkd_Pay_Code = safe_max(subset_data$Hrs_Wkd_Pay_Code),
          RROP_Pay_Code    = safe_max(subset_data$RROP_Pay_Code),
          
          Reg_Pay_Code     = safe_max(subset_data$Reg_Pay_Code),
          OT_Pay_Code      = safe_max(subset_data$OT_Pay_Code),
          DT_Pay_Code      = safe_max(subset_data$DT_Pay_Code),
          Bon_Pay_Code     = safe_max(subset_data$Bon_Pay_Code),
          Meal_Pay_Code    = safe_max(subset_data$Meal_Pay_Code),
          Rest_Pay_Code    = safe_max(subset_data$Rest_Pay_Code),
          Sick_Pay_Code    = safe_max(subset_data$Sick_Pay_Code),
          Diff_Pay_Code    = safe_max(subset_data$Diff_Pay_Code),
          Diff_OT_Pay_Code = safe_max(subset_data$Diff_OT_Pay_Code),
          Diff_DT_Pay_Code = safe_max(subset_data$Diff_DT_Pay_Code)
        )
    }
    
    total_summary <- df %>%
      summarise(
        Pay_Key_Gps = key_label,
        Pay_Code = paste(key_label, "Total", sep = ": "),
        Min_Period_End = min(Pay_Period_End, na.rm = TRUE),
        Max_Period_End = max(Pay_Period_End, na.rm = TRUE),
        Employees = uniqueN(Pay_ID, na.rm = TRUE),
        Pay_Periods = uniqueN(Pay_ID_Period_End, na.rm = TRUE),
        Total_Hours = sum(Pay_Hours, na.rm = TRUE),
        Total_Amount = sum(Pay_Amount, na.rm = TRUE),
        Avg_Amount = mean(Pay_Amount, na.rm = TRUE),
        
        Hrs_Wkd_Pay_Code = safe_max(Hrs_Wkd_Pay_Code),
        RROP_Pay_Code    = safe_max(RROP_Pay_Code),
        
        Reg_Pay_Code     = safe_max(Reg_Pay_Code),
        OT_Pay_Code      = safe_max(OT_Pay_Code),
        DT_Pay_Code      = safe_max(DT_Pay_Code),
        Bon_Pay_Code     = safe_max(Bon_Pay_Code),
        Meal_Pay_Code    = safe_max(Meal_Pay_Code),
        Rest_Pay_Code    = safe_max(Rest_Pay_Code),
        Sick_Pay_Code    = safe_max(Sick_Pay_Code),
        Diff_Pay_Code    = safe_max(Diff_Pay_Code),
        Diff_OT_Pay_Code = safe_max(Diff_OT_Pay_Code),
        Diff_DT_Pay_Code = safe_max(Diff_DT_Pay_Code)
      )
    
    summary_list[[paste(key_label, "Total", sep = "_")]] <- total_summary
    bind_rows(summary_list)
  }
  
  if (isTRUE(separate_key_gps)) {
    keys <- unique(data$Pay_Key_Gps)
    
    final_summary_df <- bind_rows(lapply(keys, function(k) {
      summarize_one_group(data %>% filter(Pay_Key_Gps == k), key_label = k)
    }))
    
    totals <- final_summary_df %>%
      filter(grepl(": Total$", Pay_Code)) %>%
      select(Pay_Key_Gps, Employees, Pay_Periods) %>%
      rename(
        Total_Employees = Employees,
        Total_Pay_Periods = Pay_Periods
      )
    
    final_summary_df <- final_summary_df %>%
      left_join(totals, by = "Pay_Key_Gps") %>%
      mutate(
        Percent_Employees   = ifelse(Total_Employees > 0, round(Employees / Total_Employees, 4), NA_real_),
        Percent_Pay_Periods = ifelse(Total_Pay_Periods > 0, round(Pay_Periods / Total_Pay_Periods, 4), NA_real_)
      ) %>%
      select(
        Pay_Key_Gps, Pay_Code,
        Min_Period_End, Max_Period_End,
        Employees, Percent_Employees,
        Pay_Periods, Percent_Pay_Periods,
        Total_Hours, Total_Amount, Avg_Amount,
        Hrs_Wkd_Pay_Code, RROP_Pay_Code,
        Reg_Pay_Code, OT_Pay_Code, DT_Pay_Code,
        Bon_Pay_Code, Meal_Pay_Code, Rest_Pay_Code, Sick_Pay_Code,
        Diff_Pay_Code, Diff_OT_Pay_Code, Diff_DT_Pay_Code
      )
    
  } else {
    final_summary_df <- summarize_one_group(data, key_label = "ALL") %>%
      mutate(
        Percent_Employees = NA_real_,
        Percent_Pay_Periods = NA_real_
      ) %>%
      select(
        Pay_Key_Gps, Pay_Code,
        Min_Period_End, Max_Period_End,
        Employees, Percent_Employees,
        Pay_Periods, Percent_Pay_Periods,
        Total_Hours, Total_Amount, Avg_Amount,
        Hrs_Wkd_Pay_Code, RROP_Pay_Code,
        Reg_Pay_Code, OT_Pay_Code, DT_Pay_Code,
        Bon_Pay_Code, Meal_Pay_Code, Rest_Pay_Code, Sick_Pay_Code,
        Diff_Pay_Code, Diff_OT_Pay_Code, Diff_DT_Pay_Code
      )
  }
  
  write_csv_and_rds(final_summary_df, output_path)
  invisible(final_summary_df)
}

# EXAMPLES: pay_code_summary()

# 1) Default behavior: separate summaries by Pay_Key_Gps
#    (most cases will use this)
# 
# pay_code_summary(
#   pay1,
#   file.path(PROCESSED_DIR, "Pay_Code_Summary.csv"),
#   separate_key_gps = separate_key_gps
# )
# 
# # What this produces:
# # • One block per Pay_Key_Gps
# # • A row for each Pay_Code within each Pay_Key_Gps
# # • Plus a ": Total" row for each Pay_Key_Gps
# # • Includes Percent_Employees and Percent_Pay_Periods within each key
# # • Saves BOTH:
# #     - Pay_Code_Summary.csv
# #     - Pay_Code_Summary.rds
# 
# 
# # 2) Pooled across ALL employees (ignore Pay_Key_Gps splits)
# 
# pay_code_summary(
#   pay1,
#   file.path(PROCESSED_DIR, "Pay_Code_Summary_ALL.csv"),
#   separate_key_gps = FALSE
# )
# 
# # What this produces:
# # • Single combined summary across all Pay_Key_Gps
# # • No percentages (they are NA by design in this mode)
# # • Still saves CSV + RDS
# 
# 
# # 3) Capture the table in R without saving files
# 
# pay_code_summary_tbl <- pay_code_summary(
#   pay1,
#   file.path(PROCESSED_DIR, "TEMP_Pay_Code_Summary.csv"),
#   separate_key_gps = TRUE
# )


# PAY CODE CATEGORIES ------------------------------------------------------------------------------

categorize_pay_codes <- function(df, flag_columns, output_csv = NULL) {
  
  # Validate input columns
  if (!"Pay_Code" %in% colnames(df)) {
    stop("[ERROR] Missing 'Pay_Code' column in input data frame.")
  }
  
  missing_flags <- setdiff(flag_columns, colnames(df))
  if (length(missing_flags) > 0) {
    stop("[ERROR] Missing flag columns: ", paste(missing_flags, collapse = ", "))
  }
  
  # Convert to data.table (safe)
  setDT(df)
  
  # Melt to long format
  melted <- melt(
    df,
    id.vars = "Pay_Code",
    measure.vars = flag_columns,
    variable.name = "Pay_Code_Category",
    value.name = "flag"
  )
  
  # Keep only flagged rows
  melted <- melted[flag == 1L]
  
  # Aggregate category strings by Pay_Code
  pay_code_categories <- melted[, .(
    Pay_Code_Category = paste(sort(unique(Pay_Code_Category)), collapse = "; ")
  ), by = Pay_Code]
  
  # Identify uncategorized Pay_Codes
  all_codes <- unique(df$Pay_Code)
  categorized_codes <- unique(pay_code_categories$Pay_Code)
  uncategorized <- setdiff(all_codes, categorized_codes)
  
  if (length(uncategorized) > 0) {
    uncategorized_dt <- data.table(
      Pay_Code = uncategorized,
      Pay_Code_Category = "Uncategorized"
    )
    pay_code_categories <- rbindlist(
      list(pay_code_categories, uncategorized_dt),
      fill = TRUE
    )
  }
  
  # Sort result
  setorder(pay_code_categories, Pay_Code_Category)
  
  # ---- WRITE OUTPUT USING YOUR SAFE WRITER ----
  if (!is.null(output_csv)) {
    
    if (!exists("write_csv_and_rds", inherits = TRUE)) {
      stop("write_csv_and_rds() not found in environment. Source functions.R first.")
    }
    
    write_csv_and_rds(pay_code_categories, output_csv)
    
  } else {
    message("Output path not supplied; returning data only.")
  }
  
  invisible(pay_code_categories)
}


# REGULAR RATE: GROUP PAY DATA FOR BONUS MAPPING ------------------------------------------------------------------------------

# Function: group_pay_data: flexible rate and hours summary function5 ---

# By calendar quarter
# rate_hours_summary <- group_pay_data(pay1, "Pay_ID_Current_Qtr")

# By multiple columns
# rate_hours_summary <- group_pay_data(pay1, c("Pay_ID", "Cost_Center", "Pay_Period"))

# By year
# rate_hours_summary <- group_pay_data(pay1, "Pay_ID_Yr")

# By semi-annual period
# rate_hours_summary <- group_pay_data(pay1, "Pay_ID_Semi_Ann")

group_pay_data <- function(data, group_by = "Pay_ID_Period_End") {
  
  # Safe max function that returns NA instead of -Inf
  safe_max <- function(x) {
    if(all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  }
  
  # Safe min function that returns NA instead of -Inf
  safe_min <- function(x) {
    if(all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
  }
  
  # Create summary table grouped by specified column(s)
  rate_hours_summary <- data[ 
    ,
    .(
      Base_Rate = safe_max(Base_Rate1),
      Min_Pay_Date = safe_min(Pay_Date),
      Max_Pay_Date = safe_max(Pay_Date),
      Hrs_Wkd = sum(Pay_Hours[Hrs_Wkd_Pay_Code == 1], na.rm = TRUE),
      Reg_Hrs = sum(Pay_Hours[(Reg_Pay_Code == 1 & Hrs_Wkd_Pay_Code == 1)], na.rm = TRUE),
      OT_Hrs = sum(Pay_Hours[(OT_Pay_Code == 1)], na.rm = TRUE),
      DT_Hrs = sum(Pay_Hours[(DT_Pay_Code == 1)], na.rm = TRUE),
      Meal_Prem_Hrs = sum(Pay_Hours[Meal_Pay_Code == 1], na.rm = TRUE),
      Rest_Prem_Hrs = sum(Pay_Hours[Rest_Pay_Code == 1], na.rm = TRUE),
      Sick_Hrs = sum(Pay_Hours[Sick_Pay_Code == 1], na.rm = TRUE),
      Straight_Time_Amt = sum(Straight_Time_Amt, na.rm = TRUE),
      Reg_Amt = sum(Pay_Amount[Reg_Pay_Code == 1], na.rm = TRUE),
      OT_Amt = sum(fcase(
        OT_Pay_Code == 1 & grepl("premium", Rate_Type, ignore.case = TRUE), Pay_Amount,
        OT_Pay_Code == 1 & grepl("inclusive", Rate_Type, ignore.case = TRUE) & grepl("1.0", Rate_Type, ignore.case = TRUE), 0,
        OT_Pay_Code == 1 & grepl("inclusive", Rate_Type, ignore.case = TRUE), Pay_Amount / 3,
        default = 0
      ), na.rm = TRUE),
      DT_Amt = sum(fcase(
        DT_Pay_Code == 1 & grepl("premium", Rate_Type, ignore.case = TRUE), Pay_Amount,
        DT_Pay_Code == 1 & grepl("inclusive", Rate_Type, ignore.case = TRUE), Pay_Amount / 2,
        default = 0
      ), na.rm = TRUE),
      Meal_Amt = sum(Pay_Amount[Meal_Pay_Code == 1], na.rm = TRUE),
      Rest_Amt = sum(Pay_Amount[Rest_Pay_Code == 1], na.rm = TRUE),
      Sick_Amt = sum(Pay_Amount[Sick_Pay_Code == 1], na.rm = TRUE),
      Oth_RROP_Amt = sum(Pay_Amount[Hrs_Wkd_Pay_Code == 0 & OT_Pay_Code == 0 & DT_Pay_Code == 0 & 
                                      Meal_Pay_Code == 0 & Rest_Pay_Code == 0 & Sick_Pay_Code == 0 & 
                                      Diff_Pay_Code == 0 & Diff_OT_Pay_Code == 0 & Diff_DT_Pay_Code == 0 & RROP_Pay_Code == 1], na.rm = TRUE),
      Oth_Amt = sum(Pay_Amount[Hrs_Wkd_Pay_Code == 0 & OT_Pay_Code == 0 & DT_Pay_Code == 0 & 
                                 Meal_Pay_Code == 0 & Rest_Pay_Code == 0 & Sick_Pay_Code == 0 & 
                                 Diff_Pay_Code == 0 & Diff_OT_Pay_Code == 0 & Diff_DT_Pay_Code == 0 & RROP_Pay_Code == 0], na.rm = TRUE)
    ),
    by = group_by
  ]
  
  # Filter for valid base rates
  rate_hours_summary <- rate_hours_summary[
    !is.na(Base_Rate) & Base_Rate > 0]
  
  # Sort by group and descending total hours
  setorderv(rate_hours_summary, c(group_by, "Hrs_Wkd"), c(rep(1, length(group_by)), -1))
  
  # Add rank within each group
  rate_hours_summary[, Rate_Rank := seq_len(.N), by = group_by]
  
  return(rate_hours_summary)
}


# DATA COMPARISON ------------------------------------------------------------------------------

run_data_comparison <- function(
    time_data,
    pay_data,
    output_dir = NULL,   # if NULL, tries OUT_DIR; else uses provided path
    period_breakdown = c("default", "weekly", "monthly", "quarterly"),
    save_outputs = TRUE,
    show_plot = TRUE,
    return_data = TRUE
) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  stopifnot(requireNamespace("lubridate", quietly = TRUE))
  
  period_breakdown <- match.arg(period_breakdown)
  
  # Resolve output_dir (prefer case OUT_DIR if present in environment)
  if (is.null(output_dir) || !nzchar(output_dir)) {
    if (exists("OUT_DIR", inherits = TRUE) && nzchar(get("OUT_DIR", inherits = TRUE))) {
      output_dir <- get("OUT_DIR", inherits = TRUE)
    } else {
      output_dir <- file.path(getwd(), "output")
    }
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  
  # Convert to data.table copies
  time1 <- as.data.table(copy(time_data))
  pay1  <- as.data.table(copy(pay_data))
  
  # Remove any duplicate columns (defensive)
  time1 <- time1[, .SD, .SDcols = unique(names(time1))]
  pay1  <- pay1[,  .SD, .SDcols = unique(names(pay1))]
  
  # Ensure required columns
  req_time <- c("Period_End", "ID")
  req_pay  <- c("Pay_Period_End", "Pay_ID")
  miss_time <- setdiff(req_time, names(time1))
  miss_pay  <- setdiff(req_pay,  names(pay1))
  if (length(miss_time)) stop("time_data missing: ", paste(miss_time, collapse = ", "))
  if (length(miss_pay))  stop("pay_data missing: ",  paste(miss_pay,  collapse = ", "))
  
  # Coerce dates to Date
  time1[, Period_End := as.Date(Period_End)]
  pay1[,  Pay_Period_End := as.Date(Pay_Period_End)]
  
  # Summaries
  time_summary <- time1[, .(
    Employees = uniqueN(ID, na.rm = TRUE),
    Records   = .N
  ), by = .(Period_End)]
  time_summary[, Data := "Time Data"]
  
  pay_summary <- pay1[, .(
    Employees = uniqueN(Pay_ID, na.rm = TRUE),
    Records   = .N
  ), by = .(Pay_Period_End)]
  setnames(pay_summary, "Pay_Period_End", "Period_End")
  pay_summary[, Data := "Pay Data"]
  
  comparison_dt <- rbindlist(list(time_summary, pay_summary), fill = TRUE)
  
  # Period breakdown grouping
  if (period_breakdown == "weekly") {
    comparison_dt[, Period_Group := lubridate::floor_date(Period_End, "week")]
  } else if (period_breakdown == "monthly") {
    comparison_dt[, Period_Group := as.Date(format(Period_End, "%Y-%m-01"))]
  } else if (period_breakdown == "quarterly") {
    comparison_dt[, Period_Group := as.Date(paste0(
      lubridate::year(Period_End), "-",
      sprintf("%02d", (lubridate::quarter(Period_End) - 1) * 3 + 1),
      "-01"
    ))]
  } else {
    comparison_dt[, Period_Group := Period_End]
  }
  
  if (period_breakdown != "default") {
    comparison_dt <- comparison_dt[, .(
      Employees    = sum(Employees, na.rm = TRUE),
      Records      = sum(Records,   na.rm = TRUE),
      Period_Count = .N,
      First_Period = min(Period_End, na.rm = TRUE),
      Last_Period  = max(Period_End, na.rm = TRUE)
    ), by = .(Period_Group, Data)]
    setnames(comparison_dt, "Period_Group", "Period_End")
  }
  
  # Clean + order
  comparison_dt <- comparison_dt[!is.na(Period_End)]
  setorder(comparison_dt, Period_End, Data)
  
  # Plot
  plot_line <- ggplot2::ggplot(
    comparison_dt,
    ggplot2::aes(x = Period_End, y = Employees, color = Data)
  ) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::labs(
      title = paste(
        "Time vs Pay Data Comparison",
        if (period_breakdown != "default") paste0("(", period_breakdown, ")") else ""
      ),
      subtitle = paste(
        "Date Range:",
        min(comparison_dt$Period_End, na.rm = TRUE), "to",
        max(comparison_dt$Period_End, na.rm = TRUE)
      ),
      x = "Period End Date",
      y = "Number of Unique Employees",
      color = "Data Source"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x   = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  plot_line <- plot_line +
    ggplot2::scale_color_manual(
      values = c(
        "Time Data" = "#2E86C1",   # blue
        "Pay Data"  = "#28B463"    # green
      )
    )
  
  # Auto x breaks
  dr <- as.numeric(diff(range(comparison_dt$Period_End, na.rm = TRUE)))
  if (is.na(dr) || dr == 0) {
    plot_line <- plot_line + ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
  } else if (dr <= 90) {
    plot_line <- plot_line + ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")
  } else if (dr <= 365) {
    plot_line <- plot_line + ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
  } else {
    plot_line <- plot_line + ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m")
  }
  
  # Show plot in RStudio
  if (isTRUE(show_plot)) {
    print(plot_line)
  }
  
  # Save outputs
  if (isTRUE(save_outputs)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    pdf_path <- file.path(output_dir, "Data Comparison.pdf")
    ggplot2::ggsave(pdf_path, plot = plot_line, width = 12, height = 7)
    
    # Use your helper if available; otherwise fallback to fwrite + saveRDS
    csv_path <- file.path(output_dir, "Data Comparison.csv")
    
    if (exists("write_csv_and_rds", mode = "function", inherits = TRUE)) {
      write_csv_and_rds(comparison_dt, csv_path)
    } else {
      fwrite(comparison_dt, csv_path)
      saveRDS(comparison_dt, sub("\\.csv$", ".rds", csv_path))
      message("✔ Output status:")
      message("  • CSV: ", csv_path)
      message("  • RDS: ", sub("\\.csv$", ".rds", csv_path))
    }
    
    message("Files saved to: ", output_dir)
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  
  if (isTRUE(return_data)) {
    return(invisible(comparison_dt))
  }
  
  invisible(NULL)
}


# # Basic usage
# run_data_comparison(time1, pay1)
# 
# # Monthly breakdown
# result <- run_data_comparison(time1, pay1, 
#                               period_breakdown = "monthly")
# 
# # Weekly breakdown, custom output directory
# run_data_comparison(time1, pay1,
#                     period_breakdown = "weekly", 
#                     output_dir = "output/weekly_analysis")
# 
# # Quarterly breakdown for long time periods
# run_data_comparison(time1, pay1,
#                     period_breakdown = "quarterly")
#
# # Get data without saving files (for further analysis)
# comparison_data <- run_data_comparison(time1, pay1,
#                                        save_outputs = FALSE,
#                                        return_data = TRUE)
#
# # Access returned data (now returns data directly, not a list)
# comparison_data  # Direct access to the comparison data.table
#
# # Notes:
# - Automatically converts POSIXct/POSIXt dates to Date class
# - Removes NA dates silently
# - Default output directory: "output"
# - Creates only 2 files: 
#   * Data Comparison.pdf (line graph)
#   * Data Comparison.csv (period details)
# - Minimal console output - only shows where files are saved


# EMPLOYEE PAY PERIOD-LEVEL DATA COMPARISON ------------------------------------------------------------------------------

employee_period_comparison <- function(
    time_data,
    pay_data,
    output_dir = NULL,   # if NULL -> uses OUT_DIR if it exists, else getwd()/output
    save_output = TRUE,
    return_data = TRUE
) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  setDT(time_data)
  setDT(pay_data)
  
  # Resolve output_dir
  if (is.null(output_dir) || !nzchar(output_dir)) {
    if (exists("OUT_DIR", inherits = TRUE) && nzchar(get("OUT_DIR", inherits = TRUE))) {
      output_dir <- get("OUT_DIR", inherits = TRUE)
    } else {
      output_dir <- file.path(getwd(), "output")
    }
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  
  # Copies (don’t modify originals)
  time1 <- as.data.table(copy(time_data))
  pay1  <- as.data.table(copy(pay_data))
  
  # Ensure required columns
  req_time <- c("ID", "Period_End")
  req_pay  <- c("Pay_ID", "Pay_Period_End")
  miss_time <- setdiff(req_time, names(time1))
  miss_pay  <- setdiff(req_pay,  names(pay1))
  if (length(miss_time)) stop("time_data missing: ", paste(miss_time, collapse = ", "))
  if (length(miss_pay))  stop("pay_data missing: ",  paste(miss_pay,  collapse = ", "))
  
  # Ensure dates are Date
  time1[, Period_End := as.Date(Period_End)]
  pay1[,  Pay_Period_End := as.Date(Pay_Period_End)]
  
  # Time summary by employee
  time_summary <- time1[!is.na(Period_End), .(
    Time_Periods     = uniqueN(Period_End, na.rm = TRUE),
    Time_First_Date  = min(Period_End, na.rm = TRUE),
    Time_Last_Date   = max(Period_End, na.rm = TRUE),
    Time_Records     = .N,
    Time_Period_List = list(unique(Period_End))
  ), by = .(ID)]
  
  # Pay summary by employee
  pay_summary <- pay1[!is.na(Pay_Period_End), .(
    Pay_Periods     = uniqueN(Pay_Period_End, na.rm = TRUE),
    Pay_First_Date  = min(Pay_Period_End, na.rm = TRUE),
    Pay_Last_Date   = max(Pay_Period_End, na.rm = TRUE),
    Pay_Records     = .N,
    Pay_Period_List = list(unique(Pay_Period_End))
  ), by = .(Pay_ID)]
  setnames(pay_summary, "Pay_ID", "ID")
  
  # Merge
  all_employees <- merge(time_summary, pay_summary, by = "ID", all = TRUE)
  
  # Calculate matching periods
  all_employees[, `:=`(
    Matching_Periods = mapply(function(t, p) {
      if (is.null(t) || is.null(p)) return(0L)
      length(intersect(t, p))
    }, Time_Period_List, Pay_Period_List),
    
    Time_Only_Periods = mapply(function(t, p) {
      if (is.null(t)) return(0L)
      if (is.null(p)) return(length(t))
      length(setdiff(t, p))
    }, Time_Period_List, Pay_Period_List),
    
    Pay_Only_Periods = mapply(function(t, p) {
      if (is.null(p)) return(0L)
      if (is.null(t)) return(length(p))
      length(setdiff(p, t))
    }, Time_Period_List, Pay_Period_List)
  )]
  
  # Replace NA counts with 0
  cols_to_fix <- c("Time_Periods", "Pay_Periods", "Time_Records", "Pay_Records")
  for (col in cols_to_fix) {
    all_employees[is.na(get(col)), (col) := 0L]
  }
  
  # Match rates / coverage / status
  all_employees[, `:=`(
    Total_Unique_Periods = pmax(Time_Periods, Pay_Periods, na.rm = TRUE),
    Match_Rate = fifelse(
      (Time_Periods + Pay_Periods) > 0,
      round(Matching_Periods * 2 / (Time_Periods + Pay_Periods), 3),
      0
    ),
    Time_Coverage = fifelse(
      Pay_Periods > 0,
      round(Matching_Periods / Pay_Periods, 3),
      0
    ),
    Pay_Coverage = fifelse(
      Time_Periods > 0,
      round(Matching_Periods / Time_Periods, 3),
      0
    ),
    Data_Status = fcase(
      Time_Periods == 0 & Pay_Periods == 0, "No Data",
      Time_Periods == 0, "Pay Only",
      Pay_Periods == 0, "Time Only",
      default = "Both"
    )
  )]
  
  # Drop list columns for output
  output_df <- all_employees[, -c("Time_Period_List", "Pay_Period_List")]
  
  # Sort
  setorder(output_df, Match_Rate, -Total_Unique_Periods)
  
  # Save outputs (CSV + RDS)
  if (isTRUE(save_output)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    out_csv <- file.path(output_dir, "Employee Pay Period Comparison.csv")
    
    if (exists("write_csv_and_rds", mode = "function", inherits = TRUE)) {
      write_csv_and_rds(output_df, out_csv)
    } else {
      # fallback
      fwrite(output_df, out_csv)
      saveRDS(output_df, sub("\\.csv$", ".rds", out_csv))
      message("✔ Output status:")
      message("  • CSV: ", out_csv)
      message("  • RDS: ", sub("\\.csv$", ".rds", out_csv))
    }
    
    message("Files saved to: ", output_dir)
  }
  
  if (isTRUE(return_data)) return(invisible(output_df))
  invisible(NULL)
}


# Usage:
# employee_period_comparison(time1, pay1)
# 
# Or to just get the data without saving:
# emp_comp <- employee_period_comparison(time1, pay1, save_output = FALSE)


# ALL IDS REPORT ------------------------------------------------------------------------------

all_time_pay_class_ids <- function(
    time_data,
    pay_data,
    class_data = NULL,
    output_dir = NULL,                 # NULL -> uses OUT_DIR if available, else getwd()/output
    save_output = TRUE,
    return_data = TRUE,
    out_name = "All Time Pay and Class IDs.csv"
) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  library(data.table)
  
  # Resolve output_dir
  if (is.null(output_dir) || !nzchar(output_dir)) {
    if (exists("OUT_DIR", inherits = TRUE) && nzchar(get("OUT_DIR", inherits = TRUE))) {
      output_dir <- get("OUT_DIR", inherits = TRUE)
    } else {
      output_dir <- file.path(getwd(), "output")
    }
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  
  # Copies
  time1 <- as.data.table(copy(time_data))
  pay1  <- as.data.table(copy(pay_data))
  
  # ---- TIME IDS ----
  if (!all(c("ID", "Name") %chin% names(time1))) stop("time_data must contain columns: ID, Name")
  time_ids <- unique(time1[, .(ID, Name)])
  time_ids <- time_ids[, .(Name = first(Name), Time_Present = 1L), by = ID]
  
  # ---- PAY IDS ----
  if (!all(c("Pay_ID", "Pay_Name") %chin% names(pay1))) stop("pay_data must contain columns: Pay_ID, Pay_Name")
  pay_ids <- unique(pay1[, .(ID = Pay_ID, Pay_Name)])
  pay_ids <- pay_ids[, .(Pay_Name = first(Pay_Name), Pay_Present = 1L), by = ID]
  
  # Merge time+pay
  all_ids <- merge(time_ids, pay_ids, by = "ID", all = TRUE)
  
  # ---- CLASS IDS (optional) ----
  if (!is.null(class_data)) {
    class1 <- as.data.table(copy(class_data))
    if (!("Class_ID" %chin% names(class1))) stop("class_data must contain column: Class_ID")
    
    # Optional name column(s)
    name_col <- NULL
    if ("Class_Name" %chin% names(class1)) name_col <- "Class_Name"
    if (is.null(name_col) && "Name" %chin% names(class1)) name_col <- "Name"
    
    if (!is.null(name_col)) {
      class_ids <- unique(class1[, .(Class_ID, Class_Name = get(name_col))])
      class_ids <- class_ids[, .(Class_Name = first(Class_Name), Class_Present = 1L), by = Class_ID]
    } else {
      class_ids <- unique(class1[, .(Class_ID)])
      class_ids[, Class_Present := 1L]
    }
    
    # Join key
    class_ids[, ID := Class_ID]
    
    # Merge in
    all_ids <- merge(all_ids, class_ids[, c("ID", setdiff(names(class_ids), "ID")), with = FALSE], by = "ID", all = TRUE)
  }
  
  # Fill presence flags
  for (flag in c("Time_Present", "Pay_Present", "Class_Present")) {
    if (flag %chin% names(all_ids)) all_ids[is.na(get(flag)), (flag) := 0L]
  }
  
  # ---- Source label (vector-safe) ----
  all_ids[, Source := NA_character_]
  all_ids[Time_Present == 1L, Source := "Time Data"]
  all_ids[Pay_Present  == 1L, Source := fifelse(is.na(Source), "Pay Data",  paste(Source, "Pay Data",  sep = "; "))]
  if ("Class_Present" %chin% names(all_ids)) {
    all_ids[Class_Present == 1L, Source := fifelse(is.na(Source), "Class Data", paste(Source, "Class Data", sep = "; "))]
  }
  
  # Nice ordering
  prefer_order <- c("ID", "Class_ID", "Name", "Pay_Name", "Class_Name",
                    "Time_Present", "Pay_Present", "Class_Present", "Source")
  setcolorder(all_ids, c(intersect(prefer_order, names(all_ids)),
                         setdiff(names(all_ids), intersect(prefer_order, names(all_ids)))))
  
  # Save outputs (CSV + RDS)
  if (isTRUE(save_output)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    out_csv <- file.path(output_dir, out_name)
    
    if (exists("write_csv_and_rds", mode = "function", inherits = TRUE)) {
      write_csv_and_rds(all_ids, out_csv)
    } else {
      fwrite(all_ids, out_csv)
      saveRDS(all_ids, sub("\\.csv$", ".rds", out_csv))
      message("✔ Output status:")
      message("  • CSV: ", out_csv)
      message("  • RDS: ", sub("\\.csv$", ".rds", out_csv))
    }
    message("Files saved to: ", output_dir)
  }
  
  # Narrative summary
  
  n_missing_pay <- all_ids[Time_Present == 1 & Pay_Present == 0, .N]
  if (n_missing_pay == 0) {
    message("✅ No employees are present in time but missing from pay.")
  } else {
    message("⚠ ", n_missing_pay, 
            " employees appear in time data but are missing from pay data.")
  }
  
  n_missing_time <- all_ids[Pay_Present == 1 & Time_Present == 0, .N]
  if (n_missing_time == 0) {
    message("✅ No employees are present in pay but missing from time.")
  } else {
    message("⚠ ", n_missing_time, 
            " employees appear in pay data but are missing from time data.")
  }
  
  n_class_only <- all_ids[Class_Present == 1 & Time_Present == 0 & Pay_Present == 0, .N]
  if (n_class_only == 0) {
    message("✅ No employees appear only in the class list.")
  } else {
    message("⚠ ", n_class_only, 
            " employees appear in the class list but in neither time nor pay.")
  }
  
  if (isTRUE(return_data)) return(invisible(all_ids))
  invisible(NULL)
}

# EXAMPLE USAGE: all_time_pay_class_ids()

# Assumes you already have:
#   - time1   : cleaned time data with columns ID, Name, Period_End
#   - pay1    : cleaned pay data with columns Pay_ID, Pay_Name, Pay_Period_End
#   - class1  : (optional) class list with at least Class_ID
#   - OUT_DIR : your case output directory (created earlier in clean_data.R)

# # 1) BASIC USE: time + pay only ---
# 
# all_ids <- all_time_pay_class_ids(
#   time_data = time1,
#   pay_data  = pay1
# )
# 
# # Creates:
# #   <CASE_DIR>/output/All Time Pay and Class IDs.csv
# #   <CASE_DIR>/output/All Time Pay and Class IDs.rds
# # and returns the table invisibly to all_ids
# 
# # 2) INCLUDE CLASS LIST ---
# 
# all_ids <- all_time_pay_class_ids(
#   time_data  = time1,
#   pay_data   = pay1,
#   class_data = class1
# )
# 
# # 3) SAVE TO A QC SUBFOLDER ---
# 
# all_ids <- all_time_pay_class_ids(
#   time_data  = time1,
#   pay_data   = pay1,
#   class_data = class1,
#   output_dir = file.path(OUT_DIR, "qc")
# )
# 
# # Saves to:
# #   <CASE_DIR>/output/qc/All Time Pay and Class IDs.csv
# #   <CASE_DIR>/output/qc/All Time Pay and Class IDs.rds
# 
# 
# # 4) RETURN DATA ONLY (NO FILES WRITTEN) ---
# 
# all_ids <- all_time_pay_class_ids(
#   time_data  = time1,
#   pay_data   = pay1,
#   class_data = class1,
#   save_output = FALSE
# )
# 
# 
# # 5) CUSTOM OUTPUT FILENAME ---
# 
# all_ids <- all_time_pay_class_ids(
#   time_data  = time1,
#   pay_data   = pay1,
#   class_data = class1,
#   out_name   = "All_ID_Universe.csv"
# )

# SHIFT HOURS TABLE--------------------------------------------------------------------------------------------

shift_hrs_tbl <- function(data, 
                          punch_type_value = "in", 
                          output_file = "Shift_Hrs_Table.csv",
                          out_dir = NULL) {
  
  # --- Resolve output directory ---
  # Prefer explicit out_dir; else use global OUT_DIR; else fallback to getwd()/output
  if (is.null(out_dir) || !nzchar(out_dir)) {
    if (exists("OUT_DIR", inherits = TRUE) && nzchar(get("OUT_DIR", inherits = TRUE))) {
      out_dir <- get("OUT_DIR", inherits = TRUE)
    } else {
      out_dir <- file.path(getwd(), "output")
    }
  }
  out_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_path_csv <- file.path(out_dir, output_file)
  
  # Convert data to data.table
  data <- as.data.table(data)
  
  # Error handling for required columns
  required_cols <- c("shift_hrs", "ID_Date")
  if (!all(required_cols %in% names(data))) {
    stop(paste("The input data must contain", paste(required_cols, collapse = ", "), "columns."))
  }
  
  message("Data validation completed.")
  
  # Define bins for shift hours
  bins <- list(
    "NA" = is.na(data$shift_hrs),
    "Zero" = data$shift_hrs == 0,
    ">0 to <=8" = data$shift_hrs > 0 & data$shift_hrs <= 8,
    ">8 to <=12" = data$shift_hrs > 8 & data$shift_hrs <= 12,
    ">12 to <=16" = data$shift_hrs > 12 & data$shift_hrs <= 16,
    ">16 to <=20" = data$shift_hrs > 16 & data$shift_hrs <= 20,
    ">20" = data$shift_hrs > 20
  )
  
  # Count values and unique days for each bin
  results <- lapply(bins, function(cond) {
    list(
      Records = sum(cond, na.rm = TRUE),
      Days = if (sum(cond, na.rm = TRUE) > 0) length(unique(data$ID_Date[cond])) else 0
    )
  })
  
  # Create a data.table with the results
  summary_df <- data.table(
    Shift_Hrs = names(results),
    Records = sapply(results, "[[", "Records"),
    Days = sapply(results, "[[", "Days")
  )
  
  # Calculate total records and days
  total_records <- nrow(data)
  total_days <- length(unique(data$ID_Date))
  
  # Add Percent_of_Records and Percent_of_Days columns
  summary_df[, Percent_of_Records := round(Records / total_records, 10)]
  summary_df[, Percent_of_Days := ifelse(Records == 0, 0, round(Days / total_days, 10))]
  
  # Reorder columns
  summary_df <- summary_df[, .(Shift_Hrs, Records, Percent_of_Records, Days, Percent_of_Days)]
  
  # Write to CSV + RDS (your standard writer)
  write_csv_and_rds(summary_df, out_path_csv)
  
  # Print clean summary (no “Most common…” block)
  cat("\n=== SHIFT HOURS SUMMARY ===\n")
  cat("Total shifts analyzed:", format(total_records, big.mark=","), "\n")
  cat("Total unique days:", format(total_days, big.mark=","), "\n")
  cat("Saved to:", out_path_csv, "(.csv + .rds)\n\n")
  print(summary_df)
  cat("------------------------------------------------------\n")
  
  return(summary_df)
}


# NON WORK HOURS TABLE--------------------------------------------------------------------------------------------

non_wrk_hrs_tbl <- function(data, 
                            punch_type_value = "in", 
                            output_file = "Non_Work_Hrs_Table.csv",
                            out_dir = NULL) {
  
  # --- Resolve output directory ---
  # Prefer explicit out_dir; else use global OUT_DIR; else fallback to getwd()/output
  if (is.null(out_dir) || !nzchar(out_dir)) {
    if (exists("OUT_DIR", inherits = TRUE) && nzchar(get("OUT_DIR", inherits = TRUE))) {
      out_dir <- get("OUT_DIR", inherits = TRUE)
    } else {
      out_dir <- file.path(getwd(), "output")
    }
  }
  out_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_path_csv <- file.path(out_dir, output_file)
  
  # Convert data to data.table
  data <- as.data.table(data)
  
  # Error handling for required columns
  required_cols <- c("hrs_from_prev", "punch_type", "ID_Date")
  if (!all(required_cols %in% names(data))) {
    stop(paste("The input data must contain", paste(required_cols, collapse = ", "), "columns."))
  }
  
  message("Data validation completed.")
  
  # Filter data by punch_type_value
  filtered_data <- data[punch_type == punch_type_value]
  
  # Count the NA value records (kept for diagnostics)
  na_records <- sum(is.na(filtered_data$hrs_from_prev))
  
  # Define the bins
  bins <- list(
    "NA" = is.na(filtered_data$hrs_from_prev),
    "Zero" = filtered_data$hrs_from_prev == 0,
    ">0 to <=1" = filtered_data$hrs_from_prev > 0 & filtered_data$hrs_from_prev <= 1,
    ">1 to <=2" = filtered_data$hrs_from_prev > 1 & filtered_data$hrs_from_prev <= 2,
    ">2 to <=3" = filtered_data$hrs_from_prev > 2 & filtered_data$hrs_from_prev <= 3,
    ">3 to <=4" = filtered_data$hrs_from_prev > 3 & filtered_data$hrs_from_prev <= 4,
    ">4 to <=5" = filtered_data$hrs_from_prev > 4 & filtered_data$hrs_from_prev <= 5,
    ">5 to <=6" = filtered_data$hrs_from_prev > 5 & filtered_data$hrs_from_prev <= 6,
    ">6 to <=7" = filtered_data$hrs_from_prev > 6 & filtered_data$hrs_from_prev <= 7,
    ">7 to <=8" = filtered_data$hrs_from_prev > 7 & filtered_data$hrs_from_prev <= 8,
    ">8 to <=12" = filtered_data$hrs_from_prev > 8 & filtered_data$hrs_from_prev <= 12,
    ">12 to <=16" = filtered_data$hrs_from_prev > 12 & filtered_data$hrs_from_prev <= 16,
    ">16 to <=20" = filtered_data$hrs_from_prev > 16 & filtered_data$hrs_from_prev <= 20,
    ">20" = filtered_data$hrs_from_prev > 20
  )
  
  # Calculate counts and days
  results <- lapply(bins, function(cond) {
    list(
      Records = sum(cond, na.rm = TRUE),
      Days = if (sum(cond, na.rm = TRUE) > 0) length(unique(filtered_data$ID_Date[cond])) else 0
    )
  })
  
  # Create a data.table with the counts and unique days
  summary_df <- data.table(
    Non_Work_Hrs = names(results),
    Records = sapply(results, "[[", "Records"),
    Days = sapply(results, "[[", "Days")
  )
  
  # Calculate total records and days
  total_records <- nrow(filtered_data)
  total_days <- length(unique(filtered_data$ID_Date))
  
  # Add Percent_of_Records and Percent_of_Days columns
  summary_df[, Percent_of_Records := round(Records / total_records, 10)]
  summary_df[, Percent_of_Days := ifelse(Records == 0, 0, round(Days / total_days, 10))]
  
  # Reorder columns
  summary_df <- summary_df[, .(Non_Work_Hrs, Records, Percent_of_Records, Days, Percent_of_Days)]
  
  # Write CSV + RDS (your standard writer)
  write_csv_and_rds(summary_df, out_path_csv)
  
  # Print clean summary
  cat("\n=== NON-WORK HOURS SUMMARY ===\n")
  cat("Punch type filtered to:", punch_type_value, "\n")
  cat("Total records analyzed:", format(total_records, big.mark=","), "\n")
  cat("Total unique days:", format(total_days, big.mark=","), "\n")
  cat("NA hrs_from_prev records:", format(na_records, big.mark=","), "\n")
  cat("Saved to:", out_path_csv, "(.csv + .rds)\n\n")
  print(summary_df)
  cat("------------------------------------------------------\n")
  
  return(summary_df)
}


# MEAL PERIOD TABLE--------------------------------------------------------------------------------------------

meal_period_tbl <- function(data, 
                            output_file = "Meal_Period_Table.csv",
                            out_dir = NULL) {
  
  # --- Resolve output directory ---
  if (is.null(out_dir) || !nzchar(out_dir)) {
    if (exists("OUT_DIR", inherits = TRUE) && nzchar(get("OUT_DIR", inherits = TRUE))) {
      out_dir <- get("OUT_DIR", inherits = TRUE)
    } else {
      out_dir <- file.path(getwd(), "output")
    }
  }
  out_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path_csv <- file.path(out_dir, output_file)
  
  # Convert to data.table
  data <- as.data.table(data)
  
  # Error handling for required columns
  required_cols <- c("mp", "mp_hrs", "punch_type", "ID_Date")
  if (!all(required_cols %in% names(data))) {
    stop(paste("The input data must contain", paste(required_cols, collapse = ", "), "columns."))
  }
  
  message("Data validation completed.")
  
  # Filter for meal periods (mp == 1)
  meal_data <- data[mp == 1]
  
  # If no meal rows, still write an empty shell (optional but consistent)
  if (nrow(meal_data) == 0) {
    summary_df <- data.table(
      Meal_Period_Hrs = c("NA","Zero",">0 to <0.5","0.5 (30 min)",">0.5 to <=1",">1 to <=1.5",">1.5"),
      Records = 0L,
      Percent_of_Records = 0,
      Days = 0L,
      Percent_of_Days = 0
    )
    write_csv_and_rds(summary_df, out_path_csv)
    
    cat("\n=== MEAL PERIOD SUMMARY ===\n")
    cat("Meal rows found (mp==1): 0\n")
    cat("Saved to:", out_path_csv, "(.csv + .rds)\n")
    cat("------------------------------------------------------\n")
    return(summary_df)
  }
  
  # Define bins for meal period hours
  bins <- list(
    "NA" = is.na(meal_data$mp_hrs),
    "Zero" = meal_data$mp_hrs == 0,
    ">0 to <0.5" = meal_data$mp_hrs > 0 & meal_data$mp_hrs < 0.5,
    "0.5 (30 min)" = meal_data$mp_hrs == 0.5,
    ">0.5 to <=1" = meal_data$mp_hrs > 0.5 & meal_data$mp_hrs <= 1,
    ">1 to <=1.5" = meal_data$mp_hrs > 1 & meal_data$mp_hrs <= 1.5,
    ">1.5" = meal_data$mp_hrs > 1.5
  )
  
  # Count values and unique days for each bin
  results <- lapply(bins, function(cond) {
    list(
      Records = sum(cond, na.rm = TRUE),
      Days = if (sum(cond, na.rm = TRUE) > 0) length(unique(meal_data$ID_Date[cond])) else 0
    )
  })
  
  # Create a data.table with the results
  summary_df <- data.table(
    Meal_Period_Hrs = names(results),
    Records = as.integer(sapply(results, "[[", "Records")),
    Days = as.integer(sapply(results, "[[", "Days"))
  )
  
  # Calculate total records and days
  total_records <- nrow(meal_data)
  total_days <- length(unique(meal_data$ID_Date))
  
  # Add Percent_of_Records and Percent_of_Days columns
  summary_df[, Percent_of_Records := round(Records / total_records, 10)]
  summary_df[, Percent_of_Days := ifelse(Records == 0, 0, round(Days / total_days, 10))]
  
  # Reorder columns
  summary_df <- summary_df[, .(Meal_Period_Hrs, Records, Percent_of_Records, Days, Percent_of_Days)]
  
  # Write to CSV + RDS
  write_csv_and_rds(summary_df, out_path_csv)
  
  # Print clean summary (no “most common buckets” nonsense)
  cat("\n=== MEAL PERIOD SUMMARY ===\n")
  cat("Meal rows found (mp==1):", format(total_records, big.mark=","), "\n")
  cat("Unique days with any meal rows:", format(total_days, big.mark=","), "\n")
  cat("Saved to:", out_path_csv, "(.csv + .rds)\n\n")
  print(summary_df)
  cat("------------------------------------------------------\n")
  
  return(summary_df)
}


# MEAL START TIME FREQUENCY TABLE ----------------------------

meal_start_time_tbl <- function(data, 
                                punch_type_value = NULL,  # optional filter if you ever need it
                                output_path = file.path(resolve_out_dir(), "Meal_Start_Time_Table.csv"),
                                top_n = 20) {
  
  # Convert to data.table
  data <- as.data.table(data)
  
  # Error handling for required columns
  required_cols <- c("mp_hrs", "punch_time", "ID_Date")
  if (!all(required_cols %in% names(data))) {
    stop(paste("The input data must contain", paste(required_cols, collapse = ", "), "columns."))
  }
  
  message("Data validation completed.")
  
  # Optional punch_type filter (kept harmless / off by default)
  if (!is.null(punch_type_value) && "punch_type" %in% names(data)) {
    data <- data[punch_type == punch_type_value]
  }
  
  # Filter for rows with meal periods (mp_hrs > 0)
  meal_data <- data[mp_hrs > 0]
  
  if (nrow(meal_data) == 0) {
    message("No meal periods found")
    return(NULL)
  }
  
  # Calculate meal START time
  meal_data[, meal_start := punch_time - lubridate::minutes(round(mp_hrs * 60))]
  
  # Extract TIME component (HH:MM:SS)
  meal_data[, start_time_exact := format(meal_start, "%H:%M:%S")]
  
  # Count frequency by exact time
  time_counts <- meal_data[, .(
    Records = .N,
    Days = uniqueN(ID_Date, na.rm = TRUE)
  ), by = start_time_exact]
  
  setorder(time_counts, -Records)
  
  # Top N + Other
  if (nrow(time_counts) > top_n) {
    top_rows <- time_counts[1:top_n]
    other_counts <- time_counts[(top_n+1):.N]
    
    other_row <- data.table(
      start_time_exact = "Other",
      Records = sum(other_counts$Records),
      Days = uniqueN(meal_data[!start_time_exact %in% top_rows$start_time_exact, ID_Date], na.rm = TRUE)
    )
    
    summary_df <- rbindlist(list(top_rows, other_row))
  } else {
    summary_df <- time_counts
  }
  
  # Totals
  total_records <- nrow(meal_data)
  total_days <- uniqueN(meal_data$ID_Date, na.rm = TRUE)
  
  # Percentages
  summary_df[, `:=`(
    Percent_of_Records = round(Records / total_records, 10),
    Percent_of_Days    = round(Days / total_days, 10)
  )]
  
  # Rename + order
  setnames(summary_df, "start_time_exact", "Meal_Start_Time")
  summary_df <- summary_df[, .(Meal_Start_Time, Records, Percent_of_Records, Days, Percent_of_Days)]
  
  # Write to CSV + RDS
  write_csv_and_rds(summary_df, output_path)
  
  # Clean summary (no “most common buckets” spam)
  cat("\n=== MEAL START TIME SUMMARY ===\n")
  cat("Meal rows found (mp_hrs > 0):", format(total_records, big.mark=","), "\n")
  cat("Unique days with any meal rows:", format(total_days, big.mark=","), "\n")
  cat("Top N:", top_n, "\n")
  cat("Saved to:", output_path, "(.csv + .rds)\n\n")
  print(summary_df)
  cat("------------------------------------------------------\n")
  
  return(summary_df)
}


# MEAL QUARTER HOUR ANALYSIS TABLE ----------------------------

meal_quarter_hour_tbl <- function(data, 
                                  output_path = file.path(resolve_out_dir(), "Meal_Quarter_Hour_Table.csv")) {
  
  # Convert to data.table
  data <- as.data.table(data)
  
  # Error handling for required columns
  required_cols <- c("mp_hrs", "punch_time", "ID_Date")
  if (!all(required_cols %in% names(data))) {
    stop(paste("The input data must contain", paste(required_cols, collapse = ", "), "columns."))
  }
  
  message("Data validation completed.")
  
  # Filter for rows with meal periods (mp_hrs > 0)
  meal_data <- data[mp_hrs > 0]
  
  if (nrow(meal_data) == 0) {
    message("No meal periods found")
    return(NULL)
  }
  
  # Calculate meal START time
  meal_data[, meal_start := punch_time - lubridate::minutes(round(mp_hrs * 60))]
  
  # Extract minutes and categorize
  meal_data[, start_minute := lubridate::minute(meal_start)]
  
  # Define bins for quarter hour analysis
  bins <- list(
    ":00 (On the hour)"      = meal_data$start_minute == 0,
    ":15 (Quarter past)"     = meal_data$start_minute == 15,
    ":30 (Half past)"        = meal_data$start_minute == 30,
    ":45 (Quarter to)"       = meal_data$start_minute == 45,
    "Other (Off quarter)"    = !meal_data$start_minute %in% c(0, 15, 30, 45)
  )
  
  # Count values and unique days for each bin
  results <- lapply(bins, function(cond) {
    list(
      Records = sum(cond, na.rm = TRUE),
      Days = if (sum(cond, na.rm = TRUE) > 0) length(unique(meal_data$ID_Date[cond])) else 0
    )
  })
  
  # Create a data.table with the results
  summary_df <- data.table(
    Quarter_Hour_Type = names(results),
    Records = sapply(results, "[[", "Records"),
    Days = sapply(results, "[[", "Days")
  )
  
  # Totals
  total_records <- nrow(meal_data)
  total_days <- length(unique(meal_data$ID_Date))
  
  # Add Percent_of_Records and Percent_of_Days (as proportions, consistent with your other tables)
  summary_df[, Percent_of_Records := round(Records / total_records, 10)]
  summary_df[, Percent_of_Days    := fifelse(Records == 0, 0, round(Days / total_days, 10))]
  
  # Summary rows: ANY QUARTER HOUR
  any_quarter <- summary_df[Quarter_Hour_Type != "Other (Off quarter)"]
  any_quarter_total <- data.table(
    Quarter_Hour_Type   = "ANY QUARTER HOUR",
    Records             = sum(any_quarter$Records),
    Days                = uniqueN(meal_data[start_minute %in% c(0, 15, 30, 45), ID_Date], na.rm = TRUE),
    Percent_of_Records  = round(sum(any_quarter$Records) / total_records, 10),
    Percent_of_Days     = round(uniqueN(meal_data[start_minute %in% c(0, 15, 30, 45), ID_Date], na.rm = TRUE) / total_days, 10)
  )
  
  # Separator row
  separator <- data.table(
    Quarter_Hour_Type  = "---",
    Records            = NA,
    Percent_of_Records = NA,
    Days               = NA,
    Percent_of_Days    = NA
  )
  
  # Combine
  summary_df <- rbindlist(list(summary_df, separator, any_quarter_total), fill = TRUE)
  summary_df <- summary_df[, .(Quarter_Hour_Type, Records, Percent_of_Records, Days, Percent_of_Days)]
  
  # Write to CSV + RDS
  write_csv_and_rds(summary_df, output_path)
  
  # Clean summary print (no “most common buckets” spam)
  off_row <- summary_df[Quarter_Hour_Type == "Other (Off quarter)"]
  cat("\n=== MEAL QUARTER HOUR SUMMARY ===\n")
  cat("Meal rows found (mp_hrs > 0):", format(total_records, big.mark=","), "\n")
  cat("Unique days with any meal rows:", format(total_days, big.mark=","), "\n")
  cat("On ANY quarter hour:", format(any_quarter_total$Records, big.mark=","), 
      " (", round(any_quarter_total$Percent_of_Records * 100, 2), "%)\n", sep = "")
  if (nrow(off_row) == 1) {
    cat("Off quarter hours:", format(off_row$Records, big.mark=","), 
        " (", round(off_row$Percent_of_Records * 100, 2), "%)\n", sep = "")
  }
  cat("Saved to:", output_path, "(.csv + .rds)\n")
  cat("------------------------------------------------------\n")
  
  return(summary_df)
}


# AGGREGATE DATA ------------------------------------------------------------------------------

aggregate_data <- function(dt,
                           by,
                           first_fields = NULL,
                           sum_fields = NULL,
                           max_fields = NULL,
                           min_fields = NULL,
                           mean_fields = NULL,
                           median_fields = NULL) {
  setDT(dt)
  
  first_fields  <- if (is.null(first_fields))  if (exists("first_fields_default"))  first_fields_default  else character(0) else first_fields
  sum_fields    <- if (is.null(sum_fields))    if (exists("sum_fields_default"))    sum_fields_default    else character(0) else sum_fields
  max_fields    <- if (is.null(max_fields))    if (exists("max_fields_default"))    max_fields_default    else character(0) else max_fields
  min_fields    <- if (is.null(min_fields))    if (exists("min_fields_default"))    min_fields_default    else character(0) else min_fields
  mean_fields   <- if (is.null(mean_fields))   if (exists("mean_fields_default"))   mean_fields_default   else character(0) else mean_fields
  median_fields <- if (is.null(median_fields)) if (exists("median_fields_default")) median_fields_default else character(0) else median_fields
  
  all_fields <- c(first_fields, sum_fields, max_fields, min_fields, mean_fields, median_fields)
  
  missing_fields <- setdiff(all_fields, names(dt))
  if (length(missing_fields) > 0) {
    warning(sprintf("Skipping missing fields: %s", paste(missing_fields, collapse = ", ")))
    first_fields  <- setdiff(first_fields,  missing_fields)
    sum_fields    <- setdiff(sum_fields,    missing_fields)
    max_fields    <- setdiff(max_fields,    missing_fields)
    min_fields    <- setdiff(min_fields,    missing_fields)
    mean_fields   <- setdiff(mean_fields,   missing_fields)
    median_fields <- setdiff(median_fields, missing_fields)
  }
  
  missing_by <- setdiff(by, names(dt))
  if (length(missing_by) > 0) stop(sprintf("Missing grouping fields: %s", paste(missing_by, collapse = ", ")))
  
  unaggregated_fields <- setdiff(names(dt), c(by, all_fields))
  if (length(unaggregated_fields) > 0) {
    message(sprintf("Warning: ignored fields not aggregated: %s", paste(unaggregated_fields, collapse = ", ")))
  }
  
  # --- Identify date-like fields present in dt ---
  is_date_like <- function(x) inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")
  date_like_cols <- names(dt)[vapply(dt, is_date_like, logical(1))]
  
  max_date_fields <- intersect(max_fields, date_like_cols)
  min_date_fields <- intersect(min_fields, date_like_cols)
  
  # For date-like columns, handle NA safely without changing class
  max_num_fields <- setdiff(max_fields, max_date_fields)
  min_num_fields <- setdiff(min_fields, min_date_fields)
  
  message("Starting aggregation...")
  start_time <- Sys.time()
  
  aggregation_expr <- list()
  
  # first() fields (keep original type)
  if (length(first_fields) > 0) {
    aggregation_expr <- c(
      aggregation_expr,
      setNames(lapply(first_fields, function(f)
        substitute(first(x), list(x = as.name(f)))
      ), first_fields)
    )
  }
  
  # sum() numeric -> double
  if (length(sum_fields) > 0) {
    aggregation_expr <- c(
      aggregation_expr,
      setNames(lapply(sum_fields, function(f)
        substitute(
          fifelse(all(is.na(x)), NA_real_, as.double(sum(x, na.rm = TRUE))),
          list(x = as.name(f))
        )
      ), paste0(sum_fields, "_sum"))
    )
  }
  
  # max() date-like -> preserve class
  if (length(max_date_fields) > 0) {
    aggregation_expr <- c(
      aggregation_expr,
      setNames(lapply(max_date_fields, function(f)
        substitute(
          { y <- x[!is.na(x)]; if (length(y) == 0L) as.Date(NA) else max(y) },
          list(x = as.name(f))
        )
      ), paste0(max_date_fields, "_max"))
    )
  }
  
  # max() numeric -> double
  if (length(max_num_fields) > 0) {
    aggregation_expr <- c(
      aggregation_expr,
      setNames(lapply(max_num_fields, function(f)
        substitute(
          fifelse(all(is.na(x)), NA_real_, as.double(suppressWarnings(max(x, na.rm = TRUE)))),
          list(x = as.name(f))
        )
      ), paste0(max_num_fields, "_max"))
    )
  }
  
  # min() date-like -> preserve class
  if (length(min_date_fields) > 0) {
    aggregation_expr <- c(
      aggregation_expr,
      setNames(lapply(min_date_fields, function(f)
        substitute(
          { y <- x[!is.na(x)]; if (length(y) == 0L) as.Date(NA) else min(y) },
          list(x = as.name(f))
        )
      ), paste0(min_date_fields, "_min"))
    )
  }
  
  # min() numeric -> double
  if (length(min_num_fields) > 0) {
    aggregation_expr <- c(
      aggregation_expr,
      setNames(lapply(min_num_fields, function(f)
        substitute(
          fifelse(all(is.na(x)), NA_real_, as.double(suppressWarnings(min(x, na.rm = TRUE)))),
          list(x = as.name(f))
        )
      ), paste0(min_num_fields, "_min"))
    )
  }
  
  # mean() numeric -> double
  if (length(mean_fields) > 0) {
    aggregation_expr <- c(
      aggregation_expr,
      setNames(lapply(mean_fields, function(f)
        substitute(
          fifelse(all(is.na(x)), NA_real_, as.double(mean(x, na.rm = TRUE))),
          list(x = as.name(f))
        )
      ), paste0(mean_fields, "_mean"))
    )
  }
  
  # median() numeric -> double
  if (length(median_fields) > 0) {
    aggregation_expr <- c(
      aggregation_expr,
      setNames(lapply(median_fields, function(f)
        substitute(
          fifelse(all(is.na(x)), NA_real_, as.double(median(x, na.rm = TRUE))),
          list(x = as.name(f))
        )
      ), paste0(median_fields, "_median"))
    )
  }
  
  aggregation_call <- as.call(c(quote(list), aggregation_expr))
  result <- dt[, eval(aggregation_call), by = by]
  
  # Replace Inf/-Inf only in numeric cols (Date/POSIX won't be numeric here)
  num_cols <- names(result)[vapply(result, is.numeric, logical(1))]
  if (length(num_cols) > 0) {
    result[, (num_cols) := lapply(.SD, function(x) fifelse(is.finite(x), x, NA_real_)), .SDcols = num_cols]
  }
  
  end_time <- Sys.time()
  message(sprintf("Aggregation completed in %.2f seconds.", as.numeric(end_time - start_time, units = "secs")))
  
  result
}


# REMOVE COLUMN NAME SUFFIXES (after aggregate data step, if needed)
remove_suffixes <- function(dt, suffixes) {
  setDT(dt)
  setnames(dt, old = names(dt), new = sub(paste0("(", paste0(suffixes, collapse = "|"), ")$"), "", names(dt)))
  dt
}

# Example usage:
# # Suffixes you want to remove
# suffixes_to_remove <- c("_sum", "_max", "_min", "_mean")
# 
# # Remove suffixes
# dt_clean <- remove_suffixes(dt, suffixes_to_remove)



# RANDOM SAMPLE  (if needed) ------------------------------------------------------------------------------

generate_random_sample <- function(
    all_ids, 
    class1 = NULL, 
    case_name = NULL, 
    pct = NULL, 
    use_class1 = TRUE,
    seed_num = 99999,
    output_dir = NULL        # <-- NEW (defaults to case OUT_DIR if NULL)
) {
  
  # ----------------- PATH HANDLING (ABSOLUTE) -----------------
  
  # If user did NOT supply an output_dir, use case OUT_DIR if it exists
  if (is.null(output_dir)) {
    if (exists("OUT_DIR", inherits = TRUE)) {
      output_dir <- OUT_DIR
    } else {
      # fallback: make a local absolute "output" folder
      output_dir <- normalizePath("output", winslash = "/", mustWork = FALSE)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
  } else {
    # ensure absolute path
    output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # ----------------- PARAMETER CHECKS -----------------
  
  if (is.null(case_name)) {
    stop("Please provide a case_name parameter (e.g., 'Plaintiff v Defendant')")
  }
  
  if (is.null(pct)) {
    stop("Please provide a pct parameter (e.g., 0.25 for 25%)")
  }
  
  if (pct <= 0 || pct > 1) {
    stop("Sample percentage (pct) must be between 0 and 1 (e.g., 0.25 for 25%)")
  }
  
  if (use_class1 && is.null(class1)) {
    stop("class1 is required when use_class1 = TRUE")
  }
  
  # ----------------- START -----------------
  
  start_time <- Sys.time()
  
  cat("\n========== STARTING RANDOM SAMPLE ==========\n")
  cat("Case Name:", case_name, "\n")
  cat("Sample Percentage:", sprintf("%.1f%%", pct * 100), "\n")
  cat("Class List:", use_class1, "\n")
  cat("Random Seed:", seed_num, "\n")
  cat("Output Dir:", output_dir, "\n")
  cat("===========================================\n\n")
  
  # ----------------- POPULATION COUNTS -----------------
  
  time_population  <- all_ids[grepl("Time Data", Source), uniqueN(ID, na.rm = TRUE)]
  pay_population   <- all_ids[grepl("Pay Data", Source), uniqueN(ID, na.rm = TRUE)]
  class1_population <- if (use_class1) uniqueN(class1$ID, na.rm = TRUE) else NA
  
  both_time_pay <- all_ids[Source == "Time Data; Pay Data"]
  
  if (nrow(both_time_pay) == 0) {
    stop("No IDs found with Source = 'Time Data; Pay Data'!")
  }
  
  if (use_class1) {
    ids_in_all_sources <- both_time_pay[ID %in% class1$ID, unique(ID)]
    source_description <- "all three sources (Time, Pay and EE List)"
  } else {
    ids_in_all_sources <- unique(both_time_pay$ID)
    source_description <- "both Time and Pay"
  }
  
  if (length(ids_in_all_sources) == 0) {
    stop(paste0("No IDs found in ", source_description, "!"))
  }
  
  # ----------------- SAMPLING -----------------
  
  full_list <- data.table(ID = ids_in_all_sources)
  
  set.seed(seed_num)
  full_list[, rand := sample(.N)]
  
  samplesize <- ceiling(pct * nrow(full_list))
  
  if (samplesize > nrow(full_list)) {
    warning(sprintf(
      "Sample size (%s) exceeds population (%s). Using full population.",
      samplesize, nrow(full_list)
    ))
    samplesize <- nrow(full_list)
  }
  
  sample_list <- full_list[rand <= samplesize]
  
  pct_text <- paste0(pct * 100, " percent")
  
  # ----------------- OUTPUT FILE PATHS (ABSOLUTE) -----------------
  
  sample_file <- file.path(
    output_dir,
    paste0(case_name, " - ", pct_text, " sample.xlsx")
  )
  
  full_file <- file.path(
    output_dir,
    paste0(case_name, " - ", pct_text, " sample w rand_id.xlsx")
  )
  
  write.xlsx(sample_list, sample_file)
  write.xlsx(full_list, full_file)
  
  # ----------------- SUMMARY TO CONSOLE -----------------
  
  cat("\n========== POPULATION SUMMARY ==========\n")
  cat("Case:", case_name, "\n")
  cat("Random Seed:", seed_num, "\n")
  cat("Employee List Used:", use_class1, "\n\n")
  
  cat("INITIAL POPULATIONS:\n")
  cat(sprintf("  - Time Data IDs: %s\n", format(time_population, big.mark = ",")))
  cat(sprintf("  - Pay Data IDs: %s\n", format(pay_population, big.mark = ",")))
  if (use_class1) {
    cat(sprintf("  - Employee List IDs: %s\n", format(class1_population, big.mark = ",")))
  }
  cat(sprintf("  - IDs in both Time AND Pay: %s\n", format(nrow(both_time_pay), big.mark = ",")))
  
  cat(sprintf("\nFINAL POPULATION (%s):\n", source_description))
  cat(sprintf("  - Total IDs: %s\n", format(length(ids_in_all_sources), big.mark = ",")))
  
  cat("\nSAMPLE DETAILS:\n")
  cat(sprintf("  - Sample Size: %s\n", format(samplesize, big.mark = ",")))
  cat(sprintf(
    "  - Sample Percentage: %.1f%% (exact: %.2f%%)\n",
    pct * 100,
    (samplesize / length(ids_in_all_sources)) * 100
  ))
  
  cat("\nOUTPUT FILES:\n")
  cat(sprintf("  1. %s\n", sample_file))
  cat(sprintf("  2. %s\n", full_file))
  
  cat(sprintf(
    "\nExecution time: %.2f seconds\n",
    difftime(Sys.time(), start_time, units = "secs")
  ))
  cat("========================================\n")
  
  invisible(list(
    full_list = full_list,
    sample_list = sample_list,
    ids_in_all_sources = ids_in_all_sources,
    samplesize = samplesize,
    sample_file = sample_file,
    full_file = full_file,
    use_class1 = use_class1
  ))
}

# generate_random_sample() — USAGE NOTES (updated for absolute paths)
#
# Assumptions at this point in your script:
#   - case_name is already defined (string)
#   - all_ids exists (output of all_time_pay_class_ids())
#   - class1 exists if you plan to use it (use_class1 = TRUE)
#   - OUT_DIR exists (from init_case_paths()) and is an ABSOLUTE path
#
# IMPORTANT CHANGE:
#   - Outputs now default to OUT_DIR (absolute) via output_dir = NULL
#   - You can override output location with output_dir = "C:/.../some/folder"

# REQUIRED: Set your parameters here
# pct <- 0.25                       # 0.25 = 25% sample (must be > 0 and <= 1)
# seed_num <- 99999                 # reproducible sample

# Option 1 (default): WITH Class1 List (match Time + Pay + Class)
# results <- generate_random_sample(
#   all_ids    = all_ids,
#   class1     = class1,
#   case_name  = case_name,         # already defined earlier in script
#   pct        = pct,
#   use_class1 = TRUE,              # TRUE = match all three sources
#   seed_num   = seed_num
#   # output_dir = NULL             # default -> OUT_DIR (absolute)
# )

# Example 1: WITHOUT Class1 List (only match Time and Pay)
# results_no_class1 <- generate_random_sample(
#   all_ids    = all_ids,
#   class1     = NULL,              # can be NULL when use_class1 = FALSE
#   case_name  = case_name,         # keep your existing case_name
#   pct        = 0.30,
#   use_class1 = FALSE,             # FALSE = only match Time and Pay
#   seed_num   = 99999
#   # output_dir = NULL             # default -> OUT_DIR
# )

# Example 2: With Class1 List but 10% sample, custom seed
# results_10pct <- generate_random_sample(
#   all_ids    = all_ids,
#   class1     = class1,
#   case_name  = case_name,         # keep your existing case_name
#   pct        = 0.10,
#   use_class1 = TRUE,
#   seed_num   = 12345
#   # output_dir = NULL             # default -> OUT_DIR
# )

# Example 3: Force outputs to a specific folder (absolute path override)
# results_custom_dir <- generate_random_sample(
#   all_ids    = all_ids,
#   class1     = class1,
#   case_name  = case_name,
#   pct        = 0.25,
#   use_class1 = TRUE,
#   seed_num   = 99999,
#   output_dir = file.path(OUT_DIR, "qc")   # writes under output/qc (absolute)
# )

# Outputs created (two Excel files):
#   1) "<case_name> - <pct*100> percent sample.xlsx"
#   2) "<case_name> - <pct*100> percent sample w rand_id.xlsx"
#
# Returned object (invisibly) includes:
#   results$sample_list, results$full_list, results$samplesize,
#   results$sample_file, results$full_file, etc.

# GENERATE RANDOM SAMPLE PRODUCTION FILES (if needed) ------------------------------------------------------------------------------
#   - time/pay: filtered to sample + filtered to class_dmgs_start_date
#   - class list: NEVER filtered (full class1), but includes Class_Anon_ID if you want it
#   - outputs: .xlsx files saved to <OUT_DIR>/prod (absolute) by default
#   - headers: underscores -> spaces, Proper Case; Pay_/Class_ removed in pay/class exports

# GENERATE RANDOM SAMPLE PRODUCTION FILES (if needed) ------------------------------------------------------------------------------
#   - time/pay: filtered to sample + filtered to class_dmgs_start_date
#   - class list: NEVER filtered (full class1), but includes Class_Anon_ID if you want it
#   - outputs: .xlsx files saved to <OUT_DIR>/prod (absolute) by default
#   - headers: underscores -> spaces, Proper Case; Pay_/Class_ removed in pay/class exports

# Helper functions: Validate output fields + Prettify Excel headers

validate_prod_fields <- function(dt, fields, dt_name = deparse(substitute(dt))) {
  if (is.null(fields) || length(fields) == 0) {
    stop(dt_name, ": fields vector is empty.")
  }
  
  missing <- setdiff(fields, names(dt))
  if (length(missing) > 0) {
    stop(
      dt_name, ": missing required columns for output:\n  - ",
      paste(missing, collapse = "\n  - "),
      "\n\nAvailable columns:\n  - ",
      paste(names(dt), collapse = "\n  - ")
    )
  }
  
  invisible(TRUE)
}

prep_xlsx_output <- function(x, drop_prefixes = character()) {
  
  # Copy so we never mutate analysis tables
  y <- as.data.table(copy(x))
  nms <- names(y)
  
  # Remove prefixes like "Pay_" or "Class_"
  if (length(drop_prefixes) > 0) {
    for (p in drop_prefixes) {
      nms <- sub(paste0("^", p), "", nms)
    }
  }
  
  # Underscores -> spaces
  nms <- gsub("_+", " ", nms)
  
  # Proper case
  if (requireNamespace("stringr", quietly = TRUE)) {
    nms <- stringr::str_to_title(nms)
  } else {
    # base fallback
    nms <- tools::toTitleCase(tolower(nms))
  }
  
  setnames(y, nms)
  y
}

generate_production_files <- function(time1, 
                                      pay1, 
                                      sample_list,
                                      class1 = NULL,
                                      case_name, 
                                      class_dmgs_start_date,
                                      overwrite = FALSE,
                                      prod_fields_time  = default_prod_fields_time,
                                      prod_fields_pay   = default_prod_fields_pay,
                                      prod_fields_class = default_prod_fields_class) {
  
  cat("\n========== CREATING PRODUCTION FILES ==========\n")
  
  # Ensure data.tables
  setDT(time1); setDT(pay1)
  if (!is.null(class1)) setDT(class1)
  setDT(sample_list)
  
  # Assign anonymized IDs for SAMPLE ONLY (time/pay are filtered later)
  time1[, Anon_ID := NA_real_]
  time1[sample_list, Anon_ID := i.rand, on = .(ID)]
  
  pay1[, Pay_Anon_ID := NA_real_]
  pay1[sample_list, Pay_Anon_ID := i.rand, on = .(Pay_ID = ID)]
  
  # Class list: NEVER filtered, but can include sample anon id column
  if (!is.null(class1)) {
    class1[, Class_Anon_ID := NA_real_]
    class1[sample_list, Class_Anon_ID := i.rand, on = .(ID)]
  }
  
  # Build production time data (sample + date filter)
  time1_prod <- time1[!is.na(Anon_ID) & Date >= class_dmgs_start_date]
  validate_prod_fields(time1_prod, prod_fields_time, "time1 (pre-subset)")
  time1_prod <- time1_prod[, ..prod_fields_time]
  if ("punch_type" %in% names(time1_prod) && "Hours" %in% names(time1_prod)) {
    time1_prod[punch_type == "out", Hours := NA]
  }
  if (all(c("Anon_ID", "Date") %in% names(time1_prod))) {
    setorder(time1_prod, Anon_ID, Date)
  }
  
  # Build production pay data (sample + date filter)
  pay1_prod <- pay1[!is.na(Pay_Anon_ID) & Pay_Date >= class_dmgs_start_date]
  validate_prod_fields(pay1_prod, prod_fields_pay, "pay1 (pre-subset)")
  pay1_prod <- pay1_prod[, ..prod_fields_pay]
  if (all(c("Pay_Anon_ID", "Pay_Date") %in% names(pay1_prod))) {
    setorder(pay1_prod, Pay_Anon_ID, Pay_Date)
  }
  
  # Build production class list (FULL class list; no sample/date filtering)
  class1_prod <- NULL
  if (!is.null(class1)) {
    validate_prod_fields(class1, prod_fields_class, "class1 (full list)")
    class1_prod <- class1[, ..prod_fields_class]
    if ("Class_Anon_ID" %in% names(class1_prod)) setorder(class1_prod, Class_Anon_ID)
    else if ("ID" %in% names(class1_prod)) setorder(class1_prod, ID)
  }
  
  # Prod directory (absolute): default to <OUT_DIR>/prod if OUT_DIR exists
  if (exists("OUT_DIR", inherits = TRUE) && nzchar(get("OUT_DIR", inherits = TRUE))) {
    prod_dir <- file.path(get("OUT_DIR", inherits = TRUE), "prod")
  } else {
    prod_dir <- normalizePath("prod", winslash = "/", mustWork = FALSE)
  }
  dir.create(prod_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Filenames
  date_suffix <- format(as.Date(class_dmgs_start_date), "(%Y-%m-%d)")
  time_file  <- file.path(prod_dir, paste0(case_name, " Sample Time Data ",  date_suffix, ".xlsx"))
  pay_file   <- file.path(prod_dir, paste0(case_name, " Sample Pay Data ",   date_suffix, ".xlsx"))
  class_file <- file.path(prod_dir, paste0(case_name, " Class List ",        date_suffix, ".xlsx"))
  
  # Prep Excel-friendly headers (DO NOT mutate originals)
  time_xlsx  <- prep_xlsx_output(time1_prod)                                   # keep Anon_/etc
  pay_xlsx   <- prep_xlsx_output(pay1_prod,   drop_prefixes = c("Pay_"))       # remove Pay_
  class_xlsx <- if (!is.null(class1_prod)) prep_xlsx_output(class1_prod, drop_prefixes = c("Class_")) else NULL
  
  # Write time file
  wb_time <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb_time, "Sample Time Data")
  openxlsx::writeData(wb_time, 1, time_xlsx, rowNames = FALSE)
  openxlsx::saveWorkbook(wb_time, time_file, overwrite = overwrite)
  
  # Write pay file
  wb_pay <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb_pay, "Sample Pay Data")
  openxlsx::writeData(wb_pay, 1, pay_xlsx, rowNames = FALSE)
  openxlsx::saveWorkbook(wb_pay, pay_file, overwrite = overwrite)
  
  # Write class file (full list)
  if (!is.null(class_xlsx)) {
    wb_class <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb_class, "Class List")
    openxlsx::writeData(wb_class, 1, class_xlsx, rowNames = FALSE)
    openxlsx::saveWorkbook(wb_class, class_file, overwrite = overwrite)
  }
  
  # Summary counts
  sample_count      <- nrow(sample_list)
  time_unique_count <- if (nrow(time1_prod) > 0 && prod_fields_time[1] %in% names(time1_prod)) uniqueN(time1_prod[[prod_fields_time[1]]], na.rm = TRUE) else 0
  pay_unique_count  <- if (nrow(pay1_prod)  > 0 && prod_fields_pay[1]  %in% names(pay1_prod))  uniqueN(pay1_prod[[prod_fields_pay[1]]], na.rm = TRUE)  else 0
  
  cat("\nPRODUCTION SUMMARY:\n")
  cat(sprintf("time records:  %s\n", format(nrow(time1_prod), big.mark=",")))
  cat(sprintf("pay records:   %s\n", format(nrow(pay1_prod),  big.mark=",")))
  if (!is.null(class1_prod)) cat(sprintf("class records: %s\n", format(nrow(class1_prod), big.mark=",")))
  
  cat("\nSAMPLE VERIFICATION:\n")
  cat(sprintf("sample size: %s employees\n", format(sample_count, big.mark=",")))
  if (sample_count > 0) {
    cat(sprintf("in time data:  %s (%.1f%%)\n", time_unique_count, time_unique_count/sample_count*100))
    cat(sprintf("in pay data:   %s (%.1f%%)\n", pay_unique_count,  pay_unique_count/sample_count*100))
  }
  
  if (time_unique_count < sample_count) cat(sprintf("⚠ missing from time:  %s\n", sample_count - time_unique_count))
  if (pay_unique_count  < sample_count) cat(sprintf("⚠ missing from pay:   %s\n", sample_count - pay_unique_count))
  
  cat("\nFILES CREATED:\n")
  cat(" - ", time_file, "\n", sep="")
  cat(" - ", pay_file,  "\n", sep="")
  if (!is.null(class1_prod)) cat(" - ", class_file, "\n", sep="")
  
  if (!overwrite) cat("\n(overwrite = FALSE: existing files preserved)\n")
  cat("===========================================\n")
  
  invisible(list(
    time1_prod  = time1_prod,
    pay1_prod   = pay1_prod,
    class1_prod = class1_prod,
    time_file   = time_file,
    pay_file    = pay_file,
    class_file  = if (!is.null(class1_prod)) class_file else NA_character_
  ))
}

# USAGE EXAMPLES

# 1) Define your output field vectors in the MAIN SCRIPT (edit per case)
# default_prod_fields_time  <- c("Anon_ID","ID","Name","Date","punch_type","In","Out","Hours")
# default_prod_fields_pay   <- c("Pay_Anon_ID","Pay_ID","Pay_Name","Pay_Date","Pay_Period_End","Pay_Code","Pay_Hours","Pay_Amount")
# default_prod_fields_class <- c("Class_Anon_ID","ID","Name","Hire_Date","Term_Date")

# 2) Run production export (writes XLSX files to: file.path(OUT_DIR, "prod"))
# prod_out <- generate_production_files(
#   time1 = time1,
#   pay1  = pay1,
#   sample_list = sample_list,     # from generate_random_sample() result
#   class1 = class1,               # full list; not filtered
#   case_name = case_name,
#   class_dmgs_start_date = class_dmgs_start_date,
#   overwrite = FALSE,
#   prod_fields_time  = default_prod_fields_time,
#   prod_fields_pay   = default_prod_fields_pay,
#   prod_fields_class = default_prod_fields_class
# )



# GENERATE METADATA FILES FOR POWERQUERY ------------------------------------------------------------------------------

generate_metadata <- function(data, file_name,
                              output_path = file.path(resolve_out_dir(), file_name)) {
  
  # Ensure data frame
  data <- as.data.frame(data)
  
  # Define explicit columns to be treated as Date
  explicit_date_cols <- c(
    "Pay_Date", "Pay_Period_Beg", "Pay_Period_End",
    "Period_End", "Date"
  )
  
  # Keep only columns that actually exist
  existing_date_cols <- intersect(explicit_date_cols, names(data))
  
  # Adjust column classes for existing date columns
  adjusted_data <- data
  if (length(existing_date_cols) > 0) {
    adjusted_data <- adjusted_data |>
      mutate(
        across(all_of(existing_date_cols), ~ as.Date(.))
      )
  }
  
  # Build metadata table
  metadata <- tibble::tibble(
    ColumnName = names(adjusted_data),
    DataType = purrr::map_chr(adjusted_data, ~ class(.)[1])
  )
  
  # Convert R classes to Power Query friendly types
  metadata <- metadata |>
    mutate(
      DataType = case_when(
        DataType %in% c("integer") ~ "Whole Number",
        DataType %in% c("double", "numeric") ~ "Decimal Number",
        DataType == "character" ~ "Text",
        DataType == "logical" ~ "Boolean",
        DataType == "Date" ~ "Date",
        DataType %in% c("POSIXct", "POSIXlt") ~ "Date/Time",
        TRUE ~ "Text"
      )
    )
  
  # Write BOTH CSV + RDS using your standard helper
  write_csv_and_rds(metadata, output_path)
  
  # Return metadata silently (consistent with your other helpers)
  invisible(metadata)
}


# FINAL ANALYSIS TABLE ---------------------------------------------------------------

# Unified Metrics Pipeline
# Requires: data.table, lubridate
# Uses: resolve_out_dir(), write_csv_and_rds() (from your engine helpers)

# ---------------- DENOMINATORS ----------------

denom_functions_time <- list(
  shifts_all              = function(dt) dt[, uniqueN(ID_Shift, na.rm = TRUE)],
  shifts_gt_3_5           = function(dt) dt[shift_hrs > 3.5, uniqueN(ID_Shift, na.rm = TRUE)],
  shifts_gt_5             = function(dt) dt[shift_hrs > 5, uniqueN(ID_Shift, na.rm = TRUE)],
  shifts_gt_6             = function(dt) dt[shift_hrs > 6, uniqueN(ID_Shift, na.rm = TRUE)],
  shifts_gt_10            = function(dt) dt[shift_hrs > 10, uniqueN(ID_Shift, na.rm = TRUE)],
  shifts_gt_12            = function(dt) dt[shift_hrs > 12, uniqueN(ID_Shift, na.rm = TRUE)],
  pay_periods             = function(dt) dt[, uniqueN(ID_Period_End, na.rm = TRUE)],
  weeks                   = function(dt) dt[, uniqueN(ID_Week_End, na.rm = TRUE)],
  employees               = function(dt) dt[, uniqueN(ID, na.rm = TRUE)],
  meal_periods            = function(dt) dt[, sum(shift_mps, na.rm = TRUE)],
  auto_meal_periods       = function(dt) dt[, sum(!is.na(auto_mp))],
  shifts_gt_5_late_meals  = function(dt) dt[, sum(LateMP1, na.rm = TRUE)],
  shifts_gt_6_late_meals  = function(dt) dt[, sum(LateMP1_w, na.rm = TRUE)],
  shifts_gt_5_short_meals = function(dt) dt[, sum(ShortMP1, na.rm = TRUE)],
  shifts_gt_6_short_meals = function(dt) dt[, sum(ShortMP1_w, na.rm = TRUE)],
  rest_periods            = function(dt) dt[, sum(shift_rps, na.rm = TRUE)],
  analyzed_shifts_round   = function(dt) dt[, sum(shifts_analyzed == 1, na.rm = TRUE)],
  r_analyzed_shifts_round = function(dt) dt[, sum(r_shifts_analyzed == 1, na.rm = TRUE)]
)

denom_functions_pay <- list(
  employees_pay           = function(dt) dt[, uniqueN(Pay_ID, na.rm = TRUE)],
  pay_periods_pay         = function(dt) dt[, uniqueN(Pay_ID_Period_End, na.rm = TRUE)]
)

denom_functions_pp <- list(
  pp_all                  = function(dt) dt[, .N],
  pp_with_shift           = function(dt) dt[has_shift == 1, .N],
  pp_with_pay             = function(dt) dt[has_pay == 1, .N],
  pp_with_both            = function(dt) dt[has_shift == 1 & has_pay == 1, .N],
  pp_employees            = function(dt) dt[, uniqueN(ID, na.rm = TRUE)],
  pp_pay_periods          = function(dt) dt[, uniqueN(ID_Period_End, na.rm = TRUE)],
  pp_paga_employees       = function(dt) uniqueN(dt[in_PAGA_period==1, ID], na.rm = TRUE),
  pp_paga_pay_periods     = function(dt) uniqueN(dt[in_PAGA_period==1, ID_Period_End], na.rm = TRUE),
  pp_with_shifts_gt_5     = function(dt) dt[Shifts_gt_5 > 0, .N],
  pp_with_shifts_gt_6     = function(dt) dt[Shifts_gt_6 > 0, .N],
  pp_with_shifts_gt_3_5   = function(dt) dt[Shifts_gt_3_5 > 0, .N],
  pp_with_mp_violations   = function(dt) dt[mpv_per_pp > 0, .N],
  pp_with_rp_violations   = function(dt) dt[rpv_per_pp > 0, .N]
)

denom_functions <- c(denom_functions_time, denom_functions_pay, denom_functions_pp)

eval_denom <- function(dt, denom_name) {
  if (is.null(dt) || nrow(dt) == 0 || is.na(denom_name) || denom_name == "") return(NA_real_)
  denom_fn <- denom_functions[[denom_name]]
  if (is.null(denom_fn)) return(NA_real_)
  tryCatch(as.numeric(denom_fn(dt)), error = function(e) NA_real_)
}

# ---------------- SAFE EVALUATION (dt cols + globals) ----------------
# Key fix: do NOT use dt[, eval(parse())] for expressions that can reference global objects.
# Instead, build an environment with dt columns and parent = globals env.

make_eval_env <- function(dt, globals_env = .GlobalEnv, extra = list()) {
  # dt columns first, then extra overrides, then globals fallback
  env <- list2env(as.list(dt), parent = globals_env)
  if (length(extra)) {
    for (nm in names(extra)) assign(nm, extra[[nm]], envir = env)
  }
  env
}

eval_expr_safe <- function(dt, expr_str, globals_env = .GlobalEnv, extra = list()) {
  if (is.null(dt) || nrow(dt) == 0) return(NA_real_)
  if (is.na(expr_str) || !nzchar(trimws(expr_str))) return(NA_real_)
  env <- make_eval_env(dt, globals_env = globals_env, extra = extra)
  tryCatch(eval(parse(text = expr_str), envir = env), error = function(e) NA_real_)
}

coerce_metric_result <- function(result, digits = NA) {
  # Date-ish -> numeric date
  if (inherits(result, c("Date", "POSIXct", "POSIXlt", "IDate"))) {
    result <- as.numeric(as.Date(result))
  }
  # scalar numeric coercion
  if (!is.numeric(result)) {
    result <- tryCatch(as.numeric(result), error = function(e) NA_real_)
  }
  digits_num <- suppressWarnings(as.numeric(digits))
  if (!is.na(digits_num) && length(result) == 1 && is.numeric(result) && !is.na(result)) {
    result <- round(result, digits_num)
  }
  as.numeric(result)
}

eval_metric <- function(dt, expr_str, digits = NA, globals_env = .GlobalEnv) {
  res <- eval_expr_safe(dt, expr_str, globals_env = globals_env)
  coerce_metric_result(res, digits)
}

eval_extrap <- function(dt, extrap_expr_str, digits = NA, globals_env = .GlobalEnv, extrap_env = list()) {
  # extrap_env is optional named list; it will be visible to the expression
  res <- eval_expr_safe(dt, extrap_expr_str, globals_env = globals_env, extra = extrap_env)
  coerce_metric_result(res, digits)
}

# ---------------- CALCULATE METRICS ----------------

calculate_metrics <- function(data_list, spec, extrap_env = list(), globals_env = .GlobalEnv) {
  
  # cache denom values per dt so you don't recompute for every metric row
  denom_cache <- new.env(parent = emptyenv())
  
  get_all_denoms_for_dt <- function(dt) {
    if (is.null(dt) || nrow(dt) == 0) return(list())
    # key by object address (good enough for this pipeline)
    key <- sprintf("%s_%s", nrow(dt), ncol(dt))
    if (exists(key, envir = denom_cache, inherits = FALSE)) {
      return(get(key, envir = denom_cache, inherits = FALSE))
    }
    denoms <- lapply(names(denom_functions), function(nm) eval_denom(dt, nm))
    names(denoms) <- names(denom_functions)
    assign(key, denoms, envir = denom_cache)
    denoms
  }
  
  results <- lapply(seq_len(nrow(spec)), function(i) {
    
    src <- spec$source[i]
    dt  <- data_list[[src]]
    
    digits_val <- suppressWarnings(as.numeric(spec$digits[i]))
    denom_name <- spec$denom[i]
    
    # metric value + pct denom
    val       <- eval_metric(dt, spec$expr[i], digits_val, globals_env = globals_env)
    denom_val <- eval_denom(dt, denom_name)
    
    pct <- if (!is.na(val) && !is.na(denom_val) && denom_val > 0) val / denom_val else NA_real_
    
    # extrap: expose extrap_env + ALL denom values
    extrap_val <- NA_real_
    if ("extrap_expr" %in% names(spec)) {
      ex <- spec$extrap_expr[i]
      if (!is.na(ex) && nzchar(trimws(ex))) {
        denom_env <- get_all_denoms_for_dt(dt)
        extra_env <- c(extrap_env, denom_env)   # <- this is the big fix
        extrap_val <- eval_extrap(
          dt,
          ex,
          digits_val,
          globals_env = globals_env,
          extrap_env  = extra_env
        )
      }
    }
    
    out <- list(
      metric_order = if ("metric_order" %in% names(spec)) spec$metric_order[i] else i,
      metric_group = spec$metric_group[i],
      metric_label = spec$metric_label[i],
      metric_type  = if ("metric_type" %in% names(spec)) spec$metric_type[i] else "value",
      digits       = digits_val,
      value        = val,
      denom_value  = denom_val,
      pct          = pct,
      extrap_value = extrap_val
    )
    
    if ("scenario" %in% names(spec)) out$scenario <- spec$scenario[i]
    if ("no_year_breakdown" %in% names(spec)) out$no_year_breakdown <- spec$no_year_breakdown[i]
    
    out
  })
  
  data.table::rbindlist(results, fill = TRUE)
}


# ---------------- FILTERING HELPERS ----------------

filter_data <- function(dt, filter_expr = NULL) {
  if (is.null(dt)) return(NULL)
  if (is.null(filter_expr)) return(dt)
  dt[eval(filter_expr)]
}

create_filtered_data <- function(time_dt, pay_dt, pp_dt = NULL, ee_dt = NULL,
                                 time_filter = NULL, pay_filter = NULL,
                                 pp_filter = NULL, ee_filter = NULL) {
  list(
    shift_data1 = filter_data(time_dt, time_filter),
    pay1        = filter_data(pay_dt, pay_filter),
    pp_data1    = filter_data(pp_dt, pp_filter),
    ee_data1    = filter_data(ee_dt, ee_filter)
  )
}

build_filter_configs <- function(time_dt, pay_dt, pp_dt = NULL, ee_dt = NULL, custom_filters = list()) {
  configs <- list()
  configs[["All Data"]] <- list(time_filter = NULL, pay_filter = NULL, pp_filter = NULL, ee_filter = NULL)
  
  time_years <- if (!is.null(time_dt) && "Period_End" %in% names(time_dt))
    sort(unique(lubridate::year(time_dt$Period_End))) else integer(0)
  pay_years  <- if (!is.null(pay_dt) && "Pay_Period_End" %in% names(pay_dt))
    sort(unique(lubridate::year(pay_dt$Pay_Period_End))) else integer(0)
  pp_years   <- if (!is.null(pp_dt) && "Period_End" %in% names(pp_dt))
    sort(unique(lubridate::year(pp_dt$Period_End))) else integer(0)
  
  all_years <- sort(unique(c(time_years, pay_years, pp_years)))
  
  for (yr in all_years) {
    configs[[as.character(yr)]] <- list(
      time_filter = bquote(lubridate::year(Period_End) == .(yr)),
      pay_filter  = bquote(lubridate::year(Pay_Period_End) == .(yr)),
      pp_filter   = bquote(lubridate::year(Period_End) == .(yr)),
      ee_filter   = NULL
    )
  }
  
  for (nm in names(custom_filters)) configs[[nm]] <- custom_filters[[nm]]
  configs
}

# ---------------- PIPELINE ----------------

run_metrics_pipeline <- function(time_dt, pay_dt, spec,
                                 pp_dt = NULL, ee_dt = NULL,
                                 custom_filters = list(),
                                 extrap_env = list(),
                                 globals_env = .GlobalEnv) {
  
  if (!is.null(time_dt)) setDT(time_dt)
  if (!is.null(pay_dt))  setDT(pay_dt)
  if (!is.null(pp_dt))   setDT(pp_dt)
  if (!is.null(ee_dt))   setDT(ee_dt)
  
  # normalize extrap_expr blanks -> NA
  if ("extrap_expr" %in% names(spec)) {
    spec[, extrap_expr := as.character(extrap_expr)]
    spec[trimws(extrap_expr) == "", extrap_expr := NA_character_]
  }
  
  # Split spec: year-OK vs no-year groups
  if ("no_year_breakdown" %in% names(spec)) {
    # robust logical parse (handles TRUE/FALSE/1/0/"TRUE"/"FALSE"/blank)
    spec[, no_year_flag := {
      x <- no_year_breakdown
      if (is.logical(x)) x else {
        x <- tolower(trimws(as.character(x)))
        x %chin% c("true","t","1","yes","y")
      }
    }]
    spec_no_year <- spec[no_year_flag == TRUE]
    spec_year_ok <- spec[no_year_flag == FALSE]
    spec[, no_year_flag := NULL]
  } else {
    is_no_year <- function(x) grepl("^Damages", x, ignore.case = TRUE) | grepl("^PAGA", x, ignore.case = TRUE)
    spec_no_year <- spec[is_no_year(metric_group)]
    spec_year_ok <- spec[!is_no_year(metric_group)]
  }
  
  filter_configs_all <- build_filter_configs(time_dt, pay_dt, pp_dt, ee_dt, custom_filters)
  
  keep_nm <- names(filter_configs_all)
  is_year_nm <- grepl("^\\d{4}$", keep_nm)
  filter_configs_no_year <- filter_configs_all[!is_year_nm]
  
  res1 <- if (nrow(spec_year_ok) > 0) {
    rbindlist(lapply(names(filter_configs_all), function(filter_name) {
      cfg <- filter_configs_all[[filter_name]]
      filtered_data <- create_filtered_data(
        time_dt, pay_dt, pp_dt, ee_dt,
        cfg$time_filter, cfg$pay_filter, cfg$pp_filter, cfg$ee_filter
      )
      metrics <- calculate_metrics(filtered_data, spec_year_ok, extrap_env = extrap_env, globals_env = globals_env)
      metrics[, filter_name := filter_name]
      metrics
    }), fill = TRUE)
  } else NULL
  
  res2 <- if (nrow(spec_no_year) > 0) {
    rbindlist(lapply(names(filter_configs_no_year), function(filter_name) {
      cfg <- filter_configs_no_year[[filter_name]]
      filtered_data <- create_filtered_data(
        time_dt, pay_dt, pp_dt, ee_dt,
        cfg$time_filter, cfg$pay_filter, cfg$pp_filter, cfg$ee_filter
      )
      metrics <- calculate_metrics(filtered_data, spec_no_year, extrap_env = extrap_env, globals_env = globals_env)
      metrics[, filter_name := filter_name]
      metrics
    }), fill = TRUE)
  } else NULL
  
  rbindlist(list(res1, res2), fill = TRUE)
}

# ---------------- FORMAT OUTPUT ----------------

format_metrics_table <- function(results_dt) {
  dt <- copy(results_dt)
  setDT(dt)
  
  dt[, digits := suppressWarnings(as.numeric(digits))]
  
  # Helper function to safely format numbers with commas
  safe_format_num <- function(x) {
    sapply(x, function(v) {
      if (is.na(v) || is.nan(v)) return(NA_character_)
      prettyNum(v, big.mark = ",")
    })
  }
  
  # base formatted value (with pct appended for percent metrics)
  dt[, formatted_value := {
    rounded_val <- fifelse(is.na(digits) | is.na(value), value, round(value, digits))
    
    fv <- data.table::fcase(
      metric_type == "date", as.character(as.Date(value, origin = "1970-01-01")),
      is.na(value), "0",
      is.nan(value), "0",
      default = safe_format_num(rounded_val)
    )
    
    fv <- fifelse(is.na(digits) | digits == 0, gsub("\\.0+$", "", fv), fv)
    fv <- gsub("^\\s+|\\s+$", "", fv)
    
    fifelse(
      !is.na(pct) & !is.nan(pct),
      paste0(fv, " (", round(pct * 100, 1), "%)"),
      fv
    )
  }]
  
  dt[formatted_value == "NaN", formatted_value := "0"]
  dt[grepl("NaN", formatted_value), formatted_value := gsub(" \\(NaN%\\)", "", formatted_value)]
  
  # formatted extrap (append pct like other columns)
  dt[, formatted_extrap := {
    rounded_val <- fifelse(is.na(digits) | is.na(extrap_value), extrap_value, round(extrap_value, digits))
    
    fv <- fcase(
      metric_type == "date", as.character(as.Date(extrap_value, origin = "1970-01-01")),
      is.na(extrap_value), "-",
      is.nan(extrap_value), "-",
      default = safe_format_num(rounded_val)
    )
    
    fv <- fifelse(is.na(digits) | digits == 0, gsub("\\.0+$", "", fv), fv)
    fv <- gsub("^\\s+|\\s+$", "", fv)
    
    # Append the SAME pct used for the base metric (sample proportion),
    # so Extrapolated reads like: "1,234 (12.3%)"
    fifelse(
      metric_type != "date" & !is.na(pct) & !is.nan(pct) & fv != "-" & fv != "",
      paste0(fv, " (", round(pct * 100, 1), "%)"),
      fv
    )
  }]
  
  dt[formatted_extrap == "NaN", formatted_extrap := "-"]
  
  # IMPORTANT: use a unique key for casting & merging (metric_order alone is NOT safe)
  id_cols <- c("metric_order","metric_group","metric_label","scenario","metric_type","digits","no_year_breakdown")
  
  # wide values
  wide_dt <- dcast(
    dt,
    formula   = as.formula(paste(paste(id_cols, collapse = " + "), "~ filter_name")),
    value.var = "formatted_value"
  )
  
  setorder(wide_dt, metric_order)
  
  # Extrapolated column should be per-metric key (not just metric_order)
  extrap_col <- dt[filter_name == "All Data",
                   c(id_cols, "formatted_extrap"),
                   with = FALSE]
  setnames(extrap_col, "formatted_extrap", "Extrapolated")
  
  wide_dt <- merge(wide_dt, extrap_col, by = id_cols, all.x = TRUE)
  
  # If All Data missing, fall back to first non-NA per id
  if (all(is.na(wide_dt$Extrapolated))) {
    extrap_col2 <- dt[, .(Extrapolated = first(formatted_extrap[!is.na(formatted_extrap)])),
                      by = id_cols]
    wide_dt[, Extrapolated := NULL]
    wide_dt <- merge(wide_dt, extrap_col2, by = id_cols, all.x = TRUE)
  }
  
  all_cols <- names(wide_dt)
  year_cols <- sort(all_cols[grepl("^\\d{4}$", all_cols)])
  other_cols <- setdiff(all_cols, c(id_cols, "Extrapolated", "All Data", year_cols))
  
  # final column order
  col_order <- c("metric_group", "metric_label", "scenario", "Extrapolated", "All Data", year_cols, other_cols)
  col_order <- col_order[col_order %in% all_cols]
  setcolorder(wide_dt, col_order)
  
  wide_dt[, c("metric_order","metric_type","digits","no_year_breakdown") := NULL]
  wide_dt
}

export_metrics <- function(wide_dt, base_name = "Metrics_Table", out_dir = NULL) {
  out_dir <- resolve_out_dir(out_dir)
  out_csv <- file.path(out_dir, paste0(base_name, ".csv"))
  write_csv_and_rds(wide_dt, out_csv)
  invisible(list(csv = out_csv, rds = sub("\\.csv$", ".rds", out_csv)))
}
