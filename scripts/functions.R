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


# SHIFT HOURS TABLE--------------------------------------------------------------------------------------------

library(data.table)
library(here)

shift_hrs_tbl <- function(data,
                          punch_type_value = "in",
                          output_path = here("output", "Shift_Hrs_Table.csv")) {  # Correct path

  # Ensure output directory exists
  output_dir <- here("output")
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("✓ Created output directory:", output_dir, "\n")
  }

  # Convert data to data.table
  data <- as.data.table(data)

  # Error handling for required columns
  required_cols <- c("shift_hrs", "ID_Date")
  if (!all(required_cols %in% names(data))) {
    stop(paste("The input data must contain", paste(required_cols, collapse = ", "), "columns."))
  }

  # Print diagnostic message
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

  # Write to CSV
  fwrite(summary_df, output_path)
  cat("✓ Saved shift hours summary to:", output_path, "\n")

  # Return the summary data table
  return(summary_df)
}


# NON WORK HOURS TABLE--------------------------------------------------------------------------------------------
non_wrk_hrs_tbl <- function(data, 
                            punch_type_value = "in", 
                            output_path = here("output", "Non_Work_Hrs_Table.csv")) {  # FIXED PATH
  
  # Ensure output directory exists
  output_dir <- here("output")
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("✓ Created output directory:", output_dir, "\n")
  }
  
  # Convert data to data.table
  data <- as.data.table(data)
  
  # Error handling for required columns
  required_cols <- c("hrs_from_prev", "punch_type", "ID_Date")
  if (!all(required_cols %in% names(data))) {
    stop(paste("The input data must contain", paste(required_cols, collapse = ", "), "columns."))
  }
  
  # Print diagnostic message
  message("Data validation completed.")
  
  # Filter data by punch_type_value
  filtered_data <- data[punch_type == punch_type_value]
  
  # Count the NA value records
  na_records <- sum(is.na(filtered_data$hrs_from_prev))
  
  # Define the bins
  bins <- list(
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
  
  # Create a data frame with the counts and unique days
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
  
  # Write to CSV
  fwrite(summary_df, output_path)
  cat("✓ Saved non-work hours summary to:", output_path, "\n")  # Added confirmation
  
  # Return the summary data frame
  return(summary_df)
}


# MEAL PERIOD TABLE--------------------------------------------------------------------------------------------
meal_period_tbl <- function(data, 
                            output_path = here("output", "Meal_Period_Table.csv")) {
  
  # Ensure output directory exists
  output_dir <- here("output")
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("✓ Created output directory:", output_dir, "\n")
  }
  
  # Convert to data.table
  data <- as.data.table(data)
  
  # Error handling for required columns
  required_cols <- c("mp", "mp_hrs", "punch_type", "ID_Date")
  if (!all(required_cols %in% names(data))) {
    stop(paste("The input data must contain", paste(required_cols, collapse = ", "), "columns."))
  }
  
  # Print diagnostic message
  message("Data validation completed.")
  
  # Filter for meal periods (mp == 1)
  meal_data <- data[mp == 1]
  
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
    Records = sapply(results, "[[", "Records"),
    Days = sapply(results, "[[", "Days")
  )
  
  # Calculate total records and days
  total_records <- nrow(meal_data)
  total_days <- length(unique(meal_data$ID_Date))
  
  # Add Percent_of_Records and Percent_of_Days columns
  summary_df[, Percent_of_Records := round(Records / total_records, 10)]
  summary_df[, Percent_of_Days := ifelse(Records == 0, 0, round(Days / total_days, 10))]
  
  # Reorder columns
  summary_df <- summary_df[, .(Meal_Period_Hrs, Records, Percent_of_Records, Days, Percent_of_Days)]
  
  # Write to CSV
  fwrite(summary_df, output_path)
  cat("✓ Saved meal period summary to:", output_path, "\n")
  
  # Return the summary data table
  return(summary_df)
}


# MEAL START TIME FREQUENCY TABLE ----------------------------
meal_start_time_tbl <- function(data, 
                                output_path = here("output", "Meal_Start_Time_Table.csv"),
                                top_n = 20) {  # Adjustable parameter, default 20
  
  # Ensure output directory exists
  output_dir <- here("output")
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("✓ Created output directory:", output_dir, "\n")
  }
  
  # Convert to data.table
  data <- as.data.table(data)
  
  # Error handling for required columns
  required_cols <- c("mp_hrs", "punch_time", "ID_Date")
  if (!all(required_cols %in% names(data))) {
    stop(paste("The input data must contain", paste(required_cols, collapse = ", "), "columns."))
  }
  
  # Print diagnostic message
  message("Data validation completed.")
  
  # Filter for rows with meal periods (mp_hrs > 0)
  meal_data <- data[mp_hrs > 0]
  
  if(nrow(meal_data) == 0) {
    message("No meal periods found")
    return(NULL)
  }
  
  # Calculate meal START time (punch_time minus mp_hrs converted to minutes)
  meal_data[, meal_start := punch_time - minutes(round(mp_hrs * 60))]
  
  # Extract just the TIME component (not date) and format as HH:MM
  meal_data[, start_time_exact := format(meal_start, "%H:%M:%S")]
  
  # Count frequency by EXACT time
  time_counts <- meal_data[, .(
    Records = .N,
    Days = uniqueN(ID_Date)
  ), by = start_time_exact]
  
  # Sort by frequency (most common first)
  setorder(time_counts, -Records)
  
  # Get top N times
  if(nrow(time_counts) > top_n) {
    top_rows <- time_counts[1:top_n]
    other_counts <- time_counts[(top_n+1):.N]
    
    # Create "Other" row
    other_row <- data.table(
      start_time_exact = "Other",
      Records = sum(other_counts$Records),
      Days = uniqueN(meal_data[!start_time_exact %in% top_rows$start_time_exact, ID_Date])
    )
    
    # Combine top N with Other
    summary_df <- rbindlist(list(top_rows, other_row))
  } else {
    summary_df <- time_counts
  }
  
  # Calculate totals
  total_records <- nrow(meal_data)
  total_days <- uniqueN(meal_data$ID_Date)
  
  # Add percentages
  summary_df[, `:=`(
    Percent_of_Records = round(Records / total_records * 100, 2),
    Percent_of_Days = round(Days / total_days * 100, 2)
  )]
  
  # Rename column
  setnames(summary_df, "start_time_exact", "Meal_Start_Time")
  
  # Reorder columns
  summary_df <- summary_df[, .(Meal_Start_Time, Records, Percent_of_Records, Days, Percent_of_Days)]
  
  # Write to CSV
  fwrite(summary_df, output_path)
  cat("✓ Saved meal START time frequency to:", output_path, "\n")
  cat("  Total meal periods found:", total_records, "\n")
  cat("  Showing top", min(top_n, nrow(time_counts)), "exact meal start times\n")
  cat("  Most common:\n")
  for(i in 1:min(5, nrow(summary_df))) {
    if(summary_df[i, Meal_Start_Time] != "Other") {
      cat("    ", summary_df[i, Meal_Start_Time], ":", 
          summary_df[i, Records], "records (", 
          summary_df[i, Percent_of_Records], "%)\n")
    }
  }
  
  # Return the summary data table
  return(summary_df)
}


# MEAL QUARTER HOUR ANALYSIS TABLE ----------------------------

meal_quarter_hour_tbl <- function(data, 
                                  output_path = here("output", "Meal_Quarter_Hour_Table.csv")) {
  
  # Ensure output directory exists
  output_dir <- here("output")
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("✓ Created output directory:", output_dir, "\n")
  }
  
  # Convert to data.table
  data <- as.data.table(data)
  
  # Error handling for required columns
  required_cols <- c("mp_hrs", "punch_time", "ID_Date")
  if (!all(required_cols %in% names(data))) {
    stop(paste("The input data must contain", paste(required_cols, collapse = ", "), "columns."))
  }
  
  # Print diagnostic message
  message("Data validation completed.")
  
  # Filter for rows with meal periods (mp_hrs > 0)
  meal_data <- data[mp_hrs > 0]
  
  if(nrow(meal_data) == 0) {
    message("No meal periods found")
    return(NULL)
  }
  
  # Calculate meal START time
  meal_data[, meal_start := punch_time - minutes(round(mp_hrs * 60))]
  
  # Extract minutes and categorize
  meal_data[, start_minute := minute(meal_start)]
  
  # Define bins for quarter hour analysis
  bins <- list(
    ":00 (On the hour)" = meal_data$start_minute == 0,
    ":15 (Quarter past)" = meal_data$start_minute == 15,
    ":30 (Half past)" = meal_data$start_minute == 30,
    ":45 (Quarter to)" = meal_data$start_minute == 45,
    "Other (Off quarter)" = !meal_data$start_minute %in% c(0, 15, 30, 45)
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
  
  # Calculate total records and days
  total_records <- nrow(meal_data)
  total_days <- length(unique(meal_data$ID_Date))
  
  # Add Percent_of_Records and Percent_of_Days columns
  summary_df[, Percent_of_Records := round(Records / total_records * 100, 2)]
  summary_df[, Percent_of_Days := ifelse(Records == 0, 0, round(Days / total_days * 100, 2))]
  
  # Add summary rows
  # Calculate totals for ANY quarter hour
  any_quarter <- summary_df[Quarter_Hour_Type != "Other (Off quarter)"]
  any_quarter_total <- data.table(
    Quarter_Hour_Type = "ANY QUARTER HOUR",
    Records = sum(any_quarter$Records),
    Days = uniqueN(meal_data[start_minute %in% c(0, 15, 30, 45), ID_Date]),
    Percent_of_Records = round(sum(any_quarter$Records) / total_records * 100, 2),
    Percent_of_Days = round(uniqueN(meal_data[start_minute %in% c(0, 15, 30, 45), ID_Date]) / total_days * 100, 2)
  )
  
  # Add separator row
  separator <- data.table(
    Quarter_Hour_Type = "---",
    Records = NA,
    Days = NA,
    Percent_of_Records = NA,
    Percent_of_Days = NA
  )
  
  # Combine all rows
  summary_df <- rbindlist(list(summary_df, separator, any_quarter_total), fill = TRUE)
  
  # Reorder columns
  summary_df <- summary_df[, .(Quarter_Hour_Type, Records, Percent_of_Records, Days, Percent_of_Days)]
  
  # Write to CSV
  fwrite(summary_df, output_path, na = "")
  cat("✓ Saved meal quarter hour analysis to:", output_path, "\n")
  cat("  Total meal periods:", total_records, "\n")
  cat("  On ANY quarter hour:", any_quarter_total$Records, 
      "(", any_quarter_total$Percent_of_Records, "%)\n")
  cat("  Off quarter hours:", summary_df[Quarter_Hour_Type == "Other (Off quarter)", Records], 
      "(", summary_df[Quarter_Hour_Type == "Other (Off quarter)", Percent_of_Records], "%)\n")
  
  # Return the summary data table
  return(summary_df)
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
  
  # REPLACE THE OLD LINE HERE WITH THIS NEW CODE:
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
pay_code_summary <- function(data,
                             output_path = here("output", "Pay_Code_Summary.csv")) {
  
  # Packages
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table required")
  if (!requireNamespace("here", quietly = TRUE)) stop("here required")
  
  library(dplyr)
  library(data.table)
  library(here)
  
  # Required columns
  required_cols <- c(
    "Pay_Code","Pay_Hours","Pay_Amount","Pay_Period_End","Pay_ID_Period_End",
    "Hrs_Wkd_Pay_Code","OT_Pay_Code","DT_Pay_Code","Reg_Pay_Code",
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
  
  unique_pay_sample <- unique(data$Pay_Key_Gps)
  unique_pay_codes  <- unique(data$Pay_Code)
  
  summary_list <- list()
  
  safe_max <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  }
  
  # ---- Pay_Key_Gps x Pay_Code ----
  for (pay_key_gps in unique_pay_sample) {
    for (pay_code in unique_pay_codes) {
      
      subset_data <- data %>%
        filter(Pay_Key_Gps == pay_key_gps, Pay_Code == pay_code)
      
      if (nrow(subset_data) == 0) next
      
      summary_list[[paste(pay_key_gps, pay_code, sep = "_")]] <-
        data.frame(
          Pay_Key_Gps = pay_key_gps,
          Pay_Code = pay_code,
          Min_Period_End = min(subset_data$Pay_Period_End, na.rm = TRUE),
          Max_Period_End = max(subset_data$Pay_Period_End, na.rm = TRUE),
          Employees = n_distinct(subset_data$Pay_ID),
          Pay_Periods = n_distinct(subset_data$Pay_ID_Period_End),
          Total_Hours = sum(subset_data$Pay_Hours, na.rm = TRUE),
          Total_Amount = sum(subset_data$Pay_Amount, na.rm = TRUE),
          Avg_Amount = mean(subset_data$Pay_Amount, na.rm = TRUE),
          Hrs_Wkd_Pay_Code = safe_max(subset_data$Hrs_Wkd_Pay_Code),
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
  }
  
  # ---- Totals per Pay_Key_Gps ----
  for (pay_key_gps in unique_pay_sample) {
    
    total_summary <- data %>%
      filter(Pay_Key_Gps == pay_key_gps) %>%
      summarise(
        Pay_Key_Gps = pay_key_gps,
        Pay_Code = paste(pay_key_gps, "Total", sep = ": "),
        Min_Period_End = min(Pay_Period_End, na.rm = TRUE),
        Max_Period_End = max(Pay_Period_End, na.rm = TRUE),
        Employees = n_distinct(Pay_ID),
        Pay_Periods = n_distinct(Pay_ID_Period_End),
        Total_Hours = sum(Pay_Hours, na.rm = TRUE),
        Total_Amount = sum(Pay_Amount, na.rm = TRUE),
        Avg_Amount = mean(Pay_Amount, na.rm = TRUE),
        Hrs_Wkd_Pay_Code = safe_max(Hrs_Wkd_Pay_Code),
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
    
    summary_list[[paste(pay_key_gps, "Total", sep = "_")]] <- total_summary
  }
  
  final_summary_df <- bind_rows(summary_list)
  
  # ---- Percentages ----
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
      Hrs_Wkd_Pay_Code, Reg_Pay_Code, OT_Pay_Code, DT_Pay_Code,
      Bon_Pay_Code, Meal_Pay_Code, Rest_Pay_Code, Sick_Pay_Code,
      Diff_Pay_Code, Diff_OT_Pay_Code, Diff_DT_Pay_Code
    )
  
  fwrite(final_summary_df, output_path)
  message("✓ Pay code summary written to: ", output_path)
  
  invisible(final_summary_df)
}


# PAY CODE CATEGORIES ------------------------------------------------------------------------------

library(data.table)
library(dplyr)
library(here)

# Categorize Pay Codes Based on Flag Columns
categorize_pay_codes <- function(df, flag_columns, output_csv = NULL) {
  # Validate input columns
  if (!"Pay_Code" %in% colnames(df)) {
    message("[ERROR] Missing 'Pay_Code' column in input data frame.")
    stop("Missing 'Pay_Code' column.")
  }
  
  missing_flags <- setdiff(flag_columns, colnames(df))
  if (length(missing_flags) > 0) {
    message("[ERROR] Missing flag columns: ", paste(missing_flags, collapse = ", "))
    stop("Missing one or more specified flag columns.")
  }
  
  # Melt to long format (use data.table::melt to avoid reshape2 dependency)
  melted <- melt(
    df,
    id.vars = "Pay_Code",
    measure.vars = flag_columns,
    variable.name = "Pay_Code_Category",
    value.name = "flag"
  )
  
  # Filter flagged rows
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
    uncategorized_dt <- data.table(Pay_Code = uncategorized, Pay_Code_Category = "Uncategorized")
    pay_code_categories <- rbindlist(list(pay_code_categories, uncategorized_dt))
  }
  
  # Sort result
  setorder(pay_code_categories, Pay_Code_Category)
  
  # Write CSV if specified
  if (!is.null(output_csv)) {
    write.csv(pay_code_categories, output_csv, row.names = FALSE)
  } else {
    message("Output CSV path not specified. Skipping file write.")
  }
  
  return(pay_code_categories)
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

# At the top of your helper functions script
library(data.table)

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
      Oth_RROP_Amt = sum(Pay_Amount[Hrs_Wkd_Pay_Code == 0 & Meal_Pay_Code == 0 & Rest_Pay_Code == 0 & Sick_Pay_Code == 0 & Diff_Pay_Code == 0 & Diff_OT_Pay_Code == 0 & Diff_DT_Pay_Code == 0 & RROP_Pay_Code == 1], na.rm = TRUE),
      Oth_Amt = sum(Pay_Amount[Hrs_Wkd_Pay_Code == 0 & Meal_Pay_Code == 0 & Rest_Pay_Code == 0 & Sick_Pay_Code == 0 & Diff_Pay_Code == 0 & Diff_OT_Pay_Code == 0 & Diff_DT_Pay_Code == 0 & RROP_Pay_Code == 0], na.rm = TRUE)
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


# GENERATE METADATA FILES FOR POWERQUERY ------------------------------------------------------------------------------

generate_metadata <- function(data, file_name) {
  # Set file path using here to save in "output" directory
  file_path <- here("output", file_name)
  
  # Define explicit columns to be treated as Date
  explicit_date_cols <- c("Pay_Date", "Pay_Period_Beg", "Pay_Period_End", "Period_End", "Date")
  
  # Ensure only columns that exist in the data are processed
  existing_date_cols <- intersect(explicit_date_cols, names(data))
  
  # Adjust column classes for existing date columns
  adjusted_data <- data
  if (length(existing_date_cols) > 0) {
    adjusted_data <- adjusted_data %>%
      mutate(across(all_of(existing_date_cols), ~as.Date(.))) # Ensure these are treated as Date
  }
  
  # Generate metadata with adjusted data
  metadata <- tibble(
    ColumnName = names(adjusted_data),
    DataType = map_chr(adjusted_data, ~class(.)[1]) # Get the primary class of each column
  )
  
  # Convert R classes to Power Query compatible types
  metadata <- metadata %>%
    mutate(
      DataType = case_when(
        DataType %in% c("integer") ~ "Whole Number",
        DataType == "double" | DataType == "numeric" ~ "Decimal Number",
        DataType == "character" ~ "Text",
        DataType == "logical" ~ "Boolean",
        DataType == "Date" ~ "Date",
        DataType %in% c("POSIXct", "POSIXlt") ~ "Date/Time", # Date/Time for POSIX classes
        TRUE ~ "Text"  # Default to Text for unsupported types
      )
    )
  
  # Write to CSV file in the specified folder
  write_csv(metadata, file_path)
  message("Metadata written to: ", normalizePath(file_path))
}



# DATA COMPARISON ------------------------------------------------------------------------------

run_data_comparison <- function(
    time_data, 
    pay_data, 
    output_dir = "output", 
    period_breakdown = c("default", "weekly", "monthly", "quarterly"),
    save_outputs = TRUE,
    return_data = TRUE
) {
  library(data.table)
  library(ggplot2)
  library(here)
  
  # Parameter validation
  period_breakdown <- match.arg(period_breakdown)
  
  # Convert to data.table and create copies to avoid modifying originals
  time1 <- as.data.table(copy(time_data))
  pay1 <- as.data.table(copy(pay_data))
  
  # Remove any duplicate columns
  time1 <- time1[, .SD, .SDcols = unique(names(time1))]
  pay1 <- pay1[, .SD, .SDcols = unique(names(pay1))]
  
  # Convert dates to Date class (handling both POSIXct and character)
  if("Period_End" %in% names(time1)) {
    if(inherits(time1$Period_End, c("POSIXct", "POSIXt"))) {
      time1[, Period_End := as.Date(Period_End)]
    } else if(is.character(time1$Period_End)) {
      time1[, Period_End := as.Date(Period_End)]
    }
  }
  
  if("Pay_Period_End" %in% names(pay1)) {
    if(inherits(pay1$Pay_Period_End, c("POSIXct", "POSIXt"))) {
      pay1[, Pay_Period_End := as.Date(Pay_Period_End)]
    } else if(is.character(pay1$Pay_Period_End)) {
      pay1[, Pay_Period_End := as.Date(Pay_Period_End)]
    }
  }
  
  # Validate required columns exist
  if(!"Period_End" %in% names(time1)) {
    stop("time_data must contain 'Period_End' column")
  }
  if(!"Pay_Period_End" %in% names(pay1)) {
    stop("pay_data must contain 'Pay_Period_End' column")
  }
  if(!"ID" %in% names(time1)) {
    stop("time_data must contain 'ID' column")
  }
  if(!"Pay_ID" %in% names(pay1)) {
    stop("pay_data must contain 'Pay_ID' column")
  }
  
  # Create output directory if saving
  if(save_outputs && !dir.exists(here(output_dir))) {
    dir.create(here(output_dir), recursive = TRUE)
  }
  
  # Process time data - count unique employees per period
  time_summary <- time1[, .(
    Employees = uniqueN(ID),
    Records = .N
  ), by = Period_End]
  time_summary[, Data := "Time Data"]
  
  # Process pay data - count unique employees per period
  pay_summary <- pay1[, .(
    Employees = uniqueN(Pay_ID),
    Records = .N
  ), by = Pay_Period_End]
  setnames(pay_summary, "Pay_Period_End", "Period_End")
  pay_summary[, Data := "Pay Data"]
  
  # Combine datasets
  comparison_dt <- rbindlist(list(time_summary, pay_summary), fill = TRUE)
  
  # Apply period breakdown
  if(period_breakdown == "weekly") {
    library(lubridate)
    comparison_dt[, Period_Group := floor_date(Period_End, "week")]
  } else if(period_breakdown == "monthly") {
    comparison_dt[, Period_Group := as.Date(format(Period_End, "%Y-%m-01"))]
  } else if(period_breakdown == "quarterly") {
    library(lubridate)
    comparison_dt[, Period_Group := as.Date(paste0(year(Period_End), "-", 
                                                   sprintf("%02d", (quarter(Period_End)-1)*3+1), "-01"))]
  } else {
    comparison_dt[, Period_Group := Period_End]
  }
  
  # Aggregate by period group if needed
  if(period_breakdown != "default") {
    comparison_dt <- comparison_dt[, .(
      Employees = sum(Employees, na.rm = TRUE),
      Records = sum(Records, na.rm = TRUE),
      Period_Count = .N,
      First_Period = min(Period_End),
      Last_Period = max(Period_End)
    ), by = .(Period_Group, Data)]
    setnames(comparison_dt, "Period_Group", "Period_End")
  }
  
  # Sort data
  setorder(comparison_dt, Period_End, Data)
  
  # Remove any NA dates before plotting
  if(any(is.na(comparison_dt$Period_End))) {
    comparison_dt <- comparison_dt[!is.na(Period_End)]
  }
  
  # Remove Period_Group column if it exists
  if("Period_Group" %in% names(comparison_dt)) {
    comparison_dt[, Period_Group := NULL]
  }
  
  # Create line plot
  plot_line <- ggplot(comparison_dt, aes(x = Period_End, y = Employees, 
                                         color = Data)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    labs(
      title = paste("Time vs Pay Data Comparison", 
                    ifelse(period_breakdown != "default", paste0("(", period_breakdown, ")"), "")),
      subtitle = paste("Date Range:", 
                       min(comparison_dt$Period_End, na.rm = TRUE), "to", 
                       max(comparison_dt$Period_End, na.rm = TRUE)),
      x = "Period End Date",
      y = "Number of Unique Employees",
      color = "Data Source"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    scale_color_manual(values = c("Time Data" = "#2E86C1", "Pay Data" = "#28B463"))
  
  # Adjust x-axis based on date range
  date_range <- as.numeric(diff(range(comparison_dt$Period_End, na.rm = TRUE)))
  if(is.na(date_range) || date_range == 0) {
    plot_line <- plot_line + scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
  } else if(date_range <= 90) {
    plot_line <- plot_line + scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")
  } else if(date_range <= 365) {
    plot_line <- plot_line + scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
  } else {
    plot_line <- plot_line + scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m")
  }
  
  # Save outputs
  if(save_outputs) {
    # Save line plot as Data Comparison.pdf
    ggsave(here(output_dir, "Data Comparison.pdf"), 
           plot = plot_line, width = 12, height = 7)
    
    # Save CSV as Data Comparison.csv
    fwrite(comparison_dt, here(output_dir, "Data Comparison.csv"))
    
    message("Files saved to: ", output_dir)
  }
  
  # Return data if requested
  if(return_data) {
    return(invisible(comparison_dt))
  }
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


# EMPLOYEE LEVEL DATA COMPARISON ------------------------------------------------------------------------------

employee_period_comparison <- function(
    time_data,
    pay_data,
    output_dir = "output",
    save_output = TRUE,
    return_data = TRUE
) {
  library(data.table)
  library(here)
  
  # Convert to data.table
  time1 <- as.data.table(copy(time_data))
  pay1 <- as.data.table(copy(pay_data))
  
  # Ensure dates are Date class
  if(inherits(time1$Period_End, c("POSIXct", "POSIXt"))) {
    time1[, Period_End := as.Date(Period_End)]
  }
  if(inherits(pay1$Pay_Period_End, c("POSIXct", "POSIXt"))) {
    pay1[, Pay_Period_End := as.Date(Pay_Period_End)]
  }
  
  # Get unique periods per employee in time data
  time_summary <- time1[!is.na(Period_End), .(
    Time_Periods = uniqueN(Period_End),
    Time_First_Date = min(Period_End),
    Time_Last_Date = max(Period_End),
    Time_Records = .N,
    Time_Period_List = list(unique(Period_End))
  ), by = ID]
  
  # Get unique periods per employee in pay data
  pay_summary <- pay1[!is.na(Pay_Period_End), .(
    Pay_Periods = uniqueN(Pay_Period_End),
    Pay_First_Date = min(Pay_Period_End),
    Pay_Last_Date = max(Pay_Period_End),
    Pay_Records = .N,
    Pay_Period_List = list(unique(Pay_Period_End))
  ), by = Pay_ID]
  setnames(pay_summary, "Pay_ID", "ID")
  
  # Merge all employee data
  all_employees <- merge(time_summary, pay_summary, by = "ID", all = TRUE)
  
  # Calculate matching periods
  all_employees[, `:=`(
    Matching_Periods = mapply(function(t, p) {
      if(is.null(t) || is.null(p)) return(0)
      length(intersect(t, p))
    }, Time_Period_List, Pay_Period_List),
    
    Time_Only_Periods = mapply(function(t, p) {
      if(is.null(t)) return(0)
      if(is.null(p)) return(length(t))
      length(setdiff(t, p))
    }, Time_Period_List, Pay_Period_List),
    
    Pay_Only_Periods = mapply(function(t, p) {
      if(is.null(p)) return(0)
      if(is.null(t)) return(length(p))
      length(setdiff(p, t))
    }, Time_Period_List, Pay_Period_List)
  )]
  
  # Replace NA counts with 0
  cols_to_fix <- c("Time_Periods", "Pay_Periods", "Time_Records", "Pay_Records")
  for(col in cols_to_fix) {
    all_employees[is.na(get(col)), (col) := 0]
  }
  
  # Calculate match rates and coverage
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
  
  # Remove list columns before saving
  output_df <- all_employees[, -c("Time_Period_List", "Pay_Period_List")]
  
  # Sort by match rate (ascending) to highlight issues
  setorder(output_df, Match_Rate, -Total_Unique_Periods)
  
  # Save the CSV file
  if(save_output) {
    if(!dir.exists(here(output_dir))) {
      dir.create(here(output_dir), recursive = TRUE)
    }
    
    output_file <- here(output_dir, "Employee Pay Period Comparison.csv")
    fwrite(output_df, output_file)
    message("File saved: Employee_Period_Comparison.csv")
  }
  
  # Return data
  if(return_data) {
    return(invisible(output_df))
  }
}

# Usage:
# employee_period_comparison(time1, pay1)
# 
# Or to just get the data without saving:
# emp_comp <- employee_period_comparison(time1, pay1, save_output = FALSE)



# AGGREGATE DATA ------------------------------------------------------------------------------

library(data.table)

aggregate_data <- function(dt,
                           by,
                           first_fields = NULL,
                           sum_fields = NULL,
                           max_fields = NULL,
                           min_fields = NULL,
                           mean_fields = NULL,
                           median_fields = NULL) {
  setDT(dt)
  
  first_fields <- if (is.null(first_fields)) if (exists("first_fields_default")) first_fields_default else character(0) else first_fields
  sum_fields <- if (is.null(sum_fields)) if (exists("sum_fields_default")) sum_fields_default else character(0) else sum_fields
  max_fields <- if (is.null(max_fields)) if (exists("max_fields_default")) max_fields_default else character(0) else max_fields
  min_fields <- if (is.null(min_fields)) if (exists("min_fields_default")) min_fields_default else character(0) else min_fields
  mean_fields <- if (is.null(mean_fields)) if (exists("mean_fields_default")) mean_fields_default else character(0) else mean_fields
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
  if (length(missing_by) > 0) {
    stop(sprintf("The following grouping fields are missing in the data: %s", paste(missing_by, collapse = ", ")))
  }
  
  unaggregated_fields <- setdiff(names(dt), c(by, all_fields))
  if (length(unaggregated_fields) > 0) {
    message(sprintf("Warning: The following fields are not aggregated and will be ignored: %s", paste(unaggregated_fields, collapse = ", ")))
  }
  
  message("Starting aggregation...")
  start_time <- Sys.time()
  
  aggregation_expr <- list()
  
  if (length(first_fields) > 0) {
    aggregation_expr <- c(aggregation_expr,
                          setNames(lapply(first_fields, function(f) substitute(first(f), list(f = as.name(f)))), first_fields)
    )
  }
  if (length(sum_fields) > 0) {
    aggregation_expr <- c(aggregation_expr,
                          setNames(lapply(sum_fields, function(f) substitute(if (all(is.na(f))) NA_real_ else sum(f, na.rm = TRUE), list(f = as.name(f)))), paste0(sum_fields, "_sum"))
    )
  }
  if (length(max_fields) > 0) {
    aggregation_expr <- c(aggregation_expr,
                          setNames(lapply(max_fields, function(f) substitute(if (all(is.na(f))) NA_real_ else max(f, na.rm = TRUE), list(f = as.name(f)))), paste0(max_fields, "_max"))
    )
  }
  if (length(min_fields) > 0) {
    aggregation_expr <- c(aggregation_expr,
                          setNames(lapply(min_fields, function(f) substitute(if (all(is.na(f))) NA_real_ else min(f, na.rm = TRUE), list(f = as.name(f)))), paste0(min_fields, "_min"))
    )
  }
  if (length(mean_fields) > 0) {
    aggregation_expr <- c(aggregation_expr,
                          setNames(lapply(mean_fields, function(f) substitute(if (all(is.na(f))) NA_real_ else mean(f, na.rm = TRUE), list(f = as.name(f)))), paste0(mean_fields, "_mean"))
    )
  }
  if (length(median_fields) > 0) {
    aggregation_expr <- c(aggregation_expr,
                          setNames(lapply(median_fields, function(f) substitute(if (all(is.na(f))) NA_real_ else median(f, na.rm = TRUE), list(f = as.name(f)))), paste0(median_fields, "_median"))
    )
  }
  
  aggregation_call <- as.call(c(quote(list), aggregation_expr))
  result <- dt[, eval(aggregation_call), by = by]
  
  end_time <- Sys.time()
  duration <- end_time - start_time
  message(sprintf("Aggregation completed in %.2f seconds.", as.numeric(duration, units = "secs")))
  
  return(result)
}


# Example usage:
# shift_data <- aggregate_data(time1, by = "ID_Shift")
# shift_data <- aggregate_data(time1, by = c("ID_Shift", "Period_End"))
# shift_data <- aggregate_data(time1, by = "ID_Shift", sum_fields = c("mp"))

# REMOVE COLUMN NAME SUFFIXES (after aggregate data step, if needed) ------------------------------------------------------------------------------

remove_suffixes <- function(dt, suffixes) {
  setDT(dt)
  setnames(dt, old = names(dt), new = sub(paste0("(", paste0(suffixes, collapse = "|"), ")$"), "", names(dt)))
  return(dt)
}

# Example usage:
# # Suffixes you want to remove
# suffixes_to_remove <- c("_sum", "_max", "_min", "_mean")
# 
# # Remove suffixes
# dt_clean <- remove_suffixes(dt, suffixes_to_remove)



# RANDOM SAMPLE  (if needed) ------------------------------------------------------------------------------

generate_random_sample <- function(all_ids, 
                                   class1 = NULL, 
                                   case_name = NULL, 
                                   pct = NULL, 
                                   use_class1 = TRUE,
                                   seed_num = 99999) {
  
  # Check required parameters
  if (is.null(case_name)) {
    stop("Please provide a case_name parameter (e.g., 'Plaintiff v Defendant')")
  }
  
  if (is.null(pct)) {
    stop("Please provide a pct parameter (e.g., 0.25 for 25%)")
  }
  
  # Validate percentage
  if (pct <= 0 || pct > 1) {
    stop("Sample percentage (pct) must be between 0 and 1 (e.g., 0.25 for 25%)")
  }
  
  # Check class1 requirement
  if (use_class1 && is.null(class1)) {
    stop("class1 is required when use_class1 = TRUE")
  }
  
  # Start timing
  start_time <- Sys.time()
  
  # Display parameters
  cat("\n========== STARTING RANDOM SAMPLE ==========\n")
  cat("Case Name:", case_name, "\n")
  cat("Sample Percentage:", sprintf("%.1f%%", pct * 100), "\n")
  cat("Class List:", use_class1, "\n")
  cat("Random Seed:", seed_num, "\n")
  cat("===========================================\n\n")
  
  # Get population counts for summary
  time_population <- all_ids[grepl("Time Data", Source), uniqueN(ID)]
  pay_population <- all_ids[grepl("Pay Data", Source), uniqueN(ID)]
  class1_population <- if (use_class1) uniqueN(class1$ID) else NA
  
  # Filter all_ids to get only records that appear in both Time and Pay
  both_time_pay <- all_ids[Source == "Time Data; Pay Data"]
  
  # Error check: Ensure we have data in both_time_pay
  if (nrow(both_time_pay) == 0) {
    stop("No IDs found with Source = 'Time Data; Pay Data'!")
  }
  
  # Get unique IDs based on whether we're using class1
  if (use_class1) {
    ids_in_all_sources <- both_time_pay[ID %in% class1$ID, unique(ID)]
    source_description <- "all three sources (Time, Pay and EE List)"
  } else {
    ids_in_all_sources <- unique(both_time_pay$ID)
    source_description <- "both Time and Pay"
  }
  
  # Error check: Ensure we found matching IDs
  if (length(ids_in_all_sources) == 0) {
    stop(paste0("No IDs found in ", source_description, "!"))
  }
  
  # Create full list data.table
  full_list <- data.table(ID = ids_in_all_sources)
  
  # Set random seed for reproducibility
  set.seed(seed_num)
  
  # Add random number column for sampling
  full_list[, rand := sample(.N)]
  
  # Calculate sample size (rounded up)
  samplesize <- ceiling(pct * nrow(full_list))
  
  # Warning if sample size exceeds population
  if (samplesize > nrow(full_list)) {
    warning(sprintf("Sample size (%s) exceeds population (%s). Using full population.", 
                    samplesize, nrow(full_list)))
    samplesize <- nrow(full_list)
  }
  
  # Create sample by selecting records with rand <= samplesize
  sample_list <- full_list[rand <= samplesize]
  
  # Create percentage text for filename
  pct_text <- paste0(pct * 100, " percent")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(here("output"))) {
    dir.create(here("output"), recursive = TRUE)
  }
  
  # Write outputs
  sample_file <- here("output", paste0(case_name," - ", pct_text, " sample.xlsx"))
  full_file <- here("output", paste0(case_name," - ", pct_text, " sample w rand_id.xlsx"))
  
  write.xlsx(sample_list, sample_file)
  write.xlsx(full_list, full_file)
  
  # -------------------------------------------------------------------- Summary Output ---------------------------------------------------------------
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
  cat(sprintf("  - Sample Percentage: %.1f%% (exact: %.2f%%)\n", 
              pct * 100, 
              (samplesize / length(ids_in_all_sources)) * 100))
  
  cat("\nOUTPUT FILES:\n")
  cat(sprintf("  1. %s\n", basename(sample_file)))
  cat(sprintf("  2. %s\n", basename(full_file)))
  
  # Execution time
  cat(sprintf("\nExecution time: %.2f seconds\n", difftime(Sys.time(), start_time, units = "secs")))
  cat("========================================\n")
  
  # Return results invisibly
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

# # REQUIRED: Set your parameters here
# case_name <- "Plaintiff v Defendant"  # Change this for each case
# pct <- 0.25                       # Change this for different sample size (0.25 = 25%)
# 
# # Option 1: With Class1 List (default)
# results <- generate_random_sample(
#   all_ids = all_ids,
#   class1 = class1,  # Previously Employee_Roster
#   case_name = case_name,
#   pct = pct,
#   use_class1 = TRUE,  # TRUE = match all three sources
#   seed_num = 99999
# )

# Example 1: WITHOUT Class1 List (only match Time and Pay)
# results_no_class1 <- generate_random_sample(
#   all_ids = all_ids,
#   class1 = NULL,  # Can be NULL when use_class1 = FALSE
#   case_name = "Johnson v ABC Corp",
#   pct = 0.30,
#   use_class1 = FALSE,  # FALSE = only match Time and Pay
#   seed_num = 99999
# )

# Example 2: With Class1 List but 10% sample
# results_10pct <- generate_random_sample(
#   all_ids = all_ids,
#   class1 = class1,
#   case_name = "Smith v XYZ Inc",
#   pct = 0.10,
#   use_class1 = TRUE,
#   seed_num = 12345
# )




# GENERATE RANDOM SAMPLE PRODUCTION FILES ---------------------------------------------------------------

generate_production_files <- function(time1, 
                                      pay1, 
                                      sample_list,
                                      class1 = NULL,
                                      case_name, 
                                      class_dmgs_start_date,
                                      overwrite = FALSE,
                                      prod_fields_time = default_prod_fields_time,
                                      prod_fields_pay  = default_prod_fields_pay) {
  
  cat("\n========== CREATING PRODUCTION FILES ==========\n")
  
  # Ensure data.tables
  setDT(time1); setDT(pay1)
  
  # Assign anonymized IDs
  time1[, Anon_ID := NA_real_]
  time1[sample_list, Anon_ID := i.rand, on = .(ID)]
  
  pay1[, Pay_Anon_ID := NA_real_]
  pay1[sample_list, Pay_Anon_ID := i.rand, on = .(Pay_ID = ID)]
  
  if (!is.null(class1)) {
    setDT(class1)
    class1[, Class_Anon_ID := NA_real_]
    class1[sample_list, Class_Anon_ID := i.rand, on = .(ID)]
  }
  
  # Build production time data
  time1_prod <- time1[!is.na(Anon_ID) & Date >= class_dmgs_start_date]
  time1_prod <- time1_prod[, ..prod_fields_time]
  time1_prod[punch_type == "out", Hours := NA]
  setorder(time1_prod, Anon_ID, Date)
  
  # Build production pay data
  pay1_prod <- pay1[!is.na(Pay_Anon_ID) & Pay_Date >= class_dmgs_start_date]
  pay1_prod <- pay1_prod[, ..prod_fields_pay]
  setorder(pay1_prod, Pay_Anon_ID, Pay_Date)
  
  # Ensure prod directory exists
  if (!dir.exists(here("prod"))) {
    dir.create(here("prod"), recursive = TRUE)
    cat("Created 'prod' directory\n")
  }
  
  # Build filenames
  date_suffix <- format(class_dmgs_start_date, "(%Y-%m-%d)")
  time_file <- here("prod", paste0(case_name, " Sample Time Data ", date_suffix, ".xlsx"))
  pay_file  <- here("prod", paste0(case_name, " Sample Pay Data ", date_suffix, ".xlsx"))
  
  # Write time file
  wb_time <- createWorkbook()
  addWorksheet(wb_time, "Sample Time Data")
  writeData(wb_time, 1, time1_prod, rowNames = FALSE)
  saveWorkbook(wb_time, time_file, overwrite = overwrite)
  
  # Write pay file
  wb_pay <- createWorkbook()
  addWorksheet(wb_pay, "Sample Pay Data")
  writeData(wb_pay, 1, pay1_prod, rowNames = FALSE)
  saveWorkbook(wb_pay, pay_file, overwrite = overwrite)
  
  # Sample counts
  sample_count       <- nrow(sample_list)
  time_unique_count  <- uniqueN(time1_prod[[prod_fields_time[1]]])
  pay_unique_count   <- uniqueN(pay1_prod[[prod_fields_pay[1]]])
  
  # Summary
  cat("\nPRODUCTION SUMMARY:\n")
  cat(sprintf("time records: %s\n", format(nrow(time1_prod), big.mark=",")))
  cat(sprintf("pay records:  %s\n", format(nrow(pay1_prod), big.mark=",")))
  
  cat("\nSAMPLE VERIFICATION:\n")
  cat(sprintf("sample size: %s employees\n", format(sample_count, big.mark=",")))
  cat(sprintf("in time data: %s (%.1f%%)\n", 
              time_unique_count, time_unique_count/sample_count*100))
  cat(sprintf("in pay data:  %s (%.1f%%)\n", 
              pay_unique_count, pay_unique_count/sample_count*100))
  
  if (time_unique_count < sample_count)
    cat(sprintf("⚠ missing from time: %s\n", sample_count - time_unique_count))
  if (pay_unique_count < sample_count)
    cat(sprintf("⚠ missing from pay: %s\n", sample_count - pay_unique_count))
  
  cat("\nFILES CREATED:\n")
  cat(" - ", basename(time_file), "\n", sep="")
  cat(" - ", basename(pay_file), "\n", sep="")
  
  if (!overwrite) cat("\n(overwrite = FALSE: existing files preserved)\n")
  cat("===========================================\n")
  
  invisible(list(
    time1_prod = time1_prod,
    pay1_prod  = pay1_prod,
    time_file  = time_file,
    pay_file   = pay_file
  ))
}


# FINAL ANALYSIS TABLE ---------------------------------------------------------------

# =============================================================================
# Unified Metrics Calculator - Driven by metrics_spec.csv
# =============================================================================

library(data.table)
library(lubridate)

# -----------------------------------------------------------------------------
# 1. Load and parse the spec
# -----------------------------------------------------------------------------

metric_spec <- fread("scripts/metrics_spec.csv")

# Add a column to identify metric type based on label patterns
metric_spec[, metric_type := fcase(
  grepl("date", metric_label, ignore.case = TRUE), "date",
  grepl("percent", metric_label, ignore.case = TRUE), "percent",
  default = "value"
)]

# Add row order for preserving spec order
metric_spec[, metric_order := .I]

# -----------------------------------------------------------------------------
# 2. Denominator definitions
# -----------------------------------------------------------------------------

# Time data denominators
denom_functions_time <- list(
  shifts_all              = function(dt) dt[, n_distinct(ID_Shift)],
  shifts_gt_3_5           = function(dt) dt[shift_hrs > 3.5, n_distinct(ID_Shift)],
  shifts_gt_5             = function(dt) dt[shift_hrs > 5, n_distinct(ID_Shift)],
  shifts_gt_6             = function(dt) dt[shift_hrs > 6, n_distinct(ID_Shift)],
  shifts_gt_10            = function(dt) dt[shift_hrs > 10, n_distinct(ID_Shift)],
  shifts_gt_12            = function(dt) dt[shift_hrs > 12, n_distinct(ID_Shift)],
  pay_periods             = function(dt) dt[, n_distinct(ID_Period_End)],
  weeks                   = function(dt) dt[, n_distinct(ID_Week_End)],
  employees               = function(dt) dt[, n_distinct(ID)],
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

# Pay data denominators
denom_functions_pay <- list(
  employees_pay           = function(dt) dt[, n_distinct(Pay_ID)],
  pay_periods_pay         = function(dt) dt[, n_distinct(Pay_ID_Period_End)]
)

# Combined lookup
denom_functions <- c(denom_functions_time, denom_functions_pay)

# -----------------------------------------------------------------------------
# 3. Core evaluation function
# -----------------------------------------------------------------------------

#' Evaluate a single metric expression against a data.table
#' @param dt data.table to evaluate against
#' @param expr_str character string of the R expression
#' @param digits number of decimal places (NA for no rounding)
#' @return numeric or Date result
eval_metric <- function(dt, expr_str, digits = NA) {
  if (is.null(dt) || nrow(dt) == 0) return(NA_real_)
  
  result <- tryCatch(
    dt[, eval(parse(text = expr_str))],
    error = function(e) NA_real_
  )
  
  # Convert Date/POSIXct to numeric for consistent storage
  if (inherits(result, c("Date", "POSIXct", "POSIXlt", "IDate"))) {
    result <- as.numeric(as.Date(result))
  }
  
  # Force to numeric if not already
  if (!is.numeric(result)) {
    result <- tryCatch(as.numeric(result), error = function(e) NA_real_)
  }
  
  # Only round if result is numeric and digits is specified
  digits_num <- suppressWarnings(as.numeric(digits))
  if (!is.na(digits_num) && is.numeric(result) && length(result) == 1 && !is.na(result)) {
    result <- round(result, digits_num)
  }
  
  as.numeric(result)
}

#' Evaluate a denominator function
#' @param dt data.table to evaluate against
#' @param denom_name name of the denominator
#' @param source which data source (shift_data1 or pay1)
#' @return numeric denominator value
eval_denom <- function(dt, denom_name, source) {
  if (is.null(dt) || nrow(dt) == 0 || is.na(denom_name) || denom_name == "") {
    return(NA_real_)
  }
  
  denom_fn <- denom_functions[[denom_name]]
  if (is.null(denom_fn)) return(NA_real_)
  
  tryCatch(
    denom_fn(dt),
    error = function(e) NA_real_
  )
}

# -----------------------------------------------------------------------------
# 4. Calculate all metrics for a given data list
# -----------------------------------------------------------------------------

#' Calculate all metrics from spec for given data
#' @param data_list list with 'shift_data1' and 'pay1' data.tables
#' @param spec data.table of metric specifications
#' @return data.table with columns: metric_group, metric_label, metric_type, value, denom_value, pct
calculate_metrics <- function(data_list, spec) {
  
  results <- lapply(seq_len(nrow(spec)), function(i) {
    src <- spec$source[i]
    dt <- data_list[[src]]
    denom_name <- spec$denom[i]
    digits_val <- suppressWarnings(as.numeric(spec$digits[i]))
    
    # Calculate main metric value
    val <- eval_metric(dt, spec$expr[i], digits_val)
    
    # Calculate denominator if specified
    denom_val <- eval_denom(dt, denom_name, src)
    
    # Calculate percentage
    pct <- if (!is.na(val) && !is.na(denom_val) && denom_val > 0) {
      val / denom_val
    } else {
      NA_real_
    }
    
    list(
      metric_order = spec$metric_order[i],
      metric_group = spec$metric_group[i],
      metric_label = spec$metric_label[i],
      metric_type  = spec$metric_type[i],
      digits       = digits_val,
      value        = val,
      denom_value  = denom_val,
      pct          = pct
    )
  })
  
  rbindlist(results)
}

# -----------------------------------------------------------------------------
# 5. Filtering helpers
# -----------------------------------------------------------------------------

#' Filter time data by expression
filter_time_data <- function(dt, filter_expr = NULL) {
  if (is.null(dt)) return(NULL)
  if (is.null(filter_expr)) return(dt)
  dt[eval(filter_expr)]
}

#' Filter pay data by expression
filter_pay_data <- function(dt, filter_expr = NULL) {
  if (is.null(dt)) return(NULL)
  if (is.null(filter_expr)) return(dt)
  dt[eval(filter_expr)]
}

#' Create filtered data list with consistent filtering
#' CRITICAL: List elements MUST be named to match spec source column
create_filtered_data <- function(time_dt, pay_dt, time_filter = NULL, pay_filter = NULL) {
  list(
    shift_data1 = filter_time_data(time_dt, time_filter),
    pay1 = filter_pay_data(pay_dt, pay_filter)
  )
}

# -----------------------------------------------------------------------------
# 6. Build filter configurations
# -----------------------------------------------------------------------------

#' Build standard filter configurations for time and pay data
build_filter_configs <- function(time_dt, pay_dt, custom_filters = list()) {
  
  configs <- list()
  
  # All Data (no filter)
  configs[["All Data"]] <- list(time_filter = NULL, pay_filter = NULL)
  
  # Year filters
  time_years <- if (!is.null(time_dt)) sort(unique(year(time_dt$Period_End))) else integer(0)
  pay_years  <- if (!is.null(pay_dt)) sort(unique(year(pay_dt$Pay_Period_End))) else integer(0)
  all_years  <- sort(unique(c(time_years, pay_years)))
  
  for (yr in all_years) {
    configs[[as.character(yr)]] <- list(
      time_filter = bquote(year(Period_End) == .(yr)),
      pay_filter  = bquote(year(Pay_Period_End) == .(yr))
    )
  }
  
  # Custom filters
  for (nm in names(custom_filters)) {
    configs[[nm]] <- custom_filters[[nm]]
  }
  
  configs
}

# -----------------------------------------------------------------------------
# 7. Main calculation pipeline
# -----------------------------------------------------------------------------

#' Run full metrics calculation across all filters
run_metrics_pipeline <- function(time_dt, pay_dt, spec, custom_filters = list()) {
  
  # Ensure data.table
  if (!is.null(time_dt)) setDT(time_dt)
  if (!is.null(pay_dt)) setDT(pay_dt)
  
  # Build filter configurations
  filter_configs <- build_filter_configs(time_dt, pay_dt, custom_filters)
  
  # Calculate metrics for each filter
  results_list <- lapply(names(filter_configs), function(filter_name) {
    cfg <- filter_configs[[filter_name]]
    
    filtered_data <- create_filtered_data(
      time_dt, pay_dt,
      cfg$time_filter, cfg$pay_filter
    )
    
    metrics <- calculate_metrics(filtered_data, spec)
    metrics[, filter_name := filter_name]
    metrics
  })
  
  rbindlist(results_list)
}

# -----------------------------------------------------------------------------
# 8. Format output table
# -----------------------------------------------------------------------------

#' Format the raw metrics results into a wide table
#' @param results_dt data.table from run_metrics_pipeline
#' @return formatted data.table ready for export
format_metrics_table <- function(results_dt) {
  
  dt <- copy(results_dt)
  
  # Convert digits to numeric (handles empty strings from CSV)
  dt[, digits := suppressWarnings(as.numeric(digits))]
  
  # Format values based on type, append percentage if available
  dt[, formatted_value := {
    # Round value according to spec digits (already done in eval, but format nicely)
    rounded_val <- fifelse(is.na(digits) | is.na(value), value, round(value, digits))
    
    fv <- fcase(
      metric_type == "date", as.character(as.Date(value, origin = "1970-01-01")),
      is.na(value), "0",
      is.nan(value), "0",
      default = format(rounded_val, big.mark = ",", scientific = FALSE, trim = TRUE, nsmall = 0)
    )
    
    # Remove trailing decimals from whole numbers only if digits is 0 or NA
    fv <- fifelse(is.na(digits) | digits == 0, gsub("\\.0+$", "", fv), fv)
    fv <- gsub("^\\s+|\\s+$", "", fv)  # trim whitespace
    
    # Append percentage if available (always 1 decimal)
    fv <- fifelse(
      !is.na(pct) & !is.nan(pct),
      paste0(fv, " (", round(pct * 100, 1), "%)"),
      fv
    )
    
    fv
  }]
  
  # Clean up NaN display
  dt[formatted_value == "NaN", formatted_value := "0"]
  dt[grepl("NaN", formatted_value), formatted_value := gsub(" \\(NaN%\\)", "", formatted_value)]
  
  # Pivot to wide format, preserving order
  wide_dt <- dcast(dt, metric_order + metric_group + metric_label ~ filter_name, 
                   value.var = "formatted_value")
  
  # Sort by original spec order
  setorder(wide_dt, metric_order)
  
  # Reorder columns: metric_group, metric_label, All Data, years, others
  all_cols <- names(wide_dt)
  year_cols <- sort(all_cols[grepl("^\\d{4}$", all_cols)])
  other_cols <- setdiff(all_cols, c("metric_order", "metric_group", "metric_label", "All Data", year_cols))
  
  col_order <- c("metric_group", "metric_label", "All Data", year_cols, other_cols)
  col_order <- col_order[col_order %in% all_cols]
  
  setcolorder(wide_dt, col_order)
  wide_dt[, metric_order := NULL]  # Drop the order column
  
  wide_dt
}

# =============================================================================
# USAGE EXAMPLE
# =============================================================================

# # Load your data
# shift_data1 <- fread("data/time_data.csv")
# pay1 <- fread("data/pay_data.csv")
# 
# # Load spec
# metric_spec <- fread("scripts/metrics_spec.csv")
# metric_spec[, metric_type := fcase(
#   grepl("date", metric_label, ignore.case = TRUE), "date",
#   grepl("percent", metric_label, ignore.case = TRUE), "percent",
#   default = "value"
# )]
# metric_spec[, metric_order := .I]
# 
# # Define custom filters (optional)
# custom_filters <- list(
#   "Erik Brown" = list(
#     time_filter = quote(ID == "21003"),
#     pay_filter  = quote(Pay_ID == "21003")
#   )
# )
# 
# # Run pipeline
# raw_results <- run_metrics_pipeline(shift_data1, pay1, metric_spec, custom_filters)
# 
# # Format and export
# final_table <- format_metrics_table(raw_results)
# fwrite(final_table, "output/Analysis.csv")
