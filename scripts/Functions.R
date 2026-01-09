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


# ... (including all the other functions from your Functions.R file)
# For brevity, I'll include key functions. You should paste your full Functions.R content here.
