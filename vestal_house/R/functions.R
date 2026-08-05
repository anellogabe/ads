# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Shared helpers ----

library(data.table)

# Currency / percent formatting for UI output
fmt_dollar <- function(x, accuracy = 1) scales::dollar(x, accuracy = accuracy)
fmt_pct    <- function(x, accuracy = 0.1) scales::percent(x, accuracy = accuracy)

# Read an RDS cache if present, else compute and cache
cached <- function(path, expr) {
  if (file.exists(path)) return(readRDS(path))
  val <- expr
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(val, path)
  val
}

# Append-only CSV logger used for construction actuals and task updates.
# Creates the file with headers on first write.
append_log_row <- function(path, row) {
  stopifnot(is.list(row))
  dt <- as.data.table(row)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  fwrite(dt, path, append = file.exists(path))
  invisible(dt)
}

# Safe CSV reader: returns an empty data.table with the given columns when the
# file doesn't exist yet, so modules render cleanly on a fresh checkout.
read_log <- function(path, col_types) {
  if (!file.exists(path)) {
    empty <- lapply(col_types, function(t) vector(t, 0L))
    return(as.data.table(empty))
  }
  fread(path)
}

# Validate params structure early so config mistakes fail loudly at startup
validate_params <- function(p) {
  required <- c("property", "acquisition", "financing", "operating", "rehab", "phases")
  missing <- setdiff(required, names(p))
  if (length(missing)) stop("params missing sections: ", paste(missing, collapse = ", "))
  stopifnot(
    p$financing$ltv >= 0, p$financing$ltv < 1,
    p$operating$vacancy_rate >= 0, p$operating$vacancy_rate < 1,
    is.data.table(p$rehab), is.data.table(p$phases)
  )
  invisible(TRUE)
}
