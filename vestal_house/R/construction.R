# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Construction management ----
# Two append-only logs live in data/raw/ (git-ignored):
#
#   spend_log.csv:  date, phase, category, vendor, amount, note
#   task_log.csv:   task_id, phase, task, owner, status, planned_start,
#                   planned_end, actual_start, actual_end, note
#
# Log a spend entry from any R session:
#   source("R/functions.R"); source("R/construction.R")
#   log_spend(date = Sys.Date(), phase = 1, category = "Electrical",
#             vendor = "ACME Electric", amount = 1250, note = "panel deposit")

library(data.table)

SPEND_LOG <- "data/raw/spend_log.csv"
TASK_LOG  <- "data/raw/task_log.csv"

log_spend <- function(date, phase, category, vendor, amount, note = "",
                      path = SPEND_LOG) {
  append_log_row(path, list(date = as.character(date), phase = as.integer(phase),
                            category = category, vendor = vendor,
                            amount = as.numeric(amount), note = note))
}

log_task <- function(task_id, phase, task, owner = "GA",
                     status = c("planned", "in_progress", "blocked", "done"),
                     planned_start = NA, planned_end = NA,
                     actual_start = NA, actual_end = NA, note = "",
                     path = TASK_LOG) {
  status <- match.arg(status)
  append_log_row(path, list(
    task_id = task_id, phase = as.integer(phase), task = task, owner = owner,
    status = status,
    planned_start = as.character(planned_start), planned_end = as.character(planned_end),
    actual_start = as.character(actual_start), actual_end = as.character(actual_end),
    note = note))
}

read_spend <- function(path = SPEND_LOG) {
  read_log(path, c(date = "character", phase = "integer", category = "character",
                   vendor = "character", amount = "numeric", note = "character"))
}

# Latest row per task_id wins, so status updates are just new log rows
read_tasks <- function(path = TASK_LOG) {
  dt <- read_log(path, c(task_id = "character", phase = "integer", task = "character",
                         owner = "character", status = "character",
                         planned_start = "character", planned_end = "character",
                         actual_start = "character", actual_end = "character",
                         note = "character"))
  if (!nrow(dt)) return(dt)
  dt[, row := .I][, .SD[which.max(row)], by = task_id][, row := NULL][]
}

# Budget vs actual by phase and category
budget_vs_actual <- function(rehab_budget, spend = read_spend()) {
  budget <- rehab_budget[, .(budget = sum(budget)), by = .(phase, category)]
  actual <- if (nrow(spend)) {
    spend[, .(actual = sum(amount)), by = .(phase, category)]
  } else {
    data.table(phase = integer(), category = character(), actual = numeric())
  }
  out <- merge(budget, actual, by = c("phase", "category"), all = TRUE)
  out[is.na(budget), budget := 0][is.na(actual), actual := 0]
  out[, variance := actual - budget]
  out[, pct_spent := fifelse(budget > 0, actual / budget, NA_real_)]
  out[order(phase, category)]
}

# Phase-level rollup joined to phase definitions and gates
phase_summary <- function(p, spend = read_spend()) {
  bva <- budget_vs_actual(p$rehab, spend)
  roll <- bva[, .(budget = sum(budget), actual = sum(actual)), by = phase]
  out <- merge(p$phases, roll, by = "phase", all.x = TRUE)
  out[is.na(budget), budget := 0][is.na(actual), actual := 0]
  out[, remaining := budget - actual][order(phase)]
}
