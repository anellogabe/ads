# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Market analytics ----
# Normalized comp schema:
#   comp_id, source, type ("sale" | "rent"), address, list_date, close_date,
#   price, sqft, beds, baths, year_built, dom (days on market)

library(data.table)

COMP_COLS <- c("comp_id", "source", "type", "address", "list_date", "close_date",
               "price", "sqft", "beds", "baths", "year_built", "dom")

# Load every CSV in data/raw/ that looks like a comp export and normalize it.
# Column-name matching is case-insensitive on the schema names above; files
# without at least type/price/sqft are skipped with a message.
load_comps <- function(raw_dir = "data/raw") {
  files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) return(demo_comps())
  out <- list()
  for (f in files) {
    dt <- fread(f)
    setnames(dt, tolower(names(dt)))
    if (!all(c("type", "price", "sqft") %in% names(dt))) {
      message("Skipping ", basename(f), " — missing type/price/sqft columns")
      next
    }
    for (col in COMP_COLS) if (!col %in% names(dt)) dt[, (col) := NA]
    dt[, source := fifelse(is.na(source), basename(f), as.character(source))]
    out[[f]] <- dt[, ..COMP_COLS]
  }
  if (!length(out)) return(demo_comps())
  comps <- rbindlist(out)
  comps[, comp_id := fifelse(is.na(comp_id), as.character(.I), as.character(comp_id))]
  comps[]
}

# Synthetic Vestal-area demo comps so the dashboard runs before real data
# is loaded. Clearly labeled source = "demo" — replace with MLS/Zillow exports.
demo_comps <- function(seed = 42L) {
  set.seed(seed)
  n_sale <- 24L; n_rent <- 30L
  months <- seq(as.IDate("2025-01-01"), as.IDate("2026-07-01"), by = "month")
  sales <- data.table(
    comp_id = paste0("S", seq_len(n_sale)), source = "demo", type = "sale",
    address = paste(sample(100:999, n_sale), "Demo St, Vestal NY"),
    close_date = sample(months, n_sale, replace = TRUE),
    sqft = round(rnorm(n_sale, 2400, 900)),
    beds = sample(3:5, n_sale, replace = TRUE),
    baths = sample(c(1, 1.5, 2), n_sale, replace = TRUE),
    year_built = sample(1950:1995, n_sale, replace = TRUE),
    dom = pmax(5L, round(rnorm(n_sale, 38, 15)))
  )
  sales[, price := round(sqft * rnorm(n_sale, 135, 20))]
  sales[, list_date := close_date - dom]
  rents <- data.table(
    comp_id = paste0("R", seq_len(n_rent)), source = "demo", type = "rent",
    address = paste(sample(100:999, n_rent), "Demo Ave, Vestal NY"),
    close_date = sample(months, n_rent, replace = TRUE),
    sqft = round(rnorm(n_rent, 1400, 350)),
    beds = sample(2:4, n_rent, replace = TRUE),
    baths = sample(c(1, 1.5, 2), n_rent, replace = TRUE),
    year_built = sample(1950:2000, n_rent, replace = TRUE),
    dom = pmax(3L, round(rnorm(n_rent, 24, 10)))
  )
  rents[, price := round(sqft * rnorm(n_rent, 1.22, 0.12))]
  rents[, list_date := close_date - dom]
  rbindlist(list(sales, rents), use.names = TRUE)[, ..COMP_COLS]
}

# Monthly median $/sqft trend by comp type
market_trend <- function(comps) {
  dt <- copy(comps)[!is.na(close_date) & sqft > 0]
  dt[, month := format(as.IDate(close_date), "%Y-%m")]
  dt[, ppsf := price / sqft]
  dt[, .(median_ppsf = median(ppsf), n = .N, median_dom = median(as.numeric(dom), na.rm = TRUE)),
     by = .(type, month)][order(type, month)]
}

# Rent estimate for the subject from rent-comp $/sqft, with a condition
# adjustment: renovated units in this market clear a premium to the median.
estimate_rent <- function(comps, subject_sqft, condition_premium = 0.10) {
  rent_ppsf <- comps[type == "rent" & sqft > 0, median(price / sqft, na.rm = TRUE)]
  if (!is.finite(rent_ppsf)) return(NA_real_)
  round(rent_ppsf * subject_sqft * (1 + condition_premium))
}

# Where the subject's basis sits versus sale comps ($/sqft percentile)
basis_percentile <- function(comps, basis, subject_sqft) {
  ppsf <- comps[type == "sale" & sqft > 0, price / sqft]
  if (!length(ppsf)) return(NA_real_)
  mean(ppsf <= basis / subject_sqft)
}

# Comp-based value estimate for the finished home, appraisal-style:
# above-grade finished sqft x sale-comp median $/sqft x new-construction
# premium, plus a partial contribution for finished basement space.
# Uses the upper-half of sale comps by $/sqft when enough comps exist,
# since a luxury new build competes with the top of the market.
estimate_value <- function(comps, p, basement_finished = FALSE) {
  ppsf <- comps[type == "sale" & sqft > 0, price / sqft]
  if (!length(ppsf)) return(NA_real_)
  base_ppsf <- if (length(ppsf) >= 10) median(ppsf[ppsf >= median(ppsf)]) else median(ppsf)
  v <- base_ppsf * p$property$sqft_finished * p$value$new_construction_premium
  if (basement_finished) {
    v <- v + p$property$sqft_basement * p$value$basement_contrib_per_sqft
  }
  round(v, -3)
}

# Market index series produced by scripts/fetch_market_data.R
# (ZHVI/ZORI/FRED HPI), long format: source, region, date, value
load_market_indices <- function(path = "data/processed/market_indices.rds") {
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}
