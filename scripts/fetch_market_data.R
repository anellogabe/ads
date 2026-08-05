# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Fetch public market data for the Vestal / Broome County market ----
# Run from the repo root:  source("scripts/fetch_market_data.R")
#
# Downloads free, public series and writes a normalized long table to
# data/processed/market_indices.rds (source, region, date, value), which the
# dashboard's Market > Market Indices tab reads. Each source is independent —
# a failure in one (network, URL change) skips it with a message.
#
# Property-level comps (individual sold homes) can NOT be bulk-downloaded from
# any free source — see docs/market_data.md for the comp workflow (Zillow/
# Redfin exports, MLS via agent, Broome County parcel access) that feeds
# data/raw/*.csv instead.

library(data.table)

ZIPS  <- c("13850")                       # Vestal
ZIPS_NEARBY <- c("13760", "13903", "13905")  # Endicott/Endwell, Binghamton
ALL_ZIPS <- c(ZIPS, ZIPS_NEARBY)

out_dir <- "data/processed"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
indices <- list()

try_fetch <- function(label, expr) {
  message("Fetching ", label, " ...")
  tryCatch(expr, error = function(e) {
    message("  SKIPPED ", label, ": ", conditionMessage(e))
    NULL
  })
}

# ---- 1. Zillow Home Value Index (ZHVI), ZIP level ----
# All-homes smoothed, seasonally adjusted. ~30MB CSV covering every US zip.
# Catalog: https://www.zillow.com/research/data/
indices$zhvi <- try_fetch("Zillow ZHVI (zip)", {
  url <- paste0("https://files.zillowstatic.com/research/public_csvs/zhvi/",
                "Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
  dt <- fread(url, showProgress = FALSE)
  dt <- dt[RegionName %in% as.integer(ALL_ZIPS) | RegionName %in% ALL_ZIPS]
  if (!nrow(dt)) stop("no Vestal-area zips found in ZHVI file")
  date_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(dt), value = TRUE)
  long <- melt(dt, id.vars = "RegionName", measure.vars = date_cols,
               variable.name = "date", value.name = "value")
  long[!is.na(value),
       .(source = "Zillow ZHVI ($)", region = paste0("zip ", RegionName),
         date = as.character(date), value = as.numeric(value))]
})

# ---- 2. Zillow Observed Rent Index (ZORI), ZIP level ----
indices$zori <- try_fetch("Zillow ZORI (zip)", {
  url <- paste0("https://files.zillowstatic.com/research/public_csvs/zori/",
                "Zip_zori_uc_sfrcondomfr_sm_month.csv")
  dt <- fread(url, showProgress = FALSE)
  dt <- dt[RegionName %in% as.integer(ALL_ZIPS) | RegionName %in% ALL_ZIPS]
  if (!nrow(dt)) stop("no Vestal-area zips found in ZORI file")
  date_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(dt), value = TRUE)
  long <- melt(dt, id.vars = "RegionName", measure.vars = date_cols,
               variable.name = "date", value.name = "value")
  long[!is.na(value),
       .(source = "Zillow ZORI ($/mo)", region = paste0("zip ", RegionName),
         date = as.character(date), value = as.numeric(value))]
})

# ---- 3. FRED — FHFA House Price Index, Broome County NY ----
# Annual index (2000=100). Series: ATNHPIUS36007A.
indices$fred <- try_fetch("FRED Broome County HPI", {
  url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=ATNHPIUS36007A"
  dt <- fread(url, showProgress = FALSE)
  setnames(dt, c("date", "value"))
  dt[!is.na(value),
     .(source = "FHFA HPI (index)", region = "Broome County",
       date = as.character(date), value = as.numeric(value))]
})

# ---- 4. Redfin Data Center (OPTIONAL — large download, off by default) ----
# Zip-level market tracker is ~1.5GB compressed; enable only if you want
# median sale price / $-per-sqft / DOM time series with fine granularity.
# https://www.redfin.com/news/data-center/
FETCH_REDFIN <- FALSE
if (FETCH_REDFIN) {
  indices$redfin <- try_fetch("Redfin zip market tracker (1.5GB)", {
    url <- paste0("https://redfin-public-data.s3.us-west-2.amazonaws.com/",
                  "redfin_market_tracker/zip_code_market_tracker.tsv000.gz")
    tmp <- file.path(tempdir(), "redfin_zip.tsv.gz")
    download.file(url, tmp, mode = "wb", quiet = TRUE)
    dt <- fread(tmp)
    dt <- dt[grepl(paste(ALL_ZIPS, collapse = "|"), region)]
    dt[!is.na(median_sale_price),
       .(source = "Redfin median sale ($)", region,
         date = as.character(period_end),
         value = as.numeric(median_sale_price))]
  })
}

# ---- Write combined output ----
combined <- rbindlist(Filter(Negate(is.null), indices), use.names = TRUE)
if (nrow(combined)) {
  saveRDS(combined, file.path(out_dir, "market_indices.rds"))
  fwrite(combined, file.path(out_dir, "market_indices_backup.csv"))
  message(sprintf("Wrote %s rows from %s source(s) to %s/market_indices.rds",
                  format(nrow(combined), big.mark = ","),
                  length(unique(combined$source)), out_dir))
} else {
  message("No sources succeeded — check your network and see docs/market_data.md")
}
