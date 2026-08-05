# Where the Comparable Data Comes From

Two different questions need two different kinds of data, and they come from
different places:

1. **Market level** — is the Vestal market rising, what do homes rent/sell for
   per square foot in aggregate? → *automated, free, scripted.*
2. **Property level** — what did specific comparable homes actually sell for?
   → *no free bulk source exists; exported or pulled manually into `data/raw/`.*

## 1. Market-level series (automated: `scripts/fetch_market_data.R`)

| Source | What | Granularity | Access |
|---|---|---|---|
| [Zillow Research](https://www.zillow.com/research/data/) ZHVI | Typical home value ($) | ZIP 13850 + neighbors, monthly | Free CSV, scripted |
| [Zillow Research](https://www.zillow.com/research/data/) ZORI | Typical observed rent ($/mo) | ZIP, monthly | Free CSV, scripted |
| [FRED](https://fred.stlouisfed.org/series/ATNHPIUS36007A) FHFA HPI | House price index | Broome County, annual | Free CSV, scripted |
| [Redfin Data Center](https://www.redfin.com/news/data-center/) | Median sale price, $/sqft, DOM, inventory | ZIP, monthly | Free TSV (~1.5GB) — off by default in the script |

Run `source("scripts/fetch_market_data.R")` from the repo root; results land in
`data/processed/market_indices.rds` and appear on the dashboard's
**Market → Market Indices** tab. Re-run monthly (Zillow updates mid-month).

## 2. Property-level comps (manual: drop CSVs into `data/raw/`)

Individual sold-home data is controlled by MLS and county records; there is no
legitimate free bulk download. Practical options, best first:

1. **MLS export via an agent** (best quality). Any local agent can export
   Vestal solds — new-construction and 3,000+ SF homes from the last 24-36
   months are the relevant set for this build. Greater Binghamton MLS covers
   the area. One email, one CSV.
2. **Broome County property records** (free, authoritative). Broome County
   runs public parcel access (Image Mate Online / SDG) with assessments and
   sales history: search Town of Vestal, filter sales. Slower, but it is the
   county's own record of every arm's-length sale.
3. **Zillow / Redfin sold listings** (free, quick). Search Vestal NY sold
   homes with filters (e.g. 2,500+ SF, sold last 2 years), and export/copy
   into the CSV schema below. Redfin's download button on search results
   gives a CSV directly.
4. **NYS ORPS / Open NY** ([data.ny.gov](https://data.ny.gov)) — statewide
   residential assessment and sales datasets, filterable to Broome County.

### CSV schema for `data/raw/`

Any CSV with at least `type, price, sqft` loads; full schema:

```csv
comp_id,source,type,address,list_date,close_date,price,sqft,beds,baths,year_built,dom
1,mls,sale,123 Example Dr Vestal NY,2026-01-05,2026-02-20,585000,3400,4,3.5,2018,46
2,mls,rent,45 Sample Ln Vestal NY,2026-03-01,2026-03-18,2400,1800,3,2,1995,17
```

`type` is `sale` or `rent`. The dashboard falls back to clearly-labeled
synthetic demo data until at least one real CSV exists.

### What "good comps" means for this build

The appraisal question for a 6,000 SF custom home in Vestal is comp scarcity —
few local sales are truly comparable. Prioritize:

- Sales **> 3,000 SF** in Vestal CSD, then widen to Binghamton metro
- **New or newer construction** (2015+) at any size, to calibrate the
  new-construction premium parameter (`params$value$new_construction_premium`)
- The **top decile by price** of Vestal sales — that is the segment this
  house competes in

Update `config/params.R` valuation assumptions as real comps land, and watch
the **Cost-to-Value** KPI on the Overview tab: in low-cost upstate markets a
large custom build routinely costs more than its appraised value. That is a
lifestyle decision, not necessarily a mistake — but the dashboard will show
the number honestly.
