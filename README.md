# Vestal House Project — Anello Residence

Ground-up custom home build at **117 Red Fox Run, Vestal, NY 13850**, managed
with the same R/data.table/Shiny tooling used across ADS projects. Plans by
Griffiths Engineering and Architecture, PLLC (SD set 2026-07-31, project
2026-106).

## 📋 Table of Contents

- [Project Goals](#project-goals)
- [Property & Plans](#property--plans)
- [Market Thesis](#market-thesis)
- [Architecture](#architecture)
- [Modules](#modules)
- [Cost Model](#cost-model)
- [Market Analytics & Comps](#market-analytics--comps)
- [Construction Management](#construction-management)
- [Incremental Development Strategy](#incremental-development-strategy)
- [Setup](#setup)
- [Usage](#usage)

## 🎯 Project Goals

1. **Build the Anello Residence** to the Griffiths plan set, on budget,
   phase-gated from permits through certificate of occupancy.
2. **Model everything** — every dollar (land, soft costs, construction,
   financing, carrying costs) flows through one parameterized model so
   decisions are made on numbers, not vibes.
3. **Track construction like a project** — budget vs. actual, tasks, and
   spend history live in one dashboard, updated as work happens.
4. **Stay honest on value** — comp-based projected value and the
   Cost-to-Value KPI are always on screen; scope decisions (finish level,
   basement) are made against them.

## 🏠 Property & Plans

The full SD sheet set lives in [`plans/`](plans/) as the source PDF plus
**200-DPI PNG renders (7200×4800)** viewable in the dashboard's **Plans tab**
— scroll-wheel zoom about the cursor, drag to pan, double-click to reset,
with a full-resolution link per sheet.

| Sheet | Content |
|---|---|
| A100 | Basement plan — rooms 1-3, bath, ½ bath, utility, 10 ft depth |
| A101 | First floor — great room, kitchen + butler's pantry, dining, den, office, guest suite (2 baths), mudroom, 3-car garage, covered porches, outside kitchen |
| A102 | Second floor — game room, bedrooms 2-4 (walk-ins), sitting area, 4 baths |
| A201 | Elevations (N/S/E/W) with story heights |

Program summary (measured off the SD set; update at DD/CD):
**~6,020 SF finished above grade** (first ~2,800 + second ~3,220),
~2,685 SF basement (finish optional), 1,381 SF three-car garage, ~1,500 SF
covered porches/decks, 255 SF outside kitchen, 4+ beds / 5.5 baths.
All figures live in `config/params.R`.

## 📈 Market Thesis

Vestal / Binghamton metro (Broome County, NY):

- **Anchor demand**: Binghamton University plus regional healthcare and
  defense employers underpin the Vestal CSD premium submarket.
- **Supply**: little new construction and few large modern homes — this
  build sits in a thin top-of-market segment, which cuts both ways:
  scarcity premium, but comp scarcity at appraisal.
- **The honest risk**: in low-cost upstate markets, a large custom build can
  cost more than it appraises for. The dashboard tracks this explicitly
  (Cost-to-Value KPI) instead of hiding it.

The thesis is *testable*: Market Analytics tracks Vestal-area indices and
comps so assumptions are checked against data.

## 🏗 Architecture

R + data.table + Shiny (bslib):

```
vestal-house/
├── README.md                 # This file
├── app.R                     # Shiny dashboard (wires all modules)
├── run_app.R                 # Convenience launcher
│
├── config/
│   └── params.R              # ALL assumptions + derived construction budget
│
├── R/
│   ├── functions.R           # Shared helpers (formatting, IO, validation)
│   ├── investment_model.R    # Basis, amortization, carrying cost, value metrics
│   ├── market_analytics.R    # Comps, trends, value estimate, indices
│   └── construction.R        # Budget vs actual, spend/task logs
│
├── modules/
│   ├── mod_overview.R        # KPIs and project status
│   ├── mod_plans.R           # Zoomable high-res plan viewer
│   ├── mod_investment.R      # Budget tab: cost model with live levers
│   ├── mod_market.R          # Market analytics tab
│   ├── mod_construction.R    # Construction management tab
│   └── mod_phases.R          # Phase plan and gates
│
├── plans/                    # SD sheet PDFs + 200-DPI PNG renders
├── scripts/
│   └── fetch_market_data.R   # Zillow/FRED/Redfin market series
│
├── data/
│   ├── raw/                  # Comp CSVs, spend/task logs (git-ignored)
│   └── processed/            # RDS caches incl. market indices (git-ignored)
│
└── docs/
    ├── build_plan.md         # Phase-by-phase plan with gates and risks
    ├── cost_research.md      # Cost basis, benchmarks, sources
    └── market_data.md        # Where comparable data comes from
```

Design rules: **data.table only** in compute paths; **config-driven** (change
an assumption in `params.R` and everything reflows); modules are namespaced
Shiny modules; **no personal/financial data committed** (`data/` is
git-ignored — the plan set is committed deliberately).

## 🧩 Modules

| Tab | Purpose |
|---|---|
| Overview | Total basis, construction budget, comp-based projected value, cost-to-value, equity, monthly carrying cost, phase status |
| Plans | Zoomable 200-DPI sheet viewer (A100/A101/A102/A201) |
| Budget | Live cost model — $/sqft, rate, and LTC sliders reflow the NAHB-weighted budget, carrying cost, amortization, optional rental scenario |
| Market | Comps, price trends, $/sqft distribution, DOM, Zillow/FRED indices |
| Construction | Budget vs. actual by phase/category, task board, spend timeline |
| Phases | Phase plan, gates, per-line budget detail, funding runway |

## 💰 Cost Model

The construction budget derives from **one number** —
`cost_basis$cost_per_sqft_core` ($260 planning; range $200–$325) — applied
to above-grade finished area and distributed across **NAHB 2024 phase
weights**, plus separately-priced adders (garage, porches, outside kitchen,
optional basement finish) and a 10% contingency. At the planning number the
construction subtotal is **~$1.95M** and total project basis **~$2.1M**.
Full math and sources: [`docs/cost_research.md`](docs/cost_research.md).
Financing is modeled construction-to-perm on loan-to-cost.

## 📊 Market Analytics & Comps

Short answer to "where does comparable data come from":

- **Market-level (automated)** — `scripts/fetch_market_data.R` pulls Zillow
  ZHVI/ZORI for ZIP 13850 + neighbors and the FRED Broome County house price
  index (Redfin's zip tracker optional).
- **Property-level (manual)** — sold comps come from an MLS export via any
  local agent (best), Broome County parcel records, or Zillow/Redfin sold-
  listing exports, dropped as CSVs into `data/raw/`.

Full sourcing guide with links and the CSV schema:
[`docs/market_data.md`](docs/market_data.md). The dashboard uses clearly-
labeled synthetic demo data until real comps land.

## 🔨 Construction Management

Append-only logs (`log_spend()`, `log_task()` in `R/construction.R`) feed
budget-vs-actual by phase and category, a task board, and a cumulative spend
timeline. Spend history stays auditable — useful at gate reviews and in
contractor conversations.

## 🪜 Incremental Development Strategy

Seven gated phases from pre-construction to optional basement finish — no
phase starts until the previous one passes its numeric gate. Full detail:
[`docs/build_plan.md`](docs/build_plan.md).

| Phase | Scope | Gate |
|---|---|---|
| 0 — Pre-Construction | Permits, survey, geotech, financing, GC bids | Signed bid within 10% of model; financing closed |
| 1 — Site & Foundation | Excavation, footings, 10 ft walls, slab | Foundation inspection passed |
| 2 — Shell | Framing incl. garage, roof, windows, siding | Weather-tight |
| 3 — Systems Rough-In | Electrical, plumbing, HVAC | Rough-in inspections passed |
| 4 — Interior Finish | Insulation through fixtures | Certificate of occupancy |
| 5 — Exterior & Site | Porches, outside kitchen, driveway, landscape | Punch list clear; appraisal supports model |
| 6 — Basement Finish | Optional, per A100 | Go/no-go on cost vs. value |

## 🚀 Setup

```r
install.packages(c(
  "shiny", "bslib", "data.table", "lubridate",
  "DT", "plotly", "shinycssloaders", "scales"
))
```

## ▶ Usage

```r
# From the repo root (C:/Repos/vestal-house):
shiny::runApp(".")          # or source("run_app.R")
```

1. Review the plans on the **Plans** tab.
2. Replace `TODO` values in `config/params.R` as real numbers arrive (land
   basis, GC bid, tax estimate, financing quote).
3. Run `scripts/fetch_market_data.R` for market indices; add comp CSVs to
   `data/raw/` per `docs/market_data.md`.
4. Log construction spend and tasks as they happen; review gates on the
   Overview/Phases tabs before each phase.

## 📄 License

Personal project of Anello Data Solutions LLC. All rights reserved.
Plan drawings © Griffiths Engineering and Architecture, PLLC — do not
redistribute outside the project.
