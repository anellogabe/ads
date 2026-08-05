# Vestal House Project

Personal real-estate build/investment project: acquisition, incremental renovation, and
rental operation of a property in Vestal, NY — managed with the same R/data.table/Shiny
tooling used across ADS projects.

## 📋 Table of Contents

- [Project Goals](#project-goals)
- [Property Details](#property-details)
- [Market Thesis](#market-thesis)
- [Architecture](#architecture)
- [Modules](#modules)
- [Investment Model](#investment-model)
- [Market Analytics](#market-analytics)
- [Construction Management](#construction-management)
- [Incremental Development Strategy](#incremental-development-strategy)
- [Setup](#setup)
- [Usage](#usage)

## 🎯 Project Goals

1. **Acquire and stabilize** a single property in Vestal, NY at or below market value.
2. **Renovate incrementally** — self-funded phases, no large construction loan, each phase
   completed and rent-producing before the next begins.
3. **Model everything** — every dollar (purchase, rehab, financing, operating) flows through
   a single parameterized investment model so decisions are made on numbers, not vibes.
4. **Track construction like a project** — budget vs. actual, task status, and schedule in
   one dashboard, updated as work happens.
5. **Reach target returns** — positive monthly cash flow after stabilization and a
   levered IRR that clears the hurdle rate set in `config/params.R`.

## 🏠 Property Details

All property-specific facts live in **`config/params.R`** (single source of truth).
Placeholder values are marked `# TODO` — update them with actuals from the purchase
contract, inspection, and appraisal:

- Address, parcel/tax ID, school district
- Lot size, gross square footage, beds/baths, year built
- Purchase price, closing costs, inspection findings
- Current condition notes and deferred-maintenance list

## 📈 Market Thesis

Vestal / Binghamton metro (Broome County, NY):

- **Anchor demand**: Binghamton University (~18K students, growing STEM/health-sciences
  footprint) plus regional healthcare and defense employers drive persistent rental demand.
- **Supply constraints**: aging housing stock, little new construction — renovated units
  command a rent premium over the tired median unit.
- **Entry pricing**: acquisition basis per square foot is low relative to achievable rents,
  supporting strong gross yield versus larger upstate metros.
- **Strategy fit**: buy below replacement cost, add value through renovation, hold for
  cash flow; refinance optionality once stabilized.

The thesis is *testable*: the Market Analytics module tracks local comps, rent trends, and
days-on-market so the assumptions above are checked against data, not assumed.

## 🏗 Architecture

R + data.table + Shiny (bslib), consistent with the rest of this repo:

```
vestal_house/
├── README.md                 # This file
├── app.R                     # Shiny dashboard (wires all modules)
├── run_app.R                 # Convenience launcher
│
├── config/
│   └── params.R              # ALL assumptions: property, financing, rehab, market
│
├── R/
│   ├── functions.R           # Shared helpers (formatting, IO, validation)
│   ├── investment_model.R    # Amortization, pro forma cash flow, IRR/NPV/CoC
│   ├── market_analytics.R    # Comps, rent trends, days-on-market
│   └── construction.R        # Budget vs actual, task tracking, schedule
│
├── modules/
│   ├── mod_overview.R        # KPIs and project status
│   ├── mod_investment.R      # Investment model tab
│   ├── mod_market.R          # Market analytics tab
│   ├── mod_construction.R    # Construction management tab
│   └── mod_phases.R          # Incremental development phases tab
│
├── data/
│   ├── raw/                  # Comps exports, contractor quotes (git-ignored)
│   └── processed/            # RDS caches (git-ignored)
│
└── docs/
    └── build_plan.md         # Phase-by-phase build plan detail
```

Design rules:

- **data.table only** for data manipulation — no dplyr in compute paths.
- **Config-driven**: modules read from `params`; changing an assumption re-flows the model.
- **Modules are namespaced Shiny modules** — each tab is independently testable.
- **No client/personal data committed** — `data/` is git-ignored; commit code and config only.

## 🧩 Modules

| Module | File | Purpose |
|---|---|---|
| Overview | `modules/mod_overview.R` | Headline KPIs: total basis, cash invested, projected cash flow, IRR, phase status |
| Investment Model | `modules/mod_investment.R` | Interactive pro forma — flex rent, rate, rehab cost; see cash flow, CoC, IRR, NPV update live |
| Market Analytics | `modules/mod_market.R` | Comps table, rent trend chart, $/sqft distribution, days-on-market |
| Construction | `modules/mod_construction.R` | Budget vs. actual by category, task board, spend timeline |
| Phases | `modules/mod_phases.R` | Incremental development plan: scope, budget, status, and gate criteria per phase |

## 💰 Investment Model

Implemented in `R/investment_model.R`:

- **Acquisition stack**: purchase price + closing costs + phased rehab = total basis.
- **Financing**: standard amortization schedule (`amortization_schedule()`), any rate/term;
  supports all-cash by setting `ltv = 0`.
- **Operating pro forma**: rent, vacancy, taxes, insurance, maintenance reserve, capex
  reserve, management — monthly and annual views.
- **Return metrics**: cash-on-cash, cap rate on basis, DSCR, levered IRR (`irr()` via
  uniroot on the cash-flow vector), NPV at the hurdle rate, and simple payback.
- **Sensitivity**: one-way sensitivity of IRR to rent, rate, and rehab overrun.

## 📊 Market Analytics

Implemented in `R/market_analytics.R`:

- Load comp exports (Zillow/Redfin/MLS CSVs) from `data/raw/` into a normalized
  data.table schema (`load_comps()`).
- Rent trend and $/sqft analysis for the Vestal submarket.
- Rent estimate for the subject property from comp $/sqft with condition adjustment.
- Ships with a small **synthetic demo dataset** (`demo_comps()`) so the dashboard runs
  before any real data is loaded.

## 🔨 Construction Management

Implemented in `R/construction.R`:

- **Budget**: line-item budget by phase and category (from `config/params.R`), joined to
  actual spend entries; variance and % complete by category.
- **Tasks**: task table with phase, status (`planned/in_progress/blocked/done`), owner,
  and dependency notes.
- **Schedule**: planned vs. actual dates per task, rendered as a timeline in the dashboard.
- Actuals are logged as rows (date, phase, category, vendor, amount, note) — append-only,
  so the spend history is auditable.

## 🪜 Incremental Development Strategy

The build is deliberately phased — each phase is small enough to self-fund, and each ends
with a **gate review** in the dashboard before the next begins. Full detail in
[`docs/build_plan.md`](docs/build_plan.md).

| Phase | Scope | Gate to proceed |
|---|---|---|
| 0 — Acquisition & Stabilization | Close, safety items, mechanicals inspection, secure envelope | Clean systems baseline; actual basis ≤ model |
| 1 — Core Systems | Electrical/plumbing/HVAC remediation, roof if needed | Systems pass inspection; budget variance < 10% |
| 2 — Kitchen & Baths | Highest rent-impact interiors | Post-phase rent estimate supports model |
| 3 — Interior Finish | Flooring, paint, doors, trim, fixtures | Unit rent-ready |
| 4 — Exterior & Curb | Siding/paint, landscaping, driveway | Appraisal/refi package ready |
| 5 — Stabilized Operation | Lease-up, refinance evaluation | DSCR and CoC meet targets |

## 🚀 Setup

```r
install.packages(c(
  "shiny", "bslib", "data.table", "lubridate",
  "DT", "plotly", "shinycssloaders", "scales"
))
```

## ▶ Usage

```r
# From the vestal_house/ directory:
shiny::runApp(".")          # or source("run_app.R")
```

1. Edit `config/params.R` with real property, financing, and rehab numbers.
2. Drop comp CSVs into `data/raw/` (dashboard falls back to demo data until then).
3. Log construction spend and task updates (see `R/construction.R` header for schema).
4. Review the Overview tab before each phase gate.

## 📄 License

Personal project of Anello Data Solutions LLC. All rights reserved.
