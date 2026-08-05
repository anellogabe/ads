# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Master parameter file ----
# Single source of truth for every assumption in the project.
# All modules and model functions read from `params`; nothing else hard-codes
# a dollar figure. Values marked TODO are placeholders — replace with actuals
# from the contract, inspection, and quotes.

params <- list(

  # ---- Property details ----
  property = list(
    name          = "Vestal House",
    address       = "TODO — street address, Vestal, NY 13850",
    parcel_id     = "TODO",
    school_dist   = "Vestal Central School District",
    year_built    = NA_integer_,   # TODO
    sqft          = 1600,          # TODO — gross living area
    lot_acres     = 0.35,          # TODO
    beds          = 3,             # TODO
    baths         = 1.5,           # TODO
    condition     = "TODO — inspection summary"
  ),

  # ---- Acquisition ----
  acquisition = list(
    purchase_price = 165000,       # TODO — contract price
    closing_costs  = 5500,         # TODO — title, legal, recording
    inspection     = 600
  ),

  # ---- Financing ----
  financing = list(
    ltv          = 0.75,           # loan-to-value on purchase price; 0 = all cash
    rate_annual  = 0.0675,         # note rate
    term_years   = 30,
    hurdle_rate  = 0.10            # discount rate for NPV / IRR hurdle
  ),

  # ---- Operating assumptions (monthly unless noted) ----
  operating = list(
    rent_monthly      = 1850,      # TODO — validate against Market Analytics tab
    vacancy_rate      = 0.06,
    taxes_annual      = 4800,      # TODO — Broome County + Vestal CSD
    insurance_annual  = 1400,
    maintenance_pct   = 0.07,      # % of gross rent
    capex_reserve_pct = 0.05,      # % of gross rent
    mgmt_pct          = 0.00,      # self-managed; set 0.08-0.10 if outsourced
    rent_growth       = 0.03,      # annual
    expense_growth    = 0.025,     # annual
    hold_years        = 10,
    exit_cap_rate     = 0.075
  ),

  # ---- Rehab budget by phase and category ----
  # Incremental development: phases are executed and funded sequentially.
  rehab = data.table::rbindlist(list(
    # phase, category, item, budget
    list(0L, "Safety",     "Locks, smoke/CO detectors, hazard removal",  1200),
    list(0L, "Systems",    "Mechanical/electrical/plumbing inspection",   800),
    list(0L, "Envelope",   "Roof patch, gutter repair, weatherproofing", 2500),
    list(1L, "Electrical", "Panel upgrade and circuit remediation",      4500),
    list(1L, "Plumbing",   "Supply/drain remediation, water heater",     3800),
    list(1L, "HVAC",       "Furnace service or replacement",             5500),
    list(2L, "Kitchen",    "Cabinets, counters, appliances, plumbing",  14000),
    list(2L, "Bath",       "Full bath renovation",                       8000),
    list(2L, "Bath",       "Half bath refresh",                          2500),
    list(3L, "Flooring",   "LVP main areas, carpet bedrooms",            6500),
    list(3L, "Paint",      "Full interior paint",                        3500),
    list(3L, "Finish",     "Doors, trim, hardware, lighting",            3000),
    list(4L, "Exterior",   "Siding repair and exterior paint",           7000),
    list(4L, "Site",       "Landscaping, driveway seal",                 2500)
  ), use.names = FALSE) |>
    data.table::setnames(c("phase", "category", "item", "budget")),

  # ---- Phase definitions (incremental development strategy) ----
  phases = data.table::rbindlist(list(
    list(0L, "Acquisition & Stabilization",
         "Close, safety items, systems baseline, secure envelope",
         "Clean systems baseline; actual basis <= model",             "in_progress"),
    list(1L, "Core Systems",
         "Electrical, plumbing, HVAC remediation",
         "Systems pass inspection; budget variance < 10%",            "planned"),
    list(2L, "Kitchen & Baths",
         "Highest rent-impact interior renovations",
         "Post-phase rent estimate supports model",                   "planned"),
    list(3L, "Interior Finish",
         "Flooring, paint, doors, trim, fixtures",
         "Unit rent-ready",                                           "planned"),
    list(4L, "Exterior & Curb",
         "Siding/paint, landscaping, driveway",
         "Appraisal/refi package ready",                              "planned"),
    list(5L, "Stabilized Operation",
         "Lease-up and refinance evaluation",
         "DSCR and cash-on-cash meet targets",                        "planned")
  ), use.names = FALSE) |>
    data.table::setnames(c("phase", "phase_name", "scope", "gate", "status"))
)
