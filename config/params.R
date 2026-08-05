# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Master parameter file ----
# Single source of truth for every assumption in the project. All modules and
# model functions read from `params`; nothing else hard-codes a dollar figure.
#
# Property facts come from the Griffiths Engineering & Architecture SD set
# dated 2026-07-31 (plans/Anello_Residence_SD_2026-07-31.pdf, sheets
# A100/A101/A102/A201). Cost assumptions are documented with sources in
# docs/cost_research.md. Values marked TODO await real quotes/contracts.

params <- list(

  # ---- Property details (from SD set, project no. 2026-106) ----
  property = list(
    name          = "Anello Residence — New Build",
    address       = "117 Red Fox Run, Vestal, NY 13850",
    parcel_id     = "TODO — Broome County tax parcel",
    school_dist   = "Vestal Central School District",
    architect     = "Griffiths Engineering and Architecture, PLLC (Binghamton, NY)",
    plan_set      = "SD 2026-07-31, sheets A100/A101/A102/A201",

    # Areas measured off the SD room schedule (approximate; walls/stairs
    # allocated pro rata). Update after DD/CD set is issued.
    sqft_first    = 2800,    # first-floor finished living (excl. garage/porches)
    sqft_second   = 3220,    # second-floor finished living
    sqft_finished = 6020,    # above-grade finished total (appraisal basis)
    sqft_basement = 2685,    # basement rooms per A100 (finish is optional Phase 6)
    sqft_garage   = 1381,    # attached three-car garage
    sqft_porches  = 1500,    # covered porches + decks (approx)
    sqft_outdoor_kitchen = 255,
    beds          = 4,       # guest suite + bedrooms 2-4 (plus office & den)
    baths         = 5.5,     # per SD set (incl. basement bath + half baths)
    stories       = 2,
    basement_height_ft = 10, # T.O.S. basement at -10'-0" per A201
    condition     = "New construction — schematic design stage"
  ),

  # ---- Land & soft costs ----
  acquisition = list(
    land_cost   = 90000,    # TODO — actual lot cost/basis at 117 Red Fox Run
    soft_costs  = 68000,    # TODO — design/engineering (Griffiths), permits,
                            # survey, geotech, utility connection fees,
                            # builder's risk insurance
    inspection  = 0         # n/a for new build (municipal inspections in soft costs)
  ),

  # ---- Construction cost engine ----
  # Core-house cost scales from one number: cost_per_sqft_core applied to
  # above-grade finished area, distributed across NAHB 2024 phase weights
  # (see docs/cost_research.md). Adders cover non-living areas priced
  # separately. Change cost_per_sqft_core and the whole budget reflows.
  cost_basis = list(
    cost_per_sqft_core = 260,  # blended $/sqft, above-grade finished space.
                               # Planning range for this spec level in the
                               # Binghamton metro: 200 (value-engineered)
                               # to 325 (premium finishes). TODO: replace
                               # with GC bid when received.
    garage_per_sqft    = 65,
    porch_deck_per_sqft = 60,
    outdoor_kitchen_allowance = 45000,
    basement_finish_per_sqft  = 55,   # optional Phase 6
    contingency_pct    = 0.10
  ),

  # ---- Financing (construction-to-perm) ----
  financing = list(
    ltc          = 0.70,     # loan-to-cost on total project basis; 0 = all cash
    rate_annual  = 0.0675,   # TODO — quoted perm rate
    term_years   = 30,
    hurdle_rate  = 0.08      # discount rate for NPV
  ),

  # ---- Operating assumptions (annual unless noted) ----
  operating = list(
    # Carrying costs as a personal residence
    taxes_annual      = 28000,  # TODO — Broome County + Town of Vestal +
                                # Vestal CSD on the improved assessment.
                                # NY effective rates in Vestal run ~2.5-3.2%
                                # of market value; verify with assessor.
    insurance_annual  = 3500,   # TODO — quote for replacement cost
    utilities_monthly = 650,    # TODO — heat/electric for ~6,000 SF + basement
    maintenance_pct_of_value = 0.01,  # annual reserve, % of home value

    # Rental-scenario fields (only used by the optional rental view in the
    # Investment tab — e.g. if the home were ever leased)
    rent_monthly      = 4500,   # TODO — validate on Market tab if relevant
    vacancy_rate      = 0.05,
    maintenance_pct   = 0.07,
    capex_reserve_pct = 0.05,
    mgmt_pct          = 0.08,
    rent_growth       = 0.03,
    expense_growth    = 0.025,
    hold_years        = 10,
    exit_cap_rate     = 0.075
  ),

  # ---- Valuation assumptions ----
  value = list(
    # Appraisals price above-grade finished sqft off sale comps; basements,
    # garages, porches contribute partial value. new_construction_premium is
    # the multiple a new custom home commands over the median resale $/sqft.
    new_construction_premium = 1.20,
    basement_contrib_per_sqft = 30,   # finished-basement value contribution
    appreciation_annual = 0.03
  ),

  # ---- Phase definitions (incremental development strategy) ----
  phases = data.table::rbindlist(list(
    list(0L, "Pre-Construction",
         "Permits, survey, geotech, financing, GC bids, contingency funded",
         "Signed bid (or cost-confirmed budget) within 10% of model; financing closed",
         "in_progress"),
    list(1L, "Site & Foundation",
         "Clearing, excavation, footings, 10 ft foundation walls, slab, waterproofing, backfill",
         "Foundation passes inspection; phase variance < 10%",
         "planned"),
    list(2L, "Shell",
         "Framing incl. garage, roof, windows/doors, siding — weather-tight",
         "Building is weather-tight; phase variance < 10%",
         "planned"),
    list(3L, "Systems Rough-In",
         "Electrical, plumbing, HVAC rough-in",
         "All rough-in inspections passed",
         "planned"),
    list(4L, "Interior Finish",
         "Insulation, drywall, kitchen, baths, flooring, paint, trim",
         "Certificate of occupancy issued",
         "planned"),
    list(5L, "Exterior & Site Finish",
         "Porches/decks, outside kitchen, driveway, landscaping, punch list",
         "Punch list clear; final appraisal supports model value",
         "planned"),
    list(6L, "Basement Finish (optional)",
         "Finish basement rooms 1-3 and bath per A100",
         "Go/no-go on marginal cost vs. value contribution",
         "planned")
  ), use.names = FALSE) |>
    data.table::setnames(c("phase", "phase_name", "scope", "gate", "status"))
)

# ---- Derived construction budget (do not edit by hand — edit cost_basis) ----
# NAHB 2024 phase weights for single-family construction, mapped to our phases.
params$build <- local({
  cb <- params$cost_basis
  pr <- params$property
  core <- cb$cost_per_sqft_core * pr$sqft_finished

  nahb <- data.table::rbindlist(list(
    list(1L, "Site Work",        "Clearing, excavation, utilities to site",  0.076),
    list(1L, "Foundation",       "Footings, 10 ft walls, slab, waterproofing", 0.104),
    list(2L, "Framing",          "Structural framing, trusses, sheathing",   0.166),
    list(2L, "Exterior Finish",  "Roofing, siding, windows, exterior doors", 0.134),
    list(3L, "Systems Rough-In", "Electrical, plumbing, HVAC",               0.192),
    list(4L, "Interior Finish",  "Insulation, drywall, kitchen, baths, floors, paint, trim", 0.241),
    list(5L, "Final Steps",      "Driveway, landscaping, cleanup",           0.065),
    list(5L, "Other",            "Misc. construction costs",                 0.021)
  ), use.names = FALSE) |>
    data.table::setnames(c("phase", "category", "item", "weight"))
  nahb[, budget := round(weight * core, -2)][, weight := NULL]

  adders <- data.table::rbindlist(list(
    list(0L, "Contingency",     "Reserve @ contingency_pct of construction",
         round(cb$contingency_pct * core, -2)),
    list(2L, "Garage",          "Three-car attached garage (1,381 SF)",
         round(cb$garage_per_sqft * pr$sqft_garage, -2)),
    list(5L, "Porches & Decks", "Covered porches and decks (~1,500 SF)",
         round(cb$porch_deck_per_sqft * pr$sqft_porches, -2)),
    list(5L, "Outside Kitchen", "Outside kitchen allowance (255 SF)",
         round(cb$outdoor_kitchen_allowance, -2)),
    list(6L, "Basement Finish", "Finish basement rooms per A100 (optional)",
         round(cb$basement_finish_per_sqft * pr$sqft_basement, -2))
  ), use.names = FALSE) |>
    data.table::setnames(c("phase", "category", "item", "budget"))

  out <- data.table::rbindlist(list(nahb, adders), use.names = TRUE)
  out[order(phase, category)]
})
