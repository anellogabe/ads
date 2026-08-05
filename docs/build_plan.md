# Anello Residence — Build Plan

New-construction plan for 117 Red Fox Run, Vestal NY, per the Griffiths
Engineering & Architecture SD set dated 2026-07-31 (project 2026-106). Each
phase ends with a **gate review** in the dashboard (Overview + Phases tabs)
before money is committed to the next. Budgets mirror `config/params.R` —
that file is the source of truth; this document explains sequencing and
reasoning. Cost basis and sources: `docs/cost_research.md`.

## Principles

1. **Bid before build.** No ground is broken until GC bids confirm the model
   within 10% — pricing surprises are cheapest at Phase 0.
2. **Gates are numeric.** A gate passes when the dashboard shows it passing
   (inspection status, phase budget variance, appraisal support) — not when
   it feels done.
3. **Every dollar is logged.** All spend goes through `log_spend()` so budget
   vs. actual is live, auditable, and argument-ready with contractors.
4. **The basement is optional until it isn't.** Phase 6 is deliberately
   last and gated on marginal cost vs. value contribution — it can be
   deferred years without blocking occupancy.

## Phase 0 — Pre-Construction

**Scope:** Building permit (Town of Vestal), survey and geotech, utility
availability confirmation (water/sewer vs. well/septic on Red Fox Run),
construction-to-perm financing, 2-3 GC bids on the plan set, contingency
funded.

**Gate:** Signed bid (or cost-confirmed budget) within 10% of model;
financing closed.

**Risk to watch:** SD-stage pricing. Bids off schematic drawings carry wide
allowances — consider funding Griffiths through DD/CD for tighter bids
before committing.

## Phase 1 — Site & Foundation

**Scope:** Clearing, excavation, footings, 10-ft foundation walls (per A201
basement section), slab, waterproofing, drainage, backfill, utilities to the
building.

**Gate:** Foundation passes municipal inspection; phase variance < 10%.

**Risk to watch:** Rock or groundwater at 10-ft depth. The geotech report in
Phase 0 exists precisely to price this before the excavator finds it.

## Phase 2 — Shell

**Scope:** Full structural framing including the three-car garage, roof
trusses and roofing, sheathing, windows and exterior doors, siding. Ends
weather-tight.

**Gate:** Building is weather-tight ("dried in"); phase variance < 10%.

**Risk to watch:** Lumber and window lead times — lock pricing at contract
where possible; long-lead windows ordered at Phase 1 start.

## Phase 3 — Systems Rough-In

**Scope:** Electrical service and circuits, plumbing supply/waste, HVAC
(size for ~6,000 SF + optional basement zone from day one).

**Gate:** All rough-in inspections passed.

**Risk to watch:** Change orders. The kitchen/outside kitchen/butler's
pantry gas-electric-vent layout should be final before rough-in starts —
moving a gas line after drywall is triple the cost.

## Phase 4 — Interior Finish

**Scope:** Insulation, drywall, kitchen and butler's pantry, all baths,
flooring, interior doors/trim, paint, fixtures, appliances.

**Gate:** Certificate of occupancy issued.

**Risk to watch:** This is the largest budget line (NAHB: ~24% of
construction) and where scope creep lives. Finish allowances are decided in
writing at Phase 0 bid time; upgrades come out of contingency, visibly.

## Phase 5 — Exterior & Site Finish

**Scope:** Covered porches and decks, outside kitchen, driveway,
landscaping, final grading, punch list.

**Gate:** Punch list clear; final appraisal supports the model's projected
value (Market tab).

## Phase 6 — Basement Finish (optional)

**Scope:** Finish basement rooms 1-3, bath, and half bath per A100
(~2,685 SF at 10-ft ceilings).

**Gate:** Go/no-go on marginal cost (~$150k at $55/sqft) vs. value
contribution (`params$value$basement_contrib_per_sqft`) and actual need.

## Operating the plan

- Log every payment with `log_spend()` and every milestone with `log_task()`
  (see `R/construction.R`) — the dashboard reads these logs directly.
- Refresh market data monthly (`scripts/fetch_market_data.R`) and comps
  quarterly (`docs/market_data.md`) so the Cost-to-Value KPI stays honest.
- Before each gate review, reconcile the spend log against bank/loan draws.
- If a gate fails, the next phase does not start: fix the variance, re-bid,
  or re-scope, then re-review.
