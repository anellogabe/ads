# Vestal House — Incremental Build Plan

The build is phased so each phase is small enough to self-fund and each ends with a
**gate review** in the dashboard (Overview + Phases tabs) before any money is committed
to the next. Budgets below mirror `config/params.R` — that file is the source of truth;
this document explains sequencing and reasoning.

## Principles

1. **Systems before surfaces.** No cosmetic dollar is spent until electrical, plumbing,
   HVAC, and the envelope are sound — cosmetic work over bad systems gets torn out.
2. **Self-funded phases.** No construction loan. Each phase's cash must be on hand
   before it starts; a phase never begins on the assumption the next dollar arrives.
3. **Gates are numeric.** A gate passes when the dashboard shows it passing (budget
   variance, rent estimate, DSCR) — not when it feels done.
4. **Rent impact ranks the backlog.** When scope must be cut, keep the items with the
   highest rent-per-dollar impact (kitchen, baths, flooring) and defer the rest.

## Phase 0 — Acquisition & Stabilization

**Scope:** Close on the property; immediately address safety items (locks, smoke/CO,
hazards); commission full mechanical/electrical/plumbing inspection; patch roof and
gutters so the envelope sheds water.

**Gate:** Systems baseline documented; total actual basis (purchase + closing + phase-0
spend) at or under the model's basis.

**Risk to watch:** Inspection surprises. Anything found here re-prices Phase 1 before
it starts — update `params$rehab` and re-check the model.

## Phase 1 — Core Systems

**Scope:** Electrical panel/circuit remediation, plumbing supply and drain remediation,
water heater, furnace service or replacement.

**Gate:** All systems pass inspection; phase budget variance under 10%.

**Risk to watch:** Hidden knob-and-tube or galvanized supply lines in a house of this
era. Get fixed quotes, not T&M, wherever possible.

## Phase 2 — Kitchen & Baths

**Scope:** Kitchen (cabinets, counters, appliances, associated plumbing), full bath
renovation, half bath refresh. The highest rent-per-dollar phase.

**Gate:** Post-phase rent estimate (Market tab, comp-based) supports the model's rent
assumption.

## Phase 3 — Interior Finish

**Scope:** LVP in main areas, carpet in bedrooms, full interior paint, doors, trim,
hardware, lighting.

**Gate:** Unit is rent-ready — punch list clear, photos taken for listing.

## Phase 4 — Exterior & Curb

**Scope:** Siding repair and exterior paint, landscaping, driveway seal.

**Gate:** Appraisal/refinance package assembled (photos, rent roll or lease, spend
history from the dashboard's spend log).

## Phase 5 — Stabilized Operation

**Scope:** Lease-up at the modeled rent; evaluate refinance once seasoned.

**Gate (ongoing):** DSCR and cash-on-cash meet the targets in `config/params.R`.

## Operating the plan

- Log every dollar with `log_spend()` and every task with `log_task()`
  (see `R/construction.R`) — the dashboard reads these logs directly.
- Before each gate review, update comps in `data/raw/` so the rent estimate is current.
- If a gate fails, the next phase does not start: fix the variance, re-quote, or
  re-scope, then re-review.
