# Build Cost Research — Anello Residence

Basis for the construction budget in `config/params.R` (`cost_basis` +
derived `params$build`). Everything here is a *planning* number pending real
GC bids — the gate for Phase 0 is a signed bid within 10% of this model.

## The house being priced

From the Griffiths SD set (2026-07-31): ~6,020 SF finished above grade over
two stories, full 10-ft basement (~2,685 SF, finish optional), attached
1,381 SF three-car garage, ~1,500 SF covered porches/decks, 255 SF outside
kitchen, 4+ beds / 5.5 baths, guest suite, game room. This is a large custom
home with an above-average finish level implied by the program (butler's
pantry, outside kitchen, multiple suites).

## Benchmarks used

- **NAHB 2024 Cost of Construction Survey** — national average construction
  cost $428,215 for an average 2,647 SF home ≈ **$162/sqft**, with phase
  weights: interior finishes 24.1%, systems rough-in 19.2%, framing 16.6%,
  exterior finishes 13.4%, foundation 10.4%, site work 7.6%, final steps
  6.5%, other 2.1%. These weights structure our budget lines.
- **Upstate NY custom-build market (2026)** — published guidance clusters
  around **$200–$350/sqft** all-in for custom construction upstate, with
  quality custom work in Western NY quoted from ~$400/sqft and true luxury
  from $750/sqft. The Binghamton metro prices below Rochester/Buffalo.
- **Adjustments for this house**: scale economies (6,000 SF spreads fixed
  costs) push $/sqft down; the 10-ft full basement, porch program, and
  finish level push it up.

**Planning number: $260/sqft core** (range $200 value-engineered to $325
premium) on above-grade finished area. All three scenarios are one slider in
the dashboard's Budget tab.

## Resulting budget (at $260/sqft core)

| Component | Basis | Amount |
|---|---|---|
| Core house (NAHB-weighted lines) | 6,020 SF × $260 | ~$1,565,000 |
| Three-car garage | 1,381 SF × $65 | ~$89,800 |
| Porches & decks | 1,500 SF × $60 | ~$90,000 |
| Outside kitchen | allowance | $45,000 |
| Contingency | 10% of core | ~$156,500 |
| **Construction subtotal** | | **~$1,946,000** |
| Basement finish (optional Phase 6) | 2,685 SF × $55 | ~$147,700 |
| Land (TODO — actual basis) | | $90,000 |
| Soft costs (design, permits, survey, utilities, builder's risk) | | $68,000 |
| **Total project basis (excl. Phase 6)** | | **~$2.10M** |

At $200/sqft core the construction subtotal drops to ~$1.55M; at $325 it
rises to ~$2.43M. The Budget tab reflows every line live.

## What still needs real numbers (TODO)

1. **GC bids** — the entire point of Phase 0. Get 2-3 bids on the SD set or
   wait for DD/CD drawings for tighter pricing.
2. **Land basis** at 117 Red Fox Run (currently a $90k placeholder).
3. **Site specifics** — municipal water/sewer vs well/septic on Red Fox Run
   changes site work by tens of thousands; geotech may move foundation cost.
4. **Property taxes** — ask the Town of Vestal assessor what the improved
   assessment will be; the $28k/yr placeholder assumes ~2.8% effective on
   ~$1M market value. This is the single biggest recurring carrying cost.
5. **Griffiths fee schedule** through CD + construction administration.

## Sources

- NAHB, *Cost of Constructing a Home in 2024* —
  https://www.nahb.org/news-and-economics/housing-economics-plus/special-studies/special-studies-pages/cost-of-constructing-a-home-in-2024
  (summary: https://eyeonhousing.org/2025/01/cost-of-constructing-a-home-in-2024/)
- Modish Custom Homes, *Cost to Build a Custom Home in Western New York
  (2026)* — https://modishcustomhomes.com/how-much-does-it-cost-to-build-a-custom-home-in-western-new-york-2026-guide/
- Houzeo, *Cost to Build a House in New York* —
  https://www.houzeo.com/blog/how-much-does-it-cost-to-build-a-house-new-york/
- Homes by Covenant, *What It Really Costs To Build A Home In New York
  (2026)* — https://www.homesbycovenant.com/what-it-really-costs-to-build-a-home-in-new-york-in-2026-with-proof/
