# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Investment model ----
# Pure functions: params in, data.tables/metrics out. No Shiny dependencies,
# so the whole model is testable from a plain R session.

library(data.table)

# Total project basis: purchase + closing + inspection + full rehab budget
total_basis <- function(p) {
  p$acquisition$purchase_price + p$acquisition$closing_costs +
    p$acquisition$inspection + sum(p$rehab$budget)
}

# Standard fixed-rate amortization schedule (monthly rows)
amortization_schedule <- function(principal, rate_annual, term_years) {
  n <- term_years * 12L
  if (principal <= 0 || n == 0L) {
    return(data.table(month = integer(), payment = numeric(),
                      interest = numeric(), principal_paid = numeric(),
                      balance = numeric()))
  }
  r <- rate_annual / 12
  pmt <- if (r == 0) principal / n else principal * r / (1 - (1 + r)^(-n))
  bal <- principal
  interest <- principal_paid <- balance <- numeric(n)
  for (m in seq_len(n)) {
    interest[m] <- bal * r
    principal_paid[m] <- pmt - interest[m]
    bal <- bal - principal_paid[m]
    balance[m] <- bal
  }
  data.table(month = seq_len(n), payment = pmt, interest = interest,
             principal_paid = principal_paid, balance = pmax(balance, 0))
}

# Annual operating pro forma over the hold period
pro_forma <- function(p) {
  o <- p$operating
  loan <- p$acquisition$purchase_price * p$financing$ltv
  sched <- amortization_schedule(loan, p$financing$rate_annual, p$financing$term_years)
  annual_debt_service <- if (nrow(sched)) sched$payment[1] * 12 else 0

  yrs <- seq_len(o$hold_years)
  dt <- data.table(year = yrs)
  dt[, gross_rent := o$rent_monthly * 12 * (1 + o$rent_growth)^(year - 1)]
  dt[, vacancy := -gross_rent * o$vacancy_rate]
  dt[, egi := gross_rent + vacancy]
  dt[, taxes := -o$taxes_annual * (1 + o$expense_growth)^(year - 1)]
  dt[, insurance := -o$insurance_annual * (1 + o$expense_growth)^(year - 1)]
  dt[, maintenance := -gross_rent * o$maintenance_pct]
  dt[, capex_reserve := -gross_rent * o$capex_reserve_pct]
  dt[, management := -gross_rent * o$mgmt_pct]
  dt[, noi := egi + taxes + insurance + maintenance + capex_reserve + management]
  dt[, debt_service := -annual_debt_service]
  dt[, cash_flow := noi + debt_service]
  dt[]
}

# Exit value and net sale proceeds in the final hold year
exit_proceeds <- function(p, pf) {
  o <- p$operating
  exit_noi <- pf[year == o$hold_years, noi] * (1 + o$rent_growth)
  sale_price <- exit_noi / o$exit_cap_rate
  loan <- p$acquisition$purchase_price * p$financing$ltv
  sched <- amortization_schedule(loan, p$financing$rate_annual, p$financing$term_years)
  # Loan is paid off if the hold outlasts the term
  exit_month <- o$hold_years * 12
  balance_at_exit <- if (nrow(sched) >= exit_month) sched$balance[exit_month] else 0
  selling_costs <- sale_price * 0.07  # commission + transfer + legal
  list(sale_price = sale_price,
       net_proceeds = sale_price - selling_costs - balance_at_exit)
}

# Full levered cash-flow vector: t0 equity out, annual cash flows, exit in final year
cash_flow_vector <- function(p) {
  loan <- p$acquisition$purchase_price * p$financing$ltv
  equity <- total_basis(p) - loan
  pf <- pro_forma(p)
  ex <- exit_proceeds(p, pf)
  cf <- c(-equity, pf$cash_flow)
  cf[length(cf)] <- cf[length(cf)] + ex$net_proceeds
  cf
}

# IRR via uniroot on NPV; returns NA if no sign change (no meaningful IRR)
irr <- function(cf, lower = -0.99, upper = 2) {
  npv_at <- function(r) sum(cf / (1 + r)^(seq_along(cf) - 1))
  f_lower <- npv_at(lower); f_upper <- npv_at(upper)
  if (is.nan(f_lower) || is.nan(f_upper) || f_lower * f_upper > 0) return(NA_real_)
  uniroot(npv_at, c(lower, upper), tol = 1e-8)$root
}

npv <- function(cf, rate) sum(cf / (1 + rate)^(seq_along(cf) - 1))

# Headline metrics used by the Overview and Investment tabs
model_metrics <- function(p) {
  loan <- p$acquisition$purchase_price * p$financing$ltv
  equity <- total_basis(p) - loan
  pf <- pro_forma(p)
  cf <- cash_flow_vector(p)
  y1 <- pf[year == 1]
  list(
    total_basis   = total_basis(p),
    loan          = loan,
    equity        = equity,
    year1_noi     = y1$noi,
    year1_cf      = y1$cash_flow,
    cap_rate      = y1$noi / total_basis(p),
    coc           = if (equity > 0) y1$cash_flow / equity else NA_real_,
    dscr          = if (loan > 0) y1$noi / -y1$debt_service else NA_real_,
    irr           = irr(cf),
    npv           = npv(cf, p$financing$hurdle_rate),
    payback_years = {
      cum <- cumsum(cf[-1]); w <- which(cum >= equity)
      if (length(w)) w[1] else NA_integer_
    }
  )
}

# One-way sensitivity of IRR to a single operating/financing lever
sensitivity_irr <- function(p, lever = c("rent_monthly", "rate_annual", "rehab_overrun"),
                            deltas = seq(-0.2, 0.2, by = 0.05)) {
  lever <- match.arg(lever)
  base <- p
  out <- data.table(delta = deltas, irr = NA_real_)
  for (i in seq_along(deltas)) {
    q <- base
    d <- deltas[i]
    if (lever == "rent_monthly") q$operating$rent_monthly <- q$operating$rent_monthly * (1 + d)
    if (lever == "rate_annual")  q$financing$rate_annual  <- q$financing$rate_annual + d / 4
    if (lever == "rehab_overrun") q$rehab <- copy(q$rehab)[, budget := budget * (1 + d)]
    out[i, irr := irr(cash_flow_vector(q))]
  }
  out[, lever := lever][]
}
