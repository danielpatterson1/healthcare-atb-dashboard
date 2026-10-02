# Generate synthetic hospital billing extracts for the ATB dashboard.
#
# Every value in these files is randomly generated. Facility names, payers,
# account IDs, dates and dollar amounts do not describe any real organization
# or person. The extracts are deliberately messy (text dates, currency
# strings, inconsistent payer spelling) so the ETL step has real work to do.

suppressPackageStartupMessages({
  library(dplyr)
})

generate_atb_extracts <- function(out_dir = "data/raw",
                                  n_accounts = 15000,
                                  as_of = as.Date("2026-09-30"),
                                  seed = 2026) {
  set.seed(seed)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Facilities
  facilities <- tibble(
    facility_id   = sprintf("F%02d", 1:12),
    facility_name = sprintf("Facility %02d", 1:12),
    region        = rep(c("East", "Central", "West"), each = 4),
    size_weight   = c(1.4, 1.0, 0.8, 1.2, 1.1, 0.7, 1.3, 0.9, 1.0, 0.6, 1.5, 0.9)
  )

  # Payer profiles: share of volume, typical expected payment, days to pay
  # after billing, and initial denial rate.
  payers <- tibble(
    payer_name      = c("Medicare", "Medicare Advantage Plan A", "Medicare Advantage Plan B",
                        "State Medicaid", "Medicaid Managed Care", "Commercial Plan A",
                        "Commercial Plan B", "Workers Comp", "Tricare", "Self Pay"),
    financial_class = c("Medicare", "Medicare Advantage", "Medicare Advantage",
                        "Medicaid", "Medicaid", "Commercial", "Commercial",
                        "Workers Comp", "Government Other", "Self Pay"),
    share           = c(0.40, 0.11, 0.07, 0.07, 0.06, 0.10, 0.07, 0.03, 0.05, 0.04),
    mean_expected   = c(18000, 15500, 14500, 9000, 9500, 21000, 19500, 17000, 14000, 2500),
    days_to_pay     = c(18, 32, 38, 45, 52, 28, 34, 65, 40, 120),
    denial_rate     = c(0.04, 0.11, 0.14, 0.09, 0.15, 0.07, 0.09, 0.12, 0.06, 0.00)
  )

  hold_reasons <- c("Missing documentation", "Coding review", "Pending authorization",
                    "Insurance verification", "Physician signature", "Charge correction",
                    "MSP questionnaire")
  denial_reasons <- c("Authorization", "Medical necessity", "Eligibility",
                      "Timely filing", "Coding", "Missing information")

  start <- as_of - 545  # about 18 months of discharges
  acct <- tibble(
    account_id     = sprintf("A%07d", sample(1e6:9e6, n_accounts)),
    facility_id    = sample(facilities$facility_id, n_accounts, TRUE, facilities$size_weight),
    discharge_date = start + sample(0:545, n_accounts, TRUE)
  ) |>
    mutate(p = sample(seq_len(nrow(payers)), n(), TRUE, payers$share))
  acct <- bind_cols(acct, payers[acct$p, ]) |> select(-p, -share)

  n <- nrow(acct)
  acct <- acct |>
    mutate(
      expected_payment = round(rlnorm(n, log(mean_expected), 0.45), 2),

      # Bill hold: most claims drop within a week, a minority get stuck.
      stuck       = runif(n) < 0.07,
      bill_lag    = ifelse(stuck, round(runif(n, 20, 260)), 3 + rpois(n, 2)),
      hold_reason = ifelse(stuck, sample(hold_reasons, n, TRUE,
                                         c(.26, .20, .16, .12, .11, .09, .06)), NA),
      bill_date   = discharge_date + bill_lag,
      billed      = bill_date <= as_of,

      # Payer response
      denied      = billed & runif(n) < denial_rate,
      denial_reason = ifelse(denied, sample(denial_reasons, n, TRUE,
                                            c(.28, .20, .18, .08, .14, .12)), NA),
      denial_date = bill_date + round(runif(n, 18, 45)),
      appeal_paid = denied & runif(n) < 0.55,
      pay_lag     = round(rgamma(n, shape = 3, scale = days_to_pay / 3)),
      pay_date    = case_when(
        !billed      ~ as.Date(NA),
        appeal_paid  ~ denial_date + round(runif(n, 30, 140)),
        denied       ~ as.Date(NA),
        TRUE         ~ bill_date + pay_lag
      ),
      pay_date    = if_else(!is.na(pay_date) & pay_date > as_of, as.Date(NA), pay_date),

      # Payment amount: most pay close to expected, some underpay.
      underpaid   = runif(n) < 0.06,
      pay_ratio   = ifelse(underpaid, runif(n, 0.55, 0.9), runif(n, 0.985, 1)),
      pay_ratio   = ifelse(financial_class == "Self Pay", runif(n, 0.2, 1), pay_ratio),
      paid_amount = ifelse(is.na(pay_date), 0, round(expected_payment * pay_ratio, 2)),

      # Small variances are written off; underpayments stay open for follow up.
      adj_amount  = ifelse(!is.na(pay_date) & !underpaid & financial_class != "Self Pay",
                           round(expected_payment - paid_amount, 2), 0),
      adj_date    = pay_date
    )

  # Messy extract 1: encounters
  messy_payer <- function(x) {
    k <- runif(length(x))
    case_when(k < 0.08 ~ toupper(x), k < 0.14 ~ paste0(" ", x, "  "), TRUE ~ x)
  }
  encounters <- acct |>
    transmute(
      ACCT_NO        = account_id,
      FAC            = facility_id,
      DISCH_DT       = format(discharge_date, "%m/%d/%Y"),
      PAYER          = messy_payer(payer_name),
      FIN_CLASS      = financial_class,
      EXPECTED_AMT   = paste0("$", formatC(expected_payment, format = "f", digits = 2, big.mark = ","))
    )

  # Messy extract 2: claims
  claims <- acct |>
    transmute(
      ACCT_NO       = account_id,
      HOLD_REASON   = ifelse(!billed, coalesce(hold_reason, "Standard bill hold"), NA),
      BILL_DT       = ifelse(billed, format(bill_date, "%Y%m%d"), NA),
      DENIAL_REASON = ifelse(denied & denial_date <= as_of, denial_reason, NA),
      DENIAL_DT     = ifelse(denied & denial_date <= as_of, format(denial_date, "%Y%m%d"), NA)
    )

  # Messy extract 3: transactions (one row per payment or adjustment)
  pays <- acct |> filter(paid_amount > 0) |>
    transmute(ACCT_NO = account_id, TXN_DT = format(pay_date, "%m/%d/%Y"),
              TXN_TYPE = "PMT", TXN_AMT = -paid_amount)
  adjs <- acct |> filter(adj_amount != 0) |>
    transmute(ACCT_NO = account_id, TXN_DT = format(adj_date, "%m/%d/%Y"),
              TXN_TYPE = "ADJ", TXN_AMT = -adj_amount)
  # A few exact duplicate rows, which the ETL must catch.
  dupes <- pays |> slice_sample(n = 25)
  transactions <- bind_rows(pays, adjs, dupes) |> slice_sample(prop = 1)

  write.csv(facilities |> select(-size_weight), file.path(out_dir, "facilities.csv"), row.names = FALSE)
  write.csv(encounters,   file.path(out_dir, "encounters.csv"),   row.names = FALSE, na = "")
  write.csv(claims,       file.path(out_dir, "claims.csv"),       row.names = FALSE, na = "")
  write.csv(transactions, file.path(out_dir, "transactions.csv"), row.names = FALSE, na = "")
  invisible(list(encounters = nrow(encounters), transactions = nrow(transactions)))
}
