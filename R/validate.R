# Data checks that must pass before the dashboard is rendered.
# Each check returns TRUE or FALSE; any failure stops the pipeline.

validate_atb <- function(res) {
  a   <- res$accounts
  atb <- res$atb

  checks <- tibble::tribble(
    ~check, ~passed,
    "Account IDs are unique",
      !anyDuplicated(a$account_id),
    "Every account has a discharge date and expected payment",
      all(!is.na(a$discharge_date) & !is.na(a$expected_payment)),
    "Every account maps to a facility",
      all(!is.na(a$facility_name)),
    "Every transaction belongs to a known account",
      all(res$txn$account_id %in% a$account_id),
    "Duplicate transactions removed before rollup",
      res$dupes_removed > 0 && !anyDuplicated(res$txn),
    "Balances tie out: expected minus payments minus adjustments",
      isTRUE(all.equal(sum(a$expected_payment) - sum(a$payments) - sum(a$adjustments),
                       sum(a$balance), tolerance = 1e-9)),
    "No negative balances",
      all(a$balance > -0.005),
    "Every unbilled account has a hold reason",
      all(!is.na(a$hold_reason[a$status == "Unbilled"])),
    "ATB total equals open balance in the account table",
      isTRUE(all.equal(sum(atb$balance), sum(a$balance[a$status != "Closed"]))),
    "Latest month end trend ties to the ATB",
      isTRUE(all.equal(tail(res$trend$open_ar, 1), sum(atb$balance), tolerance = 1e-6))
  )

  if (!all(checks$passed)) {
    print(checks[!checks$passed, ])
    stop("Data checks failed. Dashboard not rendered.")
  }
  checks
}
