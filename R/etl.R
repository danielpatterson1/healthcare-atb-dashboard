# Extract, clean and load the raw billing extracts into curated Parquet files.
#
# Steps
#   1. Read the raw CSV extracts as text so nothing is silently coerced
#   2. Standardize dates, currency strings and payer spelling
#   3. Remove exact duplicate transactions and log how many were dropped
#   4. Roll transactions up to the account and derive balance and status
#   5. Write curated Parquet, then use DuckDB SQL for aging and trend tables

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(DBI)
  library(duckdb)
})

parse_money <- function(x) as.numeric(gsub("[$,]", "", x))
parse_mdy   <- function(x) as.Date(x, format = "%m/%d/%Y")
parse_ymd   <- function(x) as.Date(x, format = "%Y%m%d")

run_etl <- function(raw_dir = "data/raw", out_dir = "data/curated",
                    as_of = as.Date("2026-09-30")) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  read_raw <- function(f) read.csv(file.path(raw_dir, f), colClasses = "character",
                                   na.strings = "")

  # 1 and 2: extract and standardize
  facilities <- read_raw("facilities.csv")

  encounters <- read_raw("encounters.csv") |>
    transmute(
      account_id       = trimws(ACCT_NO),
      facility_id      = trimws(FAC),
      discharge_date   = parse_mdy(DISCH_DT),
      payer_name       = trimws(PAYER),
      financial_class  = trimws(FIN_CLASS),
      expected_payment = parse_money(EXPECTED_AMT)
    ) |>
    # Payer spelling varies by source system, so map every variant of a name
    # to its most common spelling
    group_by(payer_key = tolower(payer_name)) |>
    mutate(payer_name = names(which.max(table(payer_name)))) |>
    ungroup() |>
    select(-payer_key)

  claims <- read_raw("claims.csv") |>
    transmute(
      account_id    = ACCT_NO,
      hold_reason   = HOLD_REASON,
      bill_date     = parse_ymd(BILL_DT),
      denial_reason = DENIAL_REASON,
      denial_date   = parse_ymd(DENIAL_DT)
    )

  # 3: duplicate transactions
  txn_raw <- read_raw("transactions.csv")
  txn <- txn_raw |>
    distinct() |>
    transmute(account_id = ACCT_NO, txn_date = parse_mdy(TXN_DT),
              txn_type = TXN_TYPE, amount = as.numeric(TXN_AMT))
  dupes_removed <- nrow(txn_raw) - nrow(txn)

  # 4: account level rollup
  txn_acct <- txn |>
    group_by(account_id) |>
    summarise(
      payments      = -sum(amount[txn_type == "PMT"]),
      adjustments   = -sum(amount[txn_type == "ADJ"]),
      last_pay_date = suppressWarnings(max(txn_date[txn_type == "PMT"])),
      .groups = "drop"
    ) |>
    mutate(last_pay_date = if_else(is.infinite(last_pay_date), as.Date(NA), last_pay_date))

  accounts <- encounters |>
    left_join(claims, by = "account_id") |>
    left_join(txn_acct, by = "account_id") |>
    left_join(facilities, by = "facility_id") |>
    mutate(
      payments    = coalesce(payments, 0),
      adjustments = coalesce(adjustments, 0),
      balance     = round(expected_payment - payments - adjustments, 2),
      status = case_when(
        is.na(bill_date)                          ~ "Unbilled",
        balance <= 0.005                          ~ "Closed",
        !is.na(denial_reason) & payments == 0     ~ "Denied",
        payments > 0                              ~ "Underpaid",
        TRUE                                      ~ "Billed, awaiting payment"
      ),
      days_since_discharge = as.integer(as_of - discharge_date),
      days_since_bill      = as.integer(as_of - bill_date),
      next_step = case_when(
        status == "Unbilled" & hold_reason == "Standard bill hold" ~ "Releases automatically",
        status == "Unbilled"  ~ paste("Resolve hold:", tolower(hold_reason)),
        status == "Denied"    ~ paste("Appeal or correct:", tolower(denial_reason)),
        status == "Underpaid" ~ "Review contract variance",
        status == "Billed, awaiting payment" & days_since_bill > 45 ~ "Payer follow up",
        status == "Billed, awaiting payment" ~ "Monitor",
        TRUE ~ NA_character_
      )
    )

  write_parquet(accounts, file.path(out_dir, "accounts.parquet"))
  write_parquet(txn,      file.path(out_dir, "transactions.parquet"))

  # 5: DuckDB SQL over Parquet
  con <- dbConnect(duckdb(shared_home = FALSE))
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  run_sql <- function(file) {
    sql <- paste(readLines(file.path("sql", file)), collapse = "\n")
    sql <- gsub("{{curated}}", out_dir, sql, fixed = TRUE)
    sql <- gsub("{{as_of}}", format(as_of), sql, fixed = TRUE)
    dbGetQuery(con, sql)
  }
  atb   <- run_sql("atb_open_accounts.sql")
  trend <- run_sql("ar_month_end_trend.sql")
  write_parquet(atb,   file.path(out_dir, "atb_open_accounts.parquet"))
  write_parquet(trend, file.path(out_dir, "ar_month_end_trend.parquet"))

  list(accounts = accounts, txn = txn, atb = atb, trend = trend,
       dupes_removed = dupes_removed, raw_txn_rows = nrow(txn_raw))
}
