/*
  Aged trial balance: every open account with its aging bucket.
  Aging is measured from discharge so unbilled and billed balances
  sit on the same clock.
*/
SELECT
  account_id,
  facility_name,
  region,
  payer_name,
  financial_class,
  status,
  COALESCE(hold_reason, denial_reason) AS reason,
  next_step,
  discharge_date,
  bill_date,
  days_since_discharge,
  ROUND(expected_payment, 2) AS expected_payment,
  ROUND(payments, 2)         AS payments,
  ROUND(balance, 2)          AS balance,
  CASE
    WHEN days_since_discharge <= 30  THEN '0 to 30'
    WHEN days_since_discharge <= 60  THEN '31 to 60'
    WHEN days_since_discharge <= 90  THEN '61 to 90'
    WHEN days_since_discharge <= 120 THEN '91 to 120'
    WHEN days_since_discharge <= 180 THEN '121 to 180'
    WHEN days_since_discharge <= 365 THEN '181 to 365'
    ELSE 'Over 365'
  END AS aging_bucket
FROM read_parquet('{{curated}}/accounts.parquet')
WHERE status <> 'Closed'
ORDER BY balance DESC
