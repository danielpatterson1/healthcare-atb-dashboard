/*
  Rebuild open AR at each of the last 12 month ends from the event history.
  An account counts toward a month end when it was discharged on or before
  that date and still had a balance after transactions posted by that date.
*/
WITH month_ends AS (
  SELECT CAST(last_day(m) AS DATE) AS month_end
  FROM range(DATE '{{as_of}}' - INTERVAL 11 MONTH, DATE '{{as_of}}' + INTERVAL 1 DAY, INTERVAL 1 MONTH) t(m)
),
acct AS (
  SELECT account_id, discharge_date, expected_payment
  FROM read_parquet('{{curated}}/accounts.parquet')
),
txn AS (
  SELECT account_id, txn_date, amount
  FROM read_parquet('{{curated}}/transactions.parquet')
),
snap AS (
  SELECT
    me.month_end,
    a.account_id,
    a.discharge_date,
    a.expected_payment + COALESCE(SUM(t.amount), 0) AS balance
  FROM month_ends me
  JOIN acct a ON a.discharge_date <= me.month_end
  LEFT JOIN txn t ON t.account_id = a.account_id AND t.txn_date <= me.month_end
  GROUP BY me.month_end, a.account_id, a.discharge_date, a.expected_payment
)
SELECT
  month_end,
  COUNT(*)                                                       AS open_accounts,
  SUM(balance)                                                   AS open_ar,
  SUM(CASE WHEN month_end - discharge_date > 90 THEN balance END) AS ar_over_90
FROM snap
WHERE balance > 0.005
GROUP BY month_end
ORDER BY month_end
