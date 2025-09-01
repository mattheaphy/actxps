# Purpose: this script is used to create images used by vignettes for gt tables

# Transaction tables example ----------------------------------------------

library(actxps)

exposed_data <- expose_py(
  census_dat,
  "2019-12-31",
  target_status = "Surrender"
) |>
  add_transactions(withdrawals) |>
  left_join(account_vals, by = c("pol_num", "pol_date_yr"))
trx_res <- exposed_data |>
  group_by(pol_yr, inc_guar) |>
  trx_stats(percent_of = "av_anniv")

trx_res |>
  # remove periods with zero transactions
  filter(trx_n > 0) |>
  # first 10 rows showed for brevity
  head(10) |>
  autotable() |>
  gt::gtsave("man/figures/trx_gt.png")
