## code to prepare examples datasets goes here
library(tidyverse)

status_list <- c("Active", "Death", "Surrender")

toy_census <- data.frame(
  pol_num = 1:3,
  status = c("Active", "Death", "Surrender") |> factor(levels = status_list),
  issue_date = as.Date(c("2010-01-01", "2011-05-27", "2009-11-10")),
  term_date = as.Date(c(NA, "2020-09-14", "2022-02-25"))
)

usethis::use_data(toy_census, overwrite = TRUE)

qx_iamb <- read_csv("data-raw/rates/qx_iamb.csv", col_types = "idc")
scale_g2 <- read_csv("data-raw/rates/scaleG2.csv", col_types = "idc")

usethis::use_data(qx_iamb, overwrite = TRUE)
usethis::use_data(scale_g2, overwrite = TRUE)

source("data-raw/simulate_data.R")

usethis::use_data(census_dat, overwrite = TRUE)
usethis::use_data(withdrawals, overwrite = TRUE)
usethis::use_data(account_vals, overwrite = TRUE)

agg_sim_dat <- expose_py(
  census_dat,
  "2019-12-31",
  target_status = "Surrender"
) |>
  add_transactions(withdrawals) |>
  left_join(account_vals, by = c("pol_num", "pol_date_yr")) |>
  group_by(pol_yr, inc_guar, qual, product) |>
  summarize(
    exposure_n = sum(exposure),
    claims_n = sum(status == "Surrender"),
    av = sum(av_anniv),
    exposure_amt = sum(exposure * av_anniv),
    claims_amt = sum((status == "Surrender") * av_anniv),
    av_sq = sum(av_anniv^2),
    n = n(),
    wd = sum(trx_amt_Rider) + sum(trx_amt_Base),
    wd_n = sum(trx_n_Rider) + sum(trx_n_Base),
    wd_flag = sum(trx_amt_Rider > 0 | trx_amt_Base > 0),
    wd_sq = sum(trx_amt_Rider^2) + sum(trx_amt_Base^2),
    av_w_wd = sum(av_anniv[trx_amt_Rider > 0 | trx_amt_Base > 0]),
    .groups = "drop"
  )

usethis::use_data(agg_sim_dat, overwrite = TRUE)
