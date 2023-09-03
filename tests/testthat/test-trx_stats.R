no_trx <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender") |>
  add_transactions(withdrawals) |>
  left_join(account_vals, by = c("pol_num", "pol_date_yr"))

res <- expo |>
  group_by(pol_yr, inc_guar) |>
  trx_stats(percent_of = c("av_anniv", "premium"),
            conf_int = TRUE)

test_that("trx_stats error checks work", {

  expect_error(trx_stats(1))
  expect_error(trx_stats(no_trx))
  expect_error(trx_stats(expo, trx_types = c("abc", "def")))
  expect_no_error(trx_stats(expo))

})

test_that("Experience study summary method checks", {
  expect_identical(res, summary(res, pol_yr, inc_guar))
  expect_equal(trx_stats(expo, percent_of = c("av_anniv", "premium"),
                         conf_int = TRUE),
               summary(res))
})

expo2 <- head(expo) |> rename(ex = exposure)

test_that("Renaming works", {
  expect_error(trx_stats(expo2))
  expect_no_error(trx_stats(expo2, col_exposure = "ex"))
})

test_that("trx_stats works", {

  expect_equal(ncol(expo |> trx_stats(trx_types = "Base",
                                      percent_of = c("av_anniv", "premium"),
                                      conf_int = TRUE)),
               ncol(res) - 2)
  expect_s3_class(res, "trx_df")
  expect_true(all(res$exposure >= res$trx_flag, na.rm = TRUE))
  expect_true(all(res$avg_trx >= res$avg_all, na.rm = TRUE))
  expect_true(all(res$trx_freq >= res$trx_util, na.rm = TRUE))
  expect_true(all(res$pct_of_av_anniv_w_trx >= res$pct_of_av_anniv_all,
                  na.rm = TRUE))
  expect_true(all(trx_stats(expo)$exposure <=
                    trx_stats(expo, full_exposures_only = FALSE)$exposure))

  expect_equal(expo |> trx_stats(combine_trx = TRUE, trx_types = "Rider") |>
                 select(-trx_type),
               expo |> trx_stats(trx_types = "Rider") |>
                 select(-trx_type))

  res2 <- expo |> trx_stats(combine_trx = TRUE) |> as.data.frame()
  attr(res2, "trx_types") <- NULL
  res3 <- no_trx |> add_transactions(
    withdrawals |> mutate(trx_type = "All")) |>
    trx_stats() |>
    as.data.frame()
  attr(res3, "trx_types") <- NULL

  expect_equal(res2, res3)

})

test_that("Confidence intervals work", {
  res2 <- filter(res, trx_util > 0)
  expect_true(all(res2$trx_util < res2$trx_util_upper))
  expect_true(all(res2$trx_util > res2$trx_util_lower))
  expect_true(all(res2$pct_of_premium_w_trx < res2$pct_of_premium_w_trx_upper))
  expect_true(all(res2$pct_of_premium_w_trx > res2$pct_of_premium_w_trx_lower))
  expect_true(all(res2$pct_of_premium_all < res2$pct_of_premium_all_upper))
  expect_true(all(res2$pct_of_premium_all > res2$pct_of_premium_all_lower))
})
