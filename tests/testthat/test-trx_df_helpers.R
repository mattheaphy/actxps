res <- expose(census_dat, "2019-12-31", target_status = "Surrender") |>
  add_transactions(withdrawals) |>
  left_join(account_vals, by = c("pol_num", "pol_date_yr")) |>
  group_by(pol_yr, inc_guar) |>
  trx_stats(percent_of = "av_anniv", trx_types = "Base", conf_int = TRUE)

test_that("is_trx_df works", {
  expect_true(is_trx_df(res))
  expect_false(is_trx_df(mtcars))
})

res2 <- as.data.frame(res)
res3 <- as_trx_df(res2, col_percent_of = "av_anniv", conf_int = TRUE)

test_that("as_trx_df works", {

  res4 <- res2 |>
    rename(expo = exposure)
  res5 <- res4 |>
    rename(tamt = trx_amt,
           tn = trx_n)

  expect_error(as_trx_df(data.frame(a = 1:3)),
               regexp = "The following columns are missing")

  expect_true(is_trx_df(as_trx_df(res)))

  expect_false(is_trx_df(res2))

  expect_true(is_trx_df(res3))

  expect_error(as_trx_df(res4), regexp = "The following columns are missing")
  expect_no_error(as_trx_df(res4, col_exposure = "expo"))
  expect_no_error(as_trx_df(res5, col_exposure = "expo", col_trx_amt = "tamt",
                            col_trx_n = "tn"))

  expect_error(as_trx_df(1), regexp = "`x` must be a data frame.")

})


test_that("as_trx_df summary matches an object created by trx_stats", {
  x <- summary(res, inc_guar) |> select(-inc_guar, -trx_type)
  y <- summary(res3, inc_guar) |> select(-inc_guar, -trx_type)
  expect_true(dplyr::near(x - y, 0) |> all())
})
