expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender") |>
  add_transactions(withdrawals) |>
  mutate(q_exp = ifelse(inc_guar, 0.015, 0.03)) |>
  group_by(pol_yr, inc_guar)

exp_res <- expo |> exp_stats()
trx_res <- expo |> trx_stats()
exp_res2 <- expo |> exp_stats(wt = "premium", credibility = TRUE,
                              expected = "q_exp", conf_int = TRUE)
trx_res2 <- expo |> trx_stats(percent_of = 'premium', conf_int = TRUE)

test_that("Autotable works", {
  expect_s3_class(autotable(exp_res), c("gt_tbl", "list"))
  expect_s3_class(autotable(trx_res), c("gt_tbl", "list"))
  expect_s3_class(autotable(exp_res2, conf_int_show = TRUE),
                  c("gt_tbl", "list"))
  expect_s3_class(autotable(trx_res2, conf_int_show = TRUE),
                  c("gt_tbl", "list"))
})

test_that("Table confidence interval warning works", {
  expect_warning(autotable(exp_res, conf_int_show = TRUE),
                 regex = "has no confidence intervals")
  expect_warning(autotable(trx_res, conf_int_show = TRUE),
                 regex = "has no confidence intervals")
})
