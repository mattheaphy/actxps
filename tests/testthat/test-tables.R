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
  expect_s3_class(autotable(exp_res2, show_conf_int = TRUE),
                  c("gt_tbl", "list"))
  expect_s3_class(autotable(trx_res2, show_conf_int = TRUE),
                  c("gt_tbl", "list"))
})

test_that("Renaming works", {
  expect_s3_class(
    autotable(exp_res2,
              rename_cols = list(pol_yr = "Policy year", inc_guar = "GLWB")),
    c("gt_tbl", "list"))
  expect_s3_class(
    autotable(trx_res2,
              rename_cols = list(pol_yr = "Policy year", inc_guar = "GLWB")),
    c("gt_tbl", "list"))
})

test_that("Table confidence interval warning works", {
  expect_warning(autotable(exp_res, show_conf_int = TRUE),
                 regex = "has no confidence intervals")
  expect_warning(autotable(trx_res, show_conf_int = TRUE),
                 regex = "has no confidence intervals")
})

test_that("Table credibility-weighted termination rates warning works", {
  expect_warning(autotable(exp_res, show_cred_adj = TRUE),
                 regex = "has no credibility-weighted")
})

test_that("Totals are shown in tables", {
  expect_s3_class(autotable(exp_res, show_total = TRUE), c("gt_tbl", "list"))
  expect_s3_class(autotable(trx_res, show_total = TRUE), c("gt_tbl", "list"))
})
