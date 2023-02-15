expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender") |>
  add_transactions(withdrawals) |>
  dplyr::mutate(q_exp = ifelse(inc_guar, 0.015, 0.03)) |>
  dplyr::group_by(inc_guar, pol_yr)

exp_res <- expo |> exp_stats()
trx_res <- expo |> trx_stats()
exp_res2 <- expo |> exp_stats(wt = "premium", credibility = TRUE,
                              expected = "q_exp")
trx_res2 <- expo |> trx_stats(percent_of = 'premium')

test_that("Autotable works", {
  expect_s3_class(autotable(exp_res), c("gt_tbl", "list"))
  expect_s3_class(autotable(trx_res), c("gt_tbl", "list"))
  expect_s3_class(autotable(exp_res2), c("gt_tbl", "list"))
  expect_s3_class(autotable(trx_res2), c("gt_tbl", "list"))
})
