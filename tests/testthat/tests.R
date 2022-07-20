study_py <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
study_cy <- expose_cy(census_dat, "2019-12-31", target_status = "Surrender")

expected_table <- c(seq(0.005, 0.03, length.out = 10), 0.2, 0.15, rep(0.05, 3))

set.seed(123)
study_py <- study_py |>
  mutate(expected_1 = expected_table[pol_yr],
         expected_2 = ifelse(study_py$inc_guar, 0.015, 0.03),
         weights = rnorm(nrow(study_py), 100, 50) |> abs())

exp_res <- study_py |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(expected = c("expected_1", "expected_2"),
            credibility = TRUE)

exp_res_weighted <- study_py |>
  group_by(pol_yr, inc_guar) |>
  exp_stats(expected = c("expected_1", "expected_2"),
            credibility = TRUE, wt = "weights")

test_that("py_exposure_checks", {
  expect_gt(min(study_py$exposure), 0)
  expect_lte(max(study_py$exposure), 1)
  expect_equal(sum(is.na(study_py$exposure)), 0)
})

test_that("cy_exposure_checks", {
  expect_gt(min(study_cy$exposure), 0)
  expect_lte(max(study_cy$exposure), 1)
  expect_equal(sum(is.na(study_cy$exposure)), 0)
})

test_that("exp_df_summary_checks", {
  expect_lte(max(exp_res$credibility, exp_res$q_obs), 1)
  expect_gte(min(exp_res$credibility, exp_res$q_obs), 0)
  expect_identical(exp_res, summary(exp_res, pol_yr, inc_guar))
  expect_equal(exp_stats(study_py, expected = c("expected_1", "expected_2"),
                             credibility = TRUE),
                   summary(exp_res))
  expect_equal(exp_stats(study_py, expected = c("expected_1", "expected_2"),
                         credibility = TRUE),
               summary(exp_res))
  expect_equal(exp_stats(study_py, expected = c("expected_1", "expected_2"),
                         credibility = TRUE, wt = "weights"),
               summary(exp_res_weighted))
})
