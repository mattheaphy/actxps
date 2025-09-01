expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender") |>
  add_transactions(withdrawals) |>
  mutate(q_exp = ifelse(inc_guar, 0.015, 0.03))

exp_stats2 <- function(dat) {
  exp_stats(
    dat,
    wt = "premium",
    credibility = TRUE,
    expected = "q_exp",
    conf_int = TRUE
  )
}
trx_stats2 <- function(dat) {
  trx_stats(dat, percent_of = 'premium', conf_int = TRUE)
}

# ungrouped summaries
exp_res <- exp_stats2(expo)
trx_res <- trx_stats2(expo)

# 1 grouping variables
expo <- expo |> group_by(pol_yr)
exp_res2 <- exp_stats2(expo)
trx_res2 <- trx_stats2(expo)

# 2 grouping variables
expo <- expo |> group_by(inc_guar, .add = TRUE)
exp_res3 <- exp_stats2(expo)
trx_res3 <- trx_stats2(expo)

# 3 grouping variables
expo <- expo |> group_by(product, .add = TRUE)
exp_res4 <- exp_stats2(expo)
trx_res4 <- trx_stats2(expo)

test_that("Autoplot works", {
  expect_s3_class(autoplot(exp_res), c("gg", "ggplot"))
  expect_s3_class(autoplot(exp_res2), c("gg", "ggplot"))
  expect_s3_class(autoplot(exp_res3), c("gg", "ggplot"))
  expect_s3_class(autoplot(exp_res4), c("gg", "ggplot"))
  expect_s3_class(autoplot(trx_res), c("gg", "ggplot"))
  expect_s3_class(autoplot(trx_res2), c("gg", "ggplot"))
  expect_s3_class(autoplot(trx_res3), c("gg", "ggplot"))
  expect_s3_class(autoplot(trx_res4), c("gg", "ggplot"))
})

test_that("Autoplot works with mapping overrides", {
  expect_s3_class(
    autoplot(
      exp_res4,
      inc_guar,
      x = pol_yr,
      y = ae_q_exp,
      color = product,
      scales = "free_y",
      geoms = "bars",
      y_labels = scales::label_number()
    ),
    c("gg", "ggplot")
  )
  expect_s3_class(
    autoplot(
      trx_res4,
      trx_type,
      inc_guar,
      x = pol_yr,
      y = pct_of_premium_w_trx,
      color = product,
      scales = "free_y",
      geoms = "bars",
      y_labels = scales::label_number()
    ),
    c("gg", "ggplot")
  )

  expect_s3_class(
    autoplot(
      exp_res4,
      inc_guar,
      mapping = ggplot2::aes(x = pol_yr, y = ae_q_exp, fill = product),
      scales = "free_y",
      geoms = "bars",
      y_labels = scales::label_number()
    ),
    c("gg", "ggplot")
  )
  expect_s3_class(
    autoplot(
      trx_res4,
      trx_type,
      inc_guar,
      mapping = ggplot2::aes(
        x = pol_yr,
        y = pct_of_premium_w_trx,
        fill = product
      ),
      scales = "free_y",
      geoms = "bars",
      y_labels = scales::label_number()
    ),
    c("gg", "ggplot")
  )

  expect_s3_class(autoplot(exp_res4, geoms = "points"), c("gg", "ggplot"))
  expect_s3_class(autoplot(trx_res4, geoms = "points"), c("gg", "ggplot"))
})

test_that("Second axis works", {
  expect_s3_class(autoplot(exp_res4, second_axis = TRUE), c("gg", "ggplot"))
  expect_s3_class(autoplot(trx_res4, second_axis = TRUE), c("gg", "ggplot"))
  expect_s3_class(
    autoplot(trx_res4, second_axis = TRUE, second_y = trx_n),
    c("gg", "ggplot")
  )
  expect_s3_class(
    autoplot(
      trx_res4,
      second_axis = TRUE,
      second_y = trx_n,
      second_y_labels = scales::comma(0.1)
    ),
    c("gg", "ggplot")
  )
})

test_that("Termination plots works", {
  expect_s3_class(plot_termination_rates(exp_res), c("gg", "ggplot"))
  expect_s3_class(
    plot_termination_rates(
      exp_res,
      include_cred_adj = TRUE,
      conf_int_bars = TRUE
    ),
    c("gg", "ggplot")
  )
  expect_error(plot_termination_rates(trx_res), regexp = "must be an `exp_df`")
})

test_that("AE plots works", {
  expect_s3_class(plot_actual_to_expected(exp_res), c("gg", "ggplot"))
  expect_s3_class(
    plot_actual_to_expected(exp_res, conf_int_bars = TRUE),
    c("gg", "ggplot")
  )
  expect_error(plot_actual_to_expected(trx_res), regexp = "must be an `exp_df`")
  expect_error(
    plot_actual_to_expected(expo |> exp_stats()),
    regexp = "does not have any actual-to-expected"
  )
})

test_that("Transaction utilization plots work", {
  expect_s3_class(plot_utilization_rates(trx_res), c("gg", "ggplot"))
  expect_s3_class(plot_utilization_rates(trx_res2), c("gg", "ggplot"))
  expect_s3_class(plot_utilization_rates(trx_res3), c("gg", "ggplot"))
  expect_s3_class(plot_utilization_rates(trx_res4), c("gg", "ggplot"))
  expect_error(plot_utilization_rates(exp_res), regexp = "must be a `trx_df`")
})

test_that("Log y scale works", {
  expect_s3_class(
    autoplot(exp_res4, y_log10 = TRUE, second_axis = TRUE),
    c("gg", "ggplot")
  )
  expect_s3_class(
    autoplot(trx_res4, y_log10 = TRUE, second_axis = TRUE),
    c("gg", "ggplot")
  )
})

no_ci <- expo |>
  group_by(pol_yr, inc_guar, product) |>
  exp_stats(expected = "q_exp")

test_that("Confidence interval warning messages work for termination plots", {
  expect_no_warning(autoplot(exp_res4, conf_int_bars = TRUE))
  expect_warning(
    autoplot(no_ci, conf_int_bars = TRUE),
    regexp = "has no confidence intervals"
  )
  expect_warning(
    plot_termination_rates(no_ci, conf_int_bars = TRUE),
    regexp = "has no confidence intervals"
  )
  expect_warning(
    plot_actual_to_expected(no_ci, conf_int_bars = TRUE),
    regexp = "has no confidence intervals"
  )

  expect_warning(
    autoplot(exp_res4, conf_int_bars = TRUE, y = exposure),
    regexp = "Confidence intervals are not available"
  )
})

test_that("Confidence interval warning messages work for transaction plots", {
  expect_no_warning(autoplot(trx_res4, conf_int_bars = TRUE))

  no_ci_trx <- expo |>
    group_by(pol_yr, inc_guar, product) |>
    trx_stats(percent_of = "premium")
  expect_warning(
    autoplot(no_ci_trx, conf_int_bars = TRUE),
    regexp = "has no confidence intervals"
  )
  expect_warning(
    plot_utilization_rates(no_ci_trx, conf_int_bars = TRUE),
    regexp = "has no confidence intervals"
  )
  expect_warning(
    autoplot(trx_res4, conf_int_bars = TRUE, y = exposure),
    regexp = "Confidence intervals are not available"
  )
})

test_that("plot_termination_rates credibility-adjusted message works", {
  expect_warning(
    plot_termination_rates(no_ci, include_cred_adj = TRUE),
    regexp = "has no credibility-weighted"
  )
  no_expected <- expo |> group_by(pol_yr) |> exp_stats(credibility = TRUE)
  expect_warning(
    plot_termination_rates(no_expected, include_cred_adj = TRUE),
    regexp = "has no credibility-weighted"
  )
})
