expo <- expose_py(census_dat, "2019-12-31") |>
  mutate(surrender = status == "Surrender")
mod <- glm(surrender ~ inc_guar + pol_yr, expo, family = 'binomial')

dat <- data.frame(x = 1:30, y = 5 + (1:30) * 2.5 + rnorm(30))
mod2 <- lm(y ~ x, dat)

test_that("add_predictions works with exposed_df and data frames", {
  expo <- add_predictions(expo, mod, type = 'response')

  expect_s3_class(expo, 'exposed_df')
  expect_identical(
    expo$expected,
    predict(mod, new_data = expo, type = 'response') |>
      unname()
  )

  dat <- add_predictions(dat, mod2)
  expect_s3_class(dat, 'data.frame')
})

test_that("add_predictions works with matrix output", {
  suppressMessages(
    expect_s3_class(add_predictions(expo, mod, type = 'terms'), 'exposed_df')
  )
})

test_that("add_predictions col_expected works", {
  expo2 <- add_predictions(expo, mod, type = 'response', col_expected = 'q_exp')
  expo2 <- add_predictions(
    expo2,
    mod,
    type = 'terms',
    col_expected = c('term1', 'term2')
  )
  expect_true(all(c('q_exp', 'term1', 'term2') %in% names(expo2)))
})
