res <- expose(toy_census, "2022-12-31", target_status = "Surrender") |>
  exp_stats()

test_that("is_exp_df works", {
  expect_true(is_exp_df(res))
  expect_false(is_exp_df(mtcars))
})

res2 <- as.data.frame(res)

test_that("as_exp_df works", {
  res3 <- as_exp_df(res2)
  res4 <- res2 |>
    rename(expo = exposure)
  res5 <- res4 |>
    rename(clms = claims)

  expect_error(
    as_exp_df(data.frame(a = 1:3)),
    regexp = "The following columns are missing"
  )

  expect_true(is_exp_df(as_exp_df(res)))

  expect_false(is_exp_df(res2))

  expect_true(is_exp_df(res3))

  expect_error(as_exp_df(res4), regexp = "The following column is missing")
  expect_no_error(as_exp_df(res4, col_exposure = "expo"))
  expect_no_error(as_exp_df(res5, col_exposure = "expo", col_claims = "clms"))

  expect_error(as_exp_df(1), regexp = "`x` must be a data frame.")
})

# weighted tests
res_wt <- expose(census_dat, "2019-12-31", target_status = "Surrender") |>
  mutate(ex = 0.05) |>
  group_by(pol_yr, product) |>
  exp_stats(
    wt = "premium",
    expected = "ex",
    conf_int = TRUE,
    credibility = TRUE
  )

res_wt2 <- as.data.frame(res_wt) |>
  rename(premium = .weight)
res_wt3 <- as_exp_df(
  res_wt2,
  wt = "premium",
  expected = "ex",
  conf_int = TRUE,
  credibility = TRUE
)

test_that("as_exp_df with weights works", {
  res_wt4 <- res_wt2 |>
    rename(expo = exposure)
  res_wt5 <- res_wt4 |>
    rename(clms = claims, n = n_claims, sq = .weight_sq)

  expect_true(is_exp_df(as_exp_df(res_wt)))
  expect_true(is_exp_df(res_wt3))

  expect_error(
    as_exp_df(res_wt5, wt = "premium"),
    regexp = "The following columns are missing"
  )
  expect_no_error(as_exp_df(res_wt4, wt = "premium", col_exposure = "expo"))
  expect_no_error(as_exp_df(
    res_wt5,
    wt = "premium",
    col_exposure = "expo",
    col_claims = "clms",
    col_weight_sq = "sq",
    col_n_claims = "n"
  ))
})

test_that("as_exp_df summary matches an object created by exp_stats", {
  x <- summary(res_wt, product) |> select(-product)
  y <- summary(res_wt3, product) |> select(-product)
  expect_true(dplyr::near(x - y, 0) |> all())

  x <- summary(res_wt, pol_yr) |> select(-pol_yr)
  y <- summary(res_wt3, pol_yr) |> select(-pol_yr)
  expect_true(dplyr::near(x - y, 0) |> all())
})
