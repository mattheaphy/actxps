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

  expect_error(as_exp_df(data.frame(a = 1:3), Sys.Date()),
               regexp = "The following columns are missing")

  expect_true(is_exp_df(as_exp_df(res)))

  expect_false(is_exp_df(res2))

  expect_true(is_exp_df(res3))

  expect_error(as_exp_df(res4), regexp = "The following columns are missing")
  expect_no_error(as_exp_df(res4, col_exposure = "expo"))
  expect_no_error(as_exp_df(res5, col_exposure = "expo", col_claims = "clms"))

  expect_error(as_exp_df(1), regexp = "`x` must be a data frame.")

})
