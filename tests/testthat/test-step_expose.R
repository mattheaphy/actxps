rec_dat <- recipes::recipe(status ~ ., toy_census) |>
  step_expose(end_date = "2022-12-31", target_status = "Surrender") |>
  prep() |>
  recipes::juice()

expo_dat <- expose_py(toy_census, "2022-12-31",
                      target_status = "Surrender") |>
  tibble::as_tibble() |>
  select(all_of(names(rec_dat)))

test_that("step_expose is identical to expose", {
  attributes(rec_dat) <- NULL
  attributes(expo_dat) <- NULL
  expect_identical(rec_dat, expo_dat)
})

test_that("Policy numbers can be retained in step_expose if desired", {

  pol_num_dat <- recipes::recipe(status ~ ., toy_census) |>
    step_expose(end_date = "2022-12-31", target_status = "Surrender",
                drop_pol_num = FALSE) |>
    prep() |>
    recipes::juice()

  expect_true("pol_num" %in% names(pol_num_dat), "pol_num")
})
