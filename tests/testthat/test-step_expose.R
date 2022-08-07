rec_dat <- recipes::recipe(status ~ ., toy_census) |>
  step_expose(end_date = "2022-12-31", target_status = "Surrender") |>
  prep() |>
  recipes::juice()

expo_dat <- expose_py(toy_census, "2022-12-31",
                      target_status = "Surrender") |>
  tibble::as_tibble() |>
  dplyr::select(all_of(names(rec_dat)))

test_that("step_expose is identical to expose", {
  expect_identical(rec_dat, expo_dat)
})
