test_that("Leap years work", {
  expect_equal(pol_yr("2021-02-28", "2020-02-29"), 1)
  expect_equal(pol_yr("2021-03-01", "2020-02-29"), 2)
  expect_equal(pol_mth("2020-03-28", "2020-02-29"), 1)
  expect_equal(pol_mth("2020-03-29", "2020-02-29"), 2)
  expect_equal(pol_yr("2024-02-27", "2023-02-28"), 1)
  expect_equal(pol_yr("2024-02-29", "2023-02-28"), 2)
})

test_that("pol_interval works", {
  expect_equal(pol_yr("2021-02-28", "2020-02-29"),
               pol_interval("2021-02-28", "2020-02-29", lubridate::years(1)))
  expect_equal(pol_interval("2022-03-14", "2022-01-05", lubridate::minutes(3)),
               69L * 24L * 20L)
})
