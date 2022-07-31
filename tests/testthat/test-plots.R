toy_res <- toy_census |>
  expose_py(end_date = "2022-12-31", target_status = "Surrender") |>
  dplyr::group_by(pol_yr) |>
  exp_stats()

test_that("Autplot works", {
  expect_s3_class(toy_res |> autoplot(), c("gg", "ggplot"))
})
