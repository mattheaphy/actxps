toy_res <- toy_census |>
  expose_py(end_date = "2022-12-31", target_status = "Surrender") |>
  exp_stats()

test_that("Autotable works", {
  expect_s3_class(toy_res |> autotable(), c("gt_tbl", "list"))
})
