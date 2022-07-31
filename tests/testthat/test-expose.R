study_py <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
study_cy <- expose_cy(census_dat, "2019-12-31", target_status = "Surrender")

test_that("Policy year exposure checks", {
  expect_gt(min(study_py$exposure), 0)
  expect_lte(max(study_py$exposure), 1)
  expect_equal(sum(is.na(study_py$exposure)), 0)
})

test_that("Calendar year exposure checks", {
  expect_gt(min(study_cy$exposure), 0)
  expect_lte(max(study_cy$exposure), 1)
  expect_equal(sum(is.na(study_cy$exposure)), 0)
})
