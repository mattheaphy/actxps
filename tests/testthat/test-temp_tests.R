study_py <- expose(census_dat, "2019-12-31")
study_cy <- expose_cal(census_dat, "2019-12-31")

test_that("py_exposure_checks", {
  expect_gt(min(study_py$exposure), 0)
  expect_lte(max(study_py$exposure), 1)
  expect_equal(sum(is.na(study_py$exposure)), 0)
})

test_that("cy_exposure_checks", {
  expect_gt(min(study_cy$exposure), 0)
  expect_lte(max(study_cy$exposure), 1)
  expect_equal(sum(is.na(study_cy$exposure)), 0)
})
