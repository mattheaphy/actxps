study_py <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
study_cy <- expose_cy(census_dat, "2019-12-31", target_status = "Surrender")

test_that("Policy year exposure checks", {
  expect_gt(min(study_py$exposure), 0)
  expect_lte(max(study_py$exposure), 1)
  expect_equal(sum(is.na(study_py$exposure)), 0)
  expect_true(all(study_py$exposure[study_py$status == "Surrender"] == 1))
})

test_that("Calendar year exposure checks", {
  expect_gt(min(study_cy$exposure), 0)
  expect_lte(max(study_cy$exposure), 1)
  expect_equal(sum(is.na(study_cy$exposure)), 0)
  expect_true(all(study_cy$exposure[study_cy$status == "Surrender"] == 1))

})


check_period_end_pol <- expose_pw(toy_census, "2020-12-31",
                                  target_status = "Surrender") |>
  dplyr::select(pol_num, pol_date_wk, pol_date_wk_end) |>
  dplyr::group_by(pol_num) |>
  dplyr::mutate(x = dplyr::lead(pol_date_wk)) |>
  dplyr::ungroup() |>
  na.omit() |>
  dplyr::filter(x != pol_date_wk_end + 1) |>
  nrow()

check_period_end_cal <- expose_cm(toy_census, "2020-12-31",
                                  target_status = "Surrender") |>
  dplyr::select(pol_num, cal_mth, cal_mth_end) |>
  dplyr::group_by(pol_num) |>
  dplyr::mutate(x = dplyr::lead(cal_mth)) |>
  dplyr::ungroup() |>
  na.omit() |>
  dplyr::filter(x != cal_mth_end + 1) |>
  nrow()

test_that("Period start and end dates roll", {
  expect_true(all(study_py$pol_date_yr < study_py$pol_date_yr_end))
  expect_true(all(study_cy$cal_yr < study_cy$cal_yr_end))
  expect_equal(check_period_end_pol, 0)
  expect_equal(check_period_end_cal, 0)
})
