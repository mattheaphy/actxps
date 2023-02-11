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

leap_day <- data.frame(pol_num = 1L,
                       status = 'Active',
                       issue_date = as.Date("2020-02-29"),
                       term_date = NA)

leap_expose <- expose_pm(leap_day, end_date = "2021-02-28")



march_1 <- data.frame(pol_num = 1L,
                      status = 'Active',
                      issue_date = as.Date("2019-03-01"),
                      term_date = NA)
march_1_expose <- expose_pm(march_1, end_date = "2020-02-29")


test_that("Test leap day stability.", {
  expect_equal(nrow(leap_expose), 12)
  expect_equal(nrow(march_1_expose), 12)
})


with_start_date <- expose_py(census_dat,
                             "2019-12-31",
                             start_date = "2018-12-31",
                             target_status = "Surrender")

test_that("Start and end dates work.", {
  expect_gte(min(with_start_date$pol_date_yr), as.Date("2018-12-31"))
  expect_lte(max(with_start_date$pol_date_yr), as.Date("2019-12-31"))
})

exposed_strings <- expose(toy_census, "2020-12-31", "2016-04-01")
exposed_dates <- expose(toy_census, as.Date("2020-12-31"),
                        as.Date("2016-04-01"))

test_that("Expose date arguments can be passed strings.", {
  expect_identical(exposed_strings, exposed_dates)
})

renamer <- c("pol_num" = "a",
             "status" = "b",
             "issue_date" = "c",
             "term_date" = "d")
toy_census2 <- toy_census |> dplyr::rename_with(\(x) renamer[x])

test_that("Renaming and name conflict warnings work", {
  expect_error(expose(toy_census2, "2020-12-31"))
  expect_no_error(expose(toy_census2, "2020-12-31",
                         col_pol_num = "a",
                         col_status = "b",
                         col_issue_date = "c",
                         col_term_date = "d"))
  expect_warning(expose(toy_census |> dplyr::mutate(exposure = 1), "2020-12-31"))
  expect_warning(expose(toy_census |> dplyr::mutate(pol_yr = 1), "2020-12-31"))
  expect_warning(expose(toy_census |> dplyr::mutate(pol_date_yr = 1), "2020-12-31"))
  expect_warning(expose(toy_census |> dplyr::mutate(pol_date_yr_end = 1), "2020-12-31"))
  expect_warning(expose_cy(toy_census |> dplyr::mutate(cal_yr = 1), "2020-12-31"))
  expect_warning(expose_cy(toy_census |> dplyr::mutate(cal_yr_end = 1), "2020-12-31"))
})
