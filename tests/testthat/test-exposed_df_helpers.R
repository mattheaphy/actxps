expo <- expose(toy_census, "2020-12-31")

test_that("is_exposed_df works", {
  expect_true(is_exposed_df(expo))
  expect_false(is_exposed_df(mtcars))
})

test_that("as_exposed_df works", {

  expo2 <- as.data.frame(expo)
  expo3 <- as_exposed_df(expo2, end_date = "2022-12-31")
  expo4 <- expo2 |>
    dplyr::rename(pnum = pol_num)
  expo5 <- expo4 |>
    dplyr::rename(pstat = status,
                  expo = exposure,
                  py = pol_yr,
                  start = pol_date_yr,
                  end = pol_date_yr_end)

  expect_error(as_exposed_df(data.frame(a = 1:3), Sys.Date()))

  expect_true(is_exposed_df(as_exposed_df(expo)))

  expect_false(is_exposed_df(expo2))
  expect_error(as_exposed_df(expo2, end_date = "2022-12-31", expo_length = "yr"))

  expect_true(is_exposed_df(expo3))

  expect_error(as_exposed_df(expo4))
  expect_no_error(as_exposed_df(expo4, end_date = "2022-12-31",
                                col_pol_num = "pnum"))
  expect_no_error(as_exposed_df(expo5, end_date = "2022-12-31",
                                col_pol_num = "pnum",
                                col_status = "pstat",
                                col_exposure = "expo",
                                col_pol_per = "py",
                                cols_dates = c("start", "end")))

  #expect_true(is_exposed_df(x))
  expect_error(as_exposed_df(1))

})
