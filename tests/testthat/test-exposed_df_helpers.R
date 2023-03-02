expo <- expose(toy_census, "2020-12-31", target_status = "Surrender")

test_that("is_exposed_df works", {
  expect_true(is_exposed_df(expo))
  expect_false(is_exposed_df(mtcars))
})

expo2 <- as.data.frame(expo)

test_that("as_exposed_df works", {


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

  expect_error(as_exposed_df(1))

})

test_that("as_exposed_df works with transactions", {

  expo6 <- expo2 |>
    mutate(
      trx_n_A = 1,
      trx_amt_A = 2,
      trx_n_B = 3,
      trx_amt_B = 4)

  expect_no_error(as_exposed_df(expo6, "2022-12-31", trx_types = c("A", "B")))
  expect_error(as_exposed_df(expo6, "2022-12-31", trx_types = c("A", "C")))

  expo7 <- expo6 |>
    dplyr::rename(n_A = trx_n_A, n_B = trx_n_B,
                  amt_A = trx_amt_A, amt_B = trx_amt_B)
  expect_error(as_exposed_df(expo7, "2022-12-31", trx_types = c("A", "B")))
  expect_no_error(as_exposed_df(expo7, "2022-12-31", trx_types = c("A", "B"),
                                col_trx_n_ = "n_",
                                col_trx_amt_ = "amt_"))

})

test_that("exposed_df class persists after grouping and ungrouping", {
  expect_s3_class(expo, "exposed_df")
  expect_s3_class(expo |> group_by(pol_num), "exposed_df")
  expect_s3_class(expo |> group_by(pol_num) |> ungroup(), "exposed_df")
  expect_identical(expo, ungroup(expo))
})

test_that("exposed_df casting and coercion works with tibble and data.frame", {
  expect_s3_class(dplyr::bind_rows(expo, data.frame(x = 1)), "exposed_df")
  expect_s3_class(dplyr::bind_rows(expo, tibble::tibble(x = 1)), "exposed_df")

  expo8 <- dplyr::bind_rows(expo, expo)
  expect_s3_class(expo8, "exposed_df")
  expect_identical(attr(expo8, "target_status"), attr(expo8, "target_status") |> unique())

  expo9 <- vctrs::vec_rbind(
    expo,
    expose(toy_census, "2022-12-31", start_date = "1890-01-01",
           target_status = "Death"))

  expect_identical(attr(expo9, "end_date"), as.Date("2022-12-31"))
  expect_identical(attr(expo9, "start_date"), as.Date("1890-01-01"))
  expect_identical(attr(expo9, "target_status"), c("Surrender", "Death"))

  expo10 <- expose_cy(toy_census, "2020-12-31")
  expect_error(dplyr::bind_rows(expo, expo10))

})

test_that("exposed_df persists in a grouped and ungrouped context after using dplyr verbs", {

  # rename, relocate
  grouped <- expo |> mutate(x = ifelse(pol_num == 1, "A", "B")) |> group_by(x)

  expect_s3_class(grouped, "exposed_df")
  expect_s3_class(ungroup(expo), "exposed_df")
  expect_s3_class(ungroup(grouped), "exposed_df")
  expect_s3_class(filter(expo, pol_num == 1), "exposed_df")
  expect_s3_class(filter(grouped, pol_num == 1), "exposed_df")
  expect_s3_class(mutate(expo, z = 1), "exposed_df")
  expect_s3_class(mutate(grouped, z = 1), "exposed_df")
  expect_s3_class(select(expo, pol_num), "exposed_df")
  expect_s3_class(select(grouped, pol_num, x), "exposed_df")
  expect_s3_class(slice(expo, 1:2), "exposed_df")
  expect_s3_class(slice(grouped, 1:2), "exposed_df")
  expect_s3_class(arrange(expo, pol_yr), "exposed_df")
  expect_s3_class(arrange(grouped, pol_yr), "exposed_df")
  expect_s3_class(rename(expo, abc = pol_num), "exposed_df")
  expect_s3_class(rename(grouped, abc = pol_num), "exposed_df")
  expect_s3_class(relocate(expo, pol_num, .after = status), "exposed_df")
  expect_s3_class(relocate(grouped, pol_num, .after = status), "exposed_df")

})
