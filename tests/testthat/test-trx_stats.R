expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
expo2 <- add_transactions(expo, withdrawals)

test_that("trx_stats error checks work", {

  expect_error(trx_stats(1))
  expect_error(trx_stats(expo))
  expect_error(trx_stats(expo2, trx_types = c("abc", "def")))
  expect_no_error(trx_stats(expo2))

})


# TODO make sure to test that summary is equivalent to calling trx_stats w/o groups
