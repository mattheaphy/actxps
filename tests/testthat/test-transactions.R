expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
expo_trx <- add_transactions(expo, withdrawals)

test_that("add_transactions works", {
  expect_equal(sum(withdrawals$trx_amt), sum(expo_trx$trx_amt_Base) +
                 sum(expo_trx$trx_amt_Rider))
  expect_equal(nrow(withdrawals), sum(expo_trx$trx_n_Base) +
                 sum(expo_trx$trx_n_Rider))
})

test_that("multiple calls to add_transactions works", {

  withdrawals2 <- withdrawals |>
    dplyr::mutate(trx_type = ifelse(trx_type == "Base", "A", "B"))
  withdrawals3 <- withdrawals |> dplyr::mutate(trx_type = "something")

  expect_equal(
    nrow(expo),
    nrow(expo_trx |>
           add_transactions(withdrawals2) |>
           add_transactions(withdrawals3))
  )

})

test_that("Type checks work", {
  expect_error(add_transactions(as.data.frame(expo), withdrawals))
  expect_error(add_transactions(1, withdrawals))
  expect_error(add_transactions(expo, 1))
})
