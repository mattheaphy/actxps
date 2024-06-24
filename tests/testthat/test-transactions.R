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
    mutate(trx_type = ifelse(trx_type == "Base", "A", "B"))
  withdrawals3 <- withdrawals |> mutate(trx_type = "something")

  expect_equal(
    nrow(expo),
    nrow(expo_trx |>
           add_transactions(withdrawals2) |>
           add_transactions(withdrawals3))
  )

})

test_that("Type checks work", {
  expect_error(add_transactions(as.data.frame(expo), withdrawals),
               regexp = 'must be an `exposed_df` object')
  expect_error(add_transactions(1, withdrawals),
               regexp = 'must be an `exposed_df` object')
  expect_error(add_transactions(expo, 1), regexp = 'must be a data frame')
})

withdrawals4 <- withdrawals |> setNames(letters[1:4])

test_that("Renaming works and name conflicts work", {
  expect_error(add_transactions(expo |> head(), withdrawals4),
               regexp = "Can't rename")
  expect_no_error(add_transactions(expo, withdrawals4,
                                   col_pol_num = "a",
                                   col_trx_date = "b",
                                   col_trx_type = "c",
                                   col_trx_amt = "d"))
  expect_error(add_transactions(expo_trx, withdrawals),
               regexp = 'transaction types that have already been attached')
})

test_that("exposed_df persists after adding transactions", {
  expect_s3_class(expo_trx, "exposed_df")
  expect_s3_class(add_transactions(expo |> group_by(pol_yr), withdrawals),
                  "exposed_df")
})

test_that("Date format checks work", {
  withdrawals5 <- withdrawals
  withdrawals5$trx_date[[42]] <- NA
  expect_error(add_transactions(expo, withdrawals5),
               regexp = "Missing values are not allowed in the `trx_date`")
})
