# test multiple calls for add_transactions w/ row count unchanged
# expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
# add_transactions(expo, withdrawals)
# withdrawals3 <- withdrawals |> mutate(trx_type = "something")
# withdrawals2 <- withdrawals |> mutate(trx_type = if_else(trx_type == "Base", "A", "B"))
# add_transactions(expo, withdrawals) |> add_transactions(withdrawals)

# add_transactions(as_tibble(expo), withdrawals)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
