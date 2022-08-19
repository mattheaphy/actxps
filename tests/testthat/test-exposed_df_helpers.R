test_that("is_exposed_df works", {
  expo <- expose(toy_census, "2020-12-31")
  expect_true(is_exposed_df(expo))
  expect_false(is_exposed_df(mtcars))
})
