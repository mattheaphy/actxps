test_that("is_exposed_df works", {
  expo <- expose(toy_census, "2020-12-31")
  expect_true(is_exposed_df(expo))
  expect_false(is_exposed_df(mtcars))
})

test_that("as_exposed_df works", {

  x <- as_exposed_df(data.frame(a = 1:3), Sys.Date())
  expect_true(is_exposed_df(x))
  expect_error(as_exposed_df(1))

})
