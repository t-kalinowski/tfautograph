if(interactive()) {
  devtools::load_all()
  source("tests/testthat/utils.R")
} else
  source("utils.R")


test_that("stopifnot simple", {
  fn <- function(x) {
    stopifnot(x > 0)
    x
  }
  ag_fn <- autograph(fn)

  expect_equal(ag_fn(1), 1)
  expect_error(ag_fn(0), 'x > 0', fixed = TRUE)

  expect_equal(grab(ag_fn(as_tensor(1))), 1)
  expect_error(grab(ag_fn(as_tensor(0))), "stopifnot(x > 0)", fixed = TRUE)
  expect_error(grab(ag_fn(as_tensor(0))), "ag_fn(", fixed = TRUE)
})

test_that("stopifnot compound", {

  fn <- function(x, y, z) {
    stopifnot(x > 0, x < 10, y > x, z > y & z < 20)
    list(x, y, z)
  }
  ag_fn <- autograph(fn)

  res <- ag_fn(as_tensor(1), as_tensor(2), as_tensor(3))
  expect_equal(grab(res), list(1,2,3))

  res <- ag_fn(as_tensor(2), as_tensor(2), as_tensor(3))
  expect_error(grab(res), "y > x", fixed = TRUE)

  expect_error(fn(2, 2, 3), "y > x", fixed = TRUE)

  res <- ag_fn(as_tensor(1), as_tensor(2), as_tensor(2))
  expect_error(grab(res), "z > y & z < 20", fixed = TRUE)

  res <- ag_fn(as_tensor(8), as_tensor(7), as_tensor(13))
  expect_error(grab(res), "y > x", fixed = TRUE)

  res <- ag_fn(as_tensor(10), as_tensor(11), as_tensor(12))
  expect_error(grab(res), "x < 10", fixed = TRUE)

  # TODO: make sure this still works in-line expressions and in if statements
  # anywhere else that as_outcome_fn() is used

})

