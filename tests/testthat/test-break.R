if(interactive()) {
  devtools::load_all()
  source("tests/testthat/utils.R")
} else
  source("utils.R")

test_that("break basic", {
  fn <- function(n) {
    while (n > 0L) {
      if (n %% 2L == 0L)
        break
      subtract(n) <- 1L
    }
    n
  }

  expect_ag_equivalent(fn, 0L)
  expect_ag_equivalent(fn, 4L)
  expect_ag_equivalent(fn, 5L)
})


test_that("break while loop", {

  # this test is a translation of the first test found here:
  # tensorflow/python/autograph/converters/break_statements_test.py

  # TODO: other tests found there should be translated to R once `append<-`() is
  # handeled in ag_while

  skip("append<- not yet implemented")
  fn <- function(x) {
    v <- list()
    while (x > 0L) {
      subtract(x) <- 1L
      if (x %% 2L == 0L)
        break
      append(v) <- x
    }
    v
  }

  expect_ag_equivalent(fn, 0L)
  expect_ag_equivalent(fn, 4L)
  expect_ag_equivalent(fn, 5L)
})

