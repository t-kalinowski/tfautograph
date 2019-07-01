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

test_that("break and next basic", {
  fn <- function(n) {
    x <- 0
    while (n > 0L) {
      subtract(n) <- 1L
      if (n %% 5L == 0L)
        break
      else if (n %% 2L == 0L)
        next
      add(x) <- 1
    }
    list(n, x)
  }

  # fn(3) # 0 1
  # fn(4) # 0 2
  # fn(6) # 5 0
  # fn(9) # 5 1

  expect_ag_equivalent(fn, 3)
  expect_ag_equivalent(fn, 4)
  expect_ag_equivalent(fn, 6)
  expect_ag_equivalent(fn, 9)
})

#
# test_that("break while loop", {
#
#   # this test is a translation of the first test found here:
#   # tensorflow/python/autograph/converters/break_statements_test.py
#
#   # TODO: other tests found there should be translated to R once `append<-`() is
#   # handeled in ag_while
#
#   skip("append<- not yet implemented")
#   fn <- function(x) {
#     v <- list()
#     while (x > 0L) {
#       subtract(x) <- 1L
#       if (x %% 2L == 0L)
#         break
#       append(v) <- x
#     }
#     v
#   }
#
#   expect_ag_equivalent(fn, 0L)
#   expect_ag_equivalent(fn, 4L)
#   expect_ag_equivalent(fn, 5L)
# })

