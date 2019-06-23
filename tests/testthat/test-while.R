if(interactive()) {
  devtools::load_all()
  source("tests/testthat/utils.R")
} else
  source("utils.R")


test_that("while basic", {
  fn <- function(n) {
    i <- 0L
    s <- 0L
    while (i < n) {
      add(s) <- i
      add(i) <- 1L
    }
    list(s, i, n)
  }
  ag_fn <- autograph(fn)
  grab(ag_fn(as_tensor(5L)))

  expect_result(ag_fn, as_tensor(5L), list(10L, 5L, 5L))
})


test_that("while nested", {
  fn <- function(n) {
    i <- 0L
    j <- 0L
    s <- 0L
    while (i < n) {
      while (j < i)
        add(j) <- 3L
      u <- i + j
      add(s) <- u
      add(i) <- 1L
      j <- 0L
    }
    list(s, i, j, n)
  }

  # devtools::load_all()
  ag_fn <- autograph(fn)
  # res <- ag_fn(as_tensor(5L))
  # grab(res)

  expect_result(ag_fn, as_tensor(5L), list(25L, 5L, 0L, 5L))

})


test_that("while single output", {
  fn <- function(n) {
    while (n > 0L)
      subtract(n) <- 1L
    n
  }

  devtools::load_all()
  ag_fn <- autograph(fn)
  res <- ag_fn(as_tensor(5L))
  grab(res)

  expect_result(ag_fn, as_tensor(5L), 0L)

})


test_that("while local composite", {

  foo <- function() list(x = as_tensor(3L))

  fn <- function(n) {
    while(n > 0L) {
      tc <- foo()
      tc$x <- tc$x
      subtract(n) <- 1L
    }
    n
  }
  ag_fn <- autograph(fn)

  expect_result(ag_fn, as_tensor(5L), 0L)
})


test_that("while local composite complex nestable", {
  fn <- function(n) {
    tc <- list(x = list(as_tensor(0L)))
    while(n > 0L) {
      tc <- list(x = list(as_tensor(3L)))
      add( tc$x[[1]] ) <- 1L
      subtract(n) <- 1L
    }
    tc$x[[1]]
  }
  ag_fn <- autograph(fn)

  expect_result(ag_fn, as_tensor(5L), 4L)
})


test_that("while local composite complex illegal", {
  foo <- function() list(x = list(as_tensor(3L)))
  fn <- function(n) {
    while(n > 0L) {
      tc <- foo()
      tc$x[[1]] <- tc$x[[1]] + 1L
      subtract(n) <- 1L
    }
    tc$x[[1]]
  }
  ag_fn <- autograph(fn)

  # expect_error(ag_fn(as_tensor(5L)), 'must be defined before the loop:.*tc.*')
  # TODO: while should return a better error message
  expect_error(ag_fn(as_tensor(5L)), '*tc*')
})

test_that("while test while dispatches by cond only", {

  TensorIncompatibleNumeric <- function(val)
    structure(list(val = val), class = "TensorIncompatibleNumeric")

  `+.TensorIncompatibleNumeric` <- function(x, y)
    TensorIncompatibleNumeric(x$val + y)


  fn <- function(n, s) {
    while(n > 0L) {
      subtract(n) <- 1L
      add(s) <- n
    }
    s
  }
  ag_fn <- autograph(fn)

  expect_equal(ag_fn(5L,  TensorIncompatibleNumeric(0L)$val), 10L)
  # expect_result(ag_fn, list(5L,  TensorIncompatibleNumeric(0L)$val), 10L)
  expect_error(ag_fn(as_tensor(5L), TensorIncompatibleNumeric(0L)), "TypeError")
})



