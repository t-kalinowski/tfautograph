if(interactive()) {
  devtools::load_all()
  source("tests/testthat/utils.R")
} else
  source("utils.R")

test_that("for single output", {
  fn <- function(l) {
    s <- 0L
    for (e in l)
      add(s) <- e
    s
  }
  ag_fn <- autograph(fn)

  expect_result(ag_fn, as_tensor(list(1L, 3L)), 4L)
  expect_result(ag_fn, as_tensor(   c(1L, 3L)), 4L)
})


test_that("for simple", {
  fn <- function(l) {
    s1 <- 0L
    s2 <- 0L
    for (e in l) {
      add(s1) <- e
      add(s2) <- e * e
    }
    list(s1, s2)
  }
  ag_fn <- autograph(fn)

  expect_result(ag_fn, as_tensor(list(1L, 3L)), list(4, 10))
  expect_result(ag_fn, as_tensor(list(), dtype = 'int32'), list(0, 0))
})


test_that("for iterated expression", {
  # no first-class iterators in R, so this is really testing
  # an R vector as an
  # iterable
  eval_count <- 0L
  count_evals <- function(x) {
    add(eval_count) <<- 1L
    x
  }

  fn <- function(n) {
    s <- 0L
    for (e in count_evals(seq_len0(n)))
      add(s) <- e
    s
  }
  ag_fn <- autograph(fn)

  expect_equal(ag_fn(5L), 10L)
  expect_equal(eval_count, 1)
})

# R doesn't have tuple unpacking and comprehensions, so not implementing in autograph





## End: R translation of python tests

## Start: R tests


# test_that("for no vars modified in body", {
#   skip("rethink exporting for loop final var state")
#   fn <- function(x) {
#     for(e in x) {
#       NULL
#     }
#     e
#   }
#   ag_fn <- autograph(fn)
#   rx <- 1:4
#   tx <- as_tensor(rx)
#
#   expect_equal(fn(rx), grab(ag_fn(tx)))
# })


test_that("for with tf Dataset", {

  ds <- tf$data$Dataset$from_tensor_slices(as_tensor(seq_len(5), "float32"))

  fn <- function(ds) {
    h <- 0
    for (e in ds)
      add(h) <- e
    h
  }
  ag_fn <- autograph(fn)

  expect_result(ag_fn, ds, 15)


  fn <- function(ds) {
    s1 <- 0
    s2 <- 0
    for (e in ds) {
      add(s1) <- e
      add(s2) <- e * e
    }
    list(s1, s2)
  }
  ag_fn <- autograph(fn)

  expect_result(ag_fn, ds, list(15, 55))

  `%<-%` <- zeallot::`%<-%`
  fn <- function(ds) {
    h1 <- h2 <- h3 <- 0
    for (b in ds) {
      c(x, y, w) %<-% b
      add(h1) <- x[1, 1]
      add(h2) <- y[1]
      add(h3) <- w
    }
    list(h1, h2, h3)
  }
  ag_fn <- autograph(fn)


  ds <- tf$data$Dataset$from_tensor_slices(tuple(
    tf_arr(6, 5, 5), tf_arr(6, 8), tf_arr(6)))

  expect_result(ag_fn, ds, list(21, 21, 21))

})
