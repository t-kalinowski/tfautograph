
test_that("exporting unnamed lists works", {

  x <- list()
  autograph(for(i in 1:10) x[[i]] <- i)
  expect_identical(x, as.list(1:10))

  x <- list()
  x$y <- list()
  autograph(for(i in 1:10) x$y[[i]] <- i)
  expect_identical(x$y, as.list(1:10))

  x <- list()
  x$y <- list()
  x$y$z <- list()
  autograph(for(i in 1:10) x$y$z[[i]] <- i)
  expect_identical(x$y$z, as.list(1:10))

})


test_that("exporting undefs works", {

  fn <- function(n) {
    x <- tf$constant(0L)
    autograph(for (i in tf$range(tf$cast(n, tf$int32))) {
      x <- x + i
    })
    x
  }

  tf_fn <- .tf_function(fn)
  expect_equal(as.vector(tf_fn(tf$constant(5L))), 10L)

})
