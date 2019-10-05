
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
