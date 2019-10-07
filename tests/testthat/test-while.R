
test_that("while basic", {
  skip_if_no_tensorflow()

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
  tf_ag_fn <- tf_function(ag_fn)
  # grab(ag_fn(as_tensor(5L)))

  expect_result(ag_fn, as_tensor(5L), list(10L, 5L, 5L))
  expect_result(tf_ag_fn, as_tensor(5L), list(10L, 5L, 5L))
})


test_that("while nested", {
  skip_if_no_tensorflow()

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
  tf_ag_fn <- tf_function(ag_fn)
  # res <- ag_fn(as_tensor(5L))
  # grab(res)

  expect_result(ag_fn, as_tensor(5L), list(25L, 5L, 0L, 5L))
  expect_result(tf_ag_fn, as_tensor(5L), list(25L, 5L, 0L, 5L))

})


test_that("while single output", {
  skip_if_no_tensorflow()

  fn <- function(n) {
    while (n > 0L)
      subtract(n) <- 1L
    n
  }

  # devtools::load_all()
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  # res <- ag_fn(as_tensor(5L))
  # grab(res)

  expect_result(ag_fn, as_tensor(5L), 0L)
  expect_result(tf_ag_fn, as_tensor(5L), 0L)
  expect_result(tf_ag_fn, as_tensor(5L), 0L)

})


test_that("while local composite", {
  skip_if_no_tensorflow()

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
  tf_ag_fn <- tf_function(ag_fn)

  expect_result(ag_fn, as_tensor(5L), 0L)
  expect_result(tf_ag_fn, as_tensor(5L), 0L)
})


test_that("while local composite complex nestable", {
  skip_if_no_tensorflow()

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
  tf_ag_fn <- tf_function(ag_fn)

  expect_result(ag_fn, as_tensor(5L), 4L)
  expect_result(tf_ag_fn, as_tensor(5L), 4L)
})


test_that("while local composite complex illegal", {
  skip_if_no_tensorflow()

  foo <- function() list(x = list(as_tensor(3L)))
  fn <- function(n) {
    while(n > 0L) {
      tc <- foo()
      tc$x[[1]] <- tc$x[[1]] + 1L
      subtract(n) <- 1L
      # print(tc); #print(n)
    }
    tc$x[[1]]
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  if(tf$executing_eagerly()) {
    # no undefineds produced in eager mode
    expect_equal(grab(ag_fn(as_tensor(5L))), 4L)
    ag_fn <- tf_function(ag_fn)
  }

  expect_error(ag_fn(as_tensor(5L)), '*tc*',
               class = if(!tf$executing_eagerly()) "access_undefined")
  expect_error(ag_fn(as_tensor(5L)),
               'object with the same dtype and shape be assigned to the symbol before the loop body',
               class = if(!tf$executing_eagerly()) "access_undefined")
})

test_that("while test while dispatches by cond only", {
  skip_if_no_tensorflow()

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
  tf_ag_fn <- tf_function(ag_fn)

  expect_equal(ag_fn(5L, TensorIncompatibleNumeric(0L)$val), 10L)
  expect_equal(grab(tf_ag_fn(5L, TensorIncompatibleNumeric(0L)$val)), 10L)
  # expect_result(ag_fn, list(5L,  TensorIncompatibleNumeric(0L)$val), 10L)
  expect_error(ag_fn(as_tensor(5L), TensorIncompatibleNumeric(0L)), "TypeError|ValueError")
  expect_error(tf_ag_fn(as_tensor(5L), TensorIncompatibleNumeric(0L)), "TypeError|ValueError")
})




test_that("while loop_vars hints", {
  skip_if_no_tensorflow()

  fn <- function(n) {
    b <- 1L
    while (n > 0L) {
      b <- b
      subtract(n) <- 1L
    }
    n
  }

  ag_fn <- autograph(fn)

  wrng <- "`b` appears to be unnecessarily captured as a loop variable"
  for (i in 1:2) {
    if(i == 2)
      ag_fn <- local({
        # force retracing every call
        ag_fn <- ag_fn
        function(...) tf_function(ag_fn)(...)
      })

    if(i == 1 && tf$executing_eagerly())
      next

    expect_warning(ag_fn(as_tensor(5L)), wrng)

    ag_loop_vars(-b)
    expect_silent(ag_fn(as_tensor(5L)))

    ag_loop_vars(n)
    expect_silent(ag_fn(as_tensor(5L)))

    expect_warning(ag_fn(as_tensor(5L)), wrng)


  }
})
