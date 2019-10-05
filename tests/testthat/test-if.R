

# these tests are all modeled after:
# tensorflow/python/autograph/converters/control_flow_test.py

test_that("if non-tensor dispatches normally", {
  skip_if_no_tensorflow()

  fn <- function(n) {
    a <- 0L
    b <- 0L
    if (n > 0L)
      a <- -n
    else
      b <- 2L * n
    list(a, b)
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  expect_equal(ag_fn(1L), list(-1, 0))
  expect_equal(ag_fn(-1L), list(0, -2))
  # expect_equal(tf_ag_fn(1L), list(-1, 0))
  # expect_equal(tf_ag_fn(-1L), list(0, -2))
})


test_that("if basic", {
  skip_if_no_tensorflow()

  fn <- function(n) {
    a <- 0L
    b <- 0L
    if (n > 0L)
      a <- -n
    else
      b <- 2L * n
    list(a, b)
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  n <- as_tensor(1L)
  # res <- ag_fn(n)
  # grab(res)


  expect_result(ag_fn, as_tensor(1L), list(-1, 0))
  expect_result(ag_fn, as_tensor(-1L), list(0, -2))
  expect_result(tf_ag_fn, as_tensor(1L), list(-1, 0))
  expect_result(tf_ag_fn, as_tensor(-1L), list(0, -2))
})


test_that("if complex outputs", {
  skip_if_no_tensorflow()

  fn <- function(n, obj) {
    obj$a <- 0L
    obj$b <- 0L
    if (n > 0L)
      obj$a <- -n
    else
      obj$b <- 2L*n
    obj
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)


  foo <- function(a, b) list(a = a, b = b)

  res_obj <- ag_fn(as_tensor(1L), foo(0L, 0L))
  expect_equal(grab(res_obj), foo(-1L, 0L))

  res_obj <- tf_ag_fn(as_tensor(1L), foo(0L, 0L))
  expect_equal(grab(res_obj), foo(-1L, 0L))

  res_obj <- ag_fn(as_tensor(-1L),  foo(0L, 0L))
  expect_equal(grab(res_obj),  foo(0L,-2L))

  res_obj <- tf_ag_fn(as_tensor(-1L),  foo(0L, 0L))
  expect_equal(grab(res_obj),  foo(0L,-2L))
})


test_that("if single output", {
  skip_if_no_tensorflow()

  fn <- function(n) {
    if(n > 0L) {
      n <- -n
    }
    n
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  expect_result(ag_fn, as_tensor(1L), -1L)
  expect_result(tf_ag_fn, as_tensor(1L), -1L)
})


test_that("if single output - inline expr", {
  skip_if_no_tensorflow()

  n <- local({
    n <- as_tensor(1L)
    autograph({
      if (n > 0L)
        n <- -n
    })
    n
  })

  expect_equal(grab(n), -1L)

  n <- local({
    n <- as_tensor(-2L)
    autograph({
      if (n > 0L)
        n <- n*n
    })
    n
  })
  expect_equal(grab(n), -2L)

})





test_that("if single semi", {
  skip_if_no_tensorflow()

  fn <- function(n) {
    if(n > 0L)
      n <- 3L
    n
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  expect_result(ag_fn, as_tensor(2L), 3L)
  expect_result(ag_fn, as_tensor(-3L), -3L)
  expect_result(tf_ag_fn, as_tensor(2L), 3L)
  expect_result(tf_ag_fn, as_tensor(-3L), -3L)
})


test_that("if local var", {
  skip_if_no_tensorflow()

  fn <- function(n) {
    if(n > 0L) {
      b <- 4L
      n <- b + 1L
    }
    n
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  expect_result(ag_fn, as_tensor(1L), 5L)
  expect_result(ag_fn, as_tensor(-1L), -1L)
  expect_result(tf_ag_fn, as_tensor(1L), 5L)
  expect_result(tf_ag_fn, as_tensor(-1L), -1L)


  fn <- function(n) {
    if(n > 0L) {
      b <- 4L
      n <- b + 1L
    }
    b
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  # expect_error(ag_fn(as_tensor(1L)), "b")
  if(tf$executing_eagerly()) {
    # no undefineds produced in eager mode
    expect_equal(ag_fn(as_tensor(1L)), 4L)
    ag_fn <- tf_function(ag_fn)
  }
  expect_error(ag_fn(as_tensor(1L)), "Symbol `b` is \\*undefined\\*",
  class = if(!tf$executing_eagerly()) "access_undefined")


  if (!tf$executing_eagerly()) {
    local({
      n <- as_tensor(1L)

      autograph(if (n > 0L) {
        b <- 4L
        n <- b + 1L
      })

      expect_equal(grab(n), 5L)
      # expect_error(b, "b")
      expect_error(b, "Symbol `b` is \\*undefined\\*",
                   class = "access_undefined")
    })
  }
})


test_that("if no outputs", {
  skip_if_no_tensorflow()

  fn <- function(n) {
    if(n > 0L)
      b <- 4L
    n
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  expect_result(ag_fn, as_tensor(1L), 1L)
  expect_result(ag_fn, as_tensor(-1L), -1L)
  expect_result(tf_ag_fn, as_tensor(1L), 1L)
  expect_result(tf_ag_fn, as_tensor(-1L), -1L)
})


test_that("if unbalanced multiple composites", {
  skip_if_no_tensorflow()

  fn <- function(x, condition) {
    z = 5L
    if(condition) {
      x$b <- 7L
      x$c <- 11L
      z <- 13L
    }
    list(x$b, x$c, z)
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  foo <- function() list(b=2L, c=3L)

  expect_result(ag_fn, list(foo(), as_tensor(TRUE)), list(7L, 11L, 13L))
  expect_result(ag_fn, list(foo(), as_tensor(FALSE)), list(2L, 3L, 5L))
  expect_result(tf_ag_fn, list(foo(), as_tensor(TRUE)), list(7L, 11L, 13L))
  expect_result(tf_ag_fn, list(foo(), as_tensor(FALSE)), list(2L, 3L, 5L))
})


test_that("if unbalanced composite", {
  skip_if_no_tensorflow()

  fn <- function(x, condition) {
    z = 5L
    if(condition) {
      x$b <- 7L
      z <- 13L
    }
    list(x$b, z)
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  expect_result(ag_fn, list(list(b=2L), as_tensor(TRUE)), list(7L, 13L))
  expect_result(ag_fn, list(list(b=2L), as_tensor(FALSE)), list(2L, 5L))
  expect_result(tf_ag_fn, list(list(b=2L), as_tensor(TRUE)), list(7L, 13L))
  expect_result(tf_ag_fn, list(list(b=2L), as_tensor(FALSE)), list(2L, 5L))
})

test_that("if as last expression", {
  skip_if_no_tensorflow()
  fn <- function(n) {
    if (n > 0L)
      -n
    else
      2L * n
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  expect_result(ag_fn, as_tensor(1L), -1)
  expect_result(ag_fn, as_tensor(-1L), -2L)
  expect_result(tf_ag_fn, as_tensor(1L), -1)
  expect_result(tf_ag_fn, as_tensor(-1L), -2L)
})

test_that("nested if statement", {
  skip_if_no_tensorflow()
  fn <- function(n) {
    a <- 0L
    if (n > 0L) {
      if (n > 1L) {
        a <- -n
      } else {
        a <- 0L
      }
    } else
      a <- 2L * n

    a
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  expect_result(ag_fn, as_tensor(1L), 0)
  expect_result(ag_fn, as_tensor(2L), -2)
  expect_result(ag_fn, as_tensor(-1L), -2L)
  expect_result(tf_ag_fn, as_tensor(1L), 0)
  expect_result(tf_ag_fn, as_tensor(2L), -2)
  expect_result(tf_ag_fn, as_tensor(-1L), -2L)
})

test_that("can call from another functions", {
  skip_if_no_tensorflow()

  fn <- function(n) {
    a <- 0L
    b <- 0L
    if (n > 0L)
      a <- -n
    else
      b <- 2L * n
    list(a, b)
  }

  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn)

  g <- function(n) {
    ag_fn(n)
  }

  g_tf <- function(n) tf_ag_fn(n)

  tf_g <- tf_function(g)

  expect_result(g, as_tensor(1L), list(-1, 0))
  expect_result(g, as_tensor(-1L), list(0, -2))
  expect_result(tf_g, as_tensor(1L), list(-1, 0))
  expect_result(tf_g, as_tensor(-1L), list(0, -2))
  expect_result(g_tf, as_tensor(1L), list(-1, 0))
  expect_result(g_tf, as_tensor(-1L), list(0, -2))
})


# TODO: test if control dependencies are properly captured in an `ag_if`

# test_that("can early return", {
#   skip("early return not yet implemented")
#
#   fn <- function(n) {
#     if (n > 0)
#       return(1)
#
#     n + 1
#   }
#
#   fn <- autograph(fn)
#
#   expect_result(fn, as_tensor(1L), 1)
#   expect_result(fn, as_tensor(-1L), 0)
# })

