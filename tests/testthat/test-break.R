if(testthat::is_testing()){
  source("utils.R")
} else {
  source("tests/testthat/utils.R")
  devtools::load_all()
}

test_that("break basic", {
  fn <- function(n) {
    while (n > 0L) {
      if (n %% 2L == 0L) {
        break
      }
      subtract(n) <- 1L
    }
    n
  }
  # debugonce(ag_if)

  # autograph(fn)(as_tensor(4L))
  # autograph(fn)(as_tensor(1L))

  expect_ag_equivalent(fn, 0L)
  expect_ag_equivalent(fn, 4L)
  expect_ag_equivalent(fn, 5L)
})

test_that("break and next basic", {
  fn <- function(n) {
    x <- 0
    while (n > 0L) {
      subtract(n) <- 1L
      if (n %% 5L == 0L) {
        # print("hi")

        break
      } else {
        if (n %% 2L == 0L)
          next
      }
      add(x) <- 1
    }
    list(n, x)
  }

  # debugonce(ag_next)
  # autograph(fn)(as_tensor(3L))



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






test_that("next in for", {
  fn <- function(l) {
    x <- 0
    y <- 0
    z <- 0
    for (i in l) {
      add(x) <- 1L
      if (i %% 2L == 0L) {
        add(y) <- 1
        next
      }
      add(z) <- 1
    }
    list(x, y, z)
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn, experimental_relax_shapes = TRUE)

  for (n in 0:6) {
    l <- array(seq_len(n))
    expect_equal(fn(l), grab(ag_fn(as_tensor(l))))
    expect_equal(fn(l), grab(tf_ag_fn(as_tensor(l))))

    ds <- tf$data$Dataset$range(n)
    expect_equal(fn(seq_len0(n)), grab(ag_fn(ds)))
    expect_equal(fn(seq_len0(n)), grab(tf_ag_fn(ds)))
  }


  fn <- function(l) {
    o <- 0L
    for (e in l) {
      if (e %% 2L == 0L)
        next
      add(o) <- 1L
    }
    o
  }
  ag_fn <- autograph(fn)
  # tf_ag_fn <- tf_function(ag_fn)
  tf_ag_fn <- tf_function(ag_fn, experimental_relax_shapes = TRUE)

  for (n in 0:6) {
    l <- array(seq_len0(n))
    expect_equal(fn(l), grab(ag_fn(as_tensor(l))))
    expect_equal(fn(l), grab(tf_ag_fn(as_tensor(l))))

    # ds <- tf$data$Dataset$from_tensor_slices(as_tensor(l))
    ds <- tf$data$Dataset$range(n)
    expect_equal(fn(l), grab(ag_fn(ds)))
    expect_equal(fn(l), grab(tf_ag_fn(ds)))
  }
})



test_that("break and next in simple for", {
  fn <- function(l) {
    x <- 0
    y <- 0
    z <- 0
    for (i in l) {
      add(x) <- 1L
      if (i %% 5L == 0L)
        break
      else if (i %% 2L == 0L) {
        add(y) <- 1
        next
      }
      add(z) <- 1
    }
    list(x, y, z)
  }
  ag_fn <- autograph(fn)
  tf_ag_fn <- tf_function(ag_fn, experimental_relax_shapes = TRUE)


  for (n in 0:6) {
    l <- array(seq_len0(n))
    expect_equal(fn(l), grab(ag_fn(as_tensor(l))))
    expect_equal(fn(l), grab(tf_ag_fn(as_tensor(l))))

    # ds <- tf$data$Dataset$from_tensor_slices(as_tensor(l))
    ds <- tf$data$Dataset$range(n)
    expect_equal(fn(l), grab(ag_fn(ds)))
    expect_equal(fn(l), grab(tf_ag_fn(ds)))
  }

})

