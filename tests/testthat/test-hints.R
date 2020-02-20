

test_that("hints", {
  skip_if_no_tensorflow()

  ag_if_vars(foo)
  expect_identical(tfautograph:::next_if_vars$pop()$modified, list("foo"))

  ag_if_vars(foo$bar)
  expect_identical(tfautograph:::next_if_vars$pop()$modified, list(c("foo", "bar")))

  ag_if_vars(foo, bar$baz)
  expect_identical(tfautograph:::next_if_vars$pop()$modified, list("foo", c("bar", "baz")))
})
