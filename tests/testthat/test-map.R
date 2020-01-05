

test_that("tf_map", {
  skip_if_no_tensorflow()
  expect_equal(grab(tf_map(as_tensor(1:5), ~.x+1L)), array(2:6))
  expect_equal(grab(tf_map(as_tensor(1:5), ~ .+1L)), array(2:6))
  fn <- function(a) a+1L
  expect_equal(grab(tf_map(as_tensor(1:5),  fn )), array(2:6))
  expect_equal(grab(tf_map(as_tensor(1:5), "fn")), array(2:6))
})
