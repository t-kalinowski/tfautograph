

# ## TODO: use this as a template to test tf_switch()
# test_that("switch works", {
#
#   fn <- function(x) {
#     switch(x, 0, 1, 2)
#   }
#   ag_fn <- autograph(fn)
#
#   ag <- autograph
#
#   for(i in 0:2) {
#     ti <- as_tensor(i)
#     expect_equal( grab(ag(switch(ti, 0, 1, 2))), i)
#     expect_equal(grab(ag_fn(ti)), i)
#   }
#
#   ag_fn <- autograph(function(x) {
#     switch(
#       x,
#       `2` = x + 10L,
#       `0` = x,
#       `1` = x,
#       default = 99L
#     )
#   })
#
#   expect_equal( grab(ag_fn(as_tensor(2L))), 12L)
#   expect_equal( grab(ag_fn(as_tensor(0L))), 0L)
#   expect_equal( grab(ag_fn(as_tensor(-1L))), 99L)
#
# })
