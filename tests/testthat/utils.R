Sys.setenv(TF_CPP_MIN_LOG_LEVEL = 2)
Sys.setenv(CUDA_VISIBLE_DEVICES = 0)
Sys.setenv(TF_XLA_FLAGS='--tf_xla_cpu_global_jit')
library(testthat)

tf <- tensorflow::tf
tuple <- reticulate::tuple

as_tensor <- function(x, ...) tf$convert_to_tensor(x, ...)

.SESS <- NULL
grab <- function(x) {
  if(is.null(.SESS))
    .SESS <<- tf$compat$v1$Session()
  .SESS$run(x)
}


expect_result <- function(fun, inputs, expected) {
  if(!is.list(inputs))
    inputs <- list(inputs)
  tensor_result <- do.call(fun, inputs)
  result <- grab(tensor_result)
  expect_equal(result, expected)
}

expect_grabbed_result_equal <- function(tensor, value)
  expect_equal(grab(tensor), value)

seq_len0 <- function(x) if(x == 0L) integer() else 0L:(x - 1L)

`add<-`      <- function(x, value) x + value
`subtract<-` <- function(x, value) x - value
`multiply<-` <- function(x, value) x * value
`divide<-`   <- function(x, value) x / value

expect_ag_equivalent <- function(fn, input) {
  if (!is.list(input))
    input <- list(input)
  ag_fn <- autograph(fn)
  ag_input <- lapply(input, as_tensor)
  expect_equal(grab(do.call(ag_fn, ag_input)),
               do.call(fn, input))
}


np_arr <- function(...) reticulate::np_array(array(seq_len(prod(...)), c(...)), "float32")

tf_arr <- function(...) tf$convert_to_tensor(np_arr(...))
