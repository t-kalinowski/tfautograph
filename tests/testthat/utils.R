
library(testthat)

tf <- tensorflow::tf

as_tensor <- function(x, ...) tf$convert_to_tensor(x, ...)

.SESS <- NULL
grab <- function(x) {
  if(is.null(.SESS))
    .SESS <<- tf$Session()
  .SESS$run(x)
}


expect_result <- function(fun, inputs, expected) {
  if(!is.list(inputs))
    inputs <- list(inputs)
  tensor_result <- do.call(fun, inputs)
  result <- grab(tensor_result)
  expect_equal(result, expected)
}

seq_len0 <- function(x) 0L:(x - 1L)

`add<-`      <- function(x, value) x + value
`subtract<-` <- function(x, value) x - value
`multiply<-` <- function(x, value) x * value
`divide<-`   <- function(x, value) x / value
