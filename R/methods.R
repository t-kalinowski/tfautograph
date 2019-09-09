


# ta <- tf$TensorArray(tf$float32, size = 1L, dynamic_size = TRUE)

#' @export
`[[<-.tensorflow.python.ops.tensor_array_ops.TensorArray` <-
  function(ta, i, ..., value,  name = NULL) {
    if(...length())
      stop("TensorArrays can only be written to along the first dimension (Think of them as a list())")
    ta$write(tf$cast(i, tf$int32), value, name = name)
  }

`[<-.tensorflow.python.ops.tensor_array_ops.TensorArray` <-
  function(ta, i, value, ..., name = NULL) {
    ta$write(tf$cast(i, tf$int32), value, name = name)
  }


# train_losses     %<>% {.$write(i, step_train_loss)}
# test_losses      %<>% {.$write(i, step_test_loss)}
# train_accuracies %<>% {.$write(i, step_train_accuracy)}
# test_accuracies  %<>% {.$write(i, step_test_accuracy)}

#' @importFrom tensorflow tf_function
#' @export
tensorflow::tf_function





as.function.formula <- function(x) {
  envir <- environment(x)
  body <- list(x[[length(x)]])
  if(length(x) == 3L) {
    args <- x[[2]]
    if(is.call(args))
      args[[1]] <- NULL
  } else
    args <- NULL

  as.function.default(c(args, body), envir = envir)
}

