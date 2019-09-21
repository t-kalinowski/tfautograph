


# ta <- tf$TensorArray(tf$float32, size = 1L, dynamic_size = TRUE)

#' @export
`[[<-.tensorflow.python.ops.tensor_array_ops.TensorArray` <-
  function(ta, i, ..., name = NULL, value) {
    if(...length())
      stop("TensorArrays can only be written to along the first dimension (Think of them as a list())")
    ta$write(tf$cast(i, tf$int32), value, name = name)
  }


# train_losses     %<>% {.$write(i, step_train_loss)}
# test_losses      %<>% {.$write(i, step_test_loss)}
# train_accuracies %<>% {.$write(i, step_train_accuracy)}
# test_accuracies  %<>% {.$write(i, step_test_accuracy)}

#' @importFrom tensorflow tf_function
#' @export
tensorflow::tf_function


#' @importFrom tensorflow tf
#' @export
tensorflow::tf



as.function.formula <- function(x) {
  envir <- environment(x)
  body <- list(x[[length(x)]])
  if (length(x) == 3L) {
    args <- x[[2L]]
    if (is.call(args))
      args[[1L]] <- NULL
    if(!is.list(args))
      args <- pairlist(args)

    # args <- as.pairlist(args)
    nms <- rlang::names2(args)
    for (i in seq_along(args))
      if (nms[[i]] == "") {
        stopifnot(is.name(args[[i]]))
        nms[[i]] <- as.character(args[[i]])
        args[[i]] <- quote(expr =)
      }
    names(args) <- nms
  } else
    args <- NULL

  as.function.default(c(args, body), envir = envir)
}

