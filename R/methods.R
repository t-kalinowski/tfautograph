

#' `TensorArray.write()`
#'
#' @param ta a tensorflow `TensorArray`
#' @param i something castable to an int32 scalar tensor. 0-based.
#' @param ... Error if anything is passed to `...`
#' @param name A scalar string, name of the op
#' @param value The value to write.
#'
#' @export
#' @examples
#' \dontrun{
#' ta <- tf$TensorArray(tf$float32, size = 5L)
#' for(i in 0:4)
#'   ta[[i]] <- i
#' ta$stack()
#'
#' # You can use this to grow objects in graph mode
#' accuracies_log <- tf$TensorArray(tf$float32, size = 0L, dynamic_size=TRUE)
#' for(epoch in 0:4)
#'   accuracies_log[[epoch]] <- runif(1)
#' acc <- accuracies_log$stack()
#' acc
#' }
`[[<-.tensorflow.python.ops.tensor_array_ops.TensorArray` <-
  function(ta, i, ..., name = NULL, value) {
    if(...length())
      stop("TensorArrays can only be written to along the first dimension (Think of them as a list())")
    ta$write(tf$cast(i, tf$int32), value, name = name)
  }



#' @importFrom tensorflow tf_function
#' @export
tensorflow::tf_function


#' @importFrom tensorflow tf
#' @export
tensorflow::tf





as.function.formula <- function(x, envir = environment(x)) {
  if (length(x) == 3L)
    stop("Lambda functions provided as a formula cannot ",
         "have anything on the left hand side of the `~`")

  as.function.default(list(x[[2L]]), envir = envir)
}

as.function.character <- function(x, envir = parent.frame())
  get(x, envir = envir, mode = "function")


# as.function.formula <- function(x) {
#   envir <- environment(x)
#   body <- list(x[[length(x)]])
#   if (length(x) == 3L) {
#     args <- x[[2L]]
#     if (is.call(args))
#       args[[1L]] <- NULL
#     if(!is.list(args))
#       args <- pairlist(args)
#
#     # args <- as.pairlist(args)
#     nms <- rlang::names2(args)
#     for (i in seq_along(args))
#       if (nms[[i]] == "") {
#         stopifnot(is.name(args[[i]]))
#         nms[[i]] <- as.character(args[[i]])
#         args[[i]] <- quote(expr =)
#       }
#     names(args) <- nms
#   } else
#     args <- NULL
#
#   as.function.default(c(args, body), envir = envir)
# }
#
