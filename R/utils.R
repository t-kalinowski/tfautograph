

is_true <- function(x) identical(x, TRUE)
is_bool <- function(x) identical(x, TRUE) || identical(x, FALSE)
is_tensor <- function(x) inherits(x, "tensorflow.tensor")
is_eager_tensor <- function(x) is_tensor(x) && py_has_attr(x, "numpy")
is_eager <- function(x) py_has_attr(x, "numpy")
# TODO: maybe check for actual class here? with base::inherits() or
  # z$`__class__`$`__name__` == "EagerTensor"
  #

is_tensorarray <- function(x)
  inherits(x, "tensorflow.python.ops.tensor_array_ops.TensorArray")


`%||%` <- function(x, y)  if (is.null(x)) y else x

is_lazy_tensor <- function(x) is_tensor(x) && !is_eager_tensor(x)

is_op <- function(x) inherits(x, "tensorflow.python.framework.ops.Operation")

# TODO: should also return true for TensorArrays

# in tf2.0
# > class(tf$convert_to_tensor(3))
# [1] "tensorflow.tensor"
# [2] "tensorflow.python.framework.ops.EagerTensor"
# [3] "tensorflow.python.framework.ops._EagerTensorBase"
# [4] "tensorflow.python.framework.ops.Tensor"
# [5] "tensorflow.python.framework.tensor_like._TensorLike"
# [6] "python.builtin.object"



as_args <- function(x) {
  out <- rep(list(quote(expr = )), length(x))
  names(out) <- x
  out
}


sym_tensor_types <- function(expr, env) {
  # outputs one of "lazy", "eager", "both", "none"
  var_nms <- all.vars(expr)
  vals <- mget(var_nms, envir = env, inherits = TRUE,
               ifnotfound = vector("list", length(var_nms)))
  lazy <- eager <- FALSE
  # TODO use rapply here instead of lapply to handle nested structures
  types <- unique(unlist(lapply(vals, tensor_type)))
  if(!length(types))
    "none"
  else if (all(c("lazy", "eager") %in% types))
    "both"
  else types
}



tensor_type <- function(x) {
  if(!is_tensor(x))
    return(NULL)
  if(is_eager(x)) "eager" else "lazy"
}


drop_empty <- function(x)
  x[!vapply(x, function(x) !is_tensor(x) && identical(length(x), 0L), FALSE)]

is_empty <- function(x) !is_tensor(x) && identical(length(x), 0L)


#' @importFrom reticulate py_last_error py_clear_last_error
is_same_structure <- function(x, y) {
  if(is_empty(x) || is_empty(y))
    return(FALSE)

  tryCatch({
    tf$nest$assert_same_structure(x, y)
    TRUE
  },
  error = function(e) {
    py_e <- py_last_error()
    if (py_e$type == "ValueError" &&
        grepl("The two structures don't have the same nested structure.",
              py_e$value,
              fixed = TRUE)) {
      py_clear_last_error()
      FALSE
    } else
      stop(e)
  })
}



null_fn <- function() NULL
class(null_fn) <- "null"
`$.null` <- function(...) NULL


unnamed_lists_to_tuples <- function(x) {
  if(is.list(x)) {
    x <- lapply(x, unnamed_lists_to_tuples)
    if(is.null(names(x)))
      x <- tuple(x)
  }
  x
}

valid_typeofs <- c("logical", "integer", "double", "complex", "character",
                   "raw", "list")


is_named_list <- function(x) is.list(x) && !is.null(names(x))



tf_group_ops <- function(x) {
  ops <- rapply(list(x), function(t) t$op, classes = "tensorflow.tensor", how = "list")
  op <- do.call(drop_empty(ops), tf$group)
  with(tf$control_dependencies(op), robust_tf_identity(x))
}


#' @importFrom tensorflow tf_version
tf_v2 <- function() package_version(tf_version()) >= "2"
tf_v1 <- function() package_version(tf_version()) < "2"


