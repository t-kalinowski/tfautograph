

is_true <- function(x) identical(x, TRUE)

is_tensor <- function(x) inherits(x, "tensorflow.tensor")

is_bool <- function(x) identical(x, TRUE) || identical(x, FALSE)

as_args <- function(x) {
  out <- rep(list(quote(expr = )), length(x))
  names(out) <- x
  out
}


any_tensors_in <- function(expr, env) {
  var_nms <- all.vars(expr)
  vals <- mget(var_nms, envir = env, inherits = TRUE,
               ifnotfound = vector("list", length(var_nms)))
  for(val in vals)
    if(is_tensor(val))
      return(TRUE)
  FALSE
}


# drop_empty <- function(x) x[(lx <- lengths(x)) != 0 | is.na(lx)]
drop_empty <- function(x)
  x[!vapply(x, function(x) !is_tensor(x) && identical(length(x), 0L), FALSE)]

is_empty <- function(x) !is_tensor(x) && identical(length(x), 0L)


#' @importFrom reticulate py_last_error py_clear_last_error
is_same_structure <- function(x, y) {
  if(is_empty(x) || is_empty(y))
    return(FALSE)

  tryCatch({
    tf$python$util$nest$assert_same_structure(x, y)
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
                   "raw", "list") #, "environment")


is_gettable <- function(nm, env) {
  # exists(nm, envir = env) &&
  x <- get0(nm, envir = env)
  is_tensor(x) || mode(x) %in% valid_modes
}

is_named_list <- function(x) {
  is.list(x) && !is.null(names(x))
}


tf_group_ops <- function(x) {
  ops <- rapply(list(x), function(t) t$op, classes = "tensorflow.tensor", how = "list")
  op <- do.call(drop_empty(ops), tf$group)
  with(tf$control_dependencies(op), robust_tf_identity(x))
}

