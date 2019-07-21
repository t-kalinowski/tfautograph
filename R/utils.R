


is_tensor <- function(x) inherits(x, "tensorflow.tensor")

is_TRUE_or_FALSE <- function(x) isTRUE(x) || isFALSE(x)


as_args <- function(x) {
  out <- rep(list(quote(expr = )), length(x))
  names(out) <- x
  out
}


any_tensors_in <- function(expr, env) {
  var_nms <- all.vars(expr)
  vals <- mget(var_nms, envir = env, inherits = TRUE, ifnotfound = undefined_mold(var_nms))
  for(val in vals)
    if(is_tensor(val))
      return(TRUE)
  FALSE
}


# drop_empty <- function(x) x[(lx <- lengths(x)) != 0 | is.na(lx)]
drop_empty <- function(x) x[!vapply(x, function(x) identical(length(x), 0L), FALSE)]




#' @importFrom reticulate py_last_error py_clear_last_error
is_same_structure <- function(x, y) {
  tryCatch({
    tf$python$util$nest$assert_same_structure(x %||% list(), y %||% list())
    # no exceptions raised with NULLs
    TRUE
  },
  error = function(e) {
    py_e <- py_last_error()
    if (py_e$type == "ValueError" &&
        grepl("The two structures don't have the same nested structure.",
              py_e$value, fixed = TRUE)) {
      py_clear_last_error()
      FALSE
    } else
      stop(e)
  })
}
