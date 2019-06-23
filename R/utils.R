


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
