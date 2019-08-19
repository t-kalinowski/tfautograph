



# TODO: replace as_outcome_fn with as_outcome_fn2

as_outcome_fn <- function(expr, env) {
  expr <- substitute({
    tfautograph:::register_outcome_env()
    expr
  }, list(expr = expr))
  fn <- as.function.default(list(expr), envir = env)
  function() list(returned = fn(), env = get_registered_outcome_env())
}



as_outcome_fn2 <- function(expr, env) {
  if(is.null(expr))
    return(as.function(list(NULL), envir = env))
  expr <- substitute({
    tfautograph:::register_outcome_env()
    expr
  }, list(expr = expr))
  fn <- as.function.default(list(expr), envir = env)
  function() {
    ret <- fn()
    modified <- as.list(get_registered_outcome_env(), all.names = TRUE)
    modified <- prune_nested_unmodified(modified, env)

    drop_empty(list(returned = ret, modified = modified))
  }
}
