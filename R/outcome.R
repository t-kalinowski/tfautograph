


as_outcome_fn <- function(expr, env) {
  if(is.null(expr))
    return(as.function(list(NULL), envir = env))

  expr <- substitute({
    tfautograph:::register_outcome_env()
    expr
  }, list(expr = expr))
  fn <- as.function.default(list(expr), envir = env)

  function() {
    returned <- fn()
    outcome_env <- get_registered_outcome_env()

    modified <- as.list(outcome_env, all.names = TRUE)
    modified <- prune_nested_unmodified(modified, env)

    for (val in modified)
      if (identical(environment(val), outcome_env))
        environment(val) <- env

    if (identical(environment(returned), outcome_env))
        environment(returned) <- env

    drop_empty(list(returned = returned, modified = modified))
  }
}
