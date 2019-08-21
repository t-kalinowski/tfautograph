



as_outcome_fn <- function(expr, env, args = NULL) {
  if(is.null(expr))
    return(as.function(list(NULL), envir = env))

  expr <- substitute({
    tfautograph:::register_outcome_env()
    expr
  }, list(expr = expr))
  fn <- as.function.default(c(args, list(expr)), envir = env)

  function(...) {
    returned <- fn(...)
    outcome_env <- get_registered_outcome_env()

    modified <- as.list(outcome_env, all.names = TRUE)
    modified <- prune_nested_unmodified(modified, env)

    out <- drop_empty(list(returned = returned, modified = modified))

    # splice out outcome_env from closures
    out <- rapply(list(out), function(x) {
      if (identical(environment(x), outcome_env))
        environment(x) <- env
      x
    }, classes = "function", how = "replace")[[1]]

    out
  }
}


export_modified <- function(modified, env) {
  if (is_empty(modified))
    return()

  for (nm in names(modified)) {
    if (is.list(modified[[nm]]) && exists(nm, envir = env))
      modified[[nm]] <- modifyList(get(nm, env), modified[[nm]])
    else if (is_undef(modified[[nm]])) {
      makeActiveBinding(nm, modified[[nm]], env)
      modified[[nm]] <- NULL
    }
  }
  if(length(modified))
    list2env(modified, envir = env)
}



prune_nested_unmodified <- function(modified, env) {
  for (nm in names(modified)) {

    obj <- modified[[nm]]
    if(is_undef(obj) || is_undef(nm, env) || !is_named_list(obj))
      next
    orig <- get0(nm, env)
    pruned_obj <- prune_identical(obj, orig)[[1]]
    modified[[nm]] <- pruned_obj
  }
  drop_empty(modified)
}


prune_identical <- function(x, y) {
  if(anyDuplicated(names(x)) || anyDuplicated(names(y)))
    stop("names can't be duplicated")

  # TODO: need to handle unnamed lists better both in the case of recursing past
  # them and calling prune_identical on unnamed list elements, as well as being
  # able to prune unnamed elements. A hacky unsatisfactory fix is to autoname
  # them with some cryptic name, like .__ag_autoname_id_01 then unnaming them
  # later.

  for (nm in intersect(names(x), names(y))) {
    if (identical(x[[nm]], y[[nm]]))
      x[[nm]] <-  y[[nm]] <- NULL
    else if (is.list(x[[nm]]) && is.list(y[[nm]])) {
      res <- prune_identical(x[[nm]], y[[nm]])
      x[[nm]] <- res[[1]]
      y[[nm]] <- res[[2]]
    }
  }
  list(x, y)
}

