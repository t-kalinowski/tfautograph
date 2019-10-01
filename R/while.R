

ag_while <- function(cond, body) {
  cond <- substitute(cond)
  body <- substitute(body)
  env <- parent.frame()

  cond_tensor_types <- sym_tensor_types(cond, env)
  if(cond_tensor_types == "eager") {
    next_ag_name$pop()
    next_loop_vars$pop()
    next_while_loop_opts$pop()

    # cond <- substitute((cond)$`__bool__`, list(cond = cond)) ??
    cond <- substitute(as.logical(cond), list(cond = cond))
    cond_tensor_types <- "none"
  }

  if (cond_tensor_types == "none")
    return(invisible(eval(as.call(list(quote(.Primitive("while")), cond, body)), env)))

  can_break <- any(c("break", "return") %in% all.names(body, unique = TRUE))

  # TODO: consider tracing with as_concrete_fn() here for better inference of
  # loop_vars here. Downside is slight bloat of overall graph in tf v1, but in
  # tf v2 the traced graph will be able to be garbage collected. right?

  hint <- next_loop_vars$pop()

  loop_vars <-
    hint$list %||% statically_infer_loop_vars(body, env = env,
                                              also_try_include = hint$include)

  loop_vars <- setdiff(loop_vars, hint$exclude)

  # TODO: the loop vars selector should work the same as ag_if. it should handle
  # nested structures similarily, and the user-specificaiton function should
  # have the same mechanics.

  cond_fn <- as_loop_cond_fn(cond, loop_vars, env)
  body_fn <- as_loop_body_fn(body, loop_vars, env)

  loop_vars <- dict(mget(loop_vars, envir = env, inherits = TRUE))

  if (can_break) {
    did_break <- FALSE
    loop_vars <- tuple(loop_vars, did_break)
  } else
    loop_vars <- tuple(loop_vars)

  while_loop_args <- c(
    list(
      cond = cond_fn,
      body = body_fn,
      loop_vars = loop_vars,
      return_same_structure = TRUE
    ),
    name = next_ag_name$pop(),
    next_while_loop_opts$pop()
  )
  if(tf_v2())
    while_loop_args$return_same_structure <- NULL

  res <- do.call(tf$while_loop, while_loop_args)

  if (length(hint$undef))
    export_undefs(hint$undef)

  loop_vars <- res[[1]]
  list2env(loop_vars, env)

  invisible()
}


