

#' @importFrom rlang new_function
#' @importFrom zeallot %<-%
ag_while <- function(cond, body) {
  cond <- substitute(cond)
  body <- substitute(body)
  env <- parent.frame()

  # TODO: rename consuming getters to `consume_*`, eg, `consume_next_ag_name()`

  # TODO: revisit this after `append<-` is handled. Dispatching to standard R
  # control flow may no always be the right choise.
  if (!any_tensors_in(cond, env))
    return(eval(as.call(list(quote(.Primitive("while")), cond, body)), env))

  can_break <- any(c("break", "return") %in% all.names(body, unique = TRUE))

  loop_vars <-
    get_registered_next_while_loop_vars() %||%
    get_existing_var_nms(cond, body, env = env)


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
    name = get_next_ag_name(),
    get_registered_next_while_loop_opts()
  )

  res <- do.call(tf$while_loop, while_loop_args)

  loop_vars <- res[[1]]
  list2env(loop_vars, env)

  invisible()
}

get_existing_var_nms <- function(..., env) {
  # ... should all be language objects
  vars <- unique(unlist(lapply(list(...), all.vars)))
  vars[vapply(vars, exists, TRUE, envir = env)]
}


get_tensor_var_nms <- function(..., env) {
  existing <- get_existing_var_nms(..., env = env)
  existing[vapply(mget(existing, envir = env, inherits = TRUE),
                  is_tensor, TRUE)]
}



as_loop_cond_fn <- function(cond_expr, loop_vars, env) {

  .cond_fn <- new_function(as_args(loop_vars), cond_expr, env)

  function(loop_vars, did_break = NULL) {
    continue <- do.call(.cond_fn, loop_vars)
    if (!is.null(did_break))
      continue <- !did_break & continue

    continue
  }
}

as_loop_body_fn <- function(body_expr, loop_vars, env, call = sys.call(-1)) {
  force(call)
  args <- names(loop_vars) %||% loop_vars

  # TODO: use as_outcome_fn() mechanism here ?
  fn <- new_function(as_args(args), quote({
    exec_env <- new.env(parent = env)
    list2env(mget(args), envir = exec_env)
    eval(body_expr, exec_env)

    if(length(undefs <- setdiff(names(exec_env), args)))
      export_undefs(as.list(undefs), env, call)

    mget(args, exec_env)
  }))

  fn <- wrap_fn_with_loop_control_flow_handlers(fn)

  fn
}


wrap_fn_with_loop_control_flow_handlers <- function(body_fn) {
  force(body_fn)

  function(loop_vars, did_break = NULL) {

    establish_cond_registry()

    loop_control_flow_registry <-
      establish_control_flow_registry(
        loop_vars = names(loop_vars),
        can_break = !is.null(did_break),
        graph = tf$compat$v1$get_default_graph()
      )
    on.exit({
      remove_control_flow_registry()
      remove_cond_registry()
    }, add = TRUE)


    loop_vars <- do.call(body_fn, loop_vars)

    #     lapply(names(loop_vars), function(nm) {
    #       if(identical(loop_vars[nm], loop_vars_in[nm]))
    #         warning("specify loop vars with ag_loop_vars().
    #                 %s appears to be an necessarily captured as a loop_var")
    #     })

    out <- drop_empty(list(loop_vars = loop_vars, did_break = did_break))

    while (length(loop_control_flow_registry$recorded_conditions)) {
      lcf <- compact_lcf(loop_control_flow_registry$recorded_conditions$pop())

      out <- tf$cond(
        lcf$reduced_conds,
        function() drop_empty(list(loop_vars = lcf$loop_vars,
                                   did_break = lcf$is_break)),
        function() out,
        strict = TRUE)
    }

    unname(drop_empty(out[c('loop_vars', 'did_break')]))
  }
}

