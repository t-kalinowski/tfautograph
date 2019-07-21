

#' @importFrom rlang new_function
#' @importFrom zeallot %<-%
ag_while <- function(cond, body) {
  cond <- substitute(cond)
  body <- substitute(body)
  env <- parent.frame()

  # TODO: revisit this after `append<-` is handled. Dispatching to standard R
  # control flow may no always be the right choise.
  if (!any_tensors_in(cond, env))
    return(eval(as.call(list(quote(.Primitive("while")), cond, body)), env))

  can_break <- any(c("break", "return") %in% all.names(body, unique = TRUE))

  loop_vars <- get_existing_var_nms(cond, body, env = env)

  cond_fn <- as_loop_cond_fn(cond, loop_vars, env)
  body_fn <- as_loop_body_fn(body, loop_vars, env)

  loop_vars <- dict(mget(loop_vars, envir = env, inherits = TRUE))

  if (can_break) {
    did_break <- FALSE
    loop_vars <- tuple(loop_vars, did_break)
  } else
    loop_vars <- tuple(loop_vars)

  # browser()
  res <- tf$while_loop(
    cond = cond_fn,
    body = body_fn,
    loop_vars = loop_vars,
    return_same_structure = TRUE
  )

  loop_vars <- res[[1]]
  list2env(loop_vars, env)

  invisible()
}

get_existing_var_nms <- function(..., env) {
  # ... should all be language objects
  vars <- unique(unlist(lapply(list(...), all.vars)))
  vars[vapply(vars, exists, TRUE, envir = env)]
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

  fn <- new_function(as_args(args), quote({
    exec_env <- new.env(parent = env)
    list2env(mget(args), envir = exec_env)
    eval(body_expr, exec_env)

    if(length(undefs <- setdiff(names(exec_env), args)))
      make_active_undefs(undefs, env, call)

    mget(args, exec_env)
  }))

  fn <- wrap_fn_with_loop_control_flow_handlers(fn)

  fn
}


wrap_fn_with_loop_control_flow_handlers <- function(body_fn) {
  force(body_fn)


  function(loop_vars, did_break = NULL) {
    establish_cond_registry()
    on.exit(remove_cond_registry())

    uncaught_loop_control_flow_registry <- Stack()

    withCallingHandlers(
      loop_vars <- do.call(body_fn, loop_vars),

      uncaught_loop_control_flow = function(lcf) {
        uncaught_loop_control_flow_registry$push(
          list(
            is_break = class(lcf)[1] == "break",
            loop_vars = mget(names(loop_vars), lcf$env, inherits = TRUE),
            registered_conds = reduce_registered_conds()
          )
        )
      }
    )

    out <- drop_empty(list(loop_vars, did_break))

    while (length(uncaught_loop_control_flow_registry)) {
      lcf <- uncaught_loop_control_flow_registry$pop()

      if (is.null(did_break))
        lcf$is_break <- NULL

      # browser()
      out <- tf$cond(
        lcf$registered_conds,
        function() drop_empty(list(lcf$loop_vars, lcf$is_break)),
        function() out,
        strict = TRUE)
    }

    out
  }
}
