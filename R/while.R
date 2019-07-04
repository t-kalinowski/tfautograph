

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

  loop_vars <- unique(c(all.vars(cond), all.vars(body)))

  # TODO: take the undefined loop_vars and bury then in the body_fn function env
  # as active bindings, and if they are accessed throw a nicer error message for
  # test_while_local_composite_complex_illegal
  loop_vars <- loop_vars[vapply(loop_vars, exists, TRUE, envir = env)]

  cond_fn <- new_function(as_args(loop_vars), cond, env)
  body_fn <- as_loop_body_fn(body, loop_vars, env)

  loop_vars <- mget(loop_vars, envir = env, inherits = TRUE)


  tf_while <- if(can_break) while_with_break else while_no_break

  loop_vars <- tf_while(cond_fn, body_fn, loop_vars)

  list2env(loop_vars, env)

  invisible()
}

while_no_break <- function(cond_fn, body_fn, loop_vars) {
  res <- tf$while_loop(
    cond = cond_fn,
    body = function(...) tuple(body_fn(...)),
    loop_vars = tuple(loop_vars),
    return_same_structure = TRUE
  )
  names(res) <- names(loop_vars)
  res
}

while_with_break <- function(cond_fn, body_fn, loop_vars) {

  establish_cond_registry()
  on.exit(remove_cond_registry())

  .cond_fn <- cond_fn
  cond_fn <- function(did_break, loop_vars)
    !did_break & do.call(.cond_fn, loop_vars)

  .body_fn <- body_fn
  body_fn <- wrap_fn_with_loop_control_flow_handlers(.body_fn)

  did_break <- FALSE
  res <- tf$while_loop(
    cond = cond_fn,
    body = body_fn,
    loop_vars = list(did_break, loop_vars),
    return_same_structure = TRUE
  )

  res[[2L]]
}




as_loop_body_fn <- function(body_expr, loop_vars, env) {
  args <- as_args(names(loop_vars) %||% loop_vars)

  body_w_ret <- substitute({
    body_expr
    mget(loop_vars)
  }, list(body_expr = body_expr, loop_vars = loop_vars))

  new_function(args, body_w_ret, env)
}


wrap_fn_with_loop_control_flow_handlers <- function(body_fn) {
  force(body_fn)

  function(did_break, loop_vars) {
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

    while (length(uncaught_loop_control_flow_registry)) {
      lcf <- uncaught_loop_control_flow_registry$pop()
      c(did_break, loop_vars) %<-%
        tf$cond(lcf$registered_conds,
                function() list(lcf$is_break, lcf$loop_vars),
                function() list(FALSE, loop_vars))
    }
    list(did_break, loop_vars)
  }
}

