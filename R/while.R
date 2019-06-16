
#' @importFrom rlang new_function
tf_while <- function(cond, body) {
  cond <- substitute(cond)
  body <- substitute(body)
  env <- parent.frame()

  # TODO: force(cond) and dispatch to standard R control flow if cond does not evaluate to a tensor

  loop_vars <- unique(c(all.vars(cond), all.vars(body)))
  # TODO: take the undefined loop_vars and bury then in the body_fn function env
  # as active bindings, and if they are accessed throw a nicer error message for
  # test_while_local_composite_complex_illegal
  loop_vars <- loop_vars[vapply(loop_vars, exists, TRUE, envir = env)]

  cond_fn <- new_function(as_args(loop_vars), cond, env)

  body_w_ret <- substitute({
    body
    unname(mget(loop_vars))
  }, list(body = body, loop_vars = loop_vars))

  body_fn <- new_function(as_args(loop_vars), body_w_ret, env)

  res <- tf$while_loop(
    cond = cond_fn,
    body = body_fn,
    loop_vars = unname(mget(loop_vars, envir = env))
  )
  if(!is.list(res))
    res <- list(res)
  names(res) <- loop_vars
  list2env(res, env)

}

as_args <- function(x) {
  out <- rep(list(quote(expr = )), length(x))
  names(out) <- x
  out
}
