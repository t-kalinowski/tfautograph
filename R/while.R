

#' @importFrom rlang new_function
#' @importFrom zeallot %<-%
ag_while <- function(cond, body) {
  cond <- substitute(cond)
  body <- substitute(body)
  env <- parent.frame()

  # TODO: rename consuming getters to `consume_*`, eg, `consume_next_ag_name()`

  # TODO: revisit this after `append<-` is handled. Dispatching to standard R
  # control flow may no always be the right choise.


  cond_tensor_types <- sym_tensor_types(cond, env)
  if(cond_tensor_types == "eager") {
    cond <- substitute(as.logical(cond), list(cond = cond))
    cond_tensor_types <- "none"
  }

  if (cond_tensor_types == "none")
    return(eval(as.call(list(quote(.Primitive("while")), cond, body)), env))

  can_break <- any(c("break", "return") %in% all.names(body, unique = TRUE))

  # TODO: consider tracing with as_concrete_fn() here for better inference of
  # loop_vars here. Downside is slight bloat of overall graph in tf v1, but in
  # tf v2 the traced graph will be able to be garbage collected. Worth tradeoff?
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
  if(tf_v2())
    while_loop_args$return_same_structure <- NULL

  res <- do.call(tf$while_loop, while_loop_args)

  loop_vars <- res[[1]]
  list2env(loop_vars, env)

  invisible()
}

get_existing_var_nms <- function(..., env) {
  # ... should all be language objects
  # TODO: this whould be refactored into a function named
  # resolve_loop_vars(..., env). also, ag_loop_vars should accept
  # dplyr::select like specs, namely `-`
  # e.g., ag_loop_vars(-log_file)
  vars <- unique(unlist(lapply(list(...), all.vars)))
  vars <- vars[vapply(vars, exists, TRUE, envir = env)]
  vars <- mget(vars, envir = env, inherits = TRUE)
  vars <- vars[vapply(vars, function(v) is_tensor(v) ||
                        typeof(v) %in% valid_typeofs, TRUE)]
  names(vars)
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


as_loop_body_fn <- function(body_expr, loop_vars, env,
                            dont_check = NULL,
                            call = sys.call(-1)) {
  force(call)

  loop_vars <- names(loop_vars) %||% loop_vars
  outcome_fn <- as_outcome_fn(body_expr, env, args = as_args(loop_vars))

  fn <- function(...) {
    loop_vars_in <- list(...)

    outcome <- outcome_fn(...)

    if (length(undefs <- setdiff(names(outcome$modified), loop_vars)))
      export_undefs(as.list(undefs), env, call)

    if (!tf$executing_eagerly())
      warn_about_unmodified(before = loop_vars_in,
                            after = outcome$modified[loop_vars],
                            dont_check = dont_check)

    outcome$modified[loop_vars]
  }

  fn <- wrap_fn_with_loop_control_flow_handlers(fn)
  fn
}

warn_about_unmodified <- function(before, after, dont_check) {
  unmodified <- vapply(
    setdiff(names(before), dont_check),
    function(nm)  identical(before[[nm]], after[[nm]]),
    FALSE)

  if(any(unmodified)) {
    unmod <- names(unmodified[unmodified])
    mod   <- names(unmodified[!unmodified])
    warning(sprintf("%s appear to be unnecessarily captured as a loop variable",
                    yasp::pc_and(yasp::wrap(unmod, "`"))),
            "\nSpecify loop vars with ag_loop_vars(). e.g.,\n",
            "ag_loop_vars(", yasp::pcc(yasp::dbl_quote(mod)), ")\n", call. = FALSE)
  }
}



wrap_fn_with_loop_control_flow_handlers <- function(body_fn) {
  force(body_fn)

  function(loop_vars, did_break = NULL) {

    loop_control_flow_registry <-
      establish_control_flow_registry(
        loop_vars = names(loop_vars),
        can_break = !is.null(did_break),
        graph = tf$compat$v1$get_default_graph()
      )

    establish_cond_registry()

    on.exit({
      remove_control_flow_registry()
      remove_cond_registry()
    }, add = TRUE)

    loop_vars <- do.call(body_fn, loop_vars)

    out <- drop_empty(list(loop_vars = loop_vars, did_break = did_break))

    while (length(loop_control_flow_registry$recorded_conditions)) {
      lcf <- compact_lcf(loop_control_flow_registry$recorded_conditions$pop())

      out <- tf$cond(
        lcf$reduced_conds,
        function() drop_empty(list(loop_vars = lcf$loop_vars,
                                   did_break = lcf$is_break)),
        function() out)
    }

    unname(drop_empty(out[c('loop_vars', 'did_break')]))
  }
}

