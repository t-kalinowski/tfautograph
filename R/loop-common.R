

statically_infer_loop_vars <- function(body, env, also_try_include = NULL) {
  syms <- unique(find_assiged_syms(body))
  syms <- union(syms, also_try_include)

  vars <- list()
  for (sym in syms)
    vars[[sym]] <- tryCatch(get0(sym, envir = env),
                            access_undefined = function(e) NULL)

  if(!length(vars)) return(character())
  vars <- vars[vapply(vars, function(v) is_tensor(v) ||
                        is_tensorarray(v) ||
                        typeof(v) %in% valid_typeofs, TRUE)]
  names(vars) %||% character()
}

find_assiged_syms <-  function(expr) {
  if (!is.call(expr))
    return()

  fn <- expr[[1]]
  expr[[1]] <- NULL

  if (fn == quote(`<-`) || fn == quote(`=`) || fn == quote(`%<>%`)) {
    nms <- all.vars(expr[[1]])[1]
    # take the first one in case the left hand side is a complex assignment
    # (e.g., from a [<-, $<-, or foo<- call)
    expr[[1]] <- NULL
  } else if (fn == quote(`%<-%`)) {
    nms <- all.vars(expr[[1]])
    expr[[1]] <- NULL
  } else if ( fn == quote(`%->%`)) {
    nms <- all.vars(expr[[2]])
    expr[[2]] <- NULL
  } else
    nms <- NULL

  # TODO: also handle case when fn == quote(assign)

  unlist(c(nms, lapply(expr, find_assiged_syms)), use.names = FALSE)
}




as_loop_cond_fn <- function(cond_expr, loop_vars, env) {

  .cond_fn <-
    as.function.default(c(as_args(loop_vars), cond_expr), envir = env)

  function(loop_vars, did_break = NULL) {
    continue <- do.call(.cond_fn, loop_vars)
    if (!is.null(did_break))
      continue <- !did_break & continue

    continue
  }
}


as_loop_body_fn <- function(body_expr, loop_vars, env,
                            dont_check = NULL,
                            additional_undefs = NULL,
                            call = sys.call(-1)) {
  force(call)

  loop_vars <- names(loop_vars) %||% loop_vars
  outcome_fn <- as_outcome_fn(body_expr, env, args = as_args(loop_vars))

  fn <- function(...) {
    loop_vars_in <- list(...)
    outcome <- outcome_fn(...)

    if (length(undefs <- c(setdiff(names(outcome$modified), loop_vars),
                           additional_undefs)))
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
    warning(sprintf("%s appears to be unnecessarily captured as a loop variable",
                    pc_and(sprintf("`%s`", unmod))),
            "\nSpecify loop vars with ag_loop_vars(). e.g.,\n",
            "ag_loop_vars(", paste(mod, collapse = ", "), ")\n",
            "or\n",
            sprintf("ag_loop_vars(%s)\n", paste0("-", unmod, collapse = ", ")),
            call. = FALSE)
  }
}



cond_registries         <- Stack()
control_flow_registries <- Stack()



wrap_fn_with_loop_control_flow_handlers <- function(body_fn) {
  force(body_fn)

  function(loop_vars, did_break = NULL) {

    loop_control_flow_registry <-
      new_control_flow_registry(
        loop_vars = names(loop_vars),
        can_break = !is.null(did_break),
        graph = tf$compat$v1$get_default_graph()
      )

    control_flow_registries$push(loop_control_flow_registry)
    cond_registries$push(new_cond_registry())

    on.exit({
      control_flow_registries$pop()
      cond_registries$pop()
    }, add = TRUE)

    loop_vars <- do.call(body_fn, loop_vars)

    out <- drop_empty(list(loop_vars = loop_vars, did_break = did_break))

    lcfs <- loop_control_flow_registry$recorded_conditions

    while (length(lcfs)) {
      lcf <- compact_lcf(lcfs$pop())
      out <- tf$cond(lcf$reduced_conds,
                     function() drop_empty(list(loop_vars = lcf$loop_vars,
                                                did_break = lcf$is_break)),
                     function() out)
    }

    unname(drop_empty(out[c('loop_vars', 'did_break')]))
  }
}
