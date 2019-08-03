


ag_stopifnot <- function(..., exprs, local = TRUE) {
  if(missing(exprs)) {
    dots <- eval(substitute(alist(...)))
    env <- parent.frame()
  } else {
    dots <- exprs
    env <- if (isTRUE(local))
      parent.frame()
    else if (is.environment(local))
      local
    else
      stop("`local` must be TRUE or an environment")
  }


  # TODO: how to pass in custom messages?
  call_stack <- pretty_call_stack()

  dots_vars <- lapply(dots, all.vars)

  all_vars <- mget(unique(unlist(dots_vars)), envir = env, inherits = TRUE)
  all_vars <- all_vars[vapply(all_vars, is_tensor, TRUE)]
  all_var_nms <- names(all_vars)

  assert_ops <- drop_empty(lapply(seq_along(dots), function(i) {
    expr    <- dots[[i]]
    val     <- eval(expr, envir = env)

    if (!is_tensor(val)) {
      if (is.logical(val) && !anyNA(val) && all(val))
        return(NULL) # not need to evaluate expr twice if not error
      else
        # route through the same error signaling path if not all true
        return(eval(substitute(base::stopifnot(expr), list(expr = expr)), envir = env))
    }

    var_nms <- dots_vars[[i]]
    vars    <- all_vars[var_nms]
    data    <- pretty_tf_assert_data(expr, vars, call_stack)
    op      <- tf$Assert(val, data)
    list(op = op, var_nms = var_nms)
  }))

  if (length(assert_ops)) {
    ops <- lapply(assert_ops, function(op) op$op)
    control_dependencies_context <- tf$control_dependencies(ops)

    control_dependencies_context$`__enter__`()

    vars_w_ctrl_deps <- lapply(all_vars, tf$identity)
    list2env(vars_w_ctrl_deps, envir = env)

    if (identical(env, topenv(env))) {
      # is this even ever going to be TRUE, isn't this always going to be run
      # either from an autographed function or an as_outcome_fn()?
      control_dependencies_context$`__exit__`(NULL, NULL, NULL)
    } else {
      register_frame_context(control_dependencies_context, env)
      on.exit.elsewhere(return(
        tfautograph:::identity_op_tensors_and_close_contexts(returnValue())
      ), add = TRUE, after = TRUE, envir = env)
    }

  }

  invisible()
}

identity_op_tensors_and_close_contexts <- function(value) {
  on.exit(close_and_clear_registered_contexts(parent.frame()))
  robust_tf_identity(value)
}

robust_tf_identity <- function(x) {
  rapply(list(x), tf$identity, classes = "tensorflow.tensor", how = "replace")[[1]]
}


pretty_call_stack <- function() {
  calls <- rev(sys.calls())[-1]
  calls <- vapply(calls, deparse, "", nlines = 1L, width.cutoff = 500L)
  calls <- sprintf("<R call %i>: %s", seq_along(calls), calls)
  calls <- c("R call stack:", calls)
  as.list(calls)
}

pretty_tf_assert_data <- function(expr, vars, call_stack = NULL) {
  expr <- list(deparse(expr, width.cutoff = 500))
  data <- rbind(as.list(paste(names(vars), "value:")),
                unname(vars),
                deparse.level = 0)
  dim(data) <- NULL
  drop_empty(c(expr, data, call_stack))
}

