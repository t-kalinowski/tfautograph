


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
    names(all_var_nms) <- all_var_nms
    vars_w_ctrl_deps <- lapply(all_var_nms, function(var_nm) {
      var <- all_vars[[var_nm]]
      var_ops <- drop_empty(lapply(assert_ops, function(op) {
        if (var_nm %in% op$var_nms)
          op$op
      }))
      with(tf$control_dependencies(var_ops), tf$identity(var))
    })
    list2env(vars_w_ctrl_deps, envir = env)

    # TODO, currently, this next block does effectively nothing when
    # autographing an inline expression. at least `on.exit` doesn't throw an
    # error because it registers in the extra frame introduced by base::eval,
    # and capture_registered_control_dependency_ops() still clears the registry
    # of ops for that frame, but nothing more is captured because there is
    # nothing meaningful returned by `returnValue()`. Ideally we should test if
    # we're in the context of an autographed function, and if not, then we should skip
    # registring the ops and skip capturing the ops in the returnValue(). The
    # difficulty is in figuring out an elegant way to test if we're executing in
    # an autographed function.

    register_control_dependency_ops(lapply(assert_ops, function(op) op$op), env)

    on.exit.elsewhere({
      if (!is.null(
        new_return_value <-
        tfautograph:::capture_registered_control_dependency_ops(returnValue())
      ))
        return(new_return_value)
    }, envir = env)

  }
  invisible()
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

