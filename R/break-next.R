

uncaught_loop_control_flow_condition <-
  function(type, env) {
    registry <- control_flow_registries$peek()
    structure(
      class = c(type, "uncaught_loop_control_flow", "error", "condition"),
      drop_empty(
        list(
          message = "",
          call = sys.call(-1),
          is_break = if (registry$can_break)
            type == "break",
          loop_vars = mget(registry$loop_vars, envir = env, inherits = TRUE),
          reduced_conds = reduce_registered_conds(), #get_registered_conds(),
          env = env
        )
      )
    )
  }



ag_break <- function() {
  env <- parent.frame()
  tryCatch(
    eval(quote(.Primitive("break")()), env),
    error = function(e) {
      try_register_or_signal_error_with_restart(
        uncaught_loop_control_flow_condition("break", env))
      do_return(env)
    }
  )
}

ag_next <- function() {
  env <- parent.frame()
  tryCatch(
    eval(quote(.Primitive("next")()), env),
    error = function(e) {
      try_register_or_signal_error_with_restart(
        uncaught_loop_control_flow_condition("next", env))
      do_return(env)
    }
  )
}


compact_lcf <- function(x)
  drop_empty(unclass(x)[c("loop_vars", "reduced_conds", "is_break")])



dummy_compact_lcf <- function(env) {
  registry <- control_flow_registries$peek()
  drop_empty(list(
    loop_vars = mget(registry$loop_vars, envir = env, inherits = TRUE),
    reduced_conds = FALSE,
    is_break = if (registry$can_break) FALSE
  ))
}


expand_lcf <-
  function(lcf, msg = "", call = sys.call(-1), env = NULL, type = NULL) {
    lcf$message  <-  msg
    lcf$call <- call
    lcf$env <- env
    structure(lcf, class = c(type, "uncaught_loop_control_flow",
                           "error", "condition"))
  }

#' @importFrom reticulate py_has_attr
can_register_loop_control_flow <- function(lcf) {
  if(tf$executing_eagerly())
    return(TRUE)

  registry <- control_flow_registries$peek()

  for (x in unlist(compact_lcf(lcf))) {
    if(!is_tensor(x))
      next
    if(py_has_attr(x, "numpy"))
      next
    if(x$graph != registry$graph)
      return(FALSE)
  }
  TRUE
}



register_loop_control_flow <- function(lcf) {
  # if(is_eager_tensor(lcf$reduced_conds))
  #   signalCondition()
  registry <- control_flow_registries$peek()
  registry$recorded_conditions$push(compact_lcf(lcf))
}



try_register_or_signal_error_with_restart <- function(lcf) {
  if (can_register_loop_control_flow(lcf))
    register_loop_control_flow(lcf)
  else {
    withRestarts(
      stop(expand_lcf(lcf)),
      continue = function() NULL
    )
  }
}


new_control_flow_registry <-
  function(loop_vars, can_break, graph = tf$compat$v1$get_default_graph()) {
  registry <- list2env(
    list(
      loop_vars = as.character(loop_vars),
      can_break = can_break,
      graph = graph,
      recorded_conditions = Stack()
    ),
    parent = emptyenv()
  )
  registry
}


# establish_control_flow_registry <-
#   function(loop_vars, can_break, graph = tf$compat$v1$get_default_graph()) {
#     reg <- list2env(
#       list(
#         loop_vars = as.character(loop_vars),
#         can_break = can_break,
#         graph = graph,
#         recorded_conditions = Stack()
#       ),
#       parent = emptyenv()
#     )
#
#     .registries$control_flow_registries$push(reg)
#   }



do_return <- function(env, value = NULL) {
  eval(as.call(list(quote(.Primitive("return")), value)), env)
}
