

do_return <- function(env, value = NULL) {
  eval(as.call(list(quote(.Primitive("return")), value)), env)
}

uncaught_loop_control_flow_condition <- function(type, env) {
  structure(
    class = c(type, "uncaught_loop_control_flow", "condition"),
    list(message = "", call = sys.call(-1), env = env)
  )
}

mold_ag_control_flow <- function(type) {
  function(env = parent.frame()) {
    tryCatch(
      eval(
        as.call(list(as.call(list(quote(.Primitive), type)))),
        env),
      error = function(e) {
        signalCondition(uncaught_loop_control_flow_condition(type, env))
        do_return(env)
      }
    )
  }
}

ag_break <- mold_ag_control_flow("break")
ag_next  <- mold_ag_control_flow("next")

rm(mold_ag_control_flow)

