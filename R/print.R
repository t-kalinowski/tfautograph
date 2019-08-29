
ag_print <- function(x) {
  env <- parent.frame()
  x_expr <- substitute(x)
  sym_nms <- all.vars(x_expr)
  op <- tf$print(x)
  new_sym_vals <-
    with(tf$control_dependencies(list(op)),
         lapply(mget(sym_nms, env), function(val) tf$identity(val)))

  list2env(new_sym_vals, env)
  # what if sym was pulled from some parent env? Is this still the best thing to
  # do? At least this way works with ag_if(), but it'll break relatively
  # esoteric uses of local()
}



