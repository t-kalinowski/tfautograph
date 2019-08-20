


#     register_control_dependency_ops(lapply(assert_ops, function(op) op$op), env)
#
#     on.exit.elsewhere({
#       if (!is.null(
#         new_return_value <-
#         tfautograph:::capture_registered_control_dependency_ops(returnValue())
#       ))
#         return(new_return_value)
#     }, envir = env)


ag_on.exit <- function(expr = NULL, add = TRUE, after = FALSE) {
  env <- parent.frame()
  if(!add && !is.null(rlang::eval_bare(quote(sys.on.exit())), env))
    stop("autograph() requires that `add = TRUE` be specified in any `on.exit` calls.
    This is because be autographing functions that register control dependencies
    like `stopifnot()` use the `on.exit()` mechanism to ensure those control dependencies
    are properly captured. If `add = FALSE` then those on.exit hooks would otherwise be overwritten")
  cl <- as.call(c(quote(base::on.exit), substitute(expr),
                  add = add, after = after))
  rlang::eval_bare(cl, env)
}


on.exit.elsewhere <- function(expr = NULL, add = TRUE, after = TRUE, envir = parent.frame(2)) {
  cl <- as.call(c(quote(base::on.exit), substitute(expr),
                  add = add, after = after))
  rlang::eval_bare(cl, envir)
}


