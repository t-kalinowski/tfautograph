

ag_on.exit <- function(expr = NULL, add = TRUE, after = FALSE) {

  env <- parent.frame()
  if(!add &&
     !requireNamespace("rlang") ||
     !is.null(rlang::eval_bare(quote(sys.on.exit()), env)))
    stop("autograph() requires that `add = TRUE` be specified in this `on.exit` call.
    This is because autographing functions that register control dependencies
    like `stopifnot()` use the `on.exit()` mechanism to ensure those control dependencies
    are properly captured. If `add = FALSE` then those on.exit hooks would otherwise be overwritten")
  cl <- as.call(c(quote(base::on.exit), substitute(expr),
                  add = add, after = after))
  rlang::eval_bare(cl, env)
}


on.exit.elsewhere <- function(expr = NULL, add = TRUE, after = TRUE,
                              envir = parent.frame(2)) {
  if(!requireNamespace("rlang"))
    stop("Package `rlang` is required for tfautograph to capture control dependencies if you are in graph mode and outside a tf.function() context.")
  cl <- as.call(c(quote(base::on.exit), substitute(expr),
                  add = add, after = after))
  rlang::eval_bare(cl, envir)
}
