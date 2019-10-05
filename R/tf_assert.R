
#' tf_assert
#'
#' A thin wrapper around `tf$Assert()` that automatically constructs an
#' informative error message (passed on to `data` argument), which includes the
#' expression passed to `condition`, the values of the symbols found in the
#' expression, as well as the full R call stack at the time the `tf$Assert()`
#' node is created.
#'
#' @param condition A boolean tensor
#' @param ... Additional elements passed on to `data`. (e.g, an informative
#'   error message as a string, additional tensor values that might be useful to
#'   have in the error message, etc.)
#' @param expr A language object, provided in case `condition` is already
#'   computed prior to the call
#' @param summarize Print this many entries of each tensor.
#' @param name  A name for this operation (optional).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- tf$constant(-1)
#' try(tf_assert(x > 0, "oopsies! x must be greater than 0"))
#' }
tf_assert <- function(condition, ...,
                      expr = substitute(condition),
                      summarize = NULL,
                      name = NULL) {

  vars <- all.vars(expr)
  vars <- mget(vars, parent.frame(), inherits = TRUE,
               ifnotfound = vector('list', length(vars)))
  vars <- Filter(is_valid_val, vars)

  call_stack <- pretty_call_stack()

  data <- pretty_tf_assert_data(expr, vars, call_stack, ...)

  tf$Assert(condition, data, summarize, name)
}



pretty_call_stack <- function(n=1) {
  calls <- rev(sys.calls())[-seq_len(n)]
  calls <- vapply(calls, deparse, "", nlines = 1L, width.cutoff = 500L)
  calls <- sprintf("<R call %i>: %s", seq_along(calls), calls)
  calls <- c("<R call stack>:", calls)
  as.list(calls)
}

pretty_tf_assert_data <- function(expr, vars, call_stack = NULL, ...) {
  expr <- deparse(expr, width.cutoff = 500)
  if(length(expr) > 1)
    expr <- paste0(expr[1], "...TRUNCATED")
  expr <- sprintf("<Assert condition R expression>: `%s`", expr)
  data <- rbind(as.list(sprintf("`%s` value:", names(vars))),
                unname(vars),
                deparse.level = 0)
  dim(data) <- NULL
  drop_empty(c(expr, ..., data, call_stack))
}


