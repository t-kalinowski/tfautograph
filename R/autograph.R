

#' @importFrom tensorflow tf
#' @importFrom rlang env_bury
autograph <- function(fn) {
  fn <- env_bury(fn, `if` = tf_if, `while` = tf_while, `for` = tf_for)
  fn
}
