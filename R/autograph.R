

#' @importFrom tensorflow tf
#' @importFrom rlang env_bury
autograph <- function(fn) {
  env_bury(
    fn,
    `if`        = ag_if,
    `while`     = ag_while,
    `for`       = ag_for,
    `break`     = ag_break,
    `next`      = ag_next,
    `stopifnot` = ag_stopifnot
  )
}
