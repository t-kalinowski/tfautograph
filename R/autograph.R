

#' @importFrom tensorflow tf
#' @export
autograph <- function(x) {
  x <- substitute(x)
  env <- parent.frame()

  ag_mask <- list(
    `if`        = ag_if,
    `while`     = ag_while,
    `for`       = ag_for,
    `break`     = ag_break,
    `next`      = ag_next,
    `stopifnot` = ag_stopifnot
  )

  if (is.symbol(x)) {
    # function or something with `environment<-` method
    x <- get(deparse(x), envir = env)
    environment(x) <- list2env(ag_mask, parent = environment(x))

  } else {
    # in line expression
    mask <- list2env(ag_mask, parent = env)
    eval_env <- new.env(parent = mask)
    x <- eval(x, eval_env)

    modified <- as.list(eval_env, all.names = TRUE)

    syms <- names(modified)
    names(syms) <- syms
    are_active <- vapply(syms, function(sym)
      bindingIsActive(sym, eval_env), FALSE)

    active     <- modified[ syms[ are_active] ]
    not_active <- modified[ syms[!are_active] ]
    if (length(not_active))
      list2env(not_active, env)

    for (sym in names(active))
      makeActiveBinding(sym, active[[sym]], env)

  }
  x
}
