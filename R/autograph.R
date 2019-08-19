

#' @importFrom tensorflow tf
#' @export
autograph <- function(x) {
  x <- substitute(x)
  env <- parent.frame()

  if (is.symbol(x)) {
    # function or something with `environment<-` method

    x <- get(deparse(x), envir = env)
    environment(x) <- new_ag_mask(parent = environment(x))
    return(x)
  }

  # in line expression
  ag_mask <- new_ag_mask(parent = env)
  fn <- as_outcome_fn(x, ag_mask)
  outcome <- fn()

  # reset env of locally defined functions
  for (nm in names(outcome$env))
    if (!bindingIsActive(nm, outcome$env) &&
        identical(environment(outcome$env[[nm]]), outcome$env))
      environment(outcome$env[[nm]]) <- ag_mask

  transfer_env(from = outcome$env, to = env)

  # reset env of anonymous returned functions
  if (identical(environment(outcome$returned), outcome$env))
    environment(outcome$returned) <- ag_mask

  outcome$returned
}




new_ag_mask <- function(parent) {

  ag_mask <- list(
    `if`        = ag_if,
    `while`     = ag_while,
    `for`       = ag_for,
    `break`     = ag_break,
    `next`      = ag_next,
    `stopifnot` = ag_stopifnot,
    `switch`    = ag_switch,
    `on.exit`   = ag_on.exit
  )

  ag_mask <- list2env(ag_mask, parent = parent)

  attr(ag_mask, "name") <-
    sprintf("package:tfautograph:ag_mask\n parent: %s", format(parent))
  # the base R environment print functions are hardcoded to only print the
  # environment name if the name starts with "package:"
  # relevant functions:
  # R_IsPackageEnv
  # https://github.com/wch/r-source/blob/f4e6da5bea5a95fc6403160a5a04f42925990148/src/main/envir.c#L3520

  # PrintEnvironment
  # https://github.com/wch/r-source/blob/5f0affa2c7016e054f3eb4b64e247d428a6477dd/src/main/inspect.c#L42

  # EncodeEnvironment
  # https://github.com/wch/r-source/blob/bc6e559c4940ed18e99ac2fd91d20f01ed186c72/src/main/printutils.c#L148

  lockEnvironment(ag_mask)
  ag_mask
}



transfer_env <- function(from, to) {
  if(!length(from))
    return()

  syms <- names(from)
  names(syms) <- syms
  are_active <- vapply(syms, function(sym) bindingIsActive(sym, from), FALSE)

  from_list <- as.list(from, all.names = TRUE)

  if (any(!are_active)) {
    not_active <- from_list[syms[!are_active]]
    list2env(not_active, to)
  }

  # TODO: active bindings functions may need to have their environement reset

  if(any(are_active)) {
    active <- from_list[ syms[are_active] ]
    for (sym in names(active))
      makeActiveBinding(sym, active[[sym]], to)
  }

  invisible()
}



