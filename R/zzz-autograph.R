



ag_mask_list <- list(
  `if`        = ag_if,
  `while`     = ag_while,
  `for`       = ag_for,
  `break`     = ag_break,
  `next`      = ag_next,
  `stopifnot` = ag_stopifnot,
  `on.exit`   = ag_on.exit
)




#' Autograph R code
#'
#' Note, this documentation page is meant to serve as a technical reference, not
#' an introduction to `autograph`. For the latter, please visit the
#' documentation website: (https://t-kalinowski.github.io/tfautograph/) or see
#' the package vignettes.
#'
#' @param x a function supplied as a bare symbol, or an expression
#'
#' @return if `x` is a function, then the the same function with a new parent
#'   environment, `package:tfautograph:ag_mask`, which is the autograph mask
#'   that contains implementations of R control flow primitives that are capable
#'   of handling tensorflow tensors. The parent of the
#'   `package:tfautograph:ag_mask` in turn is the original environment of `x`.
#'
#'   if `x` is an expression, then that expression is evaluated in a special
#'   environment with the autograph mask `ag_mask` active. If the result of that
#'   expression included local assignment or modifications of variables, (for
#'   example, via `<-`), those modified variables are then exported into the
#'   current frame. The return value of the expression is then returned.
#' @export
autograph <- function(x) {
  xe <- substitute(x)
  env <- parent.frame()

  if (is.symbol(xe)) {
    # function, formula, or something with `environment<-` method
    environment(x) <- new_ag_mask(parent = environment(x))
    return(x)
  }

  # in line expression
  fn <- as_outcome_fn(xe, new_ag_mask(parent = env))
  outcome <- fn()

  export_modified(outcome$modified, env)

  if(isFALSE(outcome$visible) ||
     is.null(outcome$returned) && !tf$executing_eagerly())
    invisible(outcome$returned)
  else
    outcome$returned
}


new_ag_mask <- function(parent = parent.frame()) {

  ag_mask <- list2env(ag_mask_list, parent = parent)

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

  lockEnvironment(ag_mask, bindings = TRUE)
  ag_mask
}


is_autographed <- function(fn) {
  if (is.environment(e <- environment(fn)))
    while (!identical(e, emptyenv())) {
      nm <- attr(e, "name", TRUE)
      if (!is.null(nm) &&
          grepl("package:tfautograph:ag_mask", nm))
        return(TRUE)
      e <- parent.env(e)
    }
  FALSE
}

# usefull sometimes for interactive work. not exported, so it's an easter egg.
attach_ag_mask <- function(pos = 2L, warn.conflicts = TRUE)
  get("attach")(ag_mask_list, pos = pos, name = "tfautograph:ag_mask",
         warn.conflicts = warn.conflicts)



tf <- NULL
.onLoad <- function(libname, pkgname) {
  if(requireNamespace("tensorflow", quietly = TRUE) &&
     !".__DEVTOOLS__" %in% names(asNamespace("tensorflow")))
    tf <<- tensorflow::tf
  else {
    packageStartupMessage("'tfautograph' loaded withough R package 'tensorflow'")
    tf <<- reticulate::import("tensorflow", delay_load = list(
      on_load = function() {
        packageStartupMessage("Loaded Tensorflow version ", tf$version$VERSION)
      }
    ))
  }

  # if (requireNamespace("tensorflow", quietly = TRUE)) {
  #   if (".__DEVTOOLS__" %in% names(asNamespace("tensorflow"))) {
  #     # ugly hack around devtools::load_all() forcing this namespace to load
  #     # before the tensorflow namespace is finished loading
  #     # https://github.com/r-lib/pkgload/issues/76
  #     tf <<- new.env(parent = emptyenv())
  #     attr(tf, "class") <- c("python.builtin.module", "python.builtin.object")
  #     setHook(packageEvent("tensorflow", "onLoad"),
  #             function(...) {
  #               list2env(as.list.environment(as.environment(asNamespace("tensorflow")$tf),
  #                                            all.names = TRUE),
  #                        as.environment(tf))
  #             })
  #     return()
  #   }
  #
  #   tf <<- tensorflow::tf
  # } else {
  #   packageStartupMessage("R package 'tensorflow' required but not found")
  # }

  backports::import(pkgname, c("isFALSE", "...length"))
}
