
# TODO: rename this to ag_switch_case, make room for ag_switch() that instead
# handles if elif chains
#
# TODO: consider just exporting this as a minimal ergomomic wrapper, perhaps
# called tf_switch_case() or ag_switch_case() that mostly drops the need to wrap
# all branches in functions. As it stands, autographing base::switch() is a
# little tricky because base::switch does 1-based counting while tf$switch_case
# does 0-based counting, and that's a recipe for confusion if someone's trying
# to write one code path for tensors and non-tensors.... It's probaby simpler to
# have tf_switch_case() just alwasys use 0-based counting, but perhaps have it
# dispatch to base::switch() if executing eagerly.
ag_switch <- function(EXPR, ..., default = NULL) {
  if (!is_tensor(EXPR))
    return(base::switch(EXPR, ...))


  dots <- eval(substitute(alist(...)))
  env <- parent.frame()

  if (!is.null(nms <- names(dots))) {
    nms <- suppressWarnings(as.integer(nms))
    if(!all(0:(length(dots) - 1L) %in% nms))
      stop(paste(
      "named arguments in `switch(...)` not supported in autograph mode",
      "An exception is made if the names all consist of the set of integers",
      "from 0 to (...length()-1)"))
    if(anyDuplicated(nms))
      stop("Duplicate names provided to switch(...)")
    dots <- dots[as.character(0:(length(dots)-1L))]
    names(dots) <- NULL
  }

  branch_index <- EXPR
  branch_fns <- lapply(dots, function(d) as.function.default(list(d), envir = env))

  default <- substitute(default)
  if(!is.null(default))
    default <- as.function.default(list(default), envir = env)

  tf$switch_case(branch_index, branch_fns,
                 default = default,
                 name = get_next_ag_name() %||% "switch_case")
}

