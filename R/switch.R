

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

