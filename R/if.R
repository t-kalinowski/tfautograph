
#' @importFrom reticulate dict
#' @importFrom rlang %||%
ag_if <- function(cond, true, false = NULL) {
  true <- substitute(true)
  false <- substitute(false)
  env <- parent.frame()

  if (!is_tensor(cond))
    return(eval(as.call(list(quote(.Primitive("if")), cond, true, false)), env))

  on.exit(deregister_cond(cond))

  register_cond(cond, TRUE)
  get_true_outcome <- as_outcome_fn(true, env)
  true_outcome <- get_true_outcome()
  true_branch <- true_outcome$env
  true_return <- true_outcome$returned
  # true_branch <- new.env(parent = env)
  # true_return <- eval(true, true_branch)

  register_cond(cond, FALSE)
  get_false_outcome <- as_outcome_fn(false, env)
  false_outcome <- get_false_outcome()
  false_branch <- false_outcome$env
  false_return <- false_outcome$returned
  # false_branch <- new.env(parent = env)
  # false_return <- eval(false, false_branch)

  if(!is_same_structure(true_return, false_return))
    true_return <- false_return <- NULL

  vars_modified <- union(names(true_branch), names(false_branch))

  complement_branch <- function(branch) {
    missing <- setdiff(vars_modified, names(branch))
    objs <- mget(missing, envir = env, inherits = TRUE,
                 ifnotfound = undefined_mold(missing))
    obj_modes <- vapply(objs, mode, "")
    objs <- objs[obj_modes %in% valid_modes]
    list2env(objs, branch)
  }

  complement_branch(true_branch)
  complement_branch(false_branch)

  undefineds <- find_undefined(true_branch, false_branch)
  rm(list = undefineds, envir = true_branch)
  rm(list = undefineds, envir = false_branch)

  true_outcome <- drop_empty(list(
    modified = as.list(true_branch, all.names = TRUE),
    returned = true_return
  ))

  false_outcome <-  drop_empty(list(
    modified = as.list(false_branch, all.names = TRUE),
    returned = false_return
  ))

  if(length(undefineds))
    make_active_undefs(undefineds, env, sys.call())

  if(!length(true_outcome) && !length(false_outcome))
    return()

  pruned <- prune_identical(true_outcome, false_outcome)
  true_outcome <- pruned[[1]]
  false_outcome <- pruned[[2]]

  outcome <- tf$cond(cond,
                     function() true_outcome,
                     function() false_outcome,
                     strict = TRUE)

  if (!is.null(outcome$modified)) {
    for (nm in names(outcome$modified))
      if (is.list(outcome$modified[[nm]]))
        outcome$modified[[nm]] <-
          modifyList(get(nm, env), outcome$modified[[nm]])
      list2env(outcome$modified, envir = env)
  }


  outcome$returned
}


prune_identical <- function(x, y) {
  if(anyDuplicated(names(x)) || anyDuplicated(names(y)))
    stop("names can't be duplicated")

  for (nm in intersect(names(x), names(y))) {
    if (identical(x[[nm]], y[[nm]]))
      x[[nm]] <-  y[[nm]] <- NULL
    else if (is.list(x[[nm]]) && is.list(y[[nm]])) {
      res <- prune_identical(x[[nm]], y[[nm]])
      x[[nm]] <- res[[1]]
      y[[nm]] <- res[[2]]
    }
  }
  list(x, y)
}


valid_modes <- c("logical", "numeric", "complex", "character", "raw", "list", "environment")


unnamed_lists_to_tuples <- function(x) {
  if(is.list(x)) {
    x <- lapply(x, unnamed_lists_to_tuples)
    if(is.null(names(x)))
      x <- tuple(x)
  }
  x
}
