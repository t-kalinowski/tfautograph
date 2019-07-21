
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
  true_branch <- new.env(parent = env)
  true_return <- eval(true, true_branch)

  register_cond(cond, FALSE)
  false_branch <- new.env(parent = env)
  false_return <- eval(false, false_branch)

  if(!is_same_structure(true_return, false_return))
    true_return <- false_return <- NULL

  vars_modified <- union(names(true_branch), names(false_branch))

  complement_branch <- function(branch) {
    missing <- setdiff(vars_modified, names(branch))
    objs <- mget(missing, envir = env, inherits = TRUE,
                 ifnotfound = undefined_mold(missing))
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


  outcome <- tf$cond(cond,
                     function() true_outcome,
                     function() false_outcome,
                     strict = TRUE)

  if(!is.null(outcome$modified))
    list2env(outcome$modified, envir = env)


  outcome$returned
}



