
#' @importFrom reticulate dict
tf_if <- function(cond, true, false = NULL) {
  true <- substitute(true)
  false <- substitute(false)
  env <- parent.frame()

  if (!is_tensor(cond))
    return(eval(as.call(list(quote(.Primitive("if")), cond, true, false)), env))

  true_branch <- new.env(parent = env)
  true_res <- eval(true, true_branch)

  false_branch <- new.env(parent = env)
  false_res <- eval(false, false_branch)

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

  ## TODO: the return value of true_fn and false_fn shoule include
  # the return value of the if expression (true_res or false_res)
  # if the returned shapes are compatible. something like:
  # list(modified = as.list(true_branch),
  #      return = true_res)
  modified <- tf$cond(cond,
                      function() dict(as.list(true_branch , all.names = TRUE)),
                      function() dict(as.list(false_branch, all.names = TRUE)))

  list2env(modified, envir = env)

  invisible()
}

