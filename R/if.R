

#' @export
ag_if_vars <- function(..., modified = list(...), return = FALSE,
                       undefs = NULL, control_flow = 0) {
  register_if_vars(modified = modified, return = return, undefs = undefs,
                   n_control_flow = as.integer(control_flow))
}


#' @importFrom reticulate dict
#' @importFrom rlang %||%
ag_if <- function(cond, true, false = NULL) {
  true <- substitute(true)
  false <- substitute(false)
  env <- parent.frame()

  if(is_eager_tensor(cond))
    cond <- cond$`__bool__`()

  if (!is_tensor(cond))
    return(eval(as.call(list(quote(.Primitive("if")),
                             cond, true, false)), env))

  true_fn <- as_cond_branch_fn(cond, true, TRUE, env)
  false_fn <- as_cond_branch_fn(cond, false, FALSE, env)

  target_outcome <- get_registered_next_if_vars()
  if (is.null(target_outcome)) {
    true_fn <- as_concrete_function(true_fn)
    false_fn <- if(is.null(false)) null_fn else
      as_concrete_function(false_fn)

    target_outcome <- build_target_outcome(
      true_fn$structured_outputs,
      false_fn$structured_outputs,
      env)
  }

  undefs <- target_outcome$undefs
  target_outcome$undefs <- NULL

  # TODO, the `placeholder` tensor returned should be the same, i.e., not
  # recreated separatly in each branch, but made before the branch is traced
  # then just grabbed
  outcome <- tf$cond(cond,
                     function() fix_outcome(true_fn(), target_outcome, env),
                     function() fix_outcome(false_fn(), target_outcome, env))

  for(lcf in rev(outcome$loop_control_flow))
    try_register_or_signal_error_with_restart(lcf)

  if (!is.null(outcome$modified)) {
    for (nm in names(outcome$modified))
      if (is.list(outcome$modified[[nm]]))
        outcome$modified[[nm]] <-
          modifyList(get(nm, env), outcome$modified[[nm]])
    list2env(outcome$modified, envir = env)
  }

  if(length(undefs))
    export_undefs(undefs, env)

  outcome$returned
}



as_cond_branch_fn <- function(cond, branch_expr, branch, env) {
  force(cond)
  fn <- as_outcome_fn(branch_expr, env)
  function() {
    register_cond(cond, branch)
    on.exit(deregister_cond(cond))

    local_lcf_reg <- Stack()

    withCallingHandlers(
      outcome <- fn(),

      uncaught_loop_control_flow = function(lcf) {
        local_lcf_reg$push(compact_lcf(lcf))
        invokeRestart("continue")
      }
    )

    # strip withVisible(), don't bloat graph
    outcome$visible <- NULL

    outcome <- prune_ops(outcome)
    # return object from an eager defun can't be a bare op, must be a tensor.

    if (length(local_lcf_reg))
      outcome$loop_control_flow <- as.list(local_lcf_reg)

    outcome
  }
}



prune_invalid_vals <- function(x) {
  modifyList(list(x = x),
             rapply(list(x = x), function(v)
               if (is_valid_val(v)) v else NULL,
               how = 'replace'))$x
}

prune_ops <- function(x) {
  modifyList(list(x = x),
             rapply(list(x = x), function(v) NULL,
                    classes = "tensorflow.python.framework.ops.Operation",
                    how = 'replace'))$x
}

# from_concrete_fn's `structured_outputs`
build_target_outcome <- function(true, false, env) {

  ret <- if(is_same_structure(true$returned, false$returned) &&
            !is_empty(true$returned) && !is_empty(false$returned))
    TRUE else NULL
  true_modified  <- leaf_names(true$modified)
  false_modified <- leaf_names(false$modified)
  all_modified <-  union(true_modified, false_modified)
  common <- intersect(true_modified, false_modified)
  unbalanced <- setdiff(all_modified, common)
  unbalanced_gettable <-
    Filter(function(x) !is.null(pluck_structure(x, env)),
           unbalanced)
  modified <- union(common, unbalanced_gettable)
  undefs <- setdiff(unbalanced, unbalanced_gettable)

  n_lcf <- max(length(true$loop_control_flow), length(false$loop_control_flow))
  if(n_lcf == 0)
    n_lcf <- NULL

  drop_empty(list(modified = modified, return = ret, undefs = undefs,
                  n_loop_control_flow = n_lcf))
}


fix_outcome <- function(outcome, target_outcome, env) {

  if(is.null(target_outcome))
    return(outcome)



  outcome$modified <- pluck_structure(target_outcome$modified,
                                      outcome$modified, env)

  if(!isTRUE(target_outcome$return))
    outcome$returned <- NULL

  lt <- target_outcome$n_loop_control_flow %||% 0L
  lo <- length(outcome$loop_control_flow)

  # too much control flow
  if(lo > lt) {
    #should only happen if user specified ag_if_vars() with the wrong number of
    #control flow

    #TODO: this stop() leaves the tensorflow tracing context open, need to
    #figure out a way to exit that. Raise an exception from the python side?
    stop("More control flow condition were encountered when autographing `if` ",
         "than specified in ag_if_vars(). expected: ", lt, "encountered: ", lo)
  }

  # not enough control flow
  if (lt > lo) {
    dummy_lcf <- dummy_compact_lcf(env)

    outcome$loop_control_flow[(lo + 1):lt] <-
      rep(list(dummy_lcf), lt - lo)
  }

  if(!length(outcome))
    outcome$placeholder <- tf$constant(FALSE)

  outcome
}


as_concrete_function <- function(fn) {
  tf$`function`(fn, autograph = FALSE)$get_concrete_function()
}


pluck_structure <- function(nms, ...) {
  Y <- list()
  from <- list(...)
  for (nm in nms) {

    for (fr in from) {
      if (is.environment(fr))
        fr <- mget(nm[1], envir = fr, ifnotfound = list(NULL),
                   inherits = TRUE)

      if (!is.null(val <- fr[[nm]]))
        break
    }

    if (is_valid_val(val)) {
      if (length(nm) > 1)
        for (i in seq_along(nm))
          if (is.null(Y[[nm[seq_len(i)]]])) {
            Y[[nm[seq_len(i)]]] <- list()
          }
      Y[[nm]] <- val
    }
  }

  if(length(Y)) Y else NULL
}

is_valid_val <- function(x) {
  !is.null(x) && is_tensor(x) || typeof(x) %in% valid_typeofs
}


leaf_names <- function(x) {
  n <- as.list(names(x))
  nested_nms <- lapply(x, function(x) if (!is_tensor(x)) names(x))

  which_are_nested <- which(!vapply(nested_nms, is.null, TRUE))
  for (i in which_are_nested)
    n[[i]] <- lapply(leaf_names(x[[i]]), function(zz)
      c(n[[i]], zz))

  as.list(unlist(n, recursive = FALSE, use.names = FALSE))
}

