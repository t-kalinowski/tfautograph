

#' @importFrom reticulate tuple
ag_for_impl.tensorflow.python.data.ops.dataset_ops.DatasetV2 <-
  function(iterable, var, body, env) {

    next_while_loop_opts$pop()
    # while_opts not used here others, but pop anyway in case this is a for
    # loop that can accept either a dataset or a tensor
    if(tf$executing_eagerly()) {
      next_ag_name$pop()
      next_loop_vars$pop()
      return(ag_for_impl.python.builtin.iterator(
        as_iterator(iterable), var, body, env))
    }

    var <- deparse(var)

    hint <- next_loop_vars$pop()

    body_vars <- hint$list %||%
      statically_infer_loop_vars(body, env = env,
                                 also_try_include = union(var, hint$include))

    body_vars <- setdiff(body_vars, hint$exclude)

    var_is_body_var <- var %in% body_vars
    if(!var_is_body_var)
      hint$undef <- c(hint$undef, var)

    body_fn <- as_loop_body_fn(body, unique(c(body_vars, var)), env,
                               dont_check = var,
                               additional_undefs = hint$undef,
                               call = sys.call(-2))

    body_vars <- mget(body_vars, env, inherits = TRUE)


    args <- list(
      iterable = iterable,
      var = var,
      var_is_body_var = var_is_body_var,
      body_fn = body_fn,
      body_vars = body_vars,
      env = env
    )
    can_break <- any(c("break", "return") %in% all.names(body, unique = TRUE))

    impl_fn <- if (can_break)
      dataset_for_loop_with_potential_break else
      dataset_for_loop_no_break

    final_state <- do.call(impl_fn, args)

    list2env(final_state, env)

    invisible()
  }


dataset_for_loop_with_potential_break <-
  function(iterable, var, var_is_body_var, body_fn, body_vars, env) {

    did_break <- did_break_prior_elem <- FALSE
    initial_state <- tuple(body_vars, did_break, did_break_prior_elem)

    scan_fn <- function(current_state, next_ds_elem) {

      nms <- c("body_vars", "did_break", "did_break_prior_elem")
      names(current_state) <- nms

      did_break_prior_elem <- current_state$did_break
      current_state$did_break_prior_elem <- NULL

      fn <- function(current_state) {
        res <- do.call(body_fn, unname(current_state))
        names(res) <- c("body_vars", "did_break")
        res
      }


      current_state$body_vars[[var]] <- next_ds_elem
      new_state <- tf$cond(did_break_prior_elem,
                           function() current_state,
                           function() fn(current_state))

      if (!var_is_body_var)
        new_state$body_vars[[var]] <- NULL

      new_state$did_break_prior_elem <- did_break_prior_elem
      new_state <- do.call(tuple, unname(new_state[nms]))
      tuple(new_state, new_state)
    }

    predicate_fn <- function(current_state, did_break, did_break_prior_elem)
      ! did_break_prior_elem

    ds <- iterable

    ds <- dataset_scan(ds, initial_state, scan_fn)
    ds <- dataset_take_while(ds, predicate_fn)
    res <- dataset_reduce(ds, initial_state,
                          function(current_state, next_ds_elem) next_ds_elem)

    final_state <- res[[1]]
    final_state
  }


dataset_for_loop_no_break <-
  function(iterable, var, var_is_body_var, body_fn, body_vars, env) {

    initial_state <- body_vars
    using_placeholder  <- length(initial_state) == 0

    if (using_placeholder)
      initial_state$placeholder <- tf$constant(FALSE)

    reduce_func <- function(current_state, next_ds_elem) {

      if(using_placeholder) {
        placeholder <- current_state$placeholder
        current_state$placeholder <- NULL
      }

      current_state[[var]] <- next_ds_elem
      new_state <- body_fn(current_state)[[1]]

      if(!var_is_body_var)
        new_state[[var]] <- NULL

      if(using_placeholder)
        new_state$placeholder <- placeholder

      new_state
    }

    final_state <- iterable$reduce(initial_state, reduce_func)
    if(using_placeholder)
      final_state$placeholder <- NULL

    final_state
  }



dataset_scan <- function(ds, initial_state, scan_func)
  ds$apply(tf$data$experimental$scan(initial_state, scan_func))

dataset_take_while <- function(ds, predicate)
  ds$apply(tf$data$experimental$take_while(predicate))

dataset_reduce <- function(ds, initial_state, reduce_func)
  ds$reduce(initial_state, reduce_func)
