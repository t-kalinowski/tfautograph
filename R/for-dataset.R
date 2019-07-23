

#' @importFrom reticulate tuple
ag_for_impl.tensorflow.python.data.ops.dataset_ops.DatasetV2 <-
  function(iterable, var, body, env) {

    body_vars <-
      get_registered_next_while_loop_vars() %||%
      get_existing_var_nms(body, var, env = env)
    var <- deparse(var)

    body_fn <- as_loop_body_fn(body, unique(c(body_vars, var)), env)

    body_vars <- mget(body_vars, env, inherits = TRUE)

    can_break <- any(c("break", "return") %in% all.names(body, unique = TRUE))

    final_state <- if(can_break)
      dataset_for_loop_with_potential_break(iterable, var, body_fn, body_vars, env)
    else
      dataset_for_loop_no_break(iterable, var, body_fn, body_vars, env)

    list2env(final_state, env)
    invisible()
  }



dataset_for_loop_with_potential_break <-
  function(iterable, var, body_fn, body_vars, env) {

    did_break <- did_break_prior_elem <- FALSE
    initial_state <- tuple(body_vars, did_break, did_break_prior_elem)

    scan_fn <- function(current_state, next_ds_elem) {
      # names(current_state) <- c("loop_vars", "did_break", "did_break_prior_elem")
      did_break_prior_elem <- current_state[[2]]
      current_state <- current_state[1:2]

      current_state[[1]][[var]] <- next_ds_elem

      new_state <- tf$cond(did_break_prior_elem,
                           function() current_state,
                           function() do.call(body_fn, current_state),
                           strict = TRUE)

      if (!exists(var, envir = env))
        new_state[[1]][[var]] <- NULL

      new_state[[3]] <- did_break_prior_elem
      new_state <- do.call(tuple, new_state)
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
  function(iterable, var, body_fn, body_vars, env) {

    initial_state <- body_vars

    reduce_func <- function(current_state, next_ds_elem) {
      current_state[[var]] <- next_ds_elem
      new_state <- body_fn(current_state)[[1]]

      if(!exists(var, envir = env))
        new_state[[var]] <- NULL

      new_state
    }
    final_state <- iterable$reduce(initial_state, reduce_func)
    final_state
  }



dataset_scan <- function(ds, initial_state, scan_func)
  ds$apply(tf$data$experimental$scan(initial_state, scan_func))

dataset_take_while <- function(ds, predicate)
  ds$apply(tf$data$experimental$take_while(predicate))

dataset_reduce <- function(ds, initial_state, reduce_func)
  ds$reduce(initial_state, reduce_func)
