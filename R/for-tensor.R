

ag_for_impl.tensorflow.tensor <- function(iterable, var, body, env) {

  if(tf$executing_eagerly() && is_eager(iterable)) {
    next_loop_vars$pop()
    next_ag_name$pop()
    next_while_loop_opts$pop()
    return(ag_for_impl.python.builtin.iterator(
      as_iterator(iterable), var, body, env))
  }

  var <- deparse(var)
  # track python tensorflow TODO, reimplement here if implementation there changes:
  ## TODO(b/117628877): Revisit performance once XLA has the necessary support.
  ## Note: using a TensorArray creates an extra copy, but can calculate
  ## gradients more efficiently than StridedSlice.
  n <- tf$python$autograph$operators$len_(iterable)
  ta <- tf$TensorArray(iterable$dtype, size = n)
  iter <- ta$unstack(iterable)

  hint <- next_loop_vars$pop()

  loop_vars <- hint$list %||%
    statically_infer_loop_vars(body, env = env,
                               also_try_include = union(var, hint$include))

  loop_vars <- setdiff(loop_vars, hint$exclude)

  var_is_loop_var <- var %in% loop_vars
  if(!var_is_loop_var)
    hint$undef <- c(hint$undef, var)

  .body_fn <- as_loop_body_fn(body,  unique(c(loop_vars, var)), env,
                              dont_check = var,
                              additional_undefs = hint$undef,
                              call = sys.call(-2))

  body_fn <- function(index, loop_vars = NULL, did_break = NULL) {


    loop_vars[[var]] <- iter$read(index)
    res <- .body_fn(loop_vars, did_break)

    names(res) <- c("loop_vars", "did_break")[seq_len(length(res))]
    loop_vars <- res$loop_vars
    did_break <- res$did_break

    if(!var_is_loop_var)
      loop_vars[[var]] <- NULL

    drop_empty(list(index + 1L, loop_vars, did_break))
  }

  cond_fn <- function(index, loop_vars = NULL, did_break = NULL) {
    continue <- index < n
    if (!is.null(did_break))
      continue <- !did_break & continue
    continue
  }

  can_break <- any(c("break", "return") %in% all.names(body, unique = TRUE))
  did_break <- if(can_break) FALSE else NULL

  index <- 0L

  loop_vars <- mget(loop_vars, env, inherits = TRUE)

  while_loop_args <- c(
    list(
      cond = cond_fn,
      body = body_fn,
      loop_vars = drop_empty(list(index, loop_vars, did_break)),
      return_same_structure = TRUE
    ),
    name = next_ag_name$pop(),
    next_while_loop_opts$pop()
  )

  if(tf_v2())
    while_loop_args$return_same_structure <- NULL

  res <- do.call(tf$while_loop, while_loop_args)
  names(res) <- c("index", "loop_vars", "did_break")[seq_len(length(res))]

  loop_vars <- res$loop_vars
  if(length(loop_vars))
    list2env(loop_vars, envir = env)

  invisible()
}
