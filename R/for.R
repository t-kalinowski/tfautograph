


ag_for <- function(var, iterable, body) {
  var  <- substitute(var)
  body <- substitute(body)
  env  <- parent.frame()

  ag_for_impl(iterable, var, body, env)
}


ag_for_impl <- function(iterable, var, body, env) UseMethod("ag_for_impl")

ag_for_impl.default <- function(iterable, var, body, env) {
  eval(as.call(list(quote(.Primitive("for")), var, iterable, body)), env)
}

ag_for_impl.tensorflow.python.data.ops.dataset_ops.DatasetV2 <-
  function(iterable, var, body, env) {
    .NotYetImplemented()
  }

ag_for_impl.tensorflow.python.data.ops.iterator_ops.IteratorV2 <-
  function(iterable, var, body, env) {
    .NotYetImplemented()
  }


ag_for_impl.tensorflow.tensor <- function(iterable, var, body, env) {
  # modeled after _known_len_tf_for_stmt()
  body_vars <- setdiff(all.vars(body), deparse(var))

  # TODO: still need to handle `next`,`break`, and early return() statements

  # TODO: take the undefined body_vars and bury then in the body_fn function env
  # as active bindings, and if they are accessed throw a nicer error message for
  # test_while_local_composite_complex_illegal
  body_vars <- body_vars[vapply(body_vars, exists, TRUE, envir = env)]

  body_w_ret <- substitute({
    body
    unname(mget(body_vars))
  }, list(body = body, body_vars = body_vars))


  body_fn <- new_function(as_args(body_vars), body_w_ret, env)
  # track python tensorflow TODO, reimplement here if implementation there changes:
  ## TODO(b/117628877): Revisit performance once XLA has the necessary support.
  ## Note: using a TensorArray creates an extra copy, but can calculate
  ## gradients more efficiently than StridedSlice.
  n <- tf$python$autograph$operators$len_(iterable)
  ta <- tf$TensorArray(iterable$dtype, size = n)
  iter <- ta$unstack(iterable)

  while_body <- function(index, body_args = NULL) {
    elem <- iter$read(index)
    body_fn <- env_bury(body_fn, !!var := elem)
    body_state <- do.call(body_fn, body_args)
    # print(    list(index + 1L, body_state))
    list(index + 1L, body_state)
  }

  while_cond <- function(index, body_args = NULL) index < n

  res <- tf$while_loop(
    cond = while_cond,
    body = while_body,
    loop_vars = list(0L, unname(mget(body_vars, envir = env))),
    return_same_structure = TRUE)

  # browser()
  # RES <<- res
  # res
  modified <- res[[2L]]
  names(modified) <- body_vars
  modified[[deparse(var)]] <- iter$read(res[[1]] - 1L)

  list2env(modified, envir = env)

  invisible()
}

