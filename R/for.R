


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

  # TODO: take the undefined body_vars and bury then in the body_fn function env
  # as active bindings, and if they are accessed throw a nicer error message for
  # test_while_local_composite_complex_illegal
  body_vars <- body_vars[vapply(body_vars, exists, TRUE, envir = env)]

  body_fn <- as_loop_fn(body, body_vars, env)

  # track python tensorflow TODO, reimplement here if implementation there changes:
  ## TODO(b/117628877): Revisit performance once XLA has the necessary support.
  ## Note: using a TensorArray creates an extra copy, but can calculate
  ## gradients more efficiently than StridedSlice.
  n <- tf$python$autograph$operators$len_(iterable)
  ta <- tf$TensorArray(iterable$dtype, size = n)
  iter <- ta$unstack(iterable)

  establish_cond_registry()
  on.exit(remove_cond_registry())

  while_body <- function(index, did_break, body_args = NULL) {
    elem <- iter$read(index)
    body_fn <- env_bury(body_fn, !!var := elem)
    body_fn <- wrap_fn_with_loop_control_flow_handlers(body_fn)
    c(did_break, body_state) %<-% do.call(body_fn, list(did_break, body_args))
    list(index + 1L, did_break, body_state)
  }

  while_cond <- function(index, did_break, body_args = NULL) index < n & !did_break

  did_break <- FALSE
  index <- 0L
  body_vars <- mget(body_vars, envir = env, inherits = TRUE)

  c(index, did_break, body_vars) %<-%
    tf$while_loop(
      cond = while_cond,
      body = while_body,
      loop_vars = list(index, did_break, body_vars),
      return_same_structure = TRUE
    )

  if(iterable$shape$as_list()[1] > 0)
    body_vars[[deparse(var)]] <- iterable$`__getitem__`(index-1L)
  # alternatively to calling __getitem__, it might make more sense to to
  # `ta$read(index-1L)`, however then you run into an issue where the
  # TensorArray element might have been cleared from memory already, having
  # already been read once in the loop. Having to set TensorArray(...,
  # clear_after_read = FALSE) is probably more expensive than this.
  list2env(body_vars, envir = env)

  invisible()
}

