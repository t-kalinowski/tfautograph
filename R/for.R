

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


#' @importFrom zeallot %->%
ag_for_impl.tensorflow.tensor <- function(iterable, var, body, env) {

  # track python tensorflow TODO, reimplement here if implementation there changes:
  ## TODO(b/117628877): Revisit performance once XLA has the necessary support.
  ## Note: using a TensorArray creates an extra copy, but can calculate
  ## gradients more efficiently than StridedSlice.
  n <- tf$python$autograph$operators$len_(iterable)
  ta <- tf$TensorArray(iterable$dtype, size = n)
  iter <- ta$unstack(iterable)

  loop_vars <- get_existing_var_nms(body, var, env = env)
  var <- deparse(var)

  .body_fn <- as_loop_body_fn(body,  unique(c(loop_vars, var)), env)

  body_fn <- function(index, loop_vars = NULL, did_break = NULL) {
    loop_vars[[var]] <- iter$read(index)
    res <- .body_fn(loop_vars, did_break)
    if(!exists(var, envir = env))
      res[[1]][[var]] <- NULL

    c(index + 1L, res)
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

  res <- tf$while_loop(
    cond = cond_fn,
    body = body_fn,
    loop_vars = drop_empty(list(index, loop_vars, did_break)),
    return_same_structure = TRUE
  )

  # activate_undefs(undefs, sym)
  loop_vars <- res[[2]]
  if(length(loop_vars))
    list2env(loop_vars, envir = env)

  invisible()
}


