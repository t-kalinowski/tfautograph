#' tf.cond
#'
#' This is a minimal wrapper around `tf$cond()` that allows you to supply
#' `true_fn` and `false_fn` as lambda functions defined using the tilde `~`.
#'
#' @param pred R logical or a tensor.
#' @param true_fn,false_fn a `~` function, a function, or something coercible to
#'   a function via `as.function`
#' @param name a string, passed on to `tf.cond()`
#'
#' @return if cond is a tensor, then the result of `tf.cond()`. Otherwise, if
#'   `pred` is an `EagerTensor` or an R logical, then the result of either
#'   `true_fn()` or `false_fn()`
#'
#' @note in Tensorflow version 1, the `strict` keyword argument is supplied with
#'   a value of `TRUE` (different from the default)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## square if positive
#' # using tf$cond directly:
#' raw <- function(x) tf$cond(x > 0, function() x * x, function() x)
#'
#' # using tf_cond() wrapper
#' tilde <- function(x) tf_cond(x > 0, ~ x * x, ~ x)
#' }
tf_cond <- function(pred, true_fn, false_fn, name = NULL) {

  if(is_eager_tensor(pred))
    pred <- pred$`__bool__`()

  # early return in eager mode or pred not a tensor
  if (is.logical(pred) && length(pred) == 1L && !is.na(pred)) {
    return(if (pred)
      as.function(true_fn)()
      else
        as.function(false_fn)())
  }

  args <- list(
    pred = pred,
    true_fn = as.function(true_fn),
    false_fn = as.function(false_fn)
  )

  args$name <- name

  if(tf_v1())
    args$strict <- TRUE

  do.call(tf$cond, args)
}


#' tf.case
#'
#' This is a minimal wrapper around `tf.case()` that allows you to supply the
#' `pred_fn_pairs` using the `~`.
#'
#' @param ...,pred_fn_pairs a list `pred_fn_pairs` supplied with the `~` like
#'   so: `pred ~ fn_body`
#' @param default a function, optionally specified with the `~`, (or something
#'   coercible to a function via `as.function()`)
#' @param exclusive bool, whether to evaluate all `preds` and ensure only one is
#'   true. If `FALSE` (the default), then the `preds` are evaluated in the order
#'   supplied until the first `TRUE` value is encountered (effectively, acting
#'   as an `if()... else if() ... else if() ...` chain)
#' @param name a string, passed on to `tf.case()`
#'
#' @return The result from `tf$case()`
#' @export
#'
#' @examples
#' \dontrun{
#' fizz_buzz_one <- function(x) {
#'   tf_case(
#'     x %% 15 == 0 ~ "FizzBuzz",
#'     x %%  5 == 0 ~ "Buzz",
#'     x %%  3 == 0 ~ "Fizz",
#'     default = ~ tf$as_string(x, precision = 0L)
#'   )
#' }
#'
#' fn <- tf_function(autograph(function(n) {
#'   for(e in tf$range(n))
#'     tf$print(fizz_buzz_one(e))
#' }))
#'
#' x <- tf$constant(16)
#' fn(x)
#' }
tf_case <-
  function(...,
           pred_fn_pairs = list(...),
           default = NULL,
           exclusive = FALSE,
           name = 'case') {


    pred_fn_pairs <- lapply(pred_fn_pairs, function(x) {
      stopifnot(inherits(x, "formula"), length(x) == 3L)
      pred <- eval(x[[2L]], environment(x))
      x[[2L]] <- NULL
      fn <- as.function(x)
      tuple(pred, fn)
    })

    if (!is.null(default))
      default <- as.function(default)

    # TODO, if the branch is known already because we have a know TRUE pred,
    # then we should bypass tf$case here and just call the fn() directly

    tf$case(
      pred_fn_pairs,
      default = default,
      exclusive = exclusive,
      strict = TRUE,
      name = name
    )
  }



#' tf.switch_case
#'
#' @param branch_index an integer tensor
#' @param ...,branch_fns a list of function bodies specified with a `~`,
#'   optionally supplied with a branch index on the left hand side. See examples
#' @param default A function defined with a `~`, or something coercible via
#'   `as.function()``
#' @param name a string, passed on to `tf.switch_case()`
#'
#' @return The result from `tf.switch_case()`
#' @export
#'
#' @examples
#' \dontrun{
#' tf_pow <- tf_function(function(x, pow) {
#'    tf_switch(pow,
#'    0 ~ 1,
#'    1 ~ x,
#'    2 ~ x * x,
#'    3 ~ x * x * x,
#'    default = ~ -1)
#' })
#'
#' # can optionally also omit the left hand side int, in which case the order of
#' # the functions is used.
#' tf_pow <- function(x, pow) {
#'   tf_switch(pow,
#'             ~ 1,
#'             ~ x,
#'             ~ x * x,
#'             ~ x * x * x,
#'             default = ~ -1)
#' }
#'
#' # supply just some of the ints to override the default order
#' tf_pow <- function(x, pow) {
#'   tf_switch(pow,
#'             3 ~ x * x * x,
#'             2 ~ x * x,
#'             ~ 1,
#'             ~ x,
#'             default = ~ -1)
#' }
#'
#' # A slightly less contrived example:
#' tf_norm <- tf_function(function(x, l) {
#'   tf_switch(l,
#'             0 ~ tf$reduce_sum(tf$cast(x != 0, tf$float32)), # L0 norm
#'             1 ~ tf$reduce_sum(tf$abs(x)),                   # L1 norm
#'             2 ~ tf$sqrt(tf$reduce_sum(tf$square(x))),       # L2 norm
#'             default = ~ tf$reduce_max(tf$abs(x)))         # L-infinity norm
#' })
#' }
tf_switch <- function(branch_index, ...,
                      branch_fns = list(...),
                      default = NULL,
                      name = 'switch_case') {

  env <- parent.frame()

  if(!is.null(names(branch_fns)))
    stop("list of branch_fns must not be named")

  fns <- vector("list", length(branch_fns))
  ints <- rep(NA_integer_, length(branch_fns))
  for(i in seq_along(branch_fns)) {
    fn <- branch_fns[[i]]
    if(inherits(fn, "formula") && length(fn) == 3L) {
      ints[[i]] <- as.integer(eval(fn[[2L]], env))
      fn[[2L]] <- NULL
    }
    branch_fns[[i]] <- as.function(fn)
  }

  full_seq <- seq.int(from = 0L, along.with = branch_fns)
  fill <- setdiff(full_seq, ints)
  ints[is.na(ints)] <- fill
  branch_fns <- branch_fns[order(ints)]

  stopifnot(full_seq %in% ints)

  if(!is.null(default))
    default <- as.function(default)

  # TODO: dispatch to the right branch_fn if branch_index is known, bypass
  # calling tf$switch_case()

  tf$switch_case(
    branch_index,
    branch_fns = branch_fns,
    default = default,
    name = name
  )
}

