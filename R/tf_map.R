
#' `tf.map_fn()`
#'
#' @description Thin wrapper around `tf.map_fn()` with the following
#'   differences:
#'
#'   +  accepts `purrr` style `~` lambda syntax to define function `fn`.
#'
#'   +  The order of `elems` and `fn` is switched to make it more pipe `%>%`
#'   friendly and consistent with R mappers `lapply()` and `purrr::map()`.
#'
#' @param elems A tensor or (possibly nested) sequence of tensors, each of which
#'   will be unpacked along their first dimension. The nested sequence of the
#'   resulting slices will be applied to `fn`.
#' @param fn An R function, specified using `purrr` style ~ syntax, a character
#'   string, a python function (or more generally, any python object with a
#'   `__call__` method) or anything coercible via `as.function()`. The function
#'   will be be called with one argument, which will have the same (possibly
#'   nested) structure as `elems`. Its output must return the same structure as
#'   `dtype` if one is provided, otherwise it must return the same structure as
#'   `elems`.
#' @param dtype (optional) The output type(s) of fn. If fn returns a structure
#'   of Tensors differing from the structure of elems, then dtype is not
#'   optional and must have the same structure as the output of fn.
#' @param parallel_iterations  (optional) The number of iterations allowed to
#'   run in parallel. When graph building, the default value is 10. While
#'   executing eagerly, the default value is set to 1.
#' @param back_prop (optional) True enables support for back propagation.
#' @param swap_memory (optional) True enables GPU-CPU memory swapping.
#' @param infer_shape (optional) False disables tests for consistent output
#'   shapes.
#' @param name (optional) Name prefix for the returned tensors.
#'
#' @return A tensor or (possibly nested) sequence of tensors. Each tensor packs
#'   the results of applying fn to tensors unpacked from elems along the first
#'   dimension, from first to last.
#' @export
tf_map <- function(elems, fn,
                   dtype = NULL,
                   parallel_iterations = NULL,
                   back_prop = TRUE,
                   swap_memory = FALSE,
                   infer_shape = TRUE,
                   name = NULL) {

  if (inherits(fn, "formula")) {
    # compat purrr::as_mapper() but without `.y` and a positional first match
    if (length(fn) > 2L)
      stop("Left hand side in `~`` not allowed")

    fn_body <- fn[[2L]]
    # replace all `.` symbols with `.x`. More robust than having multiple
    # symbols in the fn formals, because it allows you to assign to one and
    # return the other
    fn_body <- eval(substitute(substitute(fn_body, alist(. = .x))))
    fn <- as.function(c(alist(.x = ), fn_body), envir = environment(fn))
  } else if (!inherits(fn, "python.builtin.object"))
    fn <- as.function(fn, envir = parent.frame())

  if(is.double(parallel_iterations))
    storage.mode(parallel_iterations) <- "integer"

  tf$map_fn(
    fn = fn,
    elems = elems,
    dtype = dtype,
    parallel_iterations = parallel_iterations,
    back_prop = back_prop,
    swap_memory = swap_memory,
    infer_shape = infer_shape,
    name = name
  )
}


# alternative names tf_map_along_rows tf_apply_rows tf_map_rows
#
# ? also integrate with listarrays?, autograph listarrays::map_along_rows? ...
# ag_map_along_rows()? can do modify_along_rows for doing infer_shape = FALSE?
# wrapper around tf$unstack() in eager mode?
