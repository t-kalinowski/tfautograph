


#' Specify a tensor name
#'
#' This can be used before any autographed expression that results in the
#' creation of a tensor or op graph node. This can be used before `for` (both with tensors and datasets), `while`, and `if` statements.
#' @param x A string
#'
#' @return `x`, invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' ## when you're in graph mode. (e.g, tf$executing_eagerly == FALSE)
#'
#' ag_name("main-training-loop")
#' for(elem in dataset) ...
#'
#' ag_name("iter-along-tensor")
#' for(elem in my_tensor) ...
#'
#' ag_name("iter-along-tensor")
#' for(elem in my_tensor) ...
#' }
ag_name <- function(x) next_ag_name$set(x)



#' Specify `tf.cond()` output structure
#'
#' This function can be used to specify the output structure from `tf.cond()`
#' when autographing an `if` statement. In almost all use cases, use of this
#' function is not required because the `if` output structure is automatically
#' built.
#'
#' @param ... Variables modified by the `tf.cond()` node supplied as bare symbol
#'   names like `foo` or expressions using `$` e.g, `foo$bar`. Symbols do not
#'   have to exist before the autographed `if` so long as they are created in
#'   both branches.
#' @param modified Variables names supplied as a character vector, or a list of
#'   character vectors if specifying nested complex structures. This is an
#'   escape hatch for the lazy evaluation semantics of `...`
#' @param return logical, whether include the return value the evaluated R
#'   expression  in the `tf.cond()`.
#' @param undefs A bare character vector or a list of character vectors.
#'   Supplied names are exported as undefs in the parent frame. This is used to
#'   give a more informative error message when attempting to access a variable
#'   that can't be balanced between branches.
#' @param control_flow An integer, the maximum number of control-flow statements
#'   (`break` and/or `next`) that will be captured in a single branch as part of
#'   the `tf.cond()`.
#'
#' @details If the output structure is not explicitly supplied via
#'   `ag_if_vars()`, then the output structure is automatically composed: The
#'   true and false branches of the expression are traced into concrete
#'   functions, then the output signature from the two branch functions are
#'   balanced. Balancing is performed by either fetching a variable from an
#'   outer scope or by reclassifying a symbol as an undef.
#'
#'   When dealing with complex composites (that is, nested structures where a
#'   modified tensor is part of a named list or dictionary), care is taken to
#'   prevent unnecessarily capturing other unmodified tensors in the structure.
#'   This is done by pruning unmodified tensors from the returned output
#'   structure, and then merging them back with the original object using
#'   [`utils::modifyList()`]. Two (current) limitation of this approach are
#'   that: a) removing an element from a nested structure in an `if` statement
#'   is not possible and b) pruning and merging is currently only supported for
#'   named lists (python dictionaries). Unnamed lists or tuples are passed as is
#'   (e.g, no pruning and merging done), which can lead to unnecessarily bloat
#'   in the constructed graphs.
#'
#' @return `NULL`, invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' # these examples only have an effect in graph mode
#' # to enter graph mode easily we'll create a few helpers
#' ag <- autograph
#'
#' # pass which symbols you expect to be modifed or created liks this:
#' ag_if_vars(x)
#' ag(if (y > 0) {
#'   x <- y * y
#' } else {
#'   x <- y
#' })
#'
#' # if the return value from the if expression is important, pass `return = TRUE`
#' ag_if_vars(return = TRUE)
#' x <- ag(if(y > 0) y * y else y)
#'
#' # pass complex nested structures like this
#' x <- list(a = 1, b = 2)
#'
#' ag_if_vars(x$a)
#' ag(if(y > 0) {
#'   x$a <- y
#' })
#'
#' # undefs are for mark branch-local variables
#' ag_if_vars(y, x$a, undef = "tmp_local_var")
#' ag(if(y > 0) {
#'   y <- y * 100
#'   tmp_local_var <- y + 1
#'   x$a <- tmp_local_var
#' })
#'
#' # supplying `undef` is not necessary, it exists purely as a way to supply a
#' # guardrail for defensive programming and/or to improve code readability
#'
#' ## modified vars can be supplied in `...` or as a named arg.
#' ## these paires of ag_if_vars() calls are equivalent
#' ag_if_vars(y, x$a)
#' ag_if_vars(modified = list("y", c("x", "a")))
#'
#' ag_if_vars(x, y, z)
#' ag_if_vars(modified = c("x", "y", "z"))
#'
#'
#' ## control flow
#' # count number of odds between 0:10
#' ag({
#'   x <- 10
#'   count <- 0
#'   while(x > 0) {
#'     ag_if_vars(control_flow = 1)
#'     if(x %% 2 == 0)
#'       next
#'     count <- count + 1
#'   }
#' })
#' }
ag_if_vars <- function(..., modified = list(), return = FALSE,
                       undefs = NULL, control_flow = 0) {


  if (...length()) {
    modified <- c(modified,
                  strsplit(deparse(eval(substitute(alist(...)))),
                           "$", fixed = TRUE))
  }
  next_if_vars$set(list(
    modified = as.list(modified),
    return = return,
    undefs = undefs,
    n_control_flow = as.integer(control_flow)
  ))
  invisible()
}



#' Specify loop variables
#'
#' This can be used to manually specify which variables are to be included
#' explicitly as `loop_vars` when autographing an expression into a
#' `tf.while_loop()` call, or the `loop_vars` equivalent when building a
#' `dataset.reduce()`.
#'
#' Use of this is usually not required as the loop variables are automatically
#' inferred. Inference is done by statically looking through the loop body and
#' finding the symbols that are the targets of the common assignment operators
#' from base R (`<-`, `->`, `=`), from package:zeallot (`%<-%` and `%->%`) and
#' package:magrittr (`%<>%`).
#'
#' In certain circumstances, this approach may capture variables that are
#' intended to be local varialbes only. In those circumstances it is also
#' possible to specify them preceded with a `-`.
#'
#' Note, the specified loop vars are expected to exist before the autographed
#' expression, and a warning is issued otherwise (usually immediately preceding
#' an error thrown when attempting to actually autograph the expression)
#'
#' Only bare symbol names can be supplied as loop vars. In the future, support
#' may be expanded to allow for nested complex composites (e.g., specifying
#' variables that are nested within a more complex structure--passing
#' `ag_loop_vars(foo$bar$baz)` is currently not supported.)
#'
#' @param ... Variables as bare symbol names
#' @param list optionally, the variable names as a character vector (use this as
#'   an escape hatch from the `...` lazy evaluation semantics).
#' @param undefs character vector of symbols
#'
#'
#' @note The semantics of this function are inspired by base::rm()
#'
#' @return `NULL`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' i <- tf$constant(0L)
#'
#' autograph({
#'   ag_loop_vars(x, i)
#'   while(x > 0) {
#'     if(x %%2 == 0)
#'       i <- i + 1L
#'     x <- x - 1
#'   }
#' })
#' }
ag_loop_vars <-
  function(..., list = character(), exclude = character(), undefs = NULL) {
    vars <- eval(substitute(alist(...)))
    exclude <-
      unique(c(
        as.character(exclude),
        unlist(lapply(vars, function(v) {
          if (is.name(v))
            return(NULL)
          if (!is.call(v) || v[[1]] != quote(`-`) ||
              length(v) != 2L || !is.name(v[[2]]))
            stop(
              "loop_vars must be supplied either as bare symbol names or ",
              "a bare symbol name preceded with a `-` to specify an excluded variable"
            )
          deparse(v[[2]])
        })))
      )

  vars <- unique(c(as.character(list),
                   unlist(lapply(vars, function(v) {
                     stopifnot(is.name(v))
                     deparse(v)
                   }))))

  if(!all(vapply(vars, exists, TRUE, envir = parent.frame())))
    warning("All symbols supplied to `ag_loop_vars()` are expected to exist")
  # TODO: handle undefs
  if(!is.null(undefs))
    .NotYetImplemented()
  # next_loop_vars$set(list(loop_vars = vars, undefs = undefs)
  next_loop_vars$set(vars)
  invisible()
}


#' specify `tf.while_loop` options
#'
#' See https://www.tensorflow.org/versions/r2.0/api_docs/python/tf/while_loop
#' for additional details.
#'
#' @param ... Ignored, used to ensure all arguments supplied are named.
#' @param shape_invariants The shape invariants for the loop variables.
#' @param parallel_iterations The number of iterations allowed to run in
#'   parallel. It must be a positive integer.
#' @param back_prop  Whether backprop is enabled for this while loop.
#' @param swap_memory Whether GPU-CPU memory swap is enabled for this loop.
#' @param maximum_iterations Optional maximum number of iterations of the while
#'   loop to run. If provided, the `cond` output is AND-ed with an additional
#'   condition ensuring the number of iterations executed is no greater than
#'   `maximum_iterations`.
#'
#' @note Use [`ag_name()`] to supply `name` and [`ag_loop_vars()`] to supply
#'   `loop_vars` directly.
#'
#'   This is only applicable when autograph in graph mode, otherwise this has no
#'   effect.
#'
#' @return `NULL`` invisibly, called for it's side effect.
#' @export
#'
#' @examples
#' \dontrun{
#' ## use tf_function() to enter graph mode:
#' tf_function(autograph(function(n) {
#'   ag_name("silly-example")
#'   ag_while_opts(back_prop = FALSE)
#'   while(n > 0)
#'     n <- n - 1
#' })
#' }
ag_while_opts <- function(...,
                          shape_invariants = NULL,
                          parallel_iterations = 10L,
                          back_prop = TRUE,
                          swap_memory = FALSE,
                          maximum_iterations = NULL
) {
  if(...length())
    stop("all options passed must be named")
  storage.mode(parallel_iterations) <- "integer"
  args <- as.list(environment())
  for(a in names(args))
    if(eval(bquote(missing(.(as.name(a))))))
      args[[a]] <- NULL
  if(length(args))
    next_while_loop_opts$set(args)
  invisible()
}





next_ag_name         <- Variable()
next_if_vars         <- Variable()
next_while_loop_opts <- Variable()
next_loop_vars       <- Variable()

