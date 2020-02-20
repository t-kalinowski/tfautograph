


#' Specify `tf.cond()` output structure when autographing `if`
#'
#' This function can be used to specify the output structure from `tf.cond()`
#' when autographing an `if` statement. In most use cases, use of this function
#' is purely optional. If not supplied, the `if` output structure is
#' automatically built.
#'
#' @param ... Variables modified by the `tf.cond()` node supplied as bare
#'   symbols like `foo` or expressions using `$` e.g, `foo$bar`. Symbols do not
#'   have to exist before the autographed `if` so long as they are created in
#'   both branches.
#' @param modified Variables names supplied as a character vector, or a list of
#'   character vectors if specifying nested complex structures. This is an
#'   escape hatch for the lazy evaluation semantics of `...`
#' @param return logical, whether to include the return value the evaluated R
#'   expression in the `tf.cond()`. if `FALSE` (the default), only the objects
#'   assigned in scope are captured.
#' @param undefs A bare character vector or a list of character vectors.
#'   Supplied names are exported as undefs in the parent frame. This is used to
#'   give a more informative error message when attempting to access a variable
#'   that can't be balanced between branches.
#' @param control_flow An integer, the maximum number of control-flow statements
#'   (`break` and/or `next`) that will be captured in a single branch as part of
#'   the `tf.cond()`. Do not count statements in loops that are dispatching to
#'   standard R control flow (e.g., don't count `break` statements in a `for`
#'   loop that is iterating over an R vector)
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
#'   structure, and then merging them back with the original object recursively.
#'   One limitation of the implementation is that lists must either be fully
#'   named with unique names, or not named at all, partially named lists or
#'   duplicated names in a list throw an error. This is due to the conversion
#'   that happens when going between python and R: named lists get converted to
#'   python dictionaries, which require that all keys are unique. Additionally,
#'   pruning of unmodified objects from an autographed `if` is currently only
#'   supported for named lists (python dictionaries). Unnamed lists or tuples
#'   are passed as is (e.g, no pruning and merging done), which may lead to
#'   unnecessarily bloat in the constructed graphs.
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

  # TODO: should this have an 'exclude' argument like `ag_loop_vars()`?
  #
  # leaning towards no because I'm not seeing how ag_if will be biased towards
  # capturing more than necessary like ag_while...

  if (...length()) {
    dots <- eval(substitute(alist(...)))
    if(!is.null(names(dots)))
      stop("arguments passed to `...` must not be named")
    dots <- vapply(dots, deparse, "",
                   width.cutoff = 500L, backtick = FALSE, nlines = 1L)
    dots <- strsplit(dots, "$", fixed = TRUE)
    modified <- c(as.list(modified), dots)
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
#' intended to be local variables only. In those circumstances it is also
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
#' @param list,include,exclude optionally, the variable names as a character
#'   vector (use this as an escape hatch from the `...` lazy evaluation
#'   semantics).
#' @param undef character vector of symbols
#'
#' @note The semantics of this function are inspired by base::rm()
#'
#' @return the specified hint invisibly.
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
#'
#' ## sometimes, a variable is infered to be a loop_var unnecessarily. For example
#' x <- tf$constant(1:10)
#'
#' # imagine x is left over in the current scope from some previous calculations
#' # It's value is not important, but it exists
#' autograph({
#'   for(i in tf$constant(1:6)) {
#'     x <- i * i
#'     tf$print(x)
#'   }
#' })
#'
#' # this will throw an error because `x` was infered to be a `loop_var`,
#' # but it's shape witin the loop body is different from what it was before.
#' # there are two solutions to prevent `x` from being captured as a loop_var
#' ## 1) remove `x` from the current scope like so:
#' rm(x)
#'
#' ## 2) provide a hint like so:
#' ag_loop_vars(-x)
#'
#' ## if your variable names are being dynamically generated, there is an
#' ## escape hatch for the lazy evaluation semantics of ...
#' ag_loop_vars(exclude = "x")
#' }
ag_loop_vars <-
  function(...,
           list = character(),
           include = character(),
           exclude = character(),
           undef = character()) {

    if (...length()) {
      vars <- eval(substitute(alist(...)))
      var_is_call <- vapply(vars, is.call, TRUE)

      if (!xor(all(var_is_call),!any(var_is_call)))
        stop("symbols supplied to ... must either all be bare symbol names, ",
             "or all be bare symbol names prefixed with a `-` or `+`")

      if (all(var_is_call)) {
        # check all ... are prefixes like +foo, -bar
        modifier <- vapply(vars, function(v) deparse(v[[1L]]), "")

        if (!all(modifier %in% c("+", "-")))
          stop("`-` and `+` are the only valid modifiers, not ",
               pc_or(sprintf("`%s`", setdiff(modifier, c("+", "-")))))

        if (any(invalid <- lengths(vars) != 2L) ||
            any(invalid <- !vapply(vars, function(v) is.symbol(v[[2L]]), TRUE)))
          stop("Invalid hint: ", pc_and(vapply(vars[invalid], deparse, "")))

        exclude_hints_in_dots <-
          vapply(vars[modifier == "-"], function(v) deparse(v[[2L]]), "")
        exclude <- union(exclude, exclude_hints_in_dots)

        include_hints_in_dots <-
          vapply(vars[modifier == "+"], function(v) deparse(v[[2L]]), "")
        include <- union(include, include_hints_in_dots)

      } else {
        # check all ... are bare symbols
        if (any(invalid <- !vapply(vars, is.name, TRUE)))
          stop("Invalid hint: ", pc_and(vapply(vars[invalid], deparse, "")))

        list <- union(list, vapply(vars, deparse, ""))
      }
    }

    if (length(list) && sum(length(include), length(exclude)))
      stop(
        "`loop_vars` must either provided either as set of symbols ",
        "to `include` and/or `exclude` after static inference, ",
        "or a definitive set of symbols to that overrides inference, not both."
      )

    next_loop_vars$set(drop_empty(
      base::list(
        list = list,
        include = include,
        exclude = exclude,
        undef = undef
        # dont_export_undefs = !missing(undef) && is_empty(undef)
      )
    ))
  }

next_if_vars   <- Variable()
next_loop_vars <- Variable()
