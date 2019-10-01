


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
#' }
ag_name <- function(x) next_ag_name$set(x)



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
#' }))
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
next_while_loop_opts <- Variable()

