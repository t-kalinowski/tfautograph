

#' @export
ag_loop_vars <- function(..., list = character()) {
  vars <- as.character(eval(substitute(alist(...))))
  vars <- unique(c(vars, list))
  register_next_while_loop_vars(vars)
  invisible()
}

#' @export
ag_while_opts <- function(...,
                          shape_invariants = NULL,
                          parallel_iterations = 10L,
                          back_prop = TRUE,
                          swap_memory = FALSE,
                          name = NULL,
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
    register_next_while_loop_opts(args)
  invisible()
}
