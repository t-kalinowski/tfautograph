

#' @export
ag_loop_vars <- function(..., list = character()) {
  vars <- as.character(eval(substitute(alist(...))))
  vars <- unique(c(vars, list))
  next_loop_vars$set(vars)
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
    next_while_loop_opts$set(args)
  invisible()
}


#' @export
ag_name <- function(x) next_ag_name$set(x)



next_ag_name         <- Variable()
next_if_vars         <- Variable()
next_while_loop_opts <- Variable()
next_loop_vars       <- Variable()
