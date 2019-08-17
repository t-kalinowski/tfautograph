

prune_nested_unmodified <- function(modified, env) {
  for (nm in names(modified)) {

    obj <- modified[[nm]]
    if(!is_named_list(obj))
      next
    orig <- get(nm, env)
    pruned_obj <- prune_identical(obj, orig)[[1]]
    modified[[nm]] <- pruned_obj
  }
  drop_empty(modified)
}




prune_identical <- function(x, y) {
  if(anyDuplicated(names(x)) || anyDuplicated(names(y)))
    stop("names can't be duplicated")

  # TODO: need to handle unnamed lists better both in the case of recursing past
  # them and calling prune_identical on unnamed list elements, as well as being
  # able to prune unnamed elements. A hacky unsatisfactory fix is to autoname
  # them with some cryptic name, like .__ag_autoname_id_01 then unnaming them
  # later.

  for (nm in intersect(names(x), names(y))) {
    if (identical(x[[nm]], y[[nm]]))
      x[[nm]] <-  y[[nm]] <- NULL
    else if (is.list(x[[nm]]) && is.list(y[[nm]])) {
      res <- prune_identical(x[[nm]], y[[nm]])
      x[[nm]] <- res[[1]]
      y[[nm]] <- res[[2]]
    }
  }
  list(x, y)
}

