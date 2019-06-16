



undefined <- structure(list(), class = "undefined")

undefined_mold <- function(x) {
  out <- rep(list(undefined), length(x))
  names(out) <- x
  out
}

is_undefined <- function(x) inherits(x, "undefined")


find_undefined <- function(...) {
  y <- lapply(list(...), function(env) {
    x <- unlist(eapply(env, is_undefined, all.names = TRUE))
    names(x[x])
  })
  unique(unlist(y))
}

