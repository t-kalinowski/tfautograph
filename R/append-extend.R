

#' @export
`append<-` <- function(x, value) {
  x[[length(x) + 1L]] <- value
  x
}


#' @export
`extend<-` <- function(x, value) {
  x[seq.int(from = length(x) + 1L, along.with = value)] <- value
  x
}

