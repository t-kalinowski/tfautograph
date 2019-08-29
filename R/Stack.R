
Stack <- function() {
  stack <- new.env(parent = emptyenv())

  local({
    push <- function(x) {
      stack[[as.character(length(stack) + 1L)]] <- x
      invisible(x)
    }

    pop <- function() {
      i <- as.character(length(stack))
      on.exit(rm(list = i, envir = stack))
      stack[[i]]
    }

    peek <- function() {
      stack[[as.character(length(stack))]]
    }

    structure(environment(), class = "Stack")
  })
}

length.Stack <- function(x)
  length(parent.env(x)$stack)

`[[.Stack` <- function(x, idx, ...) {
  parent.env(x)$stack[[as.character(idx)]]
}

as.list.Stack <- function(x) {
  x <- parent.env(x)$stack
  unname(mget(as.character(seq_along(x)), envir = x))
}
