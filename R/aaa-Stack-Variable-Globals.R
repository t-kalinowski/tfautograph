
Stack <- function(hash = TRUE, size = 29L) {
  stack <- new.env(parent = emptyenv(), hash = hash, size = size)

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
  }, envir = new.env(hash = FALSE))
}

length.Stack <- function(x)
  length(parent.env(x)$stack)

`[[.Stack` <- function(x, idx, ...)
  parent.env(x)$stack[[as.character(idx)]]

as.list.Stack <- function(x) {
  x <- parent.env(x)$stack
  unname(mget(as.character(seq_along(x)), envir = x))
}


Variable <- function() {
  VAL <- NULL
  structure(list2env(list(
    set = function(x) VAL <<- x,
    pop = function() {
      if (is.null(VAL)) return()
      on.exit(VAL <<- NULL)
      VAL
    },
    peek = function() VAL
  ),
  parent = emptyenv()), class = "Variable")
}



# set  <- function(x, val) x$set(val)
# push <- function(x, val) x$push(val)
# pop  <- function(x) x$pop()
# peek <- function(x) x$peek()
