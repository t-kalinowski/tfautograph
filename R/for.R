

ag_for <- function(var, iterable, body) {
  var  <- substitute(var)
  body <- substitute(body)
  env  <- parent.frame()

  ag_for_impl(iterable, var, body, env)
}


ag_for_impl <- function(iterable, var, body, env) UseMethod("ag_for_impl")

ag_for_impl.default <- function(iterable, var, body, env)
  eval(as.call(list(quote(.Primitive("for")), var, iterable, body)), env)







active_iterators <- Stack()

#' @importFrom reticulate iter_next
active_iter_assign_next <- function(sym) {
  val <- iter_next(active_iterators$peek())
  if (is.null(val))
    return(FALSE)
  assign(sym, val, envir = parent.frame())
  TRUE
}


#' @importFrom reticulate as_iterator
ag_for_impl.python.builtin.iterator <-
  function(iterable, var, body, env) {
    active_iterators$push(as_iterator(iterable))
    on.exit(active_iterators$pop())

    expr <- substitute(
      while (get("active_iter_assign_next", envir = asNamespace("tfautograph"), inherits = FALSE)(var)) body,
      list(var = as.character(var), body = body))

    eval(expr, env)
  }


## Think about more directly catching the StopIteraion exception.
## pro: would be slightly more robust
## con: would slightly slower because `iter_next()` catches it in C++
## for now going with `iter_next()`. Here are the code snippets in case you
## change your mind later:
# expr <- substitute(
#   while (tfautograph:::did_not_raise_StopIteration(
#     var <- tfautograph:::active_iterator_get_next()))
#   body, list(var = var, body = body))

# did_not_raise_StopIteration <- function(expr) {
#   tryCatch({
#     force(expr)
#     TRUE
#   },
#   error = function(e)
#     grepl("StopIteration", e))
# }
#
# # it <- peek_active_iterator()
# # it$`__next__`()





