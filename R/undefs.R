


undefined_vars_condition <- function(undefs, env = NULL) {
  cat("Undefs:", paste(undefs, collapse = ", "), "\n")
}


make_active_undefs <- function(undefs, env, call) {
  force(call)

  lapply(undefs, function(undef)  {
    makeActiveBinding(
      undef,
      function() stop(access_undefined_condition(undef, call)),
      env)
  })
}


access_undefined_condition <- function(sym, call) {
  type <- deparse(call[[1]])

  pretty_call <- deparse(call)
  if (length(pretty_call) > 6) {
    pretty_call <- pretty_call[1:6]
    pretty_call[6] <- paste(pretty_call[6], "...")
  }
  pretty_call <- paste0("\t", pretty_call, collapse = "\n")

  msg <- paste0(
    sprintf(
      "Symbol `%s` is *undefined* after the expression:\n %s\n", sym, pretty_call),
    "To access this symbol, Tensorflow requires that the symbol must be defined ",
    switch(type,
      "if" = "either before the `if` statement, or in all branches of the `if` statement",
      "for" = , "while" = "before the loop body"
    )
  )

  structure(class = c("access_undefined", "error", "condition"),
            list(message = msg, call = call))
}

