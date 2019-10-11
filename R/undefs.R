
new_undef_fn <- function(sym, call) {
  force(sym)
  force(call)
  fn <- function(x) {
    if (missing(x))
      stop(access_undefined_condition(sym, call))
    else {
      env <- parent.frame()
      remove(list = sym, envir = env)
      assign(sym, value = x, envir = env)
    }
  }
  structure(fn, class = "undef")
}



is_undef <- function(x, env = NULL, inherits = TRUE) {
  if (is.null(env))
    inherits(x, "undef")
  else {
    tryCatch({
      get0(x, envir = env, inherits = inherits)
      FALSE
    },
    access_undefined = function(e) TRUE)
  }
}

export_undef <- function(leaf_nm, env, call) {
  if(length(leaf_nm) == 1) {
    if(exists(leaf_nm, envir = env, inherits = FALSE))
      rm(list = leaf_nm, envir = env)
    makeActiveBinding(leaf_nm, new_undef_fn(leaf_nm, call), env)
  }
  else {
    x <- get(leaf_nm[1], env)
    x[[leaf_nm[-1]]] <- new_undef_fn(paste0(leaf_nm, collapse = "$"), call)
    env[[leaf_nm[1]]] <- x
  }
}

export_undefs <- function(undefs, env, call = sys.call(-1)) {
  lapply(undefs, function(leaf_nm) export_undef(leaf_nm, env, call))
}

# TODO: add more methods that throw errors for `undef` S3 class (Math Generics,
# print, etc)

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
      "Symbol `%s` is *undefined* after autographing the expression:\n %s\n",
      sym, pretty_call),
    "To access this symbol, Tensorflow requires that an object with the same dtype and shape be assigned to the symbol ",
    switch(type,
      "if" = "either before the `if` statement, or in all branches of the `if` statement",
      "for" = , "while" = "before the loop body"
    )
  )

  structure(class = c("access_undefined", "error", "condition"),
            list(message = msg, call = call))
}


