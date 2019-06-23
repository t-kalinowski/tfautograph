

.registries <- new.env(parent = emptyenv())
.registries$cond_registries <- Stack()


new_cond_registry <- function() {
  registry <- new.env(parent = emptyenv())
  registry$true <- new.env(parent = emptyenv())
  registry$false <- new.env(parent = emptyenv())
  registry
}

establish_cond_registry <- function() {
  registry <- new_cond_registry()
  .registries$cond_registries$push(registry)
}

remove_cond_registry <- function() {
  .registries$cond_registries$pop()
}

get_active_cond_registry <- function() {
  .registries$cond_registries$peek()
}


register_cond <- function(cond, branch, registry = get_active_cond_registry()) {
  if(!is_cond_registry_established()) return()

  stopifnot(is_tensor(cond), is_TRUE_or_FALSE(branch))
  deregister_cond(cond, registry)
  branch <- if(branch) "true" else "false"
  registry[[branch]][[cond$name]] <- cond
}

is_cond_registry_established <- function() {
  as.logical(length(.registries$cond_registries))
}

deregister_cond <- function(cond, registry = get_active_cond_registry()) {
  if(!is_cond_registry_established()) return()
  for(branch in c("true", "false"))
    if(exists(cond$name, registry[[branch]]))
      rm(list = cond$name, envir = registry[[branch]])
}

reduce_registered_conds <- function(registry = get_active_cond_registry()) {
  conds <- c(as.list(registry$true), lapply(registry$false, `!`))
  Reduce(`&`, conds)
}


