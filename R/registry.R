

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


.registries$while_opts <- new.env(parent = emptyenv())

register_next_while_loop_vars <- function(x) {
  .registries$while_opts$next_vars <- x
}

get_registered_next_while_loop_vars <- function() {
  on.exit(.registries$while_opts$next_vars <- NULL)
  .registries$while_opts$next_vars
}


register_next_while_loop_opts <- function(args) {
  .registries$while_opts$next_opts <- args
}

get_registered_next_while_loop_opts <- function() {
  on.exit(.registries$while_opts$next_opts <- NULL)
  .registries$while_opts$next_opts
}



get_next_ag_name <- function() {
  on.exit(.registries$next_ag_name <- NULL)
  .registries$next_ag_name
}

register_next_ag_name <- function(nm) {
  .registries$next_ag_name <- nm
}

