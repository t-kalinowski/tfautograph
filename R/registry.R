

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



.registries$control_dependency_ops_registries <- new.env(parent = emptyenv())

register_control_dependency_ops <- function(ops, env) {
  env_id <- format(env)

  registry <- .registries$control_dependency_ops_registries[[env_id]]
  if (is.null(registry))
    registry <-
    .registries$control_dependency_ops_registries[[env_id]] <-
    new.env(parent = emptyenv())

  if(!is.list(ops))
    ops <- list(ops)

  for(op in ops)
    registry[[op$name]] <- op
}

capture_registered_control_dependency_ops <- function(x, clear = TRUE) {
  env_id <- format(parent.frame())
  registry <- .registries$control_dependency_ops_registries[[env_id]]

  # browser()
  if(is.null(registry))
    stop("Could not find control dependency ops registry")

  if(clear)
    on.exit(rm(list = env_id, envir = .registries$control_dependency_ops_registries))

  ops <- unname(as.list(registry, all.names = TRUE))

  ## tf$identity and tf$identity_n can't be trusted to preserve shape
  # x <- list(as_tensor(1), as_tensor(2), as_tensor(3))
  # tf$identity_n(list(x))[[1]]
  #>># Tensor("IdentityN_7:0", shape=(3,), dtype=float32)

  with(tf$control_dependencies(ops),
       rapply(list(x), tf$identity, classes = "tensorflow.tensor", how = "replace"))[[1]]
  # TODO: what about user constructed python outputs set with convert = FALSE?
  # eg, tuple outputs like? `tuple(x, y, z)`
}

