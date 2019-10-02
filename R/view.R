#' Visualizes the generated graph
#'
#' @param fn TensorFlow function (returned from `tf.function()`)
#' @param args arguments passed to `fun`
#' @param ... other arguments passed to [tensorflow::tensorboard()]
#' @param name string, provided to tensorboard
#' @param profiler logical, passed on to `tf.compat.v2.summary.trace_on()` (only
#'   used in eager mode)
#' @param concrete_fn a `ConcreteFunction` (only used in graph mode, ignored
#'   with a warning if executing eagerly)
#' @param graph a tensorflow graph (only used in graph mode, ignored with a
#'   warning if executing eagerly)
#'
#' @export
#' @examples
#' \dontrun{
#' fn <- tf_function(function(x) autograph(if(x > 0) x * x else x))
#' view_function_graph(fn, list(tf$constant(5)))
#' }
view_function_graph <- function(fn, args, ...,
                                name = deparse(substitute(fn)),
                                profiler=FALSE,
                                concrete_fn = do.call(fn$get_concrete_fn, args),
                                graph = concrete_fn$graph
                                ) {

  logdir <- tempfile(pattern = "tflogdir")
  if (tf$executing_eagerly()) {
    stopifnot(inherits(fn, "tensorflow.python.eager.def_function.Function"))
    if(!missing(concrete_fn) || !missing(graph))
      warning("`concrete_fn` and `graph` ignored if `tf$executing_eagerly == TRUE`")
    writer <- tf$summary$create_file_writer(logdir)

    tf$compat$v2$summary$trace_on(graph = TRUE, profiler = profiler)
    do.call(fn, args)

    with(writer$as_default(), {
      tf$summary$trace_export(
        name = name,
        step = 0L,
        profiler_outdir = logdir
      )
    })

  } else {
    tf$compat$v1$summary$FileWriter(logdir, graph = graph)
    names(logdir) <- name
  }

  tensorflow::tensorboard(log_dir = logdir, ..., reload_interval = 0L)
}

