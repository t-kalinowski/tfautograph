#' Visualizes the generated graph
#'
#' @param fun TensorFlow function
#' @param args arguments passed to `fun`
#' @param ... other arguments passed to [tensorflow::tensorboard()]
#'
#' @export
view_function_graph <- function(fn, args, ...) {

  if (!tf$executing_eagerly())
    stop("Eager execution is required.")

  logdir <- tempfile(pattern = "tflogdir")
  writer <- tf$summary$create_file_writer(logdir)

  # make sure fn is autographed
  if(!is_autographed(fn))
    fn <- autograph(fn)

  # enable tracing
  tf$summary$trace_on(graph=TRUE, profiler=TRUE)

  do.call(fn, args)

  # write the graph and profiling
  with(writer$as_default(), {
    tensorflow::tf$summary$trace_export(
      name = "function",
      step = 0L,
      profiler_outdir = logdir
    )
  })

  # launch tensorboard
  tensorflow::tensorboard(log_dir = logdir, ...)
}

