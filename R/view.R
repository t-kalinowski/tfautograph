#' Visualizes the generated graph
#'
#' @param fun TensorFlow function
#' @param args arguments passed to `fun`
#' @param ... other arguments passed to [tensorflow::tensorboard()]
#'
#' @keywords internal
#'
view_function_graph <- function(fn, args, ...) {

  if (!tf$executing_eagerly())
    stop("Eager execution is required.")

  logdir <- tempfile(pattern = "tflogdir")
  writer <- tf$summary$create_file_writer(logdir)

  ## Needs a rethink. what is fn already had `tf.function()` called on it?
  # if(!is_autographed(fn))
    # fn <- tf$`function`(autograph(fn), autograph = FALSE)

  # enable tracing
  tf$compat$v2$summary$trace_on(graph=TRUE, profiler=TRUE)

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

