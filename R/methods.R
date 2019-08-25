


# ta <- tf$TensorArray(tf$float32, size = 1L, dynamic_size = TRUE)

#' @export
`[<-.tensorflow.python.ops.tensor_array_ops.TensorArray` <-
  function(ta, i, value, ..., name = NULL) {
    ta$write(tf$cast(i, tf$int32), value, name = name)
  }

# train_losses     %<>% {.$write(i, step_train_loss)}
# test_losses      %<>% {.$write(i, step_test_loss)}
# train_accuracies %<>% {.$write(i, step_train_accuracy)}
# test_accuracies  %<>% {.$write(i, step_test_accuracy)}
