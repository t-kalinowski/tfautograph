
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tfautograph

# :construction: **Under Construction** :construction:

<!-- badges: start -->

<!-- badges: end -->

This package implements autograph for R. It can be used to translate R
control flow statements like `if` into tensorflow graphs.

Implemented so far:

  - \[x\] `if`
  - \[x\] `while`
  - \[x\] `next` in `while` (python `continue`)
  - \[x\] `break` in `while`
  - \[x\] `for` with tensors
  - \[x\] `next/break` in `for` with tensors
  - \[x\] `for` with tf datasets
  - \[x\] `next/break` in `for` with tf datasets
  - \[x\] `stopifnot` (python `assert`)
  - \[x\] `switch` (autograph to `tf.switch_case`)
  - \[ \] `print`, `stop` and `warning` (these fall into the same
    category as ops that need to be registered as control dependencies.
    Currently exploring (perhaps) more elegant solutions than the
    approach current implemented in `stopifnot`)

Additional remaining tasks:

  - \[x\] autograph inline expressions also, in addition to functions
  - \[x\] nice informative error messages warning about undefined
    symbols and unbalanced branches
  - \[x\] a way to pass through additional options to `tf.while_loop`
  - \[ \] an escape hatch to prevent a specific statement from being
    autographed
  - \[ \] a verbose/debug mode that logs what autograph is doing
  - \[ \] R function documentation
  - \[ \] vignette / README
  - \[ \] submit to CRAN

Planned for CRAN release \#2

  - \[ \] early `return` in `while` and `for`
  - \[ \] autograph `if ... else if ... else if` chains into `tf.case`
  - \[ \] `recursive` support

# Quick Demo

Here is a full mnist training loop implemented in R using tfautograph.
(adapted from [here](https://www.tensorflow.org/beta/guide/autograph))

``` r
library(magrittr)
library(purrr, warn.conflicts = FALSE)

library(tensorflow)
library(tfdatasets)
library(keras)

library(tfautograph)

# All of tfautograph works in tf 1.14 also, but this readme expects 2.0.
tf$version$VERSION
#> [1] "2.0.0-beta1"
stopifnot(tf_version() >= "2")
```

### Download data

``` r
prepare_mnist_features_and_labels <- function(x, y) {
  x = tf$cast(x, tf$float32) / 255
  y = tf$cast(y, tf$int64)
  list(x, y)
}

mnist_dataset <- function() {
  c(c(x, y), .) %<-% tf$keras$datasets$mnist$load_data()
  tensor_slices_dataset(list(x, y)) %>%
    dataset_map(prepare_mnist_features_and_labels) %>%
    dataset_take(20000) %>%
    dataset_shuffle(20000) %>%
    dataset_batch(100)
}

train_dataset <- mnist_dataset()
```

### Define the model

``` r
new_model_and_optimizer <- function() {
  model <- keras_model_sequential() %>%
    layer_reshape(target_shape = c(28 * 28),
                  input_shape = shape(28, 28)) %>%
    layer_dense(100, activation = 'relu') %>%
    layer_dense(100, activation = 'relu') %>%
    layer_dense(10)
  model$build()
  optimizer <- tf$keras$optimizers$Adam()
  list(model, optimizer)
}
c(model, optimizer) %<-% new_model_and_optimizer()
```

### Define the training loop

``` r
compute_loss <- tf$keras$losses$SparseCategoricalCrossentropy(from_logits = TRUE)
compute_accuracy <- tf$keras$metrics$SparseCategoricalAccuracy()

train_one_step <- function(model, optimizer, x, y) {
  with(tf$GradientTape() %as% tape, {
    logits <- model(x)
    loss <- compute_loss(y, logits)
  })

  grads <- tape$gradient(loss, model$trainable_variables)
  optimizer$apply_gradients(
    transpose(list(grads, model$trainable_variables)))

  compute_accuracy(y, logits)
  loss
}

train <- autograph(function(model, optimizer) {
  train_ds <- mnist_dataset()
  step <- 0L
  loss <- 0
  ag_loop_vars("step", "loss") # to prevent `log_file` from being captured
  for (batch in train_ds) {
    c(x, y) %<-% batch
    step %<>% add(1L)
    loss <- train_one_step(model, optimizer, x, y)
    if (compute_accuracy$result() > 0.8) {
      tf$print( 'Step', step, ': loss', loss, '; accuracy', compute_accuracy$result(),
        output_stream = log_file)
      # We direct all tf$print() outputs to a log file only so that Rmarkdown can
      # show output that would otherwise just go to stdout
      tf$print("Breaking early", output_stream = log_file)
      break
    } else if (step %% 10L == 0L)
      tf$print('Step', step, ': loss', loss,
               '; accuracy', compute_accuracy$result(), 
               output_stream = log_file)
  }
  list(step, loss)
})
```

### train in graph mode

``` r

log_file <- sprintf("file://%s", tempfile("TF-print-log", fileext = ".out"))

train_graph <- tf_function(train)
c(step, loss) %<-% train_graph(model, optimizer)

cat(readLines(log_file), sep = "\n")
#> Step 10 : loss 1.72327757 ; accuracy 0.379
#> Step 20 : loss 1.21117306 ; accuracy 0.539
#> Step 30 : loss 0.701422334 ; accuracy 0.61833334
#> Step 40 : loss 0.479259223 ; accuracy 0.67325
#> Step 50 : loss 0.46734181 ; accuracy 0.708
#> Step 60 : loss 0.366834402 ; accuracy 0.7375
#> Step 70 : loss 0.233186334 ; accuracy 0.760428548
#> Step 80 : loss 0.360162765 ; accuracy 0.776375
#> Step 90 : loss 0.555048108 ; accuracy 0.788333356
#> Step 100 : loss 0.317718416 ; accuracy 0.7974
#> Step 103 : loss 0.244447649 ; accuracy 0.800679624
#> Breaking early
cat(sprintf(
  'Final step %i: loss %.6f; accuracy %.6f',
  as.array(step), as.array(loss), as.array(compute_accuracy$result())))
#> Final step 103: loss 0.244448; accuracy 0.800680
```

### train in eager mode

``` r
# autograph also works in eager mode

log_file <- sprintf("file://%s", tempfile("TF-print-log", fileext = ".out"))

c(model, optimizer) %<-% new_model_and_optimizer()
c(step, loss) %<-% train(model, optimizer)

cat(readLines(log_file), sep = "\n")
#> Step 10 : loss 1.75092924 ; accuracy 0.764336288
#> Step 20 : loss 1.0013392 ; accuracy 0.761951208
#> Step 30 : loss 0.8112607 ; accuracy 0.763909757
#> Step 40 : loss 0.592272818 ; accuracy 0.768531442
#> Step 50 : loss 0.367938846 ; accuracy 0.774183035
#> Step 60 : loss 0.431332469 ; accuracy 0.780122697
#> Step 70 : loss 0.308761865 ; accuracy 0.785722554
#> Step 80 : loss 0.283011258 ; accuracy 0.791967213
#> Step 90 : loss 0.238270551 ; accuracy 0.796528518
#> Step 97 : loss 0.433700532 ; accuracy 0.80025
#> Breaking early
cat(sprintf(
  'Final step %i: loss %.6f; accuracy %.6f',
  as.array(step), as.array(loss), as.array(compute_accuracy$result())))
#> Final step 97: loss 0.433701; accuracy 0.800250
```

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("t-kalinowski/tfautograph")
```
