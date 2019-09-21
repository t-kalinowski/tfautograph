
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tfautograph

# :construction: **Under Construction** :construction:

<!-- badges: start -->

<!-- badges: end -->

This package implements autograph for R. It can be used to translate R
control flow statements like `if` into tensorflow graphs.

Checkout a work-in-progress draft of the [intro
vignette](https://t-kalinowski.github.io/tfautograph/articles/autograph-basics.html)

Implemented so far:

  - \[x\] `if`
  - \[x\] `while`
  - \[x\] `for` with tensors
  - \[x\] `for` with tf datasets
  - \[x\] `next` and `break` in `while`
  - \[x\] `next` and `break` in `for` with tensors
  - \[x\] `next` and `break` in `for` with tf datasets
  - \[x\] `switch` (autograph to `tf.switch_case`)
  - \[x\] `stopifnot` (python `assert`); translates to `tf.Assert()`
    (mostly useful for tensorflow v1 code)

Additional remaining tasks:

  - \[x\] full compatability with both tf versions 1.15 and 2.0
  - \[x\] full compatbility with both eager and graph mode
  - \[x\] autograph inline expressions also, in addition to functions
  - \[x\] nice informative error messages warning about undefined
    symbols and unbalanced branches
  - \[x\] a way to pass through additional options to `tf.while_loop`
  - \[ \] R function documentation (in progress)
  - \[ \] vignette / README (in progress)
  - \[ \] an escape hatch to prevent a specific statement from being
    autographed
  - \[ \] a verbose/debug mode that logs what autograph is doing
  - \[ \] submit to CRAN

Planned for CRAN release \#2

  - \[ \] early `return` in `while` and `for`
  - \[ \] autograph `if ... else if ... else if` chains into `tf.case`
  - \[ \] an ergonomic API around `TensorArray`s and `TensorList`s
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
#> [1] "2.0.0-dev20190919"
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
  step <- 0L
  loss <- 0
  for (batch in train_dataset) {
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
#> Step 10 : loss 1.73541915 ; accuracy 0.364
#> Step 20 : loss 1.05687034 ; accuracy 0.5255
#> Step 30 : loss 0.690594316 ; accuracy 0.607
#> Step 40 : loss 0.64093256 ; accuracy 0.663
#> Step 50 : loss 0.402439117 ; accuracy 0.7058
#> Step 60 : loss 0.466928661 ; accuracy 0.733666658
#> Step 70 : loss 0.537537932 ; accuracy 0.755571425
#> Step 80 : loss 0.418009192 ; accuracy 0.773
#> Step 90 : loss 0.314459115 ; accuracy 0.787555575
#> Step 100 : loss 0.344970822 ; accuracy 0.7979
#> Step 102 : loss 0.275335193 ; accuracy 0.80058825
#> Breaking early
cat(sprintf(
  'Final step %i: loss %.6f; accuracy %.6f',
  as.array(step), as.array(loss), as.array(compute_accuracy$result())))
#> Final step 102: loss 0.275335; accuracy 0.800588
```

### train in eager mode

``` r
# autograph also works in eager mode

log_file <- sprintf("file://%s", tempfile("TF-print-log", fileext = ".out"))

c(model, optimizer) %<-% new_model_and_optimizer()
c(step, loss) %<-% train(model, optimizer)

cat(readLines(log_file), sep = "\n")
#> Step 10 : loss 1.84928226 ; accuracy 0.75401783
#> Step 20 : loss 1.23407435 ; accuracy 0.743852437
#> Step 30 : loss 0.804253817 ; accuracy 0.74606061
#> Step 40 : loss 0.652474582 ; accuracy 0.750845075
#> Step 50 : loss 0.434434742 ; accuracy 0.757894754
#> Step 60 : loss 0.636063337 ; accuracy 0.76432097
#> Step 70 : loss 0.436043233 ; accuracy 0.770406961
#> Step 80 : loss 0.428212434 ; accuracy 0.776758254
#> Step 90 : loss 0.50538522 ; accuracy 0.782656252
#> Step 100 : loss 0.299005717 ; accuracy 0.788316846
#> Step 110 : loss 0.216239676 ; accuracy 0.793207526
#> Step 120 : loss 0.218401521 ; accuracy 0.798378408
#> Step 124 : loss 0.356895983 ; accuracy 0.80053097
#> Breaking early
cat(sprintf(
  'Final step %i: loss %.6f; accuracy %.6f',
  as.array(step), as.array(loss), as.array(compute_accuracy$result())))
#> Final step 124: loss 0.356896; accuracy 0.800531
```

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("t-kalinowski/tfautograph")
```
