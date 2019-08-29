
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
  - \[x\] `for` with tensors
  - \[x\] `for` with tf datasets
  - \[x\] `next` and `break` in `while`
  - \[x\] `next` and `break` in `for` with tensors
  - \[x\] `next` and `break` in `for` with tf datasets
  - \[x\] `switch` (autograph to `tf.switch_case`)
  - \[x\] `stopifnot` (python `assert`); translates to `tf.Assert()`
    (mostly useful for tensorflow v1 code)

Additional remaining tasks:

  - \[x\] full compatability with both tf versions 1.14 and 2.0
  - \[x\] full compatbility with both eager and graph mode
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
#> [1] "2.0.0-rc0"
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
  ag_loop_vars("step", "loss") # to prevent `log_file` from being captured
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
#> Step 10 : loss 1.86508906 ; accuracy 0.355
#> Step 20 : loss 1.23046172 ; accuracy 0.525
#> Step 30 : loss 0.752179921 ; accuracy 0.609333336
#> Step 40 : loss 0.689547718 ; accuracy 0.6615
#> Step 50 : loss 0.440382183 ; accuracy 0.6996
#> Step 60 : loss 0.460933238 ; accuracy 0.724166691
#> Step 70 : loss 0.327962488 ; accuracy 0.747
#> Step 80 : loss 0.393846691 ; accuracy 0.76525
#> Step 90 : loss 0.376492083 ; accuracy 0.77922225
#> Step 100 : loss 0.406526923 ; accuracy 0.7925
#> Step 108 : loss 0.343511343 ; accuracy 0.800092578
#> Breaking early
cat(sprintf(
  'Final step %i: loss %.6f; accuracy %.6f',
  as.array(step), as.array(loss), as.array(compute_accuracy$result())))
#> Final step 108: loss 0.343511; accuracy 0.800093
```

### train in eager mode

``` r
# autograph also works in eager mode

log_file <- sprintf("file://%s", tempfile("TF-print-log", fileext = ".out"))

c(model, optimizer) %<-% new_model_and_optimizer()
c(step, loss) %<-% train(model, optimizer)

cat(readLines(log_file), sep = "\n")
#> Step 10 : loss 1.74904215 ; accuracy 0.761694908
#> Step 20 : loss 1.13440621 ; accuracy 0.754531264
#> Step 30 : loss 0.737210751 ; accuracy 0.75557971
#> Step 40 : loss 0.57017678 ; accuracy 0.761283755
#> Step 50 : loss 0.486070752 ; accuracy 0.765126586
#> Step 60 : loss 0.575892091 ; accuracy 0.772142828
#> Step 70 : loss 0.327947319 ; accuracy 0.77831459
#> Step 80 : loss 0.400503427 ; accuracy 0.784680843
#> Step 90 : loss 0.419900507 ; accuracy 0.789697
#> Step 100 : loss 0.278181255 ; accuracy 0.794567287
#> Step 110 : loss 0.508931816 ; accuracy 0.799633
#> Step 111 : loss 0.352785826 ; accuracy 0.800182641
#> Breaking early
cat(sprintf(
  'Final step %i: loss %.6f; accuracy %.6f',
  as.array(step), as.array(loss), as.array(compute_accuracy$result())))
#> Final step 111: loss 0.352786; accuracy 0.800183
```

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("t-kalinowski/tfautograph")
```
