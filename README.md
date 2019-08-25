
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
model <- keras_model_sequential() %>%
  layer_reshape(target_shape = c(28 * 28),
                input_shape = shape(28, 28)) %>%
  layer_dense(100, activation = 'relu') %>%
  layer_dense(100, activation = 'relu') %>%
  layer_dense(10)
model$build()
optimizer <- tf$keras$optimizers$Adam() 
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

# this is only so that Rmarkdown can capture output that would otherwise go to
# stdout
log_file <- sprintf("file://%s", tempfile("TF-print-log", fileext = ".out"))

train <- tf_function(autograph(function(model, optimizer) {
  train_ds <- mnist_dataset()
  step <- 0L
  loss <- 0
  ag_loop_vars("step", "loss") # to prevent `log_file` from being captured
  for (batch in train_ds) {
    c(x, y) %<-% batch
    step %<>% add(1L)
    loss <- train_one_step(model, optimizer, x, y)
    if (step %% 10L == 0L)
      tf$print('Step', step, ': loss', loss,
               '; accuracy', compute_accuracy$result(), 
               output_stream = log_file)
  }
  list(step, loss)
}))


c(step, loss) %<-% train(model, optimizer)
cat(readLines(log_file), sep = "\n")
#> Step 10 : loss 1.77995944 ; accuracy 0.363
#> Step 20 : loss 1.06181931 ; accuracy 0.518
#> Step 30 : loss 0.766562581 ; accuracy 0.602333307
#> Step 40 : loss 0.539229751 ; accuracy 0.6585
#> Step 50 : loss 0.51139605 ; accuracy 0.6956
#> Step 60 : loss 0.495062023 ; accuracy 0.7225
#> Step 70 : loss 0.538499296 ; accuracy 0.745428562
#> Step 80 : loss 0.444634974 ; accuracy 0.763
#> Step 90 : loss 0.603235066 ; accuracy 0.776666641
#> Step 100 : loss 0.31680581 ; accuracy 0.7882
#> Step 110 : loss 0.233325675 ; accuracy 0.797818184
#> Step 120 : loss 0.388830751 ; accuracy 0.805083334
#> Step 130 : loss 0.281395674 ; accuracy 0.812846124
#> Step 140 : loss 0.349985242 ; accuracy 0.819285691
#> Step 150 : loss 0.308940768 ; accuracy 0.8252
#> Step 160 : loss 0.309595823 ; accuracy 0.829812527
#> Step 170 : loss 0.355459481 ; accuracy 0.834823549
#> Step 180 : loss 0.2305246 ; accuracy 0.839
#> Step 190 : loss 0.357420385 ; accuracy 0.84331578
#> Step 200 : loss 0.17443186 ; accuracy 0.84705
cat('Final step ', as.array(step),
  ': loss ', as.array(loss),
  '; accuracy ', as.array(compute_accuracy$result()), "\n", sep = "")
#> Final step 200: loss 0.1744319; accuracy 0.84705
```

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("t-kalinowski/tfautograph")
```
