
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tfautograph

# :construction: **Under Construction** :construction:

<!-- badges: start -->

<!-- badges: end -->

This package implements autograph for R. It can be used to translate R
control flow statements like `if` into tensorflow graphs.

Checkout a work-in-progress draft of the [intro
vignette](https://t-kalinowski.github.io/tfautograph/articles/tfautograph.html)

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

  - \[x\] full compatibility with both tf versions 1.15 and 2.0
  - \[x\] full compatibility with both eager and graph mode
  - \[x\] autograph inline expressions also, in addition to functions
  - \[x\] nice informative error messages warning about undefined
    symbols and unbalanced branches
  - \[x\] a way to pass through additional options to `tf.while_loop`
  - \[ \] R function documentation (in progress)
  - \[ \] vignette / README (in progress)
  - \[ \] a verbose/debug mode that logs what autograph is doing
  - \[ \] submit to CRAN

Planned for CRAN release \#2

  - \[ \] early `return` in `while` and `for`
  - \[ \] autograph `if ... else if ... else if` chains into `tf.case`
  - \[ \] an ergonomic API around `TensorArray`s and `TensorList`s
  - \[ \] `recursive` support

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("t-kalinowski/tfautograph")
```
