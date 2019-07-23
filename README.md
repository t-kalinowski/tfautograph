
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
  - \[ \] `print`
  - \[ \] `switch` (autograph to `tf.switch_case`)
  - \[ \] early `return` in `while` and `for` (maybe for v2)

Additional remaining tasks:

  - \[ \] `if ... else if ... else if` chains autograph into `tf.case`
    (maybe)
  - \[x\] autograph inline expressions also, in addition to functions
  - \[x\] nice informative error messages warning about undefined
    symbols and unbalanced branches
  - \[ \] a way to pass through additional options to `tf.while_loop`
    and others
  - \[ \] an escape hatch to prevent a specific statement from being
    autographed
  - \[ \] a verbose/debug mode that logs what autograph is doing
  - \[ \] R function documentation
  - \[ \] vignette / README
  - \[ \] submit to CRAN

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("t-kalinowski/tfautograph")
```
