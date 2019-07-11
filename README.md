
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
  - \[x\] `for` with tf datasets
  - \[ \] `for` with tf iterators
  - \[x\] `next` and `break` in `for`
  - \[x\] `stopifnot` (python `assert`)
  - \[ \] early `return` in `while` and `for`
  - \[ \] `print`
  - \[ \] `switch` (autograph to `tf.switch_case`)

Additional remaining tasks:

  - \[ \] `if ... else if ... else if` chains autograph into `tf.case`
    (maybe)
  - \[ \] nicer error messages warning about undefined symbols and
    unbalanced branches
  - \[ \] autograph inline expressions (only functions suported at the moment)
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
