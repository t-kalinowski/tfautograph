
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tfautograph

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tfautograph)](https://CRAN.R-project.org/package=tfautograph)
[![Travis build
status](https://travis-ci.com/t-kalinowski/tfautograph.svg?branch=master)](https://travis-ci.com/t-kalinowski/tfautograph)
<!-- badges: end -->

This package implements autograph for R.

It lets you use tensors in R control flow expressions like `if`, `while`
and `for` and automatically translates those expressions into tensorflow
graphs.

It is compatible with and tested against all Tensorflow versions \>=
1.15 (including versions 2.x).

## Learn More:

  - [Getting
    Started](https://t-kalinowski.github.io/tfautograph/articles/tfautograph.html)

## Demos

  - Full [MNIST training
    loop](https://t-kalinowski.github.io/tfautograph/articles/demo-mnist.html)
    using `autograph` and Tensorflow 2.

## Additional Articles:

  - Control dependencies in [Tensorflow
    version 1](https://t-kalinowski.github.io/tfautograph/articles/tf-v1.html)
  - Giving `autograph`
    [hints](https://t-kalinowski.github.io/tfautograph/articles/hints.html)

## Installation

You can install tfautograph from CRAN with:

``` r
install.packages("tfautograph")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("t-kalinowski/tfautograph")
```
