
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tfautograph

<!-- badges: start -->

<!-- [![CRAN status](https://www.r-pkg.org/badges/version/tfautograph)](https://CRAN.R-project.org/package=tfautograph) -->

<!-- badges: end -->

This package implements autograph for R.

It lets you use tensors in R control flow expressions like `if`, `while`
and `for` and automatically translates those expressions into tensorflow
graphs.

It is compatible with and tested against Tensorflow Versions 2.0, 1.15
and 1.14.

# Learn More:

Start here:

  - [Getting
    Started](https://t-kalinowski.github.io/tfautograph/articles/tfautograph.html)

## Demos

  - Full [MNIST training
    loop](https://t-kalinowski.github.io/tfautograph/articles/demo-mnist.html)
    using `autograph` and Tensorflow 2.0

## Additional Articles:

  - Tensorflow Version 1 and control dependencies:
    [here](https://t-kalinowski.github.io/tfautograph/articles/tf-v1.html)
  - Giving `autograph`
    [hints](https://t-kalinowski.github.io/tfautograph/articles/hints.html)

## Installation

tfautograph is not yet on CRAN (it will be soon\!)

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("t-kalinowski/tfautograph")
```
