library(testthat)
library(tfautograph)

# reticulate::use_virtualenv("tf1", TRUE)
# reticulate::use_virtualenv("tf2", TRUE)


if(identical(Sys.getenv("NOT_CRAN"), "true")) {
  if (reticulate::py_module_available("tensorflow")) {
    test_check("tfautograph")
  } else
    message("TensorFlow not available for testing")
} else
  message("Skipping tests on CRAN")
