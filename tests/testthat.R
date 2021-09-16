library(testthat)
library(tfautograph)

# reticulate::use_virtualenv("tf1", TRUE)
# reticulate::use_virtualenv("tf2", TRUE)


if (reticulate::py_module_available("tensorflow")) {
  print("HI")

  test_check("tfautograph")
} else
  message("TensorFlow not available for testing")
