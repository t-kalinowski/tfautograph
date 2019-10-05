library(testthat)
library(tfautograph)

# reticulate::use_virtualenv("tf1", TRUE)
# reticulate::use_virtualenv("tf2", TRUE)


if (reticulate::py_module_available("tensorflow")) {
  print("HI")

  if (!exists(".DID_EMIT_TF_VERSION")) {
    message("Testing Against Tensorflow Version: ",
            tensorflow::tf$version$VERSION)
    .DID_EMIT_TF_VERSION <- TRUE
  }
  test_check("tfautograph")
} else
  message("TensorFlow not available for testing")
