library(testthat)
library(tfautograph)

# reticulate::use_virtualenv("tf1", TRUE)
# reticulate::use_virtualenv("tf2", TRUE)


message("Testing Against Tensorflow Version: ", tf$version$VERSION)

# test_dir("tests/testthat/")
test_check("tfautograph")
