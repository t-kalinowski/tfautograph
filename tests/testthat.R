library(testthat)
library(tfautograph)

# reticulate::use_virtualenv("tf1-rc", TRUE)
# reticulate::use_virtualenv("tf2-nightly", TRUE)


message("Testing Against Tensorflow Version: ", tf$version$VERSION)

test_check("tfautograph")
