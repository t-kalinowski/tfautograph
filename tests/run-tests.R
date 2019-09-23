#! /usr/bin/Rscript --vanilla

devtools::install(quick = TRUE)

run_tst_cmd <- function(virtualenv) {
  paste(
    "Rscript --vanilla -e",
    "'",
    "library(testthat);",
    "library(tfautograph);",
    sprintf('reticulate::use_virtualenv("%s", TRUE);', virtualenv),
    'message("Testing Against Tensorflow Version: ", tensorflow::tf$version$VERSION);',
    'testthat::test_dir("tests/testthat/")',
    "'"
  )
}


system(run_tst_cmd("tf1-rc"))
system(run_tst_cmd("tf2-nightly"))

# reticulate::virtualenv_install("tf2-nightly", "tf-nightly-2.0-preview")
