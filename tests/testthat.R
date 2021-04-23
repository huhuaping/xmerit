Sys.setenv("R_TESTS" = "")
library(testthat)
library(xmerit)

test_check("xmerit")
