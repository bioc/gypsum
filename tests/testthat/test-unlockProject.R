# This tests the latest refresher functions 
# library(testthat); library(gypsum); source("setup.R"); source("test-unlockProject.R")

test_that("project unlocking works as expected", {
    skip_if(is.na(gh_token))
    expect_error(unlockProject("test-R"), NA)
})
