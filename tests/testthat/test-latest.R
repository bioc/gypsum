# This tests the latest refresher functions 
# library(testthat); library(gypsum); source("setup.R"); source("test-latest.R")

test_that("latest setting works as expected", {
    skip_if(is.na(gh_token))
    v <- refreshLatest("test-R", "basic")
    expect_identical(v, "v3")
    expect_identical(fetchLatest("test-R", "basic"), v)
})
