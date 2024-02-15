# This tests the removal functions 
# library(testthat); library(gypsum); source("setup.R"); source("test-remove.R")

test_that("removal functions work as expected", {
    skip_if(is.na(gh_token))
    removeProject("test-R-remove", token=gh_token)

    createProject("test-R-remove", owners="LTLA", token=gh_token)
    for (v in c("v1", "v2")) {
        init <- startUpload(
            project="test-R-remove", 
            asset="sacrifice", 
            version=v, 
            files=character(0),
            token=gh_token
        )
        completeUpload(init)
    }

    expect_error(fetchManifest("test-R-remove", "sacrifice", "v2", cache=NULL), NA)
    removeVersion("test-R-remove", "sacrifice", "v2", token=gh_token)
    expect_error(fetchManifest("test-R-remove", "sacrifice", "v2", cache=NULL), "does not exist")

    expect_identical(fetchLatest("test-R-remove", "sacrifice"), "v1")
    removeAsset("test-R-remove", "sacrifice", token=gh_token)
    expect_error(fetchLatest("test-R-remove", "sacrifice"), "does not exist")

    expect_error(fetchUsage("test-R-remove"), NA)
    removeProject("test-R-remove", token=gh_token)
    expect_error(fetchUsage("test-R-remove"), "does not exist")
})
