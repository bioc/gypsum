# This tests the probation approval functions. 
# library(testthat); library(gypsum); source("setup.R"); source("test-probation.R")

test_that('probation approval works as expected', {
    skip_if(is.na(gh_token))
    removeProject("test-R", "probation", version=NULL, token=gh_token)

    # Mocking up a versioned asset.
    init <- startUpload(
        project="test-R", 
        asset="probation", 
        version="v1", 
        files=character(0),
        probation=TRUE,
        token=gh_token
    )
    completeUpload(init)

    summ <- fetchSummary("test-R", "probation", "v1", cache=NULL)
    expect_true(summ$on_probation)

    approveProbation("test-R", "probation", "v1", token=gh_token)
    summ <- fetchSummary("test-R", "probation", "v1", cache=NULL)
    expect_null(summ$on_probation)
})

test_that('probation rejection works as expected', {
    skip_if(is.na(gh_token))
    removeProject("test-R", "probation", version=NULL, token=gh_token)

    # Mocking up a versioned asset.
    init <- startUpload(
        project="test-R", 
        asset="probation", 
        version="v1", 
        files=character(0),
        probation=TRUE,
        token=gh_token
    )
    completeUpload(init)

    summ <- fetchSummary("test-R", "probation", "v1", cache=NULL)
    expect_true(summ$on_probation)

    rejectProbation("test-R", "probation", "v1", token=gh_token)
    expect_error(fetchSummary("test-R", "probation", "v1", cache=NULL), "does not exist")
})
