# This tests the quota setter functions 
# library(testthat); library(gypsum); source("setup.R"); source("test-quota.R")

test_that("quota setting works as expected", {
    skip_if(is.na(gh_token))
    removeProject("test-R-quota", asset=NULL, version=NULL, token=gh_token)
    createProject("test-R-quota", owners="LTLA", token=gh_token)

    setQuota("test-R-quota",
        baseline=1234,
        growth=5678,
        year=2020,
        token=gh_token
    )

    quot <- fetchQuota("test-R-quota")
    expect_identical(quot$baseline, 1234L)
    expect_identical(quot$growth, 5678L)
    expect_identical(quot$year, 2020L)
})

test_that("usage refreshment works as expected", {
    skip_if(is.na(gh_token))

    # Well, I just created it, so I would hope it doesn't have anything in it!
    expect_identical(fetchUsage("test-R-quota"), 0L) 

    refreshed <- refreshUsage("test-R-quota")
    expect_identical(refreshed, 0L)
})
