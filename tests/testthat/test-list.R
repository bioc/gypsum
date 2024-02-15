# This tests the various listing functions.
# library(testthat); library(gypsum); source("setup.R"); source("test-list.R")

test_that("listVersions works as expected", {
    versions <- listVersions("test-R", "basic")
    expect_true("v1" %in% versions)
    expect_true("v2" %in% versions)
    expect_true("v3" %in% versions)
})

test_that("listAssets works as expected", {
    assets <- listAssets("test-R")
    expect_true("basic" %in% assets)
})

test_that("listProjects works as expected", {
    projects <- listProjects()
    expect_true("test-R" %in% projects)
})

test_that("listFiles works as expected", {
    in.basic <- listFiles("test-R", "basic", "v1")
    expect_identical(sort(in.basic), sort(c("..summary", "..manifest", "blah.txt", "foo/bar.txt")))

    in.basic <- listFiles("test-R", "basic", "v2")
    expect_identical(sort(in.basic), sort(c("..summary", "..manifest", "..links", "foo/..links")))

    in.basic <- listFiles("test-R", "basic", "v1", prefix="foo/")
    expect_identical(sort(in.basic), sort(c("foo/bar.txt")))

    in.basic <- listFiles("test-R", "basic", "v1", prefix="..")
    expect_identical(sort(in.basic), sort(c("..summary", "..manifest")))
})
