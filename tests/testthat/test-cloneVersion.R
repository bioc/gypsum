# This checks the cloneVersion function.
# library(testthat); library(gypsum); source("test-cloneVersion.R")

test_that("cloneVersion works as expected with existing files", {
    cache <- tempfile()
    dest <- tempfile()
    out <- cloneVersion("test-R", "basic", "v1", destination=dest, cache=cache)

    l1 <- Sys.readlink(file.path(dest, "blah.txt"))
    expect_true(endsWith(l1, "test-R/basic/v1/blah.txt"))
    expect_true(file.exists(l1))

    l2 <- Sys.readlink(file.path(dest, "foo/bar.txt"))
    expect_true(endsWith(l2, "test-R/basic/v1/foo/bar.txt"))
    expect_true(file.exists(l2))
})

test_that("cloneVersion happily works without any download", {
    cache <- tempfile()
    dest <- tempfile()
    out <- cloneVersion("test-R", "basic", "v1", download=FALSE, destination=dest, cache=cache)

    l1 <- Sys.readlink(file.path(dest, "blah.txt"))
    expect_true(endsWith(l1, "test-R/basic/v1/blah.txt"))
    expect_false(file.exists(l1))

    l2 <- Sys.readlink(file.path(dest, "foo/bar.txt"))
    expect_true(endsWith(l2, "test-R/basic/v1/foo/bar.txt"))
    expect_false(file.exists(l2))
})

test_that("cloneVersion works correctly if version itself contains links", {
    cache <- tempfile()
    dest <- tempfile()
    out <- cloneVersion("test-R", "basic", "v2", destination=dest, cache=cache, download=FALSE, relink=FALSE)

    l1 <- Sys.readlink(file.path(dest, "blah.txt"))
    expect_true(endsWith(l1, "test-R/basic/v2/blah.txt"))
    expect_false(file.exists(l1))

    l2 <- Sys.readlink(file.path(dest, "foo/bar.txt"))
    expect_true(endsWith(l2, "test-R/basic/v2/foo/bar.txt"))
    expect_false(file.exists(l2))
})
