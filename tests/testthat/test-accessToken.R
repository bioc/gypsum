# This tests that the GitHub token is correctly set.
# library(testthat); library(gypsum); source("setup.R"); source("test-accessToken.R")

test_that("setting the token works as expected", {
    skip_if(is.na(gh_token))

    out <- setAccessToken(gh_token, cache=NULL)
    expect_identical(out$token, gh_token)
    expect_identical(accessToken(cache=NULL), gh_token)

    # Works when we cache it to disk.
    cache <- tempfile()
    out <- setAccessToken(gh_token, cache=cache)
    expect_identical(out$token, gh_token)
    expect_identical(accessToken(cache=cache), gh_token)
    tokpath <- file.path(cache, "credentials", "token.txt")
    expect_identical(readLines(tokpath)[1], gh_token)

    # Unsetting it works as expected.
    out <- setAccessToken(NULL, cache=NULL)
    expect_null(gypsum:::token.cache$auth_info)
    expect_true(file.exists(tokpath))

    out <- setAccessToken(NULL, cache=cache)
    expect_null(gypsum:::token.cache$auth_info)
    expect_false(file.exists(tokpath))
})

test_that("access token retrieval works as expected", {
    setAccessToken(NULL, cache=NULL) # wipe out in-memory cache.

    cache <- tempfile()
    tokpath <- file.path(cache, "credentials", "token.txt")
    dir.create(dirname(tokpath), recursive=TRUE)
    writeLines(c("foobar", "urmom", Sys.time() + 10000), con=tokpath)

    info <- accessToken(full=TRUE, request=FALSE, cache=cache)
    expect_identical(info$token, "foobar")
    expect_identical(info$name, "urmom")
    expect_type(info$expires, "double")

    # Works from cache.
    info <- accessToken(full=TRUE, request=FALSE, cache=NULL)
    expect_identical(info$token, "foobar")
    expect_identical(info$name, "urmom")
    expect_type(info$expires, "double")

    # Falls through to request.
    setAccessToken(NULL, cache=NULL) # wipe out in-memory cache.
    unlink(tokpath)
    info <- accessToken(full=TRUE, request=FALSE, cache=cache)
    expect_null(info)
})

test_that("access token expiry works as expected", {
    setAccessToken(NULL, cache=NULL) # wipe out in-memory cache.

    cache <- tempfile()
    tokpath <- file.path(cache, "credentials", "token.txt")
    dir.create(dirname(tokpath), recursive=TRUE)
    writeLines(c("foobar", "urmom", Sys.time() - 10000), con=tokpath)

    info <- accessToken(full=TRUE, request=FALSE, cache=cache)
    expect_null(info)

    # Works in memory as well.
    info <- accessToken(full=TRUE, request=FALSE, cache=NULL)
    expect_null(info)
})
