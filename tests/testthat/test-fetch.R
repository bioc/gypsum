# This tests the various fetching functions.
# library(testthat); library(gypsum); source("setup.R"); source("test-fetch.R")

test_that("fetchManifest works as expected", {
    cache <- tempfile()
    man <- fetchManifest("test-R", "basic", "v1", cache=cache)
    expect_identical(sort(names(man)), c("blah.txt", "foo/bar.txt"))

    # Uses the cache.
    writeLines(con=file.path(cache, "bucket", "test-R", "basic", "v1", "..manifest"), "[]")
    man <- fetchManifest("test-R", "basic", "v1", cache=cache)
    expect_true(length(man) == 0L)

    # Unless we overwrite it.
    man <- fetchManifest("test-R", "basic", "v1", cache=cache, overwrite=TRUE)
    expect_true(length(man) > 0L)

    expect_error(fetchManifest('test-R', 'basic', 'non-existent', cache=cache), "does not exist")
})

test_that("fetchSummary works as expected", {
    cache <- tempfile()
    xx <- fetchSummary("test-R", "basic", "v1", cache=cache)
    original.user <- xx$upload_user_id
    expect_s3_class(xx$upload_start, "POSIXct")
    expect_s3_class(xx$upload_finish, "POSIXct")

    # Uses the cache.
    sumpath <- file.path(cache, "bucket", "test-R", "basic", "v1", "..summary")
    writeLines(
        con=sumpath,
        '{ "upload_user_id": "adrian", "upload_start": "2022-01-01T01:01:01Z", "upload_finish": "2022-01-01T01:01:02Z" }'
    )
    xx <- fetchSummary("test-R", "basic", "v1", cache=cache)
    expect_identical(xx$upload_user_id, "adrian")

    # Unless we overwrite it.
    xx <- fetchSummary("test-R", "basic", "v1", cache=cache, overwrite=TRUE)
    expect_identical(xx$upload_user_id, original.user)

    # Self-deletes from the cache if it's on probation.
    writeLines(
        con=sumpath,
        '{ "upload_user_id": "adrian", "upload_start": "2022-01-01T01:01:01Z", "upload_finish": "2022-01-01T01:01:02Z", "on_probation": true }'
    )
    xx <- fetchSummary("test-R", "basic", "v1", cache=cache)
    expect_true(xx$on_probation)
    expect_false(file.exists(sumpath))

    expect_error(fetchSummary('test-R', 'basic', 'non-existent', cache=cache), "does not exist")
})

test_that("fetchLatest works as expected", {
    expect_identical(fetchLatest("test-R", "basic"), "v3")
})

test_that("fetchUsage works as expected", {
    expect_true(fetchUsage("test-R") > 0)
})

test_that("fetchPermissions works as expected", {
    perms <- fetchPermissions("test-R")
    expect_type(perms$owners, "list")
    expect_type(perms$uploaders, "list")
})

