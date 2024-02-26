# Test some miscellaneous functions.
# library(testthat); library(gypsum); source("test-misc.R")

test_that("restUrl works as expected", {
    old <- restUrl("FOO")
    expect_identical(restUrl(), "FOO")
    reset <- restUrl(old)
    expect_identical(old, restUrl())
})

test_that("cacheDirectory works as expected", {
    old <- cacheDirectory("FOO")
    expect_identical(cacheDirectory(), "FOO")
    reset <- cacheDirectory(old)
    expect_identical(old, cacheDirectory())
})

test_that("publicS3Config caches as expected", {
    cache <- tempfile()
    out <- publicS3Config(cache=cache)
    expect_type(out$endpoint, "character")
    disked <- file.path(cache, "credentials", "s3.json")
    expect_true(file.exists(disked))

    # Reloads from disk perfectly well.
    cache2 <- tempfile()
    dir.create(file.path(cache2, "credentials"), recursive=TRUE)
    file.copy(disked, file.path(cache2, "credentials", "s3.json"))
    out2 <- publicS3Config(cache=cache2)
    expect_identical(out, out2)

    # Subsequent requests just re-use the in-memory cache.
    unlink(disked)
    reout <- publicS3Config(cache=cache)
    expect_identical(reout, out)
    expect_false(file.exists(disked))

    # ... unless we force a refresh.
    reout <- publicS3Config(refresh=TRUE, cache=cache)
    expect_true(file.exists(disked))
})

test_that("URL chomper works as expected", {
    expect_identical(gypsum:::chomp_url("https://asdasd.com/asdasd"), "https://asdasd.com/asdasd")
    expect_identical(gypsum:::chomp_url("https://asdasd.com/asdasd//"), "https://asdasd.com/asdasd")
})

test_that("URL encoder works as expected", {
    expect_identical(gypsum:::uenc('asd/asdas'), "asd%2Fasdas")
})

test_that("S3 credential request works as expected", {
    out <- publicS3Config()
    expect_type(out$endpoint, "character")
    expect_type(out$key, "character")
    expect_type(out$secret, "character")
    expect_type(out$bucket, "character")
})

test_that("casting of datetimes works as expected", {
    out <- gypsum:::.cast_datetime("2023-12-04T14:41:19+01:00")
    expect_false(is.na(out))

    # Behaves with fractional seconds.
    out <- gypsum:::.cast_datetime("2023-12-04T14:41:19.21323+01:00")
    expect_false(is.na(out))

    # Behaves with 'Z'.
    out <- gypsum:::.cast_datetime("2023-12-04T14:41:19Z")
    expect_false(is.na(out))
})
