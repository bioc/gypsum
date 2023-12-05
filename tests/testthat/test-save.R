# As the name suggests, this checks that saveFile/saveVersion work as expected.
# library(testthat); library(gypsum); source("setup.R"); source("test-save.R")

test_that("saveFile works as expected", {
    cache <- tempfile()
    out <- saveFile("test-R", "basic", "v1", "blah.txt", cache=cache)
    expect_identical(readLines(out), blah_contents)

    out <- saveFile("test-R", "basic", "v1", "foo/bar.txt", cache=cache)
    expect_identical(readLines(out), foobar_contents)

    # Actually uses the cache.
    writeLines(con=out, letters)
    out <- saveFile("test-R", "basic", "v1", "foo/bar.txt", cache=cache)
    expect_identical(readLines(out), letters)

    # Until it doesn't.
    out <- saveFile("test-R", "basic", "v1", "foo/bar.txt", cache=cache, overwrite=TRUE)
    expect_identical(readLines(out), foobar_contents)

    expect_error(saveFile("test-R", "basic", "v1", "no.exist.txt", cache=cache), "does not exist")
})

test_that("saveVersion works as expected without links", {
    cache <- tempfile()
    out <- saveVersion("test-R", "basic", "v1", cache=cache)
    expect_true(file.exists(file.path(out, "..manifest")))
    expect_identical(readLines(file.path(out, "blah.txt")), blah_contents)
    expect_identical(readLines(file.path(out, "foo", "bar.txt")), foobar_contents)

    # Caching behaves as expected.
    path <- file.path(out, "blah.txt")
    writeLines("foo", con=path)
    out <- saveVersion("test-R", "basic", "v1", cache=cache)
    expect_identical(readLines(path), "foo")

    # Unless we overwrite.
    out <- saveVersion("test-R", "basic", "v1", cache=cache, overwrite=TRUE)
    expect_identical(readLines(path), blah_contents)

    # Also works concurrently.
    cache2 <- tempfile()
    out <- saveVersion("test-R", "basic", "v1", cache=cache2, concurrent=2)
    expect_true(file.exists(file.path(cache2, "bucket", "test-R", "basic", "v1", "..manifest")))
    expect_identical(readLines(file.path(cache2, "bucket", "test-R", "basic", "v1", "blah.txt")), blah_contents)
    expect_identical(readLines(file.path(cache2, "bucket", "test-R", "basic", "v1", "foo", "bar.txt")), foobar_contents)
})

test_that("saveVersion works as expected with links", {
    cache <- tempfile()
    out <- saveVersion("test-R", "basic", "v2", cache=cache)
    expect_identical(readLines(file.path(out, "blah.txt")), blah_contents)
    expect_identical(readLines(file.path(out, "foo", "bar.txt")), foobar_contents)
    expect_true(file.exists(file.path(out, "blah.txt"))) # populates the previous versions as well.
    expect_true(file.exists(file.path(out, "foo", "bar.txt")))

    # Unless we turn off link resolution.
    cache <- tempfile()
    out <- saveVersion("test-R", "basic", "v2", cache=cache, relink=FALSE)
    expect_false(file.exists(file.path(out, "blah.txt")))
    expect_false(file.exists(file.path(out, "foo", "bar.txt")))

    # Works recursively.
    cache <- tempfile()
    out <- saveVersion("test-R", "basic", "v3", cache=cache)
    expect_identical(readLines(file.path(out, "blah.txt")), blah_contents)
    expect_identical(readLines(file.path(out, "foo", "bar.txt")), foobar_contents)
    expect_true(file.exists(file.path(out, "blah.txt"))) # populates the previous versions as well.
    expect_true(file.exists(file.path(out, "foo", "bar.txt")))
    expect_true(file.exists(file.path(out, "blah.txt")))
    expect_true(file.exists(file.path(out, "foo", "bar.txt")))

    # Link resolver doesn't do more work if it's already present.
    path <- file.path(out, "foo", "bar.txt")
    writeLines(con=path, "Aaron wuz here")
    out <- saveVersion("test-R", "basic", "v3", cache=cache)
    expect_identical(readLines(path), "Aaron wuz here")

    # Unless we force it to. 
    out <- saveVersion("test-R", "basic", "v3", cache=cache, overwrite=TRUE)
    expect_identical(readLines(path), foobar_contents)
})
