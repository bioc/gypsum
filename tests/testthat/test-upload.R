# This tests the upload functions. 
# library(testthat); library(gypsum); source("setup.R"); source("test-upload.R")

test_that("upload sequence works as expected for regular files", {
    skip_if(is.na(gh_token))
    removeProject("test-R", asset="upload", token=gh_token)

    tmp <- tempfile()
    dir.create(tmp)
    write(file=file.path(tmp, "blah.txt"), LETTERS)
    dir.create(file.path(tmp, "foo"))
    write(file=file.path(tmp, "foo", "bar.txt"), 1:10)

    init <- startUpload(
        project="test-R", 
        asset="upload", 
        version="1", 
        files=list.files(tmp, recursive=TRUE),
        directory=tmp,
        token=gh_token
    )

    expect_identical(length(init$file_urls), 2L)
    expect_type(init$abort_url, "character")
    expect_type(init$complete_url, "character")
    expect_type(init$session_token, "character")

    uploadFiles(init, directory=tmp)
    completeUpload(init)

    # Checking that the files were, in fact, correctly uploaded.
    man <- fetchManifest("test-R", "upload", "1", cache=NULL)
    expect_identical(sort(names(man)), c("blah.txt", "foo/bar.txt"))
    expect_false(any(vapply(man, function(x) !is.null(x$link), TRUE)))

    # Deduplication happens naturally.
    init <- startUpload(
        project="test-R", 
        asset="upload", 
        version="2", 
        files=list.files(tmp, recursive=TRUE),
        directory=tmp,
        token=gh_token
    )
    expect_identical(length(init$file_urls), 0L)
    uploadFiles(init, directory=tmp)
    completeUpload(init)

    man <- fetchManifest("test-R", "upload", "2", cache=NULL)
    expect_identical(sort(names(man)), c("blah.txt", "foo/bar.txt"))
    expect_true(all(vapply(man, function(x) !is.null(x$link), TRUE)))
})

test_that("upload sequence works as expected for links", {
    skip_if(is.na(gh_token))
    removeProject("test-R", asset="upload", token=gh_token)

    tmp <- tempfile()
    dir.create(tmp)
    fpath <- file.path(tmp, "urmom.json")
    write(file=fpath, "[ \"Aaron\" ]")

    link.df <- rbind(
        data.frame(from.path="whee/stuff.txt", to.project="test-R", to.asset="basic", to.version="v1", to.path="blah.txt"),
        data.frame(from.path="michaela", to.project="test-R", to.asset="basic", to.version="v1", to.path="foo/bar.txt")
    )

    init <- startUpload(
        project="test-R", 
        asset="upload", 
        version="1", 
        files=data.frame(path="urmom.json", size=file.info(fpath)$size, md5sum=digest::digest(file=fpath)), # constructing a DF to test the other path.
        links=link.df,
        directory=tmp,
        token=gh_token
    )
    expect_identical(length(init$file_urls), 1L)
    uploadFiles(init, directory=tmp)
    completeUpload(init)

    man <- fetchManifest("test-R", "upload", "1", cache=NULL)
    expect_identical(sort(names(man)), c("michaela", "urmom.json", "whee/stuff.txt"))
    expect_false(is.null(man[["michaela"]]$link))
    expect_false(is.null(man[["whee/stuff.txt"]]$link))
    expect_null(man[["urmom.json"]]$link)
})

test_that("aborting the upload works correctly", {
    skip_if(is.na(gh_token))
    removeProject("test-R", asset="upload", token=gh_token)

    init <- startUpload(
        project="test-R", 
        asset="upload", 
        version="1", 
        files=character(),
        token=gh_token
    )
    expect_identical(length(init$file_urls), 0L)
    expect_true("upload" %in% listAssets("test-R"))

    abortUpload(init)
    expect_false("upload" %in% listAssets("test-R"))
})
