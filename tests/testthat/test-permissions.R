# This tests the permission setter functions 
# library(testthat); library(gypsum); source("setup.R"); source("test-permissions.R")

test_that("permission setting works as expected", {
    skip_if(is.na(gh_token))
    removeProject("test-R-perms", asset=NULL, version=NULL, token=gh_token)
    createProject("test-R-perms", owners="LTLA", token=gh_token)

    until <- round(Sys.time() + 1000000)
    setPermissions("test-R-perms",
        owners="jkanche", 
        uploaders=list(
           list(id="lawremi", until=until)
        ),
        token=gh_token
    )

    perms <- fetchPermissions("test-R-perms")
    expect_identical(perms$owners, list("LTLA", "jkanche"))
    expect_identical(length(perms$uploaders), 1L)
    expect_identical(perms$uploaders[[1]]$id, "lawremi")
    expect_equal(perms$uploaders[[1]]$until, until)

    # Checking uploader appending, while also checking owners=NULL.
    setPermissions("test-R-perms", uploaders=list(list(id="ArtifactDB-bot", trusted=TRUE)), token=gh_token)
    perms <- fetchPermissions("test-R-perms")
    expect_identical(perms$owners, list("LTLA", "jkanche"))
    expect_identical(length(perms$uploaders), 2L)
    expect_identical(perms$uploaders[[1]]$id, "lawremi")
    expect_identical(perms$uploaders[[2]]$id, "ArtifactDB-bot")
    expect_true(perms$uploaders[[2]]$trusted)

    # Checking union of owners, and also that uploaders=NULL works.
    setPermissions("test-R-perms", owners=c("PeteHaitch", "LTLA"), token=gh_token)
    perms <- fetchPermissions("test-R-perms")
    expect_identical(perms$owners, list("LTLA", "jkanche", "PeteHaitch"))
    expect_identical(length(perms$uploaders), 2L)

    # Resetting the owners back.
    setPermissions("test-R-perms", owners="LTLA", append=FALSE, token=gh_token)
    perms <- fetchPermissions("test-R-perms")
    expect_identical(perms$owners, list("LTLA"))
    expect_identical(length(perms$uploaders), 2L)

    # Now resetting the uploaders. 
    setPermissions("test-R-perms", uploaders=list(), append=FALSE, token=gh_token)
    perms <- fetchPermissions("test-R-perms")
    expect_identical(perms$owners, list("LTLA"))
    expect_identical(length(perms$uploaders), 0L)
})
