#' @importFrom utils URLencode
uenc <- function(x) URLencode(x, reserved=TRUE)

chomp_url <- function(url) sub("/*$", "", url)

.cast_datetime <- function(x) {
    zend <- endsWith(x, "Z")

    if (any(zend)) {
        # strptime doesn't know how to handle 'Z' offsets.
        xz <- x[zend]
        x[zend] <- sprintf("%s+0000", substr(xz, 1L, nchar(xz)-1L))
    }

    if (!all(zend)) {
        # Remove colon in the timezone, which confuses as.POSIXct().
        x[!zend] <- sub(":([0-9]{2})$", "\\1", x[!zend])
    }

    # Remove fractional seconds.
    x <- sub("\\.[0-9]+", "", x)

    as.POSIXct(x, format="%Y-%m-%dT%H:%M:%S%z")
}

sanitize_uploaders <- function(uploaders) {
    for (i in seq_along(uploaders)) {
        current <- uploaders[[i]]
        if ("until" %in% names(current)) {
            uploaders[[i]]$until <- sub("([0-9]{2})$", ":\\1", strftime(current$until, "%Y-%m-%dT%H:%M:%S%z"))
        }
    }
    uploaders
}

#' @importFrom paws.storage s3
create_s3 <- function(config) {
    s3(
        endpoint=config$endpoint, 
        region="auto",
        credentials=list(
            creds=list(
                access_key_id=config$key, 
                secret_access_key=config$secret
            )
        )
    )
}

get_file <- function(path, config) {
    s <- create_s3(config)
    tryCatch(
        (s$get_object(config$bucket, Key=path))$Body,
        error=function(e) stop("failed to retrieve '", path, "'; ", e$message)
    )
}

save_file <- function(path, destination, overwrite, config, error=TRUE) {
    if (overwrite || !file.exists(destination)) {
        dir.create(dirname(destination), recursive=TRUE, showWarnings=FALSE)
        s <- create_s3(config)

        # We use a write-and-rename approach to avoid problems with interrupted
        # downloads that make it seem as if the cache is populated.
        tmp <- tempfile(tmpdir=dirname(destination))
        on.exit(unlink(tmp), add=TRUE, after=FALSE)

        status <- tryCatch(
            s$download_file(config$bucket, Key=path, Filename=tmp),
            error=function(e) e$message
        )

        if (is.character(status)) {
            if (error) {
                stop("failed to save '", path, "'; ", status)
            } else {
                return(FALSE)
            }
        }

        rename_file(tmp, destination)
    }

    return(TRUE)
}

rename_file <- function(src, dest) {
    if (!file.rename(src, dest) && !file.copy(src, dest)) {
        stop("cannot move temporary file for '", src, "' to its destination '", dest, "'")
    }
}

#' @import httr2
download_and_rename_file <- function(url, dest) {
    # Using the usual write-and-rename strategy.
    tmp <- tempfile(tmpdir=dirname(dest))
    on.exit(unlink(tmp), add=TRUE, after=FALSE)

    req <- request(url)
    req_perform(req, path=tmp)

    rename_file(tmp, dest)
}

BUCKET_CACHE_NAME <- 'bucket'

#' @importFrom jsonlite fromJSON
get_cacheable_json <- function(project, asset, version, path, cache, config, overwrite) {
    bucket_path <- paste(project, asset, version, path, sep="/")
    if (is.null(cache)) {
        out <- get_file(bucket_path, config=config) 
        out <- rawToChar(out)
    } else {
        out <- file.path(cache, BUCKET_CACHE_NAME, project, asset, version, path)
        acquire_lock(cache, project, asset, version)
        on.exit(release_lock(project, asset, version), add=TRUE, after=FALSE)
        save_file(bucket_path, destination=out, overwrite=overwrite, config=config)
    }
    fromJSON(out, simplifyVector=FALSE)
}

is.locked <- new.env()
is.locked$locks <- list()

#' @importFrom filelock lock
acquire_lock <- function(cache, project, asset, version) {
    key <- paste(project, asset, version, sep="/")
    if (is.null(is.locked$locks[[key]])) {
        lockloc <- file.path(cache, "status", project, asset, version, "LOCK")
        dir.create(dirname(lockloc), recursive=TRUE, showWarnings=FALSE)
        is.locked$locks[[key]] <- lock(lockloc)
    }
}

#' @importFrom filelock unlock
release_lock <- function(project, asset, version) {
    key <- paste(project, asset, version, sep="/")
    if (!is.null(is.locked$locks[[key]])) {
        unlock(is.locked$locks[[key]])
        is.locked$locks[[key]] <- NULL
    }
}

list_for_prefix <- function(prefix, config) {
    s <- create_s3(config)

    token <- NULL
    out <- character()
    while (TRUE) {
        listing <- s$list_objects_v2(config$bucket, Prefix=prefix, Delimiter="/", ContinuationToken=token)
        out <- c(out, unlist(listing$CommonPrefixes, use.names=FALSE))
        if (!listing$IsTruncated) {
            break
        }
        token <- listing$NextContinuationToken
    }

    if (!is.null(prefix)) {
        out <- substr(out, nchar(prefix) + 1L, nchar(out) - 1L)
    } else {
        out <- substr(out, 1L, nchar(out) - 1L)
    }
    out[!startsWith(out, "..")]
}

sanitize_path <- function(x) {
    if (.Platform$OS.type == "windows") {
        x <- gsub("\\\\", "/", x)
    }
    gsub("//+", "/", x)
}
