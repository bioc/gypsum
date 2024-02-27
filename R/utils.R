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

create_arguments <- function(path, config) {
    list(
        object=path,
        bucket=config$bucket, 
        key=config$key, 
        secret=config$secret, 
        base_url=sub("^http[s]://", "", config$endpoint), 
        region=""
    )
}

#' @importFrom aws.s3 get_object object_exists
get_file <- function(path, config, precheck) {
    args <- create_arguments(path, config)
    if (precheck && !do.call(object_exists, args)) {
        stop("'", path, "' does not exist in the bucket")
    }
    do.call(get_object, args)
}

#' @importFrom aws.s3 save_object object_exists
save_file <- function(path, destination, overwrite, config, precheck, error=TRUE) {
    if (overwrite || !file.exists(destination)) {
        dir.create(dirname(destination), recursive=TRUE, showWarnings=FALSE)

        args <- create_arguments(path, config)
        if (precheck && !do.call(object_exists, args)) {
            if (error) {
                stop("'", path, "' does not exist in the bucket")
            }
            return(FALSE)
        }

        tmp <- tempfile()
        args$file <- tmp
        args$parse_response <- FALSE
        do.call(save_object, args)

        # We use a write-and-rename approach to avoid problems with interrupted
        # downloads that make it seem as if the cache is populated.
        if (!file.rename(tmp, destination) && !file.copy(tmp, destination)) {
            stop("cannot move temporary file for '", path, "' to its destination '", destination, "'")
        }
    }

    return(TRUE)
}

BUCKET_CACHE_NAME <- 'bucket'

#' @importFrom jsonlite fromJSON
get_cacheable_json <- function(project, asset, version, path, cache, config, overwrite, precheck) {
    bucket_path <- paste(project, asset, version, path, sep="/")
    if (is.null(cache)) {
        out <- get_file(bucket_path, config=config, precheck=precheck)
        out <- rawToChar(out)
    } else {
        out <- file.path(cache, BUCKET_CACHE_NAME, project, asset, version, path)
        acquire_lock(cache, project, asset, version)
        on.exit(release_lock(project, asset, version))
        save_file(bucket_path, destination=out, overwrite=overwrite, config=config, precheck=precheck)
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

#' @importFrom aws.s3 get_bucket
list_for_prefix <- function(prefix, config) {
    listing <- get_bucket(
        bucket=config$bucket, 
        prefix=prefix,
        delimiter="/",
        key=config$key, 
        secret=config$secret,
        base_url=sub("^http[s]://", "", config$endpoint), 
        region="",
        max=Inf
    )

    out <- attr(listing, "CommonPrefixes")
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
