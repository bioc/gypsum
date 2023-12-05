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
save_file <- function(path, destination, overwrite, config, precheck) {
    if (overwrite || !file.exists(destination)) {
        dir.create(dirname(destination), recursive=TRUE, showWarnings=FALSE)

        args <- create_arguments(path, config)
        if (precheck && !do.call(object_exists, args)) {
            stop("'", path, "' does not exist in the bucket")
        }
        args$file <- destination
        args$parse_response <- FALSE
        do.call(save_object, args)
    }
}

BUCKET_CACHE_NAME <- 'bucket'

#' @importFrom jsonlite fromJSON
#' @importFrom filelock unlock
get_cacheable_json <- function(project, asset, version, path, cache, config, overwrite, precheck) {
    path <- paste(project, asset, version, path, sep="/")
    if (is.null(cache)) {
        out <- get_file(path, config=config, precheck=precheck)
        out <- rawToChar(out)
    } else {
        out <- file.path(cache, BUCKET_CACHE_NAME, project, asset, version, path)
        lck <- create_lock(project, asset, version)
        on.exit(unlock(lck))
        save_file(path, destination=out, overwrite=overwrite, config=config, precheck=precheck)
    }
    fromJSON(out, simplifyVector=FALSE)
}

#' @importFrom filelock lock
create_lock <- function(project, asset, version) {
    lockloc <- file.path(cacheDirectory(), "status", project, asset, version, "LOCK")
    dir.create(dirname(lockloc), recursive=TRUE, showWarnings=FALSE)
    lock(lockloc)
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
