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

get_file <- function(path, config) {
    args <- create_arguments(path, config)
    if (!do.call(object_exists, args)) {
        stop("no permissions present for '", project, "'")
    }
    do.call(get_object, args)
}

#' @importFrom jsonlite fromJSON
get_cacheable_json <- function(components, cache, config) {
    if (is.null(cache)) {
        path <- paste(components, collapse="/")
        out <- get_file(path, config=config)
        out <- rawToChar(out)
    } else {
        destination <- do.call(file.path, c(list(cache, "bucket"), as.list(components)))
        out <- saveFile(project, asset, version, "..manifest", destination=destination, config=config, overwrite=FALSE)
    }
    fromJSON(out, simplifyVector=FALSE)
}

