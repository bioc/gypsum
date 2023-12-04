uenc <- function(x) URLencode(x, reserved=TRUE)

chomp_url <- function(url) sub("/*$", "", url)

.cast_datetime <- function(x) {
    # Remove colon in the timezone, which confuses as.POSIXct().
    as.POSIXct(sub(":([0-9]{2})$", "\\1", x), format="%Y-%m-%dT%H:%M:%S%z")
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

