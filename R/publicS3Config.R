creds.env <- new.env()
creds.env$info <- list()

config_cache_path <- function(cache) {
    file.path(cache, "credentials", "s3.json")
}

#' Public S3 configuration
#'
#' Fetch S3 credentials and other configuration details for read-only access to the underlying gypsum bucket.
#'
#' @param refresh Logical scalar indicating whether to refresh the credentials in the in-memory cache.
#' @param url String containing a URL to the gypsum REST API.
#' @param cache String containing a path to the cache directory, to store the configuration across R sessions.
#'
#' @return List containing:
#' \itemize{
#' \item \code{key}, a string containing the read-only S3 access key ID.
#' \item \code{secret}, a string containing the associated S3 access secret.
#' \item \code{bucket}, a string containing the name of the bucket.
#' \item \code{endpoint}, a string containing the URL for the S3 API.
#' }
#'
#' @details
#' The configuration is obtained through a query to \code{url} on the first use of this function.
#' The result is automatically cached in memory and on disk to reduce the number of network requests to the API.
#' New credentials are automatically fetched if the on-disk cache is older than a week;
#' this refresh can be performed manually by calling this function with \code{refresh=TRUE}.
#'
#' @author Aaron Lun
#'
#' @examples
#' publicS3Config()
#'
#' @export
#' @import httr2
#' @importFrom jsonlite fromJSON
publicS3Config <- function(refresh = FALSE, url=restUrl(), cache=cacheDirectory()) {
    if (!refresh) {
        # We use 'cache'-specific credentials here, so that the behavior of the
        # in-memory cache is consistent with the behavior of the on-disk cache
        # at 'cache'. I suppose this is useful just in case people have
        # different cache locations for different 'url' calls.
        creds <- creds.env$info[[cache]]
        if (!is.null(creds)) {
            return(creds)
        }

        config.path <- config_cache_path(cache)
        if (file.exists(config.path)) {
            # Only using these configuration parameters if they are less than a week old. 
            if (Sys.time() - file.info(config.path)$ctime <= 7) {
                creds <- fromJSON(config.path, simplifyVector=FALSE)
                creds.env$info[[cache]] <- creds
                return(creds)
            }
        }
    }

    req <- request(paste0(url, "/credentials/s3-api"))
    res <- req_perform(req)
    creds <- resp_body_json(res)
    creds.env$info[[cache]] <- creds

    config.path <- config_cache_path(cache)
    dir.create(dirname(config.path), showWarnings=FALSE, recursive=TRUE)
    lck <- lock(paste0(config.path, ".LOCK"))
    on.exit(unlock(lck))
    write(file=config.path, toJSON(creds, auto_unbox=TRUE))

    creds
}
