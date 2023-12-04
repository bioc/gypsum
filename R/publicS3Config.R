creds.env <- new.env()
creds.env$info <- NULL

#' Public S3 configuration
#'
#' Fetch S3 credentials and other configuration details for read-only access to the underlying gypsum bucket.
#'
#' @param refresh Logical scalar indicating whether to refresh the credentials in the in-memory cache.
#' @param url String containing a URL to the gypsum REST API.
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
#' The configuration is obtained through a query to \code{url} but is automatically cached in memory on first use in an R session.
#' This reduces the number of network requests in read operations on the bucket.
#'
#' @author Aaron Lun
#'
#' @examples
#' publicS3Config()
#'
#' @export
#' @import httr2
publicS3Config <- function(refresh = FALSE, url=restUrl()) {
    if (refresh || is.null(creds.env$info)) {
        req <- request(paste0(url, "/credentials/s3-api"))
        res <- req_perform(req)
        creds <- resp_body_json(res)
        creds.env$info <- creds

    }
    creds.env$info
}
