#' List files for a version
#'
#' List the contents of a version of a project asset.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param prefix String containing the prefix for the path.
#' If provided, files are only listed if they have a relative path (i.e., inside the version subdirectory) that starts with this prefix.
#' If \code{NULL}, all files associated with this version are listed.
#' @param include.. Logical scalar indicating whether to list files with path components that start with \code{..}.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#'
#' @author Aaron Lun
#'
#' @return Character vector of relative paths of files associated with the versioned asset.
#'
#' @examples
#' listFiles("test-R", "basic", "v1")
#' 
#' @export
#' @importFrom aws.s3 get_bucket
listFiles <- function(project, asset, version, prefix=NULL, include..=TRUE, config=publicS3Config()) {
    actual.prefix <- paste(project, asset, version, "", sep="/") # empty string to force trailing slash.
    truncator <- nchar(actual.prefix) + 1L
    if (!is.null(prefix)) {
        actual.prefix <- paste0(actual.prefix, prefix)
    }

    listing <- get_bucket(
        bucket=config$bucket, 
        prefix=actual.prefix,
        key=config$key, 
        secret=config$secret,
        base_url=sub("^http[s]://", "", config$endpoint), 
        region="",
        max=Inf
    )

    out <- vapply(listing, function(x) substr(x$Key, truncator, nchar(x$Key)), "", USE.NAMES=FALSE)
    if (!include..) {
        out <- out[!startsWith(out, "..") & !grepl("\\/\\.\\.", out)]
    }

    out
}
