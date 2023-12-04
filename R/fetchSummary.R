#' Fetch version summary
#'
#' Fetch the summary for a version of an asset of a project.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param cache String containing the cache directory.
#' If \code{NULL}, no caching is performed.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#'
#' @author Aaron Lun
#' 
#' @return List containing the summary for this version, with the following fields:
#' \itemize{
#' \item \code{upload_user_id}, string containing the identity of the uploader.
#' \item \code{upload_start}, a \link{POSIXct} object containing the upload start time.
#' \item \code{upload_finish}, a \link{POSIXct} object containing the upload finish time.
#' \item \code{on_probation} (optional), a logical scalar indicating whether the upload is probational.
#' If missing, this can be assumed to be \code{FALSE}.
#' }
#' 
#' @examples
#' fetchSummary("test-R", "upload-check", "v1")
#' 
#' @export
fetchSummary <- function(project, asset, version, cache=cacheDirectory(), config=publicS3Config()) {
    out <- get_cacheable_json(c(project, asset, version, "..summary"), cache=cache, config=config)
    out$upload_start <- .cast_datetime(out$upload_start)
    out$upload_finish <- .cast_datetime(out$upload_finish)
    out
}
