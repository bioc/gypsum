#' List asset versions
#'
#' List all versions of a project asset.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#'
#' @author Aaron Lun
#'
#' @return Character vector of versions.
#'
#' @examples
#' listVersions("test-R", "basic")
#' 
#' @export
listVersions <- function(project, asset, config=publicS3Config()) {
    list_for_prefix(paste0(project, "/", asset, "/"), config)
}
