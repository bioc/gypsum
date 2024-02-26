#' List assets
#'
#' List all assets in a project.
#'
#' @param project String containing the project name.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#'
#' @author Aaron Lun
#'
#' @return Character vector of asset names.
#'
#' @examples
#' listAssets("test-R")
#' 
#' @export
listAssets <- function(project, config=publicS3Config()) {
    list_for_prefix(paste0(project, "/"), config=config)
}
