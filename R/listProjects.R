#' List all projects
#'
#' List all projects in the gypsum backent.
#'
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#'
#' @author Aaron Lun
#'
#' @return Character vector of project names.
#'
#' @examples
#' if (interactive()) {
#'     listProjects()
#' }
#' 
#' @export
listProjects <- function(config=publicS3Config()) {
    list_for_prefix(NULL, config=config)
}
