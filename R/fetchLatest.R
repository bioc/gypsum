#' Fetch the latest version
#'
#' Fetch the latest version of a project's asset.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#'
#' @return String containing the latest version of the project.
#'
#' @author Aaron Lun
#'
#' @examples
#' fetchLatest("test-R", "basic")
#'
#' @seealso
#' \code{\link{refreshLatest}}, to refresh the latest version.
#'
#' @export
#' @importFrom jsonlite fromJSON
fetchLatest <- function(project, asset, config=publicS3Config()) {
    out <- get_file(paste(project, asset, "..latest", sep="/"), config=config, precheck=TRUE)
    msg <- rawToChar(out)
    if (grepl("^<", msg)) {
        stop(msg)
    }
    vers <- fromJSON(msg, simplifyVector=FALSE)
    vers$version
}
