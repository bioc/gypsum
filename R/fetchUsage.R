#' Fetch project usage details
#'
#' Fetch the quota usage for a project.
#' 
#' @param project String containing the project name.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#'
#' @return Numeric scalar specifying the quota usage for the project, in bytes.
#'
#' @author Aaron Lun
#'
#' @examples
#' fetchUsage("test-R")
#'
#' @seealso
#' \code{\link{refreshUsage}}, to recompute the used quota.
#'
#' @export
#' @importFrom jsonlite fromJSON
fetchUsage <- function(project, config=publicS3Config()) {
    out <- get_file(paste(project, "..usage", sep="/"), config=config, precheck=TRUE)
    msg <- rawToChar(out)
    if (grepl("^<", msg)) {
        stop(msg)
    }
    fromJSON(msg, simplifyVector=FALSE)$total
}
