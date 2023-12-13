#' Fetch project quota details
#'
#' Fetch the quota details for a project.
#' 
#' @param project String containing the project name.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#'
#' @return List containing \code{baseline}, the baseline quota at time zero in bytes;
#' \code{growth_rate}, the annual growth rate for the quota in bytes;
#' and \code{year}, the creation year (i.e., time zero) for this project.
#'
#' @author Aaron Lun
#'
#' @examples
#' fetchQuota("test-R")
#'
#' @seealso
#' \code{\link{setQuota}}, to set the quota details.
#'
#' @export
#' @importFrom jsonlite fromJSON
fetchQuota <- function(project, config=publicS3Config()) {
    out <- get_file(paste(project, "..quota", sep="/"), config=config, precheck=TRUE)
    msg <- rawToChar(out)
    if (grepl("^<", msg)) {
        stop(msg)
    }
    fromJSON(msg, simplifyVector=FALSE)
}
