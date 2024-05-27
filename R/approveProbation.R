#' Approve a probational upload
#'
#' Pretty much as it says: approve a probational upload of a version of a project's asset.
#' This removes the \code{on_probation} tag from the uploaded version.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to an owner of \code{project}.
#'
#' @return \code{NULL} is invisibly returned upon successful approval.
#'
#' @seealso
#' \code{\link{rejectProbation}}, to reject the probational upload.
#'
#' \code{\link{startUpload}}, to specify probational uploads.
#' 
#' @author Aaron Lun
#' @examples
#' if (interactive()) {
#'     # Mocking up a versioned asset.
#'     init <- startUpload(
#'         project="test-R", 
#'         asset="probation-approve", 
#'         version="v1", 
#'         files=character(0),
#'         probation=TRUE
#'     )
#'     completeUpload(init) 
#'
#'     # Approving the probation:
#'     approveProbation("test-R", "probation-approve", "v1")
#' 
#'     # Just cleaning up after we're done.
#'     removeProjectAsset("test-R", "probation-approve")
#' }
#' 
#' @export
#' @import httr2
approveProbation <- function(project, asset, version, url=restUrl(), token=accessToken()) {
    url <- chomp_url(url)
    req <- request(paste0(url, "/probation/approve/", uenc(project) , "/", uenc(asset), "/", uenc(version)))
    req <- req_method(req, "POST")
    req <- req_auth_bearer_token(req, token)
    req <- process_error(req)
    req_perform(req)
    invisible(NULL)
}
