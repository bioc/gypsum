#' Reject a probational upload
#'
#' Pretty much as it says: reject a probational upload of a version of a project's asset.
#' This removes all files associated with that version.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to an owner of \code{project}.
#'
#' @return \code{NULL} is invisibly returned upon successful rejection.
#'
#' @seealso
#' \code{\link{approveProbation}}, to approve the probational upload.
#'
#' \code{\link{startUpload}}, to specify probational uploads.
#' 
#' @author Aaron Lun
#' @examples
#' if (interactive()) {
#'     # Mocking up a versioned asset.
#'     init <- startUpload(
#'         project="test-R", 
#'         asset="probation-reject", 
#'         version="v1", 
#'         files=character(0),
#'         probation=TRUE
#'     )
#'     completeUpload(init) 
#'
#'     # Rejecting the probation:
#'     rejectProbation("test-R", "probation-reject", "v1")
#' }
#' 
#' @export
#' @import httr2
rejectProbation <- function(project, asset, version, url=restUrl(), token=accessToken()) {
    url <- chomp_url(url)
    req <- request(paste0(url, "/probation/reject/", uenc(project) , "/", uenc(asset), "/", uenc(version)))
    req <- req_method(req, "POST")
    req <- req_auth_bearer_token(req, token)
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)
    req_perform(req)
    invisible(NULL)
}
