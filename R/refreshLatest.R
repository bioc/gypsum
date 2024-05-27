#' Refresh the latest version
#'
#' Recompute the latest version of a project's asset.
#' This is useful on rare occasions where multiple simultaneous uploads cause the latest version to be slightly out of sync.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to a gypsum administrator account.
#'
#' @return String containing the latest version of the project, or \code{NULL} if there are no non-probational versions.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{fetchLatest}}, to get the latest version without recomputing it.
#' 
#' @examples
#' if (interactive()) {
#'     refreshLatest("test-R", "basic")
#' }
#'
#' @export
#' @importFrom jsonlite fromJSON
refreshLatest <- function(project, asset, url=restUrl(), token=accessToken()) {
    req <- request(paste0(url, "/refresh/latest/", uenc(project), "/", uenc(asset)))
    req <- req_method(req, "POST")
    req <- req_auth_bearer_token(req, token)
    req <- process_error(req)
    resp <- req_perform(req)
    invisible(resp_body_json(resp)$version)
}
