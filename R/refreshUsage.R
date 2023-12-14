#' Refresh the quota usage
#'
#' Recompute the quota usage of a project.
#' This is useful on rare occasions where multiple simultaneous uploads cause the usage calculations to be out of sync.
#' 
#' @param project String containing the project name.
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to a gypsum administrator account.
#'
#' @return Numeric scalar specifying the total quota usage of this project, in bytes.
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{fetchUsage}}, to get the usage without recomputing it.
#'
#' @examples
#' if (interactive()) {
#'     refreshUsage("test-R")
#' }
#'
#' @export
#' @importFrom jsonlite fromJSON
refreshUsage <- function(project, url=restUrl(), token=accessToken()) {
    req <- request(paste0(url, "/refresh/usage/", uenc(project)))
    req <- req_method(req, "POST")
    req <- req_auth_bearer_token(req, token)
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)
    resp <- req_perform(req)
    invisible(resp_body_json(resp)$total)
}
