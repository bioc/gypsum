#' Remove a project 
#'
#' Remove a project from the gypsum backend.
#'
#' @param project String containing the project to remove.
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to a gypsum administrator account.
#'
#' @return \code{NULL} is invisibly returned if the project was successfully removed. 
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{createProject}}, to create a project.
#'
#' \code{\link{removeAsset}} and \code{\link{removeVersion}}, to remove an asset or version.
#'
#' @examples
#' if (interactive()) {
#'     createProject("test-R-remove", owners="LTLA")
#'     removeProject("test-R-remove")
#' }
#' @export
#' @import httr2
removeProject <- function(project, url=restUrl(), token=accessToken()) {
    suffix <- uenc(project)
    request_removal(suffix, url, token)
}

request_removal <- function(suffix, url, token) {
    url <- chomp_url(url)
    req <- request(paste0(url, "/remove/", suffix))
    req <- req_method(req, "DELETE")
    req <- req_auth_bearer_token(req, token)
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)

    req_perform(req)
    invisible(NULL)
}
