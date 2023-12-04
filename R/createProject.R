#' Create a new project
#'
#' Create a new project with the associated permissions.
#'
#' @param project String containing the project name.
#' @param owners Character vector containing the GitHub users or organizations that are owners of this project.
#' @param uploaders List specifying the authorized uploaders for this project.
#' See the \code{uploaders} field in the \code{\link{fetchPermissions}} return value for the expected format. 
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to a gypsum administrator account.
#'
#' @return \code{NULL} is invisibly returned if the project was successfully created.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{removeProject}}, to remove a project.
#'
#' @examples
#' if (interactive()) {
#'     createProject(
#'         "test-create", 
#'         owners="LTLA", 
#'         uploaders=list(list(id="ArtifactDB-bot"))
#'     )
#' }
#' @export
#' @import httr2
createProject <- function(project, owners, uploaders=list(), url=restUrl(), token=accessToken()) {
    url <- chomp_url(url)
    uploaders <- sanitize_uploaders(uploaders)

    req <- request(paste0(url, "/create/", uenc(project)))
    req <- req_method(req, "POST")
    req <- req_body_json(req, list(owners=I(owners), uploaders=uploaders))
    req <- req_auth_bearer_token(req, token)
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)

    req_perform(req)
    invisible(NULL)
}
