#' Create a new project
#'
#' Create a new project with the associated permissions.
#'
#' @param project String containing the project name.
#' @param owners Character vector containing the GitHub users or organizations that are owners of this project.
#' @param uploaders List specifying the authorized uploaders for this project.
#' Each entry is a list with the following fields:
#' \itemize{
#' \item \code{id}, a string containing the GitHub user or organization that is authorized to upload.
#' \item (optional) \code{asset}, a string containing the name of the asset that the uploader is allowed to upload to.
#' If not provided, there is no restriction on the uploaded asset name.
#' \item (optional) \code{version}, a string containing the name of the version that the uploader is allowed to upload to.
#' If not provided, there is no restriction on the uploaded version name.
#' \item (optional) \code{until}, a \link{POSIXct} object containing the expiry date of this authorization.
#' If not provided, the authorization does not expire.
#' \item (optional) \code{trusted}, whether the uploader is trusted.
#' If not provided, defaults to \code{FALSE}.
#' }
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to a gypsum administrator account.
#' Defaults to the token from \code{\link{accessToken}}.
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
#'         "test-R", 
#'         owners="LTLA", 
#'         uploaders=list(list(id="ArtifactDB-bot"))
#'     )
#' }
#' @export
#' @import httr2
createProject <- function(project, owners, uploaders=list(), url=restUrl(), token=NULL) {
    for (i in seq_along(uploaders)) {
        if (!("trusted" %in% names(uploaders[[i]]))) {
            uploaders[[i]]$trusted <- FALSE
       }
    }

    if (!endsWith(url, "/")) {
        url <- paste0(url, "/")
    }
    if (is.null(token)) {
        token <- accessToken()$token
    }

    req <- request(paste0(url, "create/", uenc(project)))
    req <- req_method(req, "POST")
    req <- req_body_json(req, list(owners=I(owners), uploaders=uploaders))
    req <- req_auth_bearer_token(req, token)
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)

    req_perform(req)
    invisible(NULL)
}
