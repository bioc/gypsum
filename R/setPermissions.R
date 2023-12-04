#' Set project permissions 
#' 
#' Set the owner and uploader permissions for a project.
#'
#' @param project String containing the project name.
#' @param owners Character vector containing the GitHub users or organizations that are owners of this project.
#' If missing, no change is made to the existing owners of the project.
#' @param uploaders List specifying the authorized uploaders for this project.
#' See the \code{uploaders} field in the \code{\link{fetchPermissions}} return value for the expected format. 
#' If missing, no change is made to the existing uploaders of the project.
#' @param append Logical scalar indicating whether \code{owners} and \code{uploaders} should be appended to the existing owners and uploaders, respectively, of the project.
#' If \code{FALSE}, the \code{owners} and \code{uploaders} are used to replace the existing values.
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to an owner of the \code{project}.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{fetchPermissions}}, to fetch the permissions.
#'
#' @return \code{NULL} is invisibly returned upon successful setting of the permissions.
#'
#' @examples
#' if (interactive()) {
#'     # Creating a project for demonstration purposes.
#'     createProject("test-R-perms", owners="LTLA")
#'
#'     # Setting extra permissions on this project.
#'     setPermissions("test-R-perms",
#'         owners="jkanche", 
#'         uploaders=list(list(id="lawremi", until=Sys.time() + 1000))
#'     )
#' }
#'
#' @export
setPermissions <- function(project, owners, uploaders, append=TRUE, url=restUrl(), token=accessToken()) {
    url <- chomp_url(url)

    if (!is.null(uploaders)) {
        uploaders <- sanitize_uploaders(uploaders)
    }

    perms <- list()
    if (append) {
        old.perms <- fetchPermissions(project)
        if (!missing(owners)) {
            perms$owners <- I(union(old.perms$owners, owners))
        }
        if (!missing(uploaders)) {
            perms$uploaders <- c(old.perms$uploaders, uploaders)
        }
    } else {
        if (!missing(owners)) {
            perms$owners <- owners
        }
        if (!missing(uploaders)) {
            perms$uploaders <- uploaders 
        }
    }

    req <- request(paste0(url, "/permissions/", uenc(project)))
    req <- req_method(req, "PUT")
    req <- req_body_json(req, perms)
    req <- req_auth_bearer_token(req, token)
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)

    req_perform(req)
    invisible(NULL)
}
