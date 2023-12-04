#' Remove a project or its children
#'
#' Remove a project, an asset of the project, or a version of a particular asset.
#'
#' @param project String containing the project to remove.
#' @param asset String containing the asset to remove.
#' If \code{NULL}, all assets of the \code{project} are removed.
#' @param version String containing the version of the asset to remove.
#' If \code{NULL}, all versions of the specified \code{asset} are removed.
#' Ignored if \code{asset=NULL}.
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to a gypsum administrator account.
#'
#' @return \code{NULL} is invisibly returned if the project or its contents was successfully removed. 
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{createProject}}, to create a project.
#'
#' @examples
#' if (interactive()) {
#'     createProject("test-R-remove", owners="LTLA")
#'     removeProject("test-R-remove")
#' }
#' @export
#' @import httr2
removeProject <- function(project, asset=NULL, version=NULL, url=restUrl(), token=accessToken()) {
    url <- chomp_url(url)

    suffix <- uenc(project)
    if (!is.null(asset)) {
        suffix <- paste0(suffix, "/", uenc(asset))
        if (!is.null(version)) {
            suffix <- paste0(suffix, "/", uenc(version))
        }
    }

    req <- request(paste0(url, "/remove/", suffix))
    req <- req_method(req, "DELETE")
    req <- req_auth_bearer_token(req, token)
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)

    req_perform(req)
    invisible(NULL)
}
