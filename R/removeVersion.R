#' Remove a version of an asset
#'
#' Remove a version of an asset from the gypsum backend.
#'
#' @param project String containing the project to remove.
#' @param asset String containing the asset to remove.
#' @param version String containing the version of the asset to remove.
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to a gypsum administrator account.
#'
#' @return \code{NULL} is invisibly returned if the project or its contents was successfully removed. 
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{removeAsset}} and \code{\link{removeProject}}, to remove an asset or project.
#'
#' @examples
#' if (interactive()) {
#'     # Mocking up a versioned asset.
#'     init <- startUpload(
#'         project="test-R", 
#'         asset="removal", 
#'         version="v1", 
#'         files=character(0),
#'         probation=TRUE
#'     )
#'     completeUpload(init) 
#'
#'     removeVersion("test-R", asset="removal", version="v1")
#' }
#' @export
removeVersion <- function(project, asset, version, url=restUrl(), token=accessToken()) {
    suffix <- paste0(uenc(project), "/", uenc(asset), "/", uenc(version))
    request_removal(suffix, url, token)
}
