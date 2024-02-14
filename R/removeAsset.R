#' Remove an asset
#'
#' Remove an asset of a project from the gypsum backend.
#'
#' @param project String containing the project to remove.
#' @param asset String containing the asset to remove.
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to a gypsum administrator account.
#'
#' @return \code{NULL} is invisibly returned if the asset was successfully removed. 
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{removeProject}}, to remove a project.
#'
#' \code{\link{removeVersion}}, to remove a specific version.
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
#'     removeAsset("test-R", asset="removal")
#' }
#' @export
removeAsset <- function(project, asset, url=restUrl(), token=accessToken()) {
    suffix <- paste0(uenc(project), "/", uenc(asset))
    request_removal(suffix, url, token)
}
