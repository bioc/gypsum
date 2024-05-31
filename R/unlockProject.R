#' Unlock a project
#'
#' Unlock a project on the gypsum backend.
#' This is typically caused by a partial upload that was not properly aborted.
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
#' \code{\link{abortUpload}}, to correctly abort an upload upon failure.
#'
#' @examples
#' if (interactive()) {
#'     unlockProject("test-R")
#' }
#' @export
unlockProject <- function(project, url=restUrl(), token=accessToken()) {
    url <- chomp_url(url)
    req <- request(paste0(url, "/unlock/", project))
    req <- req_method(req, "DELETE")
    req <- req_auth_bearer_token(req, token)
    req <- process_error(req)

    req_perform(req)
    invisible(NULL)
}
