#' Complete an upload
#'
#' Complete an upload session after all files have been uploaded.
#'
#' @param init List containing \code{complete_url} and \code{session_token}.
#' This is typically the return value from \code{\link{uploadStart}}.
#' @inheritParams uploadStart
#' 
#' @return \code{NULL} is invisibly returned on successful completion.
#' 
#' @seealso
#' \code{\link{uploadStart}}, to create \code{init}.
#' 
#' @author Aaron Lun
#' @examples
#' tmp <- tempfile()
#' dir.create(tmp)
#' write(file=file.path(tmp, "blah.txt"), LETTERS)
#' dir.create(file.path(tmp, "foo"))
#' write(file=file.path(tmp, "foo", "bar.txt"), 1:10)
#'
#' if (interactive()) {
#'     init <- uploadStart(
#'         project="test-R", 
#'         asset="upload-complete-check", 
#'         version="v1", 
#'         files=list.files(tmp, recursive=TRUE),
#'         probation=TRUE,
#'         directory=tmp
#'     )
#'     uploadFiles(init, directory=tmp)
#'
#'     # Finishing the upload.
#'     uploadComplete(init) 
#' }
#'
#' @export
#' @import httr2
uploadComplete <- function(init, url=restUrl()) {
    url <- chomp_url(url)
    req <- request(paste0(url, init$complete_url))
    req <- req_method(req, "POST")
    req <- req_auth_bearer_token(req, init$session_token)
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)
    req_perform(req)
    invisible(NULL)
}
