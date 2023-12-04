#' Abort an upload
#'
#' Abort an upload session, usually after an irrecoverable error. 
#'
#' @param init List containing \code{abort_url} and \code{session_token}.
#' This is typically the return value from \code{\link{startUpload}}.
#' @inheritParams startUpload
#' 
#' @return \code{NULL} is invisibly returned on successful abort.
#' 
#' @seealso
#' \code{\link{startUpload}}, to create \code{init}.
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
#'     init <- startUpload(
#'         project="test-R", 
#'         asset="upload-abort-check", 
#'         version="v1", 
#'         files=list.files(tmp, recursive=TRUE),
#'         probation=TRUE,
#'         directory=tmp
#'     )
#'
#'     # Aborting the upload.
#'     abortUpload(init) 
#' }
#'
#' @export
#' @import httr2
abortUpload <- function(init, url=restUrl()) {
    url <- chomp_url(url)
    req <- request(paste0(url, init$abort_url))
    req <- req_method(req, "POST")
    req <- req_auth_bearer_token(req, init$session_token)
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)
    req_perform(req)
    invisible(NULL)
}
