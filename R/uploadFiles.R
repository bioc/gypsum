#' Upload files for a versioned asset
#'
#' Upload files in an initialized upload session for a version of an asset.
#'
#' @param init List containing \code{file_urls} and \code{session_token}.
#' This is typically the return value from \code{\link{startUpload}}.
#' @inheritParams startUpload
#' @param concurrent Integer specifying the number of concurrent uploads.
#' 
#' @return \code{NULL} is invisibly returned on successful upload of all files.
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
#'         asset="upload-files-check", 
#'         version="v1", 
#'         files=list.files(tmp, recursive=TRUE),
#'         directory=tmp
#'     )
#' 
#'     # Executing the upload for all files.
#'     uploadFiles(init, directory=tmp)
#'
#'     # Cleaning up after we're done.
#'     abortUpload(init) 
#' }
#'
#' @export
#' @importFrom parallel makeCluster stopCluster parLapply
uploadFiles <- function(init, directory=NULL, url=restUrl(), concurrent=1) {
    url <- chomp_url(url)
    if (concurrent <= 1) {
        lapply(init$file_urls, .upload_file, directory=directory, url=url, token=init$session_token)
    } else {
        cl <- makeCluster(concurrent)
        on.exit(stopCluster(cl), add=TRUE, after=FALSE)
        parLapply(cl, init$file_urls, .upload_file, directory=directory, url=url, token=init$session_token)
    }
    invisible(NULL)
}

#' @import httr2
.upload_file <- function(info, directory, url, token) {
    path <- info$path
    if (!is.null(directory)) {
        path <- file.path(directory, path)
    }

    if (info$method == "presigned") {
        req <- request(paste0(url, info$url))
        req <- req_method(req, "POST")
        req <- req_auth_bearer_token(req, token)
        req <- process_error(req)
        res <- req_perform(req)
        presigned <- resp_body_json(res)

        req2 <- request(presigned$url)
        req2 <- req_method(req2, "PUT")
        req2 <- req_body_file(req2, path)
        req2 <- req_headers(req2, `Content-MD5`=presigned$md5sum_base64)
        req_perform(req2)

    } else {
        stop("unknown upload method '", info$method, "' for file '", info$path, "'")
    }
}
