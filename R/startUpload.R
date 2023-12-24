#' Start an upload
#'
#' Start an upload of a new version of an asset, or a new asset of a project.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param files Character vector containing the paths of the files to be uploaded.
#' These should be relative to the version's \code{directory}. 
#'
#' Alternatively, a data frame where each row corresponds to a file and contains information about those files.
#' This data frame should contain the following fields:
#' \itemize{
#' \item \code{path}, a string containing the relative path of the file inside the version's subdirectory.
#' \item \code{size}, a non-negative integer specifying the size of the file in bytes.
#' \item \code{md5sum}, a string containing the hex-encoded MD5 checksum of the file.
#' \item (optional) \code{dedup}, a logical indicating whether deduplication should be attempted for each file.
#' }
#' @param links A data frame where each row corresponds to a linked-from file and contains the link destination for that file.
#' This data frame should contain the following fields:
#' \itemize{
#' \item \code{from.path}, a string containing the relative path of the file inside the version's subdirectory.
#' \item \code{to.project}, a string containing the project of the list destination.
#' \item \code{to.asset}, a string containing the asset of the list destination.
#' \item \code{to.version}, a string containing the version of the list destination.
#' \item \code{to.path}, a string containing the path of the list destination.
#' }
#' @param deduplicate Logical scalar indicating whether the backend should attempt deduplication of \code{files} in the immediately previous version.
#' Only has an effect if \code{files} is not a data frame or if the \code{dedup} field is missing.
#' @param probation Logical scalar indicating whether to perform a probational upload.
#' Such uploads must be approved by the project owner before they are considered official.
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to a user that is authorized to upload to the specified \code{project}.
#' @param directory String containing the path to a directory containing the \code{files} to be uploaded.
#' This directory is assumed to correspond to a version of an asset.
#' It only has an effect if \code{files} is a character vector, as it is used to determine the MD5 checksums and sizes.
#' If \code{NULL}, \code{directory} is set to the current working directory.
#' 
#' @return List containing:
#' \itemize{
#' \item \code{file_urls}, a list of lists containing information about each file to be uploaded.
#' This is used by \code{\link{uploadFiles}}.
#' \item \code{complete_url}, a string containing the completion URL, to be used by \code{\link{completeUpload}}.
#' \item \code{abort_url}, a string specifying the abort URL, to be used by \code{\link{abortUpload}}.
#' \item \code{session_token}, a string for authenticating to the newly initialized upload session.
#' }
#'
#' @seealso
#' \code{\link{uploadFiles}}, to actually upload the files.
#'
#' \code{\link{completeUpload}}, to indicate that the upload is completed.
#'
#' \code{\link{abortUpload}}, to abort an upload in progress.
#'
#' \code{\link{prepareDirectoryUpload}}, to create \code{files} and \code{links} from a directory.
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
#'     blob <- startUpload(
#'         project="test-R", 
#'         asset="upload-start-check", 
#'         version="v1", 
#'         files=list.files(tmp, recursive=TRUE),
#'         directory=tmp
#'     )
#'     print(blob)
#'
#'     abortUpload(blob) # just cleaning up after we're done.
#' }
#'
#' @export
#' @import httr2
startUpload <- function(project, asset, version, files, links=NULL, deduplicate=TRUE, probation=FALSE, url=restUrl(), token=accessToken(), directory=NULL) {
    if (is.character(files)) {
        targets <- files
        if (!is.null(directory)) {
            targets <- file.path(directory, files)
        }

        files <- data.frame(
            path = files,
            size = vapply(targets, function(x) file.info(x)$size, 0, USE.NAMES=FALSE),
            md5sum = vapply(targets, function(x) digest::digest(file=x, algo="md5"), "", USE.NAMES=FALSE),
            dedup = rep(deduplicate, length(targets))
        )
    } else if (!("dedup" %in% colnames(files))) {
        files$dedup <- rep(deduplicate, nrow(files))
    }

    formatted <- vector("list", nrow(files))
    for (i in seq_len(nrow(files))) {
        type <- "simple"
        if (files$dedup[i]) {
            type <- "dedup"
        }
        formatted[[i]] <- list(
            type = type,
            path = files$path[i],
            size = files$size[i],
            md5sum = files$md5sum[i]
        )
    }

    if (!is.null(links)) {
        out.links <- vector("list", nrow(links)) 
        for (i in seq_len(nrow(links))) {
            out.links[[i]] <- list(
                type = "link",
                path = links$from.path[i],
                link = list(
                    project = links$to.project[i],
                    asset = links$to.asset[i],
                    version = links$to.version[i],
                    path = links$to.path[i]
                )
            )
        }
        formatted <- c(formatted, out.links)
    }

    url <- chomp_url(url)
    req <- request(paste0(url, "/upload/start/", uenc(project), "/", uenc(asset), "/", uenc(version)))
    req <- req_method(req, "POST")
    req <- req_body_json(req, list(files=formatted, on_probation=probation))
    req <- req_auth_bearer_token(req, token)
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)

    res <- req_perform(req)
    resp_body_json(res)
}
