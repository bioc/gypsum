#' Upload a directory to the gypsum backend
#'
#' Convenience method to upload a directory to the gypsum backend as a versioned asset of a project.
#' This requires uploader permissions to the relevant project.
#' 
#' @param directory String containing the path to a directory to be uploaded.
#' @inheritParams startUpload
#' @param concurrent Integer scalar specifying the number of concurrent uploads in \code{\link{uploadFiles}}.
#' @param abort.failed Logical scalar indicating whether to abort the upload on any failure.
#' Setting this to \code{FALSE} can be helpful for diagnosing upload problems.
#' @param cache String containing the path to the cache for saving files, e.g., in \code{\link{saveVersion}}.
#' Used to convert symbolic links to upload links, see \code{\link{prepareDirectoryUpload}}.
#'
#' @return On successful upload, \code{NULL} is invisibly returned.
#'
#' @details
#' This function is a wrapper around \code{\link{prepareDirectoryUpload}} and \code{\link{startUpload}} and friends.
#' The aim is to streamline the upload of a directory's contents when no customization of the file listing is required.
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
#'     # Uploading a probational version for test purposes.
#'     uploadDirectory(staging, "test-R", "upload-dir-check", version, probation=TRUE)
#'
#'     # Cleaning up after ourselves.
#'     gypsum::rejectProbation("test-R", "upload-dir-check", version)
#' }
#'
#' @export
#' @importFrom gypsum cacheDirectory restUrl accessToken startUpload abortUpload uploadFiles completeUpload prepareDirectoryUpload
uploadDirectory <- function(directory, project, asset, version, cache=cacheDirectory(), deduplicate=TRUE, probation=FALSE, url=restUrl(), token=accessToken(), concurrent=1, abort.failed=TRUE) {
    listing <- prepareDirectoryUpload(directory, links="always", cache=cache)

    blob <- startUpload(
        project=project,
        asset=asset,
        version=version, 
        files=listing$files,
        links=listing$links,
        directory=directory,
        probation=probation,
        url=url,
        token=token
    )

    success <- FALSE
    if (abort.failed) {
        on.exit({
            if (!success) {
                abortUpload(blob)
            }
        }, add=TRUE, after=FALSE)
    }

    uploadFiles(blob, directory=directory, url=url, concurrent=concurrent)
    completeUpload(blob, url=url)

    success <- TRUE
    invisible(NULL)
}
