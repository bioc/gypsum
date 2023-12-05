#' Save a file from a version of a project asset
#'
#' Download a file from the gypsum bucket, for a version of an asset of a project.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param path String containing the relative path to the file inside the version.
#' @param destination String containing the destination path on the local filesystem. 
#' If \code{NULL}, a path inside the \code{\link{cacheDirectory}} is used.
#' @param precheck Whether to check if the file exists in the bucket before attempting a download.
#' This may be omitted if the file is known to exist.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#' @param overwrite Logical scalar indicating whether to overwrite an existing file at \code{destination}.
#' If \code{FALSE} and \code{destination} exists, the download is skipped.
#' Defaults to \code{!is.null(destination)}, so no extra downloads are performed if the file is already cached.
#' 
#' @return The file is downloaded to the local file system.
#' The destination file path is invisibly returned.
#'
#' @author Aaron Lun
#' 
#' @seealso
#' \code{\link{saveFiles}}, to save all files with the same prefix.
#'
#' \code{\link{cacheDirectory}}, for file caching.
#' 
#' @examples
#' out <- saveFile("test-R", "upload-check", "v1", "blah.txt")
#' readLines(out)
#' 
#' @export
saveFile <- function(project, asset, version, path, destination=NULL, overwrite=NULL, precheck=TRUE, config=publicS3Config()) {
    if (is.null(destination)) {
        if (is.null(overwrite)) {
            overwrite <- FALSE
        }
        destination <- do.call(file.path, c(list(cacheDirectory(), BUCKET_CACHE_NAME, project, asset, version), split(path, "/")[[1]]))
        lck <- create_lock(project, asset, version)
        on.exit(unlock(lck))
    }

    object <- paste0(project, "/", asset, "/", version, "/", path)
    save_file(object, destination=destination, overwrite=overwrite, config=config, precheck=precheck)
    invisible(destination)
}
