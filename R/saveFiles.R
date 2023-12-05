#' Save a file from a version of a project asset
#'
#' Download a file from the gypsum bucket, for a version of an asset of a project.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param prefix String containing the prefix for the path.
#' If provided, files are only downloaded if they have a relative path (i.e., inside the version subdirectory) that starts with this prefix.
#' If \code{NULL}, all files associated with this version are listed.
#' @param destination String containing the path to a destination subdirectory on the local filesystem. 
#' This is treated as the version's subdirectory regardless of \code{prefix}.
#' If \code{NULL}, a path inside \code{cache} is used.
#' @param cache String containing the path to the cache directory.
#' Only used if \code{destination=NULL}.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#' @param overwrite Logical scalar indicating whether to overwrite an existing file at \code{destination}.
#' If \code{FALSE} and \code{destination} exists, the download is skipped.
#' Defaults to \code{!is.null(destination)}, so no extra downloads are performed if the file is already cached.
#' @param concurrent Integer specifying the number of concurrent downloads.
#' 
#' @return The file is downloaded to the local file system.
#' The destination file path is invisibly returned.
#'
#' @author Aaron Lun
#' 
#' @seealso
#' \code{\link{saveFile}}, to save a single file.
#'
#' \code{\link{cacheDirectory}}, for file caching.
#' 
#' @examples
#' out <- saveFiles("test-R", "upload-check", "v1")
#' list.files(out, recursive=TRUE, all.files=TRUE)
#' 
#' @export
#' @importFrom filelock unlock
saveFiles <- function(project, asset, version, prefix=NULL, destination=NULL, cache=cacheDirectory(), overwrite=NULL, concurrent=1, config=publicS3Config()) {
    skip <- FALSE
    completed <- NULL
    if (is.null(destination)) {
        if (is.null(overwrite)) {
            overwrite <- FALSE
        }
        lck <- create_lock(cache, project, asset, version)
        on.exit(unlock(lck))
        destination <- file.path(cache, BUCKET_CACHE_NAME, project, asset, version)

        # If this version's directory was previously cached in its complete form, we skip it.
        if (is.null(prefix)) {
            completed <- file.path(cache, "status", project, asset, version, "COMPLETE")
            skip <- file.exists(completed)
        }
    }

    if (!skip) {
        listing <- listFiles(project, asset, version, prefix=prefix, config=config)
        FUN <- function(x) save_file(x, file.path(destination, x), overwrite=overwrite, config=config, precheck=FALSE)

        if (concurrent == 1L) {
            lapply(listing, FUN)
        } else {
            cl <- makeCluster(concurrent)
            on.exit(stopCluster(cl))
            parLapply(cl, listing, FUN)
        }

        # Marking it as complete.
        if (!is.null(completed)) {
            dir.create(dirname(completed), recursive=TRUE, showWarnings=FALSE)
            write(character(0), file=completed)
        }
    }

    invisible(destination)
}
