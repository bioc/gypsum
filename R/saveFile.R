#' Save a file from a version of a project asset
#'
#' Download a file from the gypsum bucket, for a version of an asset of a project.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param path String containing the relative path to the file inside the versioned asset.
#' @param cache String containing the path to the cache directory.
#' @param precheck Whether to check if the file exists in the bucket before attempting a download.
#' This may be omitted if the file is known to exist.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#' @param overwrite Logical scalar indicating whether to overwrite an existing file in \code{cache}.
#' If \code{FALSE} and the file exists in \code{cache}, the download is skipped.
#' 
#' @return The file is downloaded to the local file system. The destination file path is returned.
#'
#' @details
#' Unlike \code{\link{saveVersion}}, this function will \emph{not} resolve any links, so \code{path} must actually be present in the bucket.
#' Users should inspect the manifest (or link files) to determine if \code{path} is a linked-from path,
#' then call \code{saveFile} on the link target to obtain the file.
#'
#' @author Aaron Lun
#' 
#' @seealso
#' \code{\link{saveVersion}}, to save all files with the same prefix.
#'
#' \code{\link{cacheDirectory}}, for file caching.
#' 
#' @examples
#' out <- saveFile("test-R", "basic", "v1", "blah.txt")
#' readLines(out)
#' 
#' @export
saveFile <- function(project, asset, version, path, cache=cacheDirectory(), overwrite=FALSE, precheck=TRUE, config=publicS3Config()) {
    acquire_lock(cache, project, asset, version)
    on.exit(release_lock(project, asset, version))

    object <- paste0(project, "/", asset, "/", version, "/", sanitize_path(path))
    destination <- file.path(cache, BUCKET_CACHE_NAME, project, asset, version, path)
    save_file(object, destination=destination, overwrite=overwrite, config=config, precheck=precheck)
    destination
}
