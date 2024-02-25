#' Clone a version's directory structure
#'
#' Clone the directory structure for a versioned asset into a separate location.
#' This is typically used to prepare a new version for a lightweight upload.
#' 
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param destination String containing a path to a destination directory at which to create the clone.
#' @param download Logical scalar indicating whether the version's files should be downloaded first.
#' This can be set to \code{FALSE} to create a clone without actually downloading any of the version's files.
#' @param cache String containing the path to the cache directory.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#' @param ... Further arguments to pass to \code{\link{saveVersion}}.
#' Only used if \code{download=TRUE}.
#'
#' @return The directory structure of the specified version is cloned to \code{destination},
#' and a \code{NULL} is invisibly returned.
#'
#' @details
#' Cloning of a versioned asset involves creating a directory at \code{destination} that has the same contents as the corresponding project-asset-version directory.
#' All files in the specified version are represented as symlinks from \code{destination} to the corresponding file in the \code{cache}. 
#' The idea is that, when \code{destination} is used in \code{\link{prepareDirectoryUpload}}, 
#' the symlinks are converted into upload links, i.e., \code{links=} in \code{\link{startUpload}}.
#' This allows users to create new versions very cheaply as duplicate files are not uploaded to/stored in the backend.
#'
#' Users can more-or-less do whatever they want inside the cloned \code{destination}, but they should treat the symlink targets as read-only.
#' That is, they should not modify the contents of the linked-to file, as these refer to assumed-immutable files in the \code{cache}.
#' If a file in \code{destination} needs to be modified, the symlink should be deleted and replaced with an actual file;
#' this avoids mutating the \code{cache} and it ensures that \code{\link{prepareDirectoryUpload}} recognizes that a new file actually needs to be uploaded.
#'
#' Advanced users can set \code{download=FALSE}, in which case symlinks are created even if their targets are not present in \code{cache}.
#' In such cases, \code{destination} should be treated as write-only due to the potential presence of dangling symlinks.
#' This mode is useful for uploading a new version of an asset without downloading the files from the existing version,
#' assuming that the modifications associated with the former can be achieved without reading any of the latter.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{prepareDirectoryUpload}}, to prepare an upload based on the directory contents.
#'
#' @examples
#' tmp <- tempfile()
#' out <- cloneVersion("test-R", "basic", "v1", destination=tmp)
#' list.files(tmp, recursive=TRUE)
#' Sys.readlink(file.path(tmp, "foo", "bar.txt"))
#'
#' # Files should be replaced rather than modified via the symlink:
#' existing <- file.path(tmp, "foo", "bar.txt")
#' unlink(existing) # Deleting the symlink...
#' write(file=existing, "YAY") # ... and writing a replacement file.
#'
#' # Symlinks are converted to upload links:
#' prepareDirectoryUpload(tmp)
#' @export
cloneVersion <- function(project, asset, version, destination, download=TRUE, cache=cacheDirectory(), config=publicS3Config(), ...) {
    if (download) {
        saveVersion(project, asset, version, cache=cache, config=config, ...)
    }

    final.cache <- file.path(cache, BUCKET_CACHE_NAME, project, asset, version)
    listing <- fetchManifest(project, asset, version, cache=cache, config=config)
    dir.create(destination)

    # Only run this after the manifest has been fetched,
    # as this guarantees that the cache exists.
    final.cache <- normalizePath(final.cache, mustWork=TRUE)

    # Create symlinks back to the cache.
    for (x in names(listing)) {
        dpath <- file.path(destination, x)
        dir.create(dirname(dpath), showWarnings=FALSE)
        file.symlink(file.path(final.cache, x), dpath)
    }

    invisible(NULL)
}
