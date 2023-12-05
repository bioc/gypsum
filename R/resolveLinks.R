#' Resolve links in the cache directory
#'
#' Create hard links (or copies, if filesystem links are not supported) for linked-from files to their link destinations.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param cache String containing the path to the cache directory.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#' 
#' @return \code{NULL} is returned on successful completion.
#'
#' @author Aaron Lun
#'
#' @examples
#' saveVersion("test-R", "upload-check", "v1", relink=FALSE)
#' resolveLinks("test-R", "upload-check", "v1")
#'
#' @export
resolveLinks <- function(project, asset, version, cache=cacheDirectory(), overwrite=FALSE, config=publicS3Config()) {
    acquire_lock(cache, project, asset, version)
    on.exit(release_lock(project, asset, version))
    destination <- file.path(cache, BUCKET_CACHE_NAME, project, asset, version)

    manifests <- list()
    selfmanifest <- fetchManifest(project, asset, version, cache=cache, config=config)
    manifests[[paste(project, asset, version, sep="/")]] <- selfmanifest

    for (l in names(selfmanifest)) {
        entry <- selfmanifest[[l]]
        if (is.null(entry$link)) {
            next
        } 

        oldloc <- file.path(project, asset, version, l) 
        if (file.exists(oldloc) && !overwrite) {
            next
        }
        linkdata <- entry$link

        while (TRUE) {
            key <- paste(linkdata$project, linkdata$asset, linkdata$version, sep="/")
            if (is.null(manifests[[key]])) {
                curmanifest <- fetchManifest(linkdata$project, linkdata$asset, linkdata$version, cache=cache, config=config)
                manifests[[key]] <- curmanifest
            } else {
                curmanifest <- manifests[[key]]
            }

            target <- curmanifest[[linkdata$path]]
            if (is.null(target)) {
                stop("cannot find '", linkdata$path, "' in manifest for '", key, "'")
            }

            nextlinkdata <- target$link
            if (is.null(nextlinkdata)) {
                # Too tedious to try to parallelize, so we'll just go in one by one.
                out <- saveFile(linkdata$project, linkdata$asset, linkdata$version, linkdata$path, cache=cache, config=config)
                for (old in oldloc) {
                    unlink(old) # remove any existing file.
                    if (!file.link(old, out) && !file.copy(old, out)) {
                        stop("failed to resolve link for '", l, "'")
                    }
                }
                break
            }

            oldloc <- c(oldloc, file.path(linkdata$project, linkdata$asset, linkdata$version, linkdata$path))
            linkdata <- nextlinkdata
        }
    }

    invisible(NULL)
}
