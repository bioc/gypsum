#' Prepare to upload a directory 
#'
#' Prepare to upload a directory's contents via \code{\link{startUpload}}.
#' This goes through the directory to list its contents and convert symlinks to upload links.
#'
#' @param directory String containing the path to a directory, the contents of which are to be uploaded via \code{\link{startUpload}}.
#' @param links String indicating how to handle symlinks in \code{directory}.
#' \itemize{
#' \item \code{"auto"} will attempt to convert symlinks into upload links.
#' If the conversion fails, a regular upload is performed.
#' \item \code{"always"} will attempt to convert symlinks into upload links.
#' If the conversion fails, an error is raised.
#' \item \code{"never"} will never attempt to convert symlinks into upload links.
#' All symlinked files are treated as regular uploads.
#' }
#' @param cache String containing a path to the cache directory, used to convert symlinks into upload links.
#'
#' @details
#' Files in \code{directory} (that are not symlinks) are used as regular uploads, i.e., \code{files=} in \code{\link{startUpload}}.
#' 
#' If \code{directory} contains a symlink to a file in \code{cache}, 
#' we assume that it points to a file that was previously downloaded by, e.g., \code{\link{saveFile}} or \code{\link{saveVersion}}.
#' Thus, instead of performing a regular upload, we attempt to create an upload link, i.e., \code{links=} in \code{\link{startUpload}}.
#' This is achieved by examining the destination path of the symlink and inferring the link destination in the backend.
#' Note that this still works if the symlinks are dangling.
#'
#' If a symlink cannot be converted into an upload link, it will be used as a regular upload,
#' i.e., the contents of the symlink destination will be uploaded by \code{\link{startUpload}}.
#' In this case, an error will be raised if the symlink is dangling as there is no file that can actually be uploaded.
#' If \code{links="always"}, an error is raised instead upon symlink conversion failure.
#'
#' This function is intended to be used with \code{\link{cloneVersion}}, which creates symlinks to files in \code{cache}.
#'
#' @return List containing \code{files}, a character vector to be used as \code{files=} in \code{\link{startUpload}};
#' and \code{links}, a data frame to be used as \code{links=} in \code{\link{startUpload}}.
#' 
#' @seealso
#' \code{\link{startUpload}}, to actually start the upload.
#'
#' \code{\link{cloneVersion}}, to prepare the symlinks.
#'
#' @examples
#' tmp <- tempfile()
#' out <- cloneVersion("test-R", "basic", "v1", destination=tmp)
#' write(file=file.path(tmp, "heanna"), "sumire")
#' prepareDirectoryUpload(tmp)
#'
#' @export
prepareDirectoryUpload <- function(directory, links=c("auto", "always", "never"), cache=cacheDirectory()) {
    links <- match.arg(links)

    out <- list.files(directory, recursive=TRUE)
    out.files <- character()
    out.links <- list(
        from.path=character(0), 
        to.project=character(0), 
        to.asset=character(0), 
        to.version=character(0), 
        to.path=character(0)
    )

    # We sanitize all paths so that we can use exact string matching
    # without worrying about whether \ or / is the file separator.
    cache <- sanitize_path(cache)
    if (!endsWith(cache, "/")) {
        cache <- paste0(cache, "/")
    }

    for (x in out) {
        dest <- Sys.readlink(file.path(directory, x))
        if (dest == "") {
            out.files <- c(out.files, x)
            next
        }

        if (links == "never") {
            if (!file.exists(dest)) {
                stop("cannot use a dangling link to '", dest, "' as a regular upload")
            }
            out.files <- c(out.files, x)
            next
        }

        dest <- sanitize_path(dest)
        dest.components <- match_path_to_cache(dest, cache)
        if (!is.null(dest.components)) {
            out.links$from.path <- c(out.links$from.path, x)
            out.links$to.project <- c(out.links$to.project, dest.components$project)
            out.links$to.asset <- c(out.links$to.asset, dest.components$asset)
            out.links$to.version <- c(out.links$to.version, dest.components$version)
            out.links$to.path <- c(out.links$to.path, dest.components$path)
            next
        }

        if (links == "always") {
            stop("failed to convert symlink '", dest, "' to an upload link")
        } else if (!file.exists(dest)) {
            stop("cannot use a dangling link to '", dest, "' as a regular upload")
        }
        out.files <- c(out.files, x)
    }

    list(files=out.files, links=do.call(data.frame, out.links))
}

#' @importFrom utils tail
match_path_to_cache <- function(path, cache) {
    if (!startsWith(path, cache)) {
        return(NULL)
    }

    remainder <- substr(path, nchar(cache) + 1L, nchar(path))
    components <- strsplit(remainder, "/")[[1]]
    if (length(components) <= 4L || components[1] != BUCKET_CACHE_NAME) { # we need CACHE_BUCKET_NAME/project/asset/version/path
        return(NULL)
    }

    list(
        project=components[2], 
        asset=components[3], 
        version=components[4], 
        path=paste(tail(components, -4L), collapse="/")
    )
}
