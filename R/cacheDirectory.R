#' Cache directory
#'
#' Specify the cache directory in the local filesystem for gypsum-related data. 
#'
#' @param dir String containing the path to a cache directory.
#'
#' @return If \code{dir} is missing, the current setting of the cache directory is returned.
#'
#' If \code{dir} is provided, it is used to replace the current setting of the cache directory,
#' and the \emph{previous} setting is invisibly returned.
#'
#' @details
#' If the \code{GYPSUM_CACHE_DIR} environment variable is set before the first call to \code{\link{cacheDirectory}}, it is used as the initial location of the cache directory.
#' Otherwise, the initial location is based on \code{\link{R_user_dir}}. 
#'
#' @author Aaron Lun
#'
#' @examples
#' cacheDirectory()
#' old <- cacheDirectory(tempfile())
#' cacheDirectory()
#' cacheDirectory(old) # setting it back.
#'
#' @export
#' @importFrom rappdirs user_cache_dir
cacheDirectory <- (function() {
    current <- NULL
    function(dir) {
        if (is.null(current)) {
            location <- Sys.getenv("GYPSUM_CACHE_DIR", user_cache_dir("gypsum", "ArtifactDB"))
            assign("current", location, envir=parent.env(environment()))
        }
        if (missing(dir)) {
            return(current)
        } else {
            prev <- current
            assign("current", dir, envir=parent.env(environment()))
            return(invisible(prev))
        }
    }
})()
