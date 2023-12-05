#' Cache directory
#'
#' Specify the cache directory in the local filesystem for gypsum-related data. 
#'
#' @param dir String containing the path to a cache directory.
#'
#' @return If \code{dir} is missing, the current setting of the cache directory is returned.
#'
#' If \code{dir} is provided, it is used replace the current setting of the cache directory,
#' and the \emph{previous} setting is invisibly returned.
#'
#' @details
#' If the \code{GYPSUM_CACHE_DIR} environment variable is set, it is used as the initial location of the cache directory.
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
#' @import httr2
#' @importFrom tools R_user_dir
cacheDirectory <- (function() {
    current <- Sys.getenv("GYPSUM_CACHE_DIR", tools::R_user_dir("gypsum", "cache"))
    function(dir) {
        if (missing(dir)) {
            return(current)
        } else {
            prev <- current
            assign("current", dir, envir=parent.env(environment()))
            return(invisible(prev))
        }
    }
})()
