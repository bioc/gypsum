#' URL for the REST API
#'
#' Get or set the URL for the gypsum REST API.
#'
#' @param url String containing the URL of the REST API.
#'
#' @return If \code{url} is missing, the current setting of the URL is returned.
#'
#' If \code{url} is provided, it is used to replace the current setting of the URL,
#' and the \emph{previous} setting of the URL is invisibly returned.
#'
#' @author Aaron Lun
#' @examples
#' restUrl()
#' old <- restUrl("https://some-other.rest-api.io") # replace it.
#' restUrl()
#' restUrl(old) # setting it back.
#' @export
restUrl <- (function() {
    current <- "https://gypsum.artifactdb.com"
    function(url) {
        if (missing(url)) {
            return(current)
        } else {
            prev <- current
            assign("current", url, envir=parent.env(environment()))
            return(invisible(prev))
        }
    }
})()
