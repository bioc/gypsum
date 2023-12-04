#' Get and set GitHub access tokens
#'
#' Get and set GitHub access tokens for authentication to the gypsum API's endpoints.
#'
#' @param full Logical scalar indicating whether to return the full token details.
#' @param request Logical scalar indicating whether to request a new token if no cached token is available or if the current token has expired.
#' @param token String containing a GitHub personal access token.
#' This should have the \code{"read:org"} and \code{"read:user"} scopes.
#' If missing, the user will be prompted to use GitHub's Oauth web application flow to acquire a token.
#' If \code{NULL}, any existing tokens are cleared from cache.
#' @param disk.cache Logical scalar indicating whether to cache the token to disk.
#' If \code{FALSE}, the token is only cached in memory, which can be helpful for security purposes.
#' @param github.url String containing the URL for the GitHub API.
#' This is used to acquire more information about the token.
#' @param app.key String containing the key for a GitHub Oauth app.
#' @param app.secret String containing the secret for a GitHub Oauth app.
#' @param app.url String containing a URL of the gypsum REST API.
#' This is used to obtain \code{app.key} and \code{app.secret} if either are \code{NULL}.
#' @param user.agent String specifying the user agent for queries to various endpoints.
#'
#' @return
#' \code{setAccessToken} sets the access token and invisibly returns a list containing:
#' \itemize{
#' \item \code{token}, a string containing the token.
#' \item \code{name}, the name of the GitHub user authenticated by the token.
#' \item \code{expires}, the Unix time at which the token expires.
#' }
#' 
#' If \code{full=TRUE}, \code{accessToken} returns the same list, typically retrieved from one of the caches.
#' If no token was cached or the cached token has expired, it will call \code{setAccessToken} with default arguments to obtain one if \code{request=TRUE};
#' otherwise if \code{request=FALSE}, \code{NULL} is returned.
#'
#' If \code{full=FALSE}, \code{accessToken} will return a string containing a token (or \code{NULL}, if no token is available and \code{request=FALSE}).
#'
#' @author Aaron Lun
#'
#' @examples
#' if (interactive()) {
#'     accessToken()
#' }
#' @name accessToken
NULL

token.cache <- new.env()
token.cache$auth.info <- NULL

#' @importFrom tools R_user_dir
token_cache_path <- function() {
    file.path(cacheDirectory(), "credentials", "token.txt")
}

#' @export
#' @rdname accessToken
accessToken <- function(full = FALSE, request=TRUE) {
    if (full) {
        OFn <- identity
    } else {
        OFn <- function(x) x$token
    }
    expiry_leeway <- 10 # number of seconds until use of the token.

    in.memory <- token.cache$auth.info
    if (!is.null(in.memory)) {
        if (in.memory$expires > Sys.time() + expiry_leeway) {
            return(OFn(in.memory))
        } else {
            token.cache$auth.info <- NULL
        }
    }

    cache.path <- token_cache_path()
    if (file.exists(cache.path)) {
        dump <- readLines(cache.path)
        exp <- as.double(dump[3])
        if (exp > Sys.time() + expiry_leeway) {
            info <- list(token=dump[1], name=dump[2], expires=exp)
            token.cache$auth.info <- info
            return(OFn(info))
        } else {
            unlink(cache.path)
        }
    }

    if (request) {
        payload <- setAccessToken()
        OFn(payload) # making it visible.
    } else {
        NULL
    }
}

#' @export
#' @rdname accessToken
#' @import httr2
setAccessToken <- function(token, disk.cache=TRUE, app.url=restUrl(), app.key = NULL, app.secret = NULL, github.url="https://api.github.com", user.agent=NULL) {
    cache.path <- token_cache_path()
    if (!missing(token) && is.null(token)) {
        token.cache$auth.info <- NULL
        if (disk.cache) { # don't write to disk if cache=FALSE.
            unlink(cache.path)
        }
        return(invisible(NULL))
    }

    if (missing(token)) {
        if (is.null(app.key) || is.null(app.secret)) {
            req <- request(paste0(chomp_url(app.url), "/credentials/github-app"))
            req <- req_user_agent(req, user.agent)
            res <- req_perform(req)
            info <- resp_body_json(res)
            app.key <- info$id
            app.secret <- info$secret
        }

        oapp <- oauth_client(
            id = app.key,
            secret = app.secret,
            token_url = "https://github.com/login/oauth/access_token",
            name = "gypsum"
        )
        ores <- oauth_flow_auth_code(oapp, auth_url = "https://github.com/login/oauth/authorize")
        token <- ores$access_token
    }

    req <- request(paste0(chomp_url(github.url), "/user"))
    req <- req_auth_bearer_token(req, token)
    req <- req_user_agent(req, user.agent)
    res <- req_perform(req)
    name <- resp_body_json(res)$login
    expiry <- Inf
    expires <- resp_headers(res)[["github-authentication-token-expiration"]]
    if (!is.null(expires)) {
        frags <- strsplit(expires, " ")[[1]]
        expiry <- as.double(as.POSIXct(paste(frags[1], frags[2]), tz=frags[3]))
    }

    if (disk.cache) {
        dir.create(dirname(cache.path), showWarnings=FALSE, recursive=TRUE)
        writeLines(c(token, name, expiry), con=cache.path)
        Sys.chmod(cache.path, mode="0600") # prevent anyone else from reading this on shared file systems.
    }
    vals <- list(token=token, name=name, expires=expiry)
    token.cache$auth.info <- vals
    invisible(vals)
}
