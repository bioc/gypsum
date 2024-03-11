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
#' @param github.url String containing the URL for the GitHub API.
#' This is used to acquire more information about the token.
#' @param app.key String containing the key for a GitHub Oauth app.
#' @param app.secret String containing the secret for a GitHub Oauth app.
#' @param app.url String containing a URL of the gypsum REST API.
#' This is used to obtain \code{app.key} and \code{app.secret} if either are \code{NULL}.
#' @param user.agent String specifying the user agent for queries to various endpoints.
#' @param cache String containing a path to the cache directory, to store the token across R sessions.
#' If \code{NULL}, the token is not cached to (or read from) disk, which improves security on shared filesystems. 
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

token_cache_path <- function(cache) {
    file.path(cache, "credentials", "token.txt")
}

#' @export
#' @rdname accessToken
#' @importFrom filelock lock unlock
accessToken <- function(full = FALSE, request=TRUE, cache=cacheDirectory()) {
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

    if (!is.null(cache)) {
        cache.path <- token_cache_path(cache)
        if (file.exists(cache.path)) {
            dump <- (function() {
                # Lock inside a function to guarantee unlocking prior to a possible lock() inside setAccessToken().
                lck <- lock(paste0(cache.path, ".LOCK"), exclusive=FALSE)
                on.exit(unlock(lck), add=TRUE, after=FALSE)
                readLines(cache.path)
            })()

            exp <- as.double(dump[3])
            if (exp > Sys.time() + expiry_leeway) {
                info <- list(token=dump[1], name=dump[2], expires=exp)
                token.cache$auth.info <- info
                return(OFn(info))
            } else {
                unlink(cache.path)
            }
        }
    }

    if (request) {
        payload <- setAccessToken(cache=cache)
        OFn(payload) # making it visible.
    } else {
        NULL
    }
}

#' @export
#' @rdname accessToken
#' @import httr2
#' @importFrom filelock lock unlock
setAccessToken <- function(token, app.url=restUrl(), app.key = NULL, app.secret = NULL, github.url="https://api.github.com", user.agent=NULL, cache=cacheDirectory()) {
    cache.path <- NULL
    if (!is.null(cache)) {
        cache.path <- token_cache_path(cache)
    }

    if (!missing(token) && is.null(token)) {
        token.cache$auth.info <- NULL
        if (!is.null(cache.path)) {
            unlink(cache.path)
        }
        return(invisible(NULL))
    }

    if (missing(token)) {
        if (!interactive()) {
            stop("cannot request a new access token in a non-interactive session")
        }

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

    if (!is.null(cache.path)) {
        dir.create(dirname(cache.path), showWarnings=FALSE, recursive=TRUE)
        lck <- lock(paste0(cache.path, ".LOCK"))
        on.exit(unlock(lck), add=TRUE, after=FALSE)
        writeLines(c(token, name, expiry), con=cache.path)
        Sys.chmod(cache.path, mode="0600") # prevent anyone else from reading this on shared file systems.
    }

    vals <- list(token=token, name=name, expires=expiry)
    token.cache$auth.info <- vals
    invisible(vals)
}
