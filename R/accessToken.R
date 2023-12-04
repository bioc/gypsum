#' Get and set GitHub access tokens
#'
#' Get and set GitHub access tokens for authentication to the gypsum API's endpoints.
#'
#' @param token String containing a GitHub personal access token.
#' This should have the \code{"read:org"} and \code{"read:user"} scopes.
#' If missing, the user will be prompted to use GitHub's Oauth web application flow to acquire a token.
#' If \code{NULL}, any existing tokens are cleared from cache.
#' @param disk.cache Logical scalar indicating whether to cache the token to disk.
#' If \code{FALSE}, the token is only cached in memory, which can be helpful for security purposes.
#' @param github.url String containing the URL for the GitHub API.
#' @param app.key String containing the key for a GitHub Oauth app.
#' If \code{NULL}, the default \dQuote{ArtifactDB} application is used.
#' @param app.secret String containing the secret for a GitHub Oauth app.
#' If \code{NULL}, the default \dQuote{ArtifactDB} application is used.
#' @param user.agent String specifying the user agent for queries to various endpoints.
#'
#' @return
#' \code{setAccessToken} sets the access token and invisibly returns a list containing:
#' \itemize{
#' \item \code{token}, a string containing the token.
#' \item \code{name}, the name of the GitHub user authenticated by the token.
#' \item \code{expiry}, the Unix time at which the token expires.
#' }
#' 
#' \code{accessToken} returns the same list, typically retrieved from one of the caches.
#' If no token was cached, it will call \code{setAccessToken} with default arguments to obtain one. 
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
    file.path(R_user_dir("gypsum", "cache"), "token.txt")
}

#' @export
#' @rdname accessToken
accessToken <- function() {
    in.memory <- token.cache$auth.info
    if (!is.null(in.memory)) {
        return(in.memory)
    }

    cache.path <- token_cache_path()
    if (file.exists(cache.path)) {
        dump <- readLines(cache.path)
        return(list(token=dump[1], name=dump[2], expires=dump[3]))
    }

    payload <- setAccessToken()
    payload # making it visible.
}

#' @export
#' @rdname accessToken
#' @import httr2
setAccessToken <- function(token, disk.cache=TRUE, url="https://api.github.com", app.key = NULL, app.secret = NULL, user.agent=NULL) {
    cache.path <- token_cache_path()
    if (!missing(token) && is.null(token)) {
        token.cache$auth.info <- NULL
        if (disk.cache) { # don't write to disk if cache=FALSE.
            unlink(cache.path())
        }
        return(invisible(NULL))
    }

    if (missing(token)) {
        if (is.null(app.key) || is.null(app.secret)) {
            req <- request(paste0(default.laundry, "/.github-app-info"))
            req <- req_user_agent(req, user.agent)
            res <- req_perform(req)
            info <- resp_body_json(res)
            app.key <- info$key
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

    req <- request(paste0(url, "/user"))
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
    }
    vals <- list(token=token, name=name, expires=expiry)
    token.cache$auth.info <- vals
    invisible(vals)
}

default.laundry <- "https://gh2jwt.aaron-lun.workers.dev"
