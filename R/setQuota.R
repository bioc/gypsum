#' Set project quota
#' 
#' Set the storage quota for a project.
#'
#' @param project String containing the project name.
#' @param baseline Numeric scalar specifying the baseline quota (i.e., at time zero) in bytes.
#' If \code{NULL}, no change is made to the existing baseline of the project.
#' @param growth Numeric scalar specifying the annual growth rate of the quota, in bytes.
#' If \code{NULL}, no change is made to the existing growth rate of the project.
#' @param year Integer scalar specifying the year of creation (i.e., time zero) for the project.
#' If \code{NULL}, no change is made to the existing creation year of the project.
#' @param url String containing the URL of the gypsum REST API.
#' @param token String containing a GitHub access token to authenticate to the gypsum REST API.
#' The token must refer to a gypsum administrator account.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{fetchQuota}}, to fetch the quota.
#'
#' @return \code{NULL} is invisibly returned upon successful setting of the quota.
#'
#' @examples
#' if (interactive()) {
#'     # Creating a project for demonstration purposes.
#'     createProject("test-R-quota", owners="LTLA")
#'
#'     # Setting a baseline of 10 GB with 5 GB in growth per year.
#'     setQuota("test-R-quota", baseline=10^10, growth=5^9, year=2019)
#' }
#'
#' @export
setQuota <- function(project, baseline=NULL, growth=NULL, year=NULL, url=restUrl(), token=accessToken()) {
    url <- chomp_url(url)

    body <- list()
    if (!is.null(baseline)) {
        body$baseline <- baseline
    }
    if (!is.null(growth)) {
        body$growth_rate <- growth
    }
    if (!is.null(year)) {
        body$year <- year
    }

    req <- request(paste0(url, "/quota/", uenc(project)))
    req <- req_method(req, "PUT")
    req <- req_body_json(req, body)
    req <- req_auth_bearer_token(req, token)
    req <- process_error(req)

    req_perform(req)
    invisible(NULL)
}
