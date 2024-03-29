% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removeProject.R
\name{removeProject}
\alias{removeProject}
\title{Remove a project}
\usage{
removeProject(project, url = restUrl(), token = accessToken())
}
\arguments{
\item{project}{String containing the project to remove.}

\item{url}{String containing the URL of the gypsum REST API.}

\item{token}{String containing a GitHub access token to authenticate to the gypsum REST API.
The token must refer to a gypsum administrator account.}
}
\value{
\code{NULL} is invisibly returned if the project was successfully removed.
}
\description{
Remove a project from the gypsum backend.
}
\examples{
if (interactive()) {
    createProject("test-R-remove", owners="LTLA")
    removeProject("test-R-remove")
}
}
\seealso{
\code{\link{createProject}}, to create a project.

\code{\link{removeAsset}} and \code{\link{removeVersion}}, to remove an asset or version.
}
\author{
Aaron Lun
}
