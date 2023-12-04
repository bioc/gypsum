#' Fetch version manifest 
#'
#' Fetch the manifest for a version of an asset of a project.
#'
#' @param project String containing the project name.
#' @param asset String containing the asset name.
#' @param version String containing the version name.
#' @param cache String containing the cache directory.
#' If \code{NULL}, no caching is performed.
#' @param config Configuration object for the S3 bucket, see \code{\link{publicS3Config}} for details.
#'
#' @author Aaron Lun
#' 
#' @return List containing the manifest for this version.
#' Each element is named after the relative path of a file in this version.
#' The value of each element is another list with the following fields:
#' \itemize{
#' \item \code{size}, an integer specifying the size of the file in bytes.
#' \item \code{md5sum}, a string containing the hex-encoded MD5 checksum of the file.
#' \item \code{link} (optional): a list specifying the link destination for a file. 
#' This contains the strings \code{project}, \code{asset}, \code{version} and \code{path}.
#' }
#' 
#' @examples
#' fetchManifest("test-R", "upload-check", "v1")
#' 
#' @export
fetchManifest <- function(project, asset, version, cache=cacheDirectory(), config=publicS3Config()) {
    get_cacheable_json(c(project, asset, version, "..manifest"), cache=cache, config=config)
}
