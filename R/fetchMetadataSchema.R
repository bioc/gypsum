#' Fetch one of the known metadata schemas
#'
#' Fetch the JSON metadata schema for one of the known database indices.
#' These indices are generated from the files uploaded to the gypsum backend,
#' so clients planning to become part of these indices should validate their metadata against the indices' expectations.
#'
#' @param name String containing the name of the database.
#' @param cache String containing the cache directory.
#' If \code{NULL}, no caching is performed.
#' @param overwrite Logical scalar indicating whether to overwrite an existing file in \code{cache}, if one is present.
#'
#' @return String containing a path to the downloaded schema.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{validateMetadata}}, to validate metadata against a chosen schema.
#' 
#' @examples
#' fetchMetadataSchema()
#' @export
#' @importFrom filelock lock unlock
fetchMetadataSchema <- function(name="bioconductor", cache=cacheDirectory(), overwrite=FALSE) {
    if (is.null(cache)) {
        cache.path <- tempfile(fileext=".json")
    } else {
        cache.dir <- file.path(cache, "schemas")
        dir.create(cache.dir, showWarnings=FALSE, recursive=TRUE)
        cache.path <- file.path(cache.dir, paste0(name, ".json"))
        if (file.exists(cache.path) && !overwrite) {
            flck <- lock(paste0(cache.path, ".LOCK"), exclusive=FALSE)
            unlock(flck)
            return(cache.path)
        }
    }

    name <- match.arg(name)
    if (name == "bioconductor") {
        url <- "https://raw.githubusercontent.com/ArtifactDB/gypsum-bioc-index/master/schemas/bioconductor.json"
    }

    flck <- lock(paste0(cache.path, ".LOCK"))
    on.exit(unlock(flck))
    req <- request(url)
    req_perform(req, path=cache.path)

    cache.path
}
