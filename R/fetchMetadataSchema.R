#' Fetch a metadata schema
#'
#' Fetch a JSON schema file for metadata to be inserted into a SQLite database
#' (see \url{https://github.com/ArtifactDB/gypsum-to-sqlite}).
#' Each SQLite database is created from metadata files uploaded to the gypsum backend,
#' so clients uploading objects to be incorporated into the database should validate their metadata against the corresponding JSON schema.
#'
#' @param name String containing the name of the schema.
#' This can be the name of any JSON schema file in \url{https://github.com/ArtifactDB/gypsum-to-sqlite/tree/master/schemas},
#' after removing the \code{.json} suffix.
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

    url <- paste0("https://raw.githubusercontent.com/ArtifactDB/gypsum-to-sqlite/master/schemas/", name, ".json")

    flck <- lock(paste0(cache.path, ".LOCK"))
    on.exit(unlock(flck))
    req <- request(url)
    req_perform(req, path=cache.path)

    cache.path
}
