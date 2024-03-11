#' Fetch a metadata schema
#'
#' Fetch a JSON schema file for metadata to be inserted into a SQLite database
#' (see \url{https://github.com/ArtifactDB/bioconductor-metadata-index}).
#' Each SQLite database is created from metadata files uploaded to the gypsum backend,
#' so clients uploading objects to be incorporated into the database should validate their metadata against the corresponding JSON schema.
#'
#' @param name String containing the name of the schema.
#' This can be the name of any JSON schema file published at \url{https://github.com/ArtifactDB/bioconductor-metadata-index}.
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
#' \code{\link{fetchMetadataDatabase}}, to obtain the SQLite database of metadata.
#' 
#' @examples
#' fetchMetadataSchema()
#' @export
#' @importFrom filelock lock unlock
fetchMetadataSchema <- function(name="bioconductor/v1.json", cache=cacheDirectory(), overwrite=FALSE) {
    if (is.null(cache)) {
        cache.path <- tempfile(fileext=".json")
    } else {
        cache.dir <- file.path(cache, "schemas")
        cache.path <- file.path(cache.dir, name)
        dir.create(dirname(cache.path), showWarnings=FALSE, recursive=TRUE)

        if (file.exists(cache.path) && !overwrite) {
            # Seeing if we can return the path directly; we aquire lock to
            # check that we're not in the middle of a download. 
            (function() {
                flck <- lock(paste0(cache.path, ".LOCK"), exclusive=FALSE)
                on.exit(unlock(flck), add=TRUE, after=FALSE)
            })()
            return(cache.path)
        }
    }

    url <- paste0("https://artifactdb.github.io/bioconductor-metadata-index/", name)
    flck <- lock(paste0(cache.path, ".LOCK"))
    on.exit(unlock(flck), add=TRUE, after=FALSE)
    download_and_rename_file(url, cache.path)

    cache.path
}
