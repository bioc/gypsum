#' Validate metadata against a JSON schema
#'
#' Validate metadata against a JSON schema for a SQLite database.
#' This ensures that it can be successfully inserted in the database in downstream indexing steps.
#'
#' @param metadata Metadata to be checked.
#' This is usually an R object like a named list, but may also be a JSON-formatted string.
#' @param schema String containing a path to a schema.
#' @param stringify Logical scalar indicating whether to convert \code{metadata} to a JSON-formatted string.
#' Defaults to \code{TRUE} if \code{metadata} is not already a string.
#'
#' @return NULL is invisibly returned upon successful validation.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{fetchMetadataSchema}}, to get the JSON schemas.
#'
#' \code{\link{fetchMetadataDatabase}}, to obtain the SQLite database files.
#'
#' @examples
#' metadata <- list(
#'     title="Fatherhood",
#'     description="Luke ich bin dein Vater.",
#'     sources=list(
#'        list(provider="GEO", id="GSE12345")
#'     ),
#'     taxonomy_id=list("9606"),
#'     genome=list("GRCm38"),
#'     maintainer_name="Darth Vader",
#'     maintainer_email="vader@empire.gov",
#'     bioconductor_version="3.10"
#' )
#' 
#' validateMetadata(metadata)
#'
#' @export
#' @importFrom jsonlite toJSON
validateMetadata <- function(metadata, schema=fetchMetadataSchema(), stringify=NULL) {
    if (is.null(stringify)) {
        stringify <- !is.character(metadata)
    }
    if (stringify) {
        metadata <- toJSON(metadata, auto_unbox=TRUE)
    }
    jsonvalidate::json_validate(metadata, schema, error=TRUE, engine="ajv")
    invisible(NULL)
}
