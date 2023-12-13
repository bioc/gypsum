#' Validate metadata against a database schema
#'
#' Validate metadata against a JSON schema for a database, to ensure that it can be successfully included in the database's index.
#'
#' @param metadata Metadata, usually a named list.
#' @param schema String containing a path to a schema.
#'
#' @return NULL is invisibly returned upon successful validation.
#'
#' @author Aaron Lun
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
validateMetadata <- function(metadata, schema=fetchMetadataSchema()) {
    jsonvalidate::json_validate(toJSON(metadata, auto_unbox=TRUE), schema, error=TRUE, engine="ajv")
    invisible(NULL)
}
