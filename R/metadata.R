#' @include BioThings.R

# btMetadata -------------------------------------------------------------

#' @title btMetadata
#'
#' @description
#' Get metadata information from BioThings API metadata endpoints
#'
#' @param client A biothings client name: gene, variant, taxon or chem
#' @param ... Additional API parameters. See API documentation.
#' @param biothings A BioThings class object
#'
#' @return A list of results from the metadata endpoint
#'
#' @export
#' @docType methods
#' @rdname btMetadata-methods
#'
#' @examples
#' metadata <- btMetadata("gene")
#' head(metadata)
setGeneric("btMetadata", signature = c("biothings"),
           function(client, ..., biothings) {
  standardGeneric("btMetadata")
})

#' @rdname btMetadata-methods
#' @aliases btMetadata,BioThings,BioThings-method
setMethod("btMetadata", c(biothings = "BioThings"),
          function(client, ..., biothings) {
  client_config <- slot(biothings, "client")
  params <- list(...)
  res <- .request.get(biothings, client_config$endpoints$metadata$path, params)
  .return.as(res, "records")
})

#' @rdname btMetadata-methods
#' @aliases btMetadata,BioThings,BioThings-method
setMethod("btMetadata", c(biothings = "missing"),
          function(client, ..., biothings) {
  biothings <- BioThings(client)
  btMetadata(client, ..., biothings = biothings)
})

# btFields ---------------------------------------------------------------

#' @title btFields
#'
#' @description
#' Get field metadata information from BioThings API field metadata endpoints
#'
#' @param client A biothings client name: gene, variant, taxon or chem
#' @param ... Additional API parameters. See API documentation.
#' @param biothings A BioThings class object
#'
#' @return A list of results from the metadata fields endpoint
#'
#' @export
#' @docType methods
#' @rdname btFields-methods
#'
#' @examples
#' fields <- btFields("gene")
#' head(fields)
setGeneric("btFields", signature = c("biothings"),
           function(client, ..., biothings) {
   standardGeneric("btFields")
})

#' @rdname btFields-methods
#' @aliases btFields,BioThings,BioThings-method
setMethod("btFields", c(biothings = "BioThings"),
          function(client, ..., biothings) {
  client_config <- slot(biothings, "client")
  params <- list(...)
  res <- .request.get(biothings, client_config$endpoints$metadata_fields$path,
                      params)
  .return.as(res, "records")
})

#' @rdname btFields-methods
#' @aliases btFields,BioThings,BioThings-method
setMethod("btFields", c(biothings = "missing"),
          function(client, ..., biothings) {
  biothings <- BioThings(client)
  btFields(client, ..., biothings = biothings)
})
