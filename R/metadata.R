#' @include BioThings.R

# getMetadata -------------------------------------------------------------

#' @title getMetadata
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
#' @rdname getMetadata-methods
#'
#' @examples
#' metadata <- getMetadata("gene")
#' head(metadata)
setGeneric("getMetadata", signature = c("biothings"),
           function(client, ..., biothings) {
  standardGeneric("getMetadata")
})

#' @rdname getMetadata-methods
#' @aliases getMetadata,BioThings,BioThings-method
setMethod("getMetadata", c(biothings = "BioThings"),
          function(client, ..., biothings) {
  client_config <- biothings@clients[[client]]
  params <- list(...)
  res <- .request.get(biothings, client,
                      client_config$endpoints$metadata$path, params)
  .return.as(res, "records")
})

#' @rdname getMetadata-methods
#' @aliases getMetadata,BioThings,BioThings-method
setMethod("getMetadata", c(biothings = "missing"),
          function(client, ..., biothings) {
  biothings <- BioThings()
  getMetadata(client, ..., biothings = biothings)
})

# getFields ---------------------------------------------------------------

#' @title getFields
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
#' @rdname getFields-methods
#'
#' @examples
#' fields <- getFields("gene")
#' head(fields)
setGeneric("getFields", signature = c("biothings"),
           function(client, ..., biothings) {
   standardGeneric("getFields")
})

#' @rdname getFields-methods
#' @aliases getFields,BioThings,BioThings-method
setMethod("getFields", c(biothings = "BioThings"),
          function(client, ..., biothings) {
  client_config <- biothings@clients[[client]]
  params <- list(...)
  res <- .request.get(biothings, client,
                      client_config$endpoints$metadata_fields$path, params)
  .return.as(res, "records")
})

#' @rdname getFields-methods
#' @aliases getFields,BioThings,BioThings-method
setMethod("getFields", c(biothings = "missing"),
          function(client, ..., biothings) {
  biothings <- BioThings()
  getFields(client, ..., biothings = biothings)
})
