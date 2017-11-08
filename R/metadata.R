#' @include BioThingsClient.R

# btMetadata -------------------------------------------------------------

#' @title btMetadata
#'
#' @description
#' Get metadata information from BioThings API metadata endpoints
#'
#' @param biothings A BioThings class object
#' @param ... Additional API parameters. See API documentation.
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
           function(biothings, ...) {
  standardGeneric("btMetadata")
})

#' @rdname btMetadata-methods
#' @aliases btMetadata,BioThings,BioThings-method
setMethod("btMetadata", c(biothings = "BioThingsClient"),
          function(biothings, ...) {
  client_config <- slot(biothings, "client")
  params <- list(...)
  res <- .request.get(biothings, client_config$endpoints$metadata$path, params)
  .return.as(res, "records")
})

#' @rdname btMetadata-methods
#' @aliases btMetadata,BioThings,BioThings-method
setMethod("btMetadata", c(biothings = "character"),
          function(biothings, ...) {
  biothings <- BioThingsClient(biothings)
  btMetadata(biothings = biothings, ...)
})

# btFields ---------------------------------------------------------------

#' @title btFields
#'
#' @description
#' Get field metadata information from BioThings API field metadata endpoints
#'
#' @param biothings A BioThings class object
#' @param ... Additional API parameters. See API documentation.
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
           function(biothings, ...) {
   standardGeneric("btFields")
})

#' @rdname btFields-methods
#' @aliases btFields,BioThings,BioThings-method
setMethod("btFields", c(biothings = "BioThingsClient"),
          function(biothings, ...) {
  client_config <- slot(biothings, "client")
  params <- list(...)
  res <- .request.get(biothings, client_config$endpoints$metadata_fields$path,
                      params)
  .return.as(res, "records")
})

#' @rdname btFields-methods
#' @aliases btFields,BioThings,BioThings-method
setMethod("btFields", c(biothings = "character"),
          function(biothings, ...) {
  biothings <- BioThingsClient(biothings)
  btFields(biothings = biothings, ...)
})
