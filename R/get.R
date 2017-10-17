#' @include BioThings.R

# getThing ----------------------------------------------------------------

#' @title Get items from BioThings APIs
#'
#' @param thing character.
#' @param things vector.
#' @param client character.
#' @param fields vector.
#' @param return.as character.
#' @param biothings BioThings.
#'
#' @name getThing
#' @rdname getThing-methods
#' @return The API response in the form of the provided argument return.as
#' @exportMethod getThing
#'
#' @examples
#' #Equivalent:
#' getThing("1017", "gene", fields = c("symbol","name","taxid","entrezgene"),
#'           return.as = "text")
#' getGene("1017", return.as = "text")
#'
#' # Also equivalent
#' getThings(c("1017","1018","ENSG00000148795"), "gene", "gene",
#'           fields = c("symbol","name","taxid","entrezgene"),
#'           return.as = "text")
#' getGenes(c("1017","1018","ENSG00000148795"), return.as = "text")
setGeneric("getThing", signature = c("biothings"),
           function(thing, client, fields, ..., return.as,
                    biothings) {
  standardGeneric("getThing")
})

setMethod("getThing", signature = c(biothings = "BioThings"),
          function(thing, client, fields, ..., return.as, biothings) {
  # return.as <- match.arg(return.as)
  params <- list(...)
  params$fields <- .collapse(fields)
  client_config <- biothings@clients[[client]]
  res <- .request.get(biothings, client,
                      paste(client_config$endpoints$annotation$path,
                            thing, sep = "/"), params)
  .return.as(res, return.as = return.as)
})

setMethod("getThing", c(biothings = "missing"),
          function(thing, client, fields, ..., return.as, biothings) {
  biothings <- BioThings()
  getThing(thing, client, fields, ...,
           return.as = return.as, biothings = biothings)
})


# getThings ---------------------------------------------------------------
#' @rdname getThing-methods
#' @exportMethod getThings
setGeneric("getThings", signature = c("biothings"),
           function(things, client, fields, ..., return.as,
                    biothings) {
  standardGeneric("getThings")
})

setMethod("getThings", signature = c(biothings = "BioThings"),
          function(things, client, fields, ..., return.as,
                   biothings) {
  client_config <- biothings@clients[[client]]
  params <- list()
  if (exists('fields')) {
    params <- list(...)
    params[['fields']] <- .collapse(fields)
    params <- lapply(params, .collapse)
  }

  vecparams <- list(ids = .uncollapse(things))

  res <- .repeated.query(biothings, client,
                         client_config$endpoints$annotation$path,
                         vecparams = vecparams, params = params)

  .return.as(res, return.as = return.as)
})

setMethod("getThings", c(biothings = "missing"),
          function(things, client, fields, ..., return.as,
                   biothings) {
  biothings <- BioThings()
  getThings(things, client, fields, ..., return.as = return.as,
            biothings = biothings)
})
