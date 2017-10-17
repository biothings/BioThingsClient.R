#' @include get.R

# getTaxon ----------------------------------------------------------------

#' @rdname getThing-methods
#' @exportMethod getTaxon
setGeneric("getTaxon", signature = c("biothings"),
           function(taxonid, fields = NULL, ...,
                    return.as = c("records", "text", "data.frame"), biothings) {
  standardGeneric("getTaxon")
})

setMethod("getTaxon", c(biothings = "BioThings"),
          function(taxonid, fields = NULL, ...,
                   return.as = c("records", "text", "data.frame"), biothings) {
  return.as <- match.arg(return.as)
  getThing(taxonid,"taxon", fields, ...,
           return.as = return.as, biothings = biothings)
})

setMethod("getTaxon", c(biothings = "missing"),
          function(taxonid, fields = NULL, ...,
                   return.as = c("records", "text", "data.frame"), biothings) {
  biothings <- BioThings()
  getTaxon(taxonid, fields, ..., return.as = return.as,
           biothings = biothings)
})

# getTaxons ---------------------------------------------------------------

#' @rdname getThing-methods
#' @exportMethod getTaxons
setGeneric("getTaxons", signature = c("biothings"),
           function(taxonids, fields = NULL, ...,
                    return.as = c("records", "text", "data.frame"), biothings) {
  standardGeneric("getTaxons")
})

setMethod("getTaxons", c(biothings = "BioThings"),
          function(taxonids, fields = NULL, ...,
                   return.as = c("records", "text", "data.frame"), biothings) {
  return.as <- match.arg(return.as)
  getThings(taxonids,"taxon", fields, ...,
            return.as = return.as, biothings = biothings)
})

setMethod("getTaxons", c(biothings = "missing"),
          function(taxonids, fields = NULL, ...,
                   return.as = c("records", "text", "data.frame"), biothings) {
  biothings <- BioThings()
  getTaxons(taxonids, fields, ..., return.as = return.as,
            biothings = biothings)
})
