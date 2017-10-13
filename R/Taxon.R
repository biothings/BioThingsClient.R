# getTaxon ----------------------------------------------------------------

setGeneric("getTaxon", signature = c("biothings"),
           function(taxonid, fields = NULL, ...,
                    return.as = c("records", "text"), biothings) {
  standardGeneric("getGene")
})

setMethod("getTaxon", c(biothings = "BioThings"),
          function(taxonid, fields = NULL, ...,
                   return.as = c("records", "text"), biothings) {
  return.as <- match.arg(return.as)
  getThing(taxonid, "taxon", "taxon", fields, ...,
           return.as = return.as, biothings = biothings)
})

setMethod("getTaxon", c(biothings = "missing"),
          function(taxonid, fields = NULL, ...,
                   return.as = c("records", "text"), biothings) {
  biothings <- BioThings()
  getTaxon(taxonid, fields, ..., return.as = return.as,
           biothings = biothings)
})

# getTaxons ---------------------------------------------------------------

setGeneric("getTaxons", signature = c("biothings"),
           function(taxonids, fields = NULL, ...,
                    return.as = c("data.frame", "records", "text"), biothings) {
  standardGeneric("getGenes")
})

setMethod("getTaxons", c(biothings = "BioThings"),
          function(taxonids, fields = NULL, ...,
                   return.as = c("data.frame", "records", "text"), biothings) {
  return.as <- match.arg(return.as)
  getThings(taxonids, "taxon", "taxon", fields, ...,
            return.as = return.as, biothings = biothings)
})

setMethod("getTaxons", c(biothings = "missing"),
          function(taxonids, fields = NULL, ...,
                   return.as = c("data.frame", "records", "text"), biothings) {
  biothings <- BioThings()
  getTaxons(geneids, fields, ..., return.as = return.as,
            biothings = biothings)
})
