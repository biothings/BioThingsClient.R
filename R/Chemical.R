# getChemical -------------------------------------------------------------

setGeneric("getChemical", signature = c("biothings"),
           function(chemid, fields = NULL, ...,
                    return.as = c("records", "text"), biothings) {
  standardGeneric("getGene")
})


setMethod("getChemical", c(biothings = "BioThings"),
          function(chemid, fields = NULL, ...,
                   return.as = c("records", "text"), biothings) {
  return.as <- match.arg(return.as)
  getThing(chemid, "chem", "chem", fields, ...,
           return.as = return.as, biothings = biothings)
})

setMethod("getChemical", c(biothings = "missing"),
          function(chemid, fields = NULL, ...,
                   return.as = c("records", "text"), biothings) {
  biothings <- BioThings()
  getChem(chemid, fields, ..., return.as = return.as,
          biothings = biothings)
})

# getChemicals ------------------------------------------------------------

setGeneric("getChemicals", signature = c("biothings"),
           function(chemids, fields = NULL, ...,
                    return.as = c("data.frame", "records", "text"), biothings) {
  standardGeneric("getGenes")
})

setMethod("getChemicals", c(biothings = "BioThings"),
          function(chemids, fields = NULL, ...,
                   return.as = c("data.frame", "records", "text"), biothings) {
  return.as <- match.arg(return.as)
  getThings(chemids, "chem", "chem", fields, ...,
            return.as = return.as, biothings = biothings)
})

setMethod("getChemicals", c(biothings = "missing"),
          function(chemids, fields = NULL, ...,
                   return.as = c("data.frame", "records", "text"), biothings) {
  biothings <- BioThings()
  getChems(geneids, fields, ..., return.as = return.as,
           biothings = biothings)
})
