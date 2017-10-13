# getVariant --------------------------------------------------------------

setGeneric("getVariant", signature = c("biothings"),
           function(variantid, fields = NULL, ...,
                    return.as = c("records", "text"), biothings) {
  standardGeneric("getGene")
})


setMethod("getVariant", c(biothings = "BioThings"),
          function(variantid, fields = NULL, ..., return.as = c("records", "text"),
                   biothings) {
  return.as <- match.arg(return.as)
  getThing(variantid, "variant", "variant", fields, ..., return.as = return.as,
           biothings = biothings)
})

setMethod("getVariant", c(biothings = "missing"),
          function(variantid, fields = NULL, ..., return.as = c("records", "text"),
                   biothings) {
  biothings <- BioThings()
  getVariant(variantid, fields, ..., return.as = return.as, biothings = biothings)
})

# getVariants -------------------------------------------------------------

setGeneric("getVariants", signature = c("biothings"),
           function(variantids, fields = NULL, ...,
                    return.as = c("data.frame", "records", "text"), biothings) {
  standardGeneric("getGenes")
})

setMethod("getVariants", c(biothings = "BioThings"),
          function(variantids, fields = NULL, ...,
                   return.as = c("data.frame", "records", "text"), biothings) {
  return.as <- match.arg(return.as)
  getThings(variantids, "variant", "variant", fields, ..., return.as = return.as,
            biothings = biothings)
})

setMethod("getVariants", c(biothings = "missing"),
          function(variantids, fields = NULL, ...,
                   return.as = c("data.frame", "records", "text"), biothings) {
  biothings <- BioThings()
  getVariants(geneids, fields, ..., return.as = return.as,
              biothings = biothings)
})
