#' @include get.R

# getVariant --------------------------------------------------------------

#' @rdname getThing-methods
#' @exportMethod getVariant
setGeneric("getVariant", signature = c("biothings"),
           function(variantid, fields = NULL, ...,
                    return.as = c("records", "text", "data.frame"), biothings) {
  standardGeneric("getVariant")
})

setMethod("getVariant", c(biothings = "BioThings"),
          function(variantid, fields = NULL, ...,
                   return.as = c("records", "text", "data.frame"), biothings) {
  return.as <- match.arg(return.as)
  getThing(variantid, "variant", fields, ..., return.as = return.as,
           biothings = biothings)
})

setMethod("getVariant", c(biothings = "missing"),
          function(variantid, fields = NULL, ...,
                   return.as = c("records", "text", "data.frame"), biothings) {
  biothings <- BioThings()
  getVariant(variantid, fields, ..., return.as = return.as,
             biothings = biothings)
})

# getVariants -------------------------------------------------------------

#' @rdname getThing-methods
#' @exportMethod getVariants
setGeneric("getVariants", signature = c("biothings"),
           function(variantids, fields = NULL, ...,
                    return.as = c("records", "text", "data.frame"), biothings) {
  standardGeneric("getVariants")
})

setMethod("getVariants", c(biothings = "BioThings"),
          function(variantids, fields = NULL, ...,
                   return.as = c("records", "text", "data.frame"), biothings) {
  return.as <- match.arg(return.as)
  getThings(variantids, "variant", fields, ...,
            return.as = return.as, biothings = biothings)
})

setMethod("getVariants", c(biothings = "missing"),
          function(variantids, fields = NULL, ...,
                   return.as = c("records", "text", "data.frame"), biothings) {
  biothings <- BioThings()
  getVariants(variantids, fields, ..., return.as = return.as,
              biothings = biothings)
})
