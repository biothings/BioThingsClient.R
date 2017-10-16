#' @include get.R

# getChemical -------------------------------------------------------------

#' @rdname getThing-methods
#' @exportMethod getChemical
setGeneric("getChemical", signature = c("biothings"),
           function(chemid, fields = NULL, ...,
                    return.as = c("records", "text"), biothings) {
  standardGeneric("getChemical")
})

setMethod("getChemical", c(biothings = "BioThings"),
          function(chemid, fields = NULL, ...,
                   return.as = c("records", "text"), biothings) {
  return.as <- match.arg(return.as)
  getThing(chemid, "chem", "drug", fields, ...,
           return.as = return.as, biothings = biothings)
})

setMethod("getChemical", c(biothings = "missing"),
          function(chemid, fields = NULL, ...,
                   return.as = c("records", "text"), biothings) {
  biothings <- BioThings()
  getChemical(chemid, fields, ..., return.as = return.as,
              biothings = biothings)
})

#' @rdname getThing-methods
#' @export
getChem <- getChemical

# getChemicals ------------------------------------------------------------

#' @rdname getThing-methods
#' @exportMethod getChemicals
setGeneric("getChemicals", signature = c("biothings"),
           function(chemids, fields = NULL, ...,
                    return.as = c("records", "text", "data.frame"), biothings) {
  standardGeneric("getChemicals")
})

setMethod("getChemicals", c(biothings = "BioThings"),
          function(chemids, fields = NULL, ...,
                   return.as = c("records", "text", "data.frame"), biothings) {
  return.as <- match.arg(return.as)
  getThings(chemids, "chem", "drug", fields, ...,
            return.as = return.as, biothings = biothings)
})

setMethod("getChemicals", c(biothings = "missing"),
          function(chemids, fields = NULL, ...,
                   return.as = c("records", "text", "data.frame"), biothings) {
  biothings <- BioThings()
  getChemicals(chemids, fields, ..., return.as = return.as,
               biothings = biothings)
})

#' @rdname getThing-methods
#' @export
getChems <- getChemicals
