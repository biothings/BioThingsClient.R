# getGene -----------------------------------------------------------------

setGeneric("getGene", signature = c("biothings"),
           function(geneid, fields = c("symbol", "name", "taxid", "entrezgene"),
                    ..., return.as = c("records", "text"), biothings) {
  standardGeneric("getGene")
})


setMethod("getGene", c(biothings = "BioThings"),
          function(geneid, fields = c("symbol", "name", "taxid", "entrezgene"),
                   ..., return.as = c("records", "text"), biothings) {
  return.as <- match.arg(return.as)
  getThing(geneid, "gene", "gene", fields, ..., return.as = return.as,
           biothings = biothings)
})

setMethod("getGene", c(biothings = "missing"),
          function(geneid, fields = c("symbol", "name" ,"taxid", "entrezgene"),
                   ..., return.as = c("records", "text"), biothings) {
  biothings <- BioThings()
  getGene(geneid, fields, ..., return.as = return.as,
          biothings = biothings)
})

# getGenes ----------------------------------------------------------------

setGeneric("getGenes", signature = c("biothings"),
           function(geneids, fields = c("symbol", "name", "taxid",
                                        "entrezgene"),
                    ..., return.as = c("data.frame", "records", "text"),
                    biothings) {
  standardGeneric("getGenes")
})

setMethod("getGenes", c(biothings = "BioThings"),
          function(geneids, fields = c("symbol", "name", "taxid", "entrezgene"),
                   ..., return.as = c("data.frame", "records", "text"),
                   biothings) {
  return.as <- match.arg(return.as)
  getThings(geneids, "gene", "gene", fields, ..., return.as = return.as,
            biothings = biothings)
})

setMethod("getGenes", c(biothings = "missing"),
          function(geneids, fields = c("symbol", "name", "taxid", "entrezgene"),
                   ..., return.as = c("data.frame", "records", "text"),
                   biothings) {
  biothings <- BioThings()
  getGenes(geneids, fields, ..., return.as = return.as, biothings = biothings)
})
