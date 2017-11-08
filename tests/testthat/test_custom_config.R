context("Test usage of custom biothings client configuration")

# Example custom config using the dev.mygene.info endpoint
gene_dev_config = list(
  entity = "dev_gene",
  base_url = "http://dev.mygene.info/v3",
  user_agent = "dev.MyGene.R",
  endpoints = list("query" = list(path = "query"),
                   "annotation" = list(path = "gene"),
                   "metadata" = list(path = "metadata"),
                   "metadata_fields" = list(path = "metadata/fields")),
  delay = 1,
  step = 1000,
  max_query = 1000
)


biothings <- BioThingsClient(gene_dev_config)

fields <- c("symbol", "name", "taxid", "entrezgene")
genes <- c("1017", "1018", "ENSG00000148795")
gene <- "1017"

test_that("Check that btGet returns appropriate type.", {
  gene_1017_df <- btGet(biothings, gene, fields = fields,
                        return.as = "data.frame")
  gene_1017_list <- btGet(biothings, gene, fields = fields,
                          return.as = "records")
  gene_1017_char <- btGet(biothings, gene, fields = fields, return.as = "text")

  expect_is(gene_1017_df, "data.frame")
  expect_is(gene_1017_list, "list")
  expect_is(gene_1017_char, "character")

  expect_true(all(fields %in% names(gene_1017_df)))
  expect_true(all(fields %in% names(gene_1017_list[[1]])))

  genes_df <- btGet(biothings, genes, fields = fields, return.as = "data.frame")
  genes_list <- btGet(biothings, genes, fields = fields, return.as = "records")
  genes_char <- btGet(biothings, genes, fields = fields, return.as = "text")

  expect_is(genes_df, "data.frame")
  expect_is(genes_list, "list")
  expect_is(genes_char, "character")

  expect_true(all(fields %in% names(genes_df)))
  expect_true(all(fields %in% names(genes_list[[1]])))
})
