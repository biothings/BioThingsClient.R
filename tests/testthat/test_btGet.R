context("Test btGet")

biothings <- BioThingsClient("gene")

fields <- c("symbol", "name", "taxid", "entrezgene")
genes <- c("1017","1018","ENSG00000148795")
gene <- "1017"

gene_df <- btGet(biothings, gene, fields = fields, return.as = "data.frame")
gene_list <- btGet(biothings, gene, fields = fields, return.as = "records")
gene_char <- btGet(biothings, gene, fields = fields, return.as = "text")

genes_df <- btGet(biothings, genes, fields = fields, return.as = "data.frame")
genes_list <- btGet(biothings, genes, fields = fields, return.as = "records")
genes_char <- btGet(biothings, genes, fields = fields, return.as = "text")

test_that("Check that btGet returns appropriate type.", {
  expect_is(gene_df, "data.frame")
  expect_is(gene_list, "list")
  expect_is(gene_char, "character")

  expect_true(all(fields %in% names(gene_df)))
  expect_true(all(fields %in% names(gene_list[[1]])))

  # For multiple ids.
  expect_is(genes_df, "data.frame")
  expect_is(genes_list, "list")
  expect_is(genes_char, "character")

  expect_true(all(fields %in% names(genes_df)))
  expect_true(all(fields %in% names(genes_list[[1]])))
})

test_that("Check that return values are correct for btGet", {
  expect_identical(gene_list[[1]][["_id"]], gene)
  gene_list_ids <- sapply(genes_list, `[[`, "query")
  expect_equal(gene_list_ids, genes)
})
