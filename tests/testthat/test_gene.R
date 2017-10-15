context("Test getGene and getGenes")

biothings <- BioThings()

test_that("Check that getGene returns appropriate type.", {
  gene_1017_df <-
    getGene("1017")
  gene_1017_list <-
    getGene("1017", return.as = "records")
  gene_1017_char <-
    getGene("1017", return.as = "text")

  expect_is(gene_1017_df, "data.frame")
  expect_is(gene_1017_list, "list")
  expect_is(gene_1017_char, "character")

  expect_true(all(c("symbol", "name", "taxid", "entrezgene") %in%
                    names(gene_1017_df)))
  expect_true(all(c("symbol", "name", "taxid", "entrezgene") %in%
                    names(gene_1017_list[[1]])))
})

test_that("Check that getGenes returns appropriate type.", {
  genes_df <-
    getGenes(c("1017","1018","ENSG00000148795"))
  genes_list <-
    getGenes(c("1017","1018","ENSG00000148795"), return.as = "records")
  genes_char <-
    getGenes(c("1017","1018","ENSG00000148795"), return.as = "text")

  expect_is(genes_df, "data.frame")
  expect_is(genes_list, "list")
  expect_is(genes_char, "character")

  expect_true(all(c("symbol", "name", "taxid", "entrezgene") %in%
                    names(genes_df)))
  expect_true(all(c("symbol", "name", "taxid", "entrezgene") %in%
                    names(genes_list[[1]])))
})
