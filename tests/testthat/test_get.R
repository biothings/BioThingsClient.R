context("Test getThing and getThings")

biothings <- BioThings()

test_that("Check that getThing returns appropriate type.", {
  gene_1017_df <-
    getThing("1017", "gene",
             fields = c("symbol", "name", "taxid", "entrezgene"),
             return.as = "data.frame")
  gene_1017_list <-
    getThing("1017", "gene",
             fields = c("symbol", "name", "taxid", "entrezgene"),
             return.as = "records")
  gene_1017_char <-
    getThing("1017", "gene",
             fields = c("symbol", "name", "taxid", "entrezgene"),
             return.as = "text")

  expect_is(gene_1017_df, "data.frame")
  expect_is(gene_1017_list, "list")
  expect_is(gene_1017_char, "character")

  expect_true(all(c("symbol", "name", "taxid", "entrezgene") %in%
                    names(gene_1017_df)))
  expect_true(all(c("symbol", "name", "taxid", "entrezgene") %in%
                    names(gene_1017_list[[1]])))


})

test_that("Check that getThings returns appropriate type.", {
  genes_df <-
    getThings(c("1017","1018","ENSG00000148795"), "gene",
              fields = c("symbol", "name", "taxid", "entrezgene"),
              return.as = "data.frame")
  genes_list <-
    getThings(c("1017","1018","ENSG00000148795"), "gene",
              fields = c("symbol", "name", "taxid", "entrezgene"),
              return.as = "records")
  genes_char <-
    getThings(c("1017","1018","ENSG00000148795"), "gene",
              fields = c("symbol", "name", "taxid", "entrezgene"),
              return.as = "text")

  expect_is(genes_df, "data.frame")
  expect_is(genes_list, "list")
  expect_is(genes_char, "character")

  expect_true(all(c("symbol", "name", "taxid", "entrezgene") %in%
                    names(genes_df)))
  expect_true(all(c("symbol", "name", "taxid", "entrezgene") %in%
                    names(genes_list[[1]])))
})
