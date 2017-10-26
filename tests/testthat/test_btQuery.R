context("Test btQuery and btQueryMany methods for each client type")

biothings <- BioThings("gene")

test_that("Check that btQuery returns the appropriate types", {
  gene_btQuery <- btQuery(biothings, "CDK2")
  expect_is(gene_btQuery, "list")
})

test_that("Check that btQueryMany returns the appropriate types", {
  gene_btQuerymany <- btQuery(biothings, c('1053_at', '117_at', '121_at',
                                           '1255_g_at','1294_at'),
                              scopes = "reporter", species = "human")
  expect_is(gene_btQuerymany, "list")
})

test_that("Check functionality of fetch_all", {
  qres <- btQuery(biothings, "_exists_:pdb")[[1]]
  total <- qres$total

  faqres <- btQuery(biothings, "_exists_:pdb", fetch_all = TRUE, fields = 'pdb')
  expect_equal(total, length(faqres))
})
