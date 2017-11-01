context("Test btQuery")

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

test_that("Check query response content", {
  size_query <- btQuery(biothings, "sp2", size = 5)
  expect_true("hits" %in% names(size_query[[1]]))
  expect_equal(length(size_query[[1]]$hits), 5)

  reporter_query <- btQuery(biothings, "reporter:1000_at")
  expect_true("hits" %in% names(reporter_query[[1]]))
  expect_equal(length(reporter_query[[1]]$hits), 1)
  expect_equal(reporter_query[[1]]$hits[[1]][["_id"]], "5595")

  symbol_query <- btQuery(biothings, "symbol:cdk2", species = "mouse")
  expect_true("hits" %in% names(symbol_query[[1]]))
  expect_equal(length(symbol_query[[1]]$hits), 1)
  expect_equal(symbol_query[[1]]$hits[[1]][["_id"]], "12566")
})

test_that("Check functionality of fetch_all", {
  qres <- btQuery(biothings, "_exists_:pdb")[[1]]
  total <- qres$total

  faqres <- btQuery(biothings, "_exists_:pdb", fetch_all = TRUE, fields = 'pdb')
  expect_equal(total, length(faqres))
})
