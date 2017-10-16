context("Test query and queryMany methods for each client type")

test_that("Check that query returns the appropriate types", {
  gene_query <- query("CDK2", "gene")
  expect_is(gene_query, "list")
})

test_that("Check that queryMany returns the appropriate types", {
  gene_querymany <- queryMany(c('1053_at', '117_at', '121_at', '1255_g_at',
                                '1294_at'), "gene", scopes = "reporter",
                              species = "human")
  expect_is(gene_querymany, "list")
})
