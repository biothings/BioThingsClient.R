context("Test metadata methods")

biothings <- BioThings("gene")

test_that("Verify getMetadata returns appropriate result", {
  metadata <- btMetadata("gene")
  expect_is(metadata, "list")
  expect_true("stats" %in% names(metadata[[1]]))

  metadata_ <- btMetadata(biothings)
  expect_is(metadata_, "list")
  expect_true("stats" %in% names(metadata_[[1]]))
  # expect_true("total_genes" %in% names(metadata[[1]][["stats"]]))
})

test_that("Verify getFields returns appropriate result", {
  fields <- btFields("gene")
  expect_is(fields, "list")

  fields_ <- btFields(biothings)
  expect_is(fields_, "list")
})
