context("Test metadata methods")

test_that("Verify getMetadata returns appropriate result", {
  metadata <- btMetadata("gene")
  expect_is(metadata, "list")
  expect_true("stats" %in% names(metadata[[1]]))
  # expect_true("total_genes" %in% names(metadata[[1]][["stats"]]))
})

test_that("Verify getFields returns appropriate result", {
  fields <- btFields("gene")
  expect_is(fields, "list")
})
