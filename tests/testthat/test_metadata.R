context("Test metadata methods")

test_that("Verify getMetadata returns appropriate result", {
  metadata <- getMetadata("gene")
  expect_is(metadata, "list")
})

test_that("Verify getFields returns appropriate result", {
  fields <- getFields("gene")
  expect_is(fields, "list")
})
