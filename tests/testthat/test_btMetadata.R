context("Test metadata methods")

test_that("Verify getMetadata returns appropriate result", {
  metadata <- btMetadata("gene")
  expect_is(metadata, "list")
})

test_that("Verify getFields returns appropriate result", {
  fields <- btFields("gene")
  expect_is(fields, "list")
})
