context("Test getVariant and getVariants")

biothings <- BioThings()

test_that("Check that getVariant returns appropriate type.", {
  # variant_df <-
  #   getVariant("rs58991260")
  variant_list <-
    getVariant("rs58991260", return.as = "records")
  variant_char <-
    getVariant("rs58991260", return.as = "text")

  # expect_is(variant_df, "data.frame")
  expect_is(variant_list, "list")
  expect_is(variant_char, "character")
})

test_that("Check that getVariants returns appropriate type.", {
  # variants_df <-
  #   getVariants(c("rs58991260","rs2500"))
  variants_list <-
    getVariants(c("rs58991260","rs2500"), return.as = "records")
  variants_char <-
    getVariants(c("rs58991260","rs2500"), return.as = "text")

  # expect_is(variants_df, "data.frame")
  expect_is(variants_list, "list")
  expect_is(variants_char, "character")
})
