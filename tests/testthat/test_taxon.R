context("Test getTaxon and getTaxons")

biothings <- BioThings()

test_that("Check that getTaxon returns appropriate type.", {
  # taxon_df <-
  #   getTaxon("9606")
  taxon_list <-
    getTaxon("9606", return.as = "records")
  taxon_char <-
    getTaxon("9606", return.as = "text")

  # expect_is(taxon_df, "data.frame")
  expect_is(taxon_list, "list")
  expect_is(taxon_char, "character")
})

test_that("Check that getThings returns appropriate type.", {
  taxons_df <-
    getTaxons(c("9606", "10030"))
  taxons_list <-
    getTaxons(c("9606", "10030"), return.as = "records")
  taxons_char <-
    getTaxons(c("9606", "10030"), return.as = "text")

  expect_is(taxons_df, "data.frame")
  expect_is(taxons_list, "list")
  expect_is(taxons_char, "character")
})
