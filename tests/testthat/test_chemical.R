context("Test getChemical and getChemicals")

biothings <- BioThings()

test_that("Check that getChemical returns appropriate type.", {
  # chem_df <-
  #   getChemical("CALDTVBHJMBRTM-UHFFFAOYSA-N")
  chem_list <-
    getChemical("CALDTVBHJMBRTM-UHFFFAOYSA-N", return.as = "records")
  chem_char <-
    getChemical("CALDTVBHJMBRTM-UHFFFAOYSA-N", return.as = "text")

  # expect_is(chem_df, "data.frame")
  expect_is(chem_list, "list")
  expect_is(chem_char, "character")
})

test_that("Check that getThings returns appropriate type.", {
  # chems_df <- getChemicals(c("CALDTVBHJMBRTM-UHFFFAOYSA-N",
  #                            "ZKLPARSLTMPFCP-UHFFFAOYSA-N"))
  chems_list <- getChemicals(c("CALDTVBHJMBRTM-UHFFFAOYSA-N",
                               "ZKLPARSLTMPFCP-UHFFFAOYSA-N"),
                             return.as = "records")
  chems_char <- getChemicals(c("CALDTVBHJMBRTM-UHFFFAOYSA-N",
                               "ZKLPARSLTMPFCP-UHFFFAOYSA-N"),
                             return.as = "text")

  # expect_is(chems_df, "data.frame")
  expect_is(chems_list, "list")
  expect_is(chems_char, "character")
})
