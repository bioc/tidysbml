filepath <- system.file("extdata", "R-HSA-8937144.sbml", package = "tidysbml")
xml_file <- xml2::read_xml(filepath)
sbml_list <- xml2::as_list(xml_file)
species_list <- sbml_list[["sbml"]][["model"]][["listOfSpecies"]]
reactions_list <- sbml_list[["sbml"]][["model"]][["listOfReactions"]]

test_that("returns correct type of output", {
  expect_s3_class(tidysbml::as_df(species_list), "data.frame")
  expect_type(tidysbml::as_df(reactions_list), "list")
})

test_that("invalid input", {
  expect_error(tidysbml::as_df(sbml_list)) # restrict the input list to a single SBML component
  expect_error(tidysbml::as_df(), "no argument inserted")
  expect_error(tidysbml::as_df(list()), "empty output")
  expect_error(tidysbml::as_df(c()), "argument is not a list")
  expect_error(tidysbml::as_df(list("species" = list())), "invalid list format") # missing mandatory attributes
})
