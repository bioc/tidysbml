filepath <- system.file("extdata", "R-HSA-8937144.sbml", package = "tidysbml")

test_that("returns correct type of output", {
  expect_type(tidysbml::sbml_as_list(filepath), "list")
})

test_that("xml component name is correct", {
  expect_equal(names(tidysbml::sbml_as_list(filepath, "all")), "sbml")
  expect_equal(unique(names(tidysbml::sbml_as_list(filepath, "compartments"))), "compartment")
  expect_equal(unique(names(tidysbml::sbml_as_list(filepath, "species"))), "species")
  expect_equal(unique(names(tidysbml::sbml_as_list(filepath, "reactions"))), "reaction")
})

test_that("invalid arguments", {
  expect_error(tidysbml::sbml_as_list(filepath, component = "rreaction"), "Invalid 'component' argument")
  expect_error(tidysbml::sbml_as_list()) # empty input
  expect_error(tidysbml::sbml_as_list(list())) # invalid input type
  expect_error(tidysbml::sbml_as_list("invalid/path"))
})
