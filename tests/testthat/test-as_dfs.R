filepath <- system.file("extdata", "R-HSA-8937144.sbml", package = "tidysbml")
list_dfs <- tidysbml::as_dfs(filepath, type = "file")

test_that("returns correct type of output", {
  expect_type(list_dfs, "list")
  expect_s3_class(list_dfs[[1]], class = "data.frame")
  expect_s3_class(list_dfs[[2]], class = "data.frame")
  expect_type(list_dfs[[3]], "list")
  expect_s3_class(list_dfs[[3]][[1]], class = "data.frame")
  expect_s3_class(list_dfs[[3]][[2]], class = "data.frame")
})

test_that("returns output with correct lengths", {
  expect_equal(length(list_dfs), 3)
  expect_equal(length(list_dfs[[3]]), 2)
  expect_equal(ncol(list_dfs[[1]]), 7)
  expect_equal(nrow(list_dfs[[1]]), 2)
  expect_equal(ncol(list_dfs[[2]]), 13)
  expect_equal(nrow(list_dfs[[2]]), 10)
  expect_equal(ncol(list_dfs[[3]][[1]]), 15)
  expect_equal(nrow(list_dfs[[3]][[1]]), 5)
  expect_equal(ncol(list_dfs[[3]][[2]]), 7)
  expect_equal(nrow(list_dfs[[3]][[2]]), 14)
})

test_that("invalid arguments", {
  expect_error(tidysbml::as_dfs()) # empty list : inserted list where the first component is not "sbml"
  expect_error(tidysbml::as_dfs(list("sbml" = list()), type = "list"), "invalid list format")
  expect_error(tidysbml::as_dfs(list("sbml" = list("model" = list())), type = "list"), "invalid list format")
  expect_error(tidysbml::as_dfs(xml2::read_xml(filepath), type = "file")) # invalid file format: insert the path to the xml
  expect_error(tidysbml::as_dfs(filepath, type = "something"), "invalid argument")
})
