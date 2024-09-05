#' Validate SBML schema
#'
#' This function returns result of schema validation for the SBML document inserted.
#'
#' Validation is executed for SBML documents up to Level 2 Version 5, by using XML schemas provided in http://sbml.org/documents/specifications/. Otherwise returns error.
#'
#' @param sbml_file xml object, e.g. output of 'xml2::read_xml()'
#'
#' @importFrom xml2 xml_validate read_xml xml_attrs
#'
#' @return a boolean value with error details as attribute
#' @export
#'
#' @examples
#' filepath <- system.file("extdata", "MODEL1012080000.xml", package = "tidysbml")
#'
#' xml_file <- xml2::read_xml(filepath)
#'
#' sbml_validate(xml_file) ## returns a boolean value as result of validation in addition to attribute containing errors details
sbml_validate <- function(sbml_file) {
  if (xml_attrs(sbml_file)[["level"]] == 2) { # Level 2
    if (xml_attrs(sbml_file)[["version"]] == 5) { # Version 5
      path_schemal2v5 <- system.file("extdata", "sbml.level-2.version-5.release-1.xsd.xml", package = "tidysbml")
      schemal2v5 <- read_xml(path_schemal2v5)
      out <- xml2::xml_validate(sbml_file, schemal2v5)
    } else if (xml_attrs(sbml_file)[["version"]] == 4) { # Version 4
      path_schemal2v4 <- system.file("extdata", "sbml.level-2.version-4.release-1.xsd.xml", package = "tidysbml")
      schemal2v4 <- xml2::read_xml(path_schemal2v4)
      out <- xml2::xml_validate(sbml_file, schemal2v4)
    } else if (xml_attrs(sbml_file)[["version"]] == 3) { # Version 3
      path_schemal2v3 <- system.file("extdata", "sbml.level-2.version-3.release-2.xsd.xml", package = "tidysbml")
      schemal2v3 <- xml2::read_xml(path_schemal2v3)
      out <- xml2::xml_validate(sbml_file, schemal2v3)
    } else if (xml_attrs(sbml_file)[["version"]] == 2) { # Version 2
      path_schemal2v2 <- system.file("extdata", "sbml.level-2.version-2.revision-1.xsd.xml", package = "tidysbml")
      schemal2v2 <- xml2::read_xml(path_schemal2v2)
      out <- xml2::xml_validate(sbml_file, schemal2v2)
    } else if (xml_attrs(sbml_file)[["version"]] == 1) { # Version 1
      path_schemal2v1 <- system.file("extdata", "sbml.level-2.version-1.xsd.xml", package = "tidysbml")
      schemal2v1 <- xml2::read_xml(path_schemal2v1)
      out <- xml2::xml_validate(sbml_file, schemal2v1)
    }
  } else if (xml_attrs(sbml_file)[["level"]] == 1) { # Level 1
    if (xml_attrs(sbml_file)[["version"]] == 2) { # Version 2
      path_schemal1v2 <- system.file("extdata", "sbml.level-1.version-2.xsd.xml", package = "tidysbml")
      schemal1v2 <- xml2::read_xml(path_schemal1v2)
      out <- xml2::xml_validate(sbml_file, schemal1v2)
    } else if (xml_attrs(sbml_file)[["version"]] == 1) { # Version 1
      path_schemal1v1 <- system.file("extdata", "sbml.level-1.version-1.xsd.xml", package = "tidysbml")
      schemal1v1 <- xml2::read_xml(path_schemal1v1)
      out <- xml2::xml_validate(sbml_file, schemal1v1)
    }
  } else {
    stop("invalid sbml level and/or version: validation not available for the current document")
  }

  if (!out[[1]]) message("invalid sbml schema")
  out
}


#' Create R list from sbml file
#'
#' This function converts a sbml file into a R list.
#'
#' By selecting the argument 'component' as 'all' it returns the sbml-converted list looking at the highest level (i.e. 'sbml' tag), whereas if it equals either 'species', 'reactions' or 'compartments', the list can be restricted to the species/reactions/compartments level.
#' The first option permits to use the resulting list as input for as_dfs() function, while the other options provide the input for as_df() function.
#'
#' @param file path for the sbml file to be loaded
#' @param component description of which entities (i.e. sbml level) have to be extracted into a list, a character string
#'
#' @importFrom xml2 read_xml as_list
#'
#' @return sbml converted into a R list
#' @export
#'
#' @examples
#' filepath <- system.file("extdata", "R-HSA-8937144.sbml", package = "tidysbml")
#'
#' sbml_list <- sbml_as_list(filepath, "all") ## returns the R list with the entire sbml model
#'
#' sbml_as_list(filepath, "species") ## returns the R list restricted to the list of species
#'
#' sbml_as_list(filepath, "reactions") ## returns the R list restricted to the list of reactions
#'
#' sbml_as_list(filepath, "compartments") ## returns the R list restricted to the list of components
sbml_as_list <- function(file, component = "all") {
  sbml_file <- xml2::read_xml(file)

  sbml_list <- xml2::as_list(sbml_file)

  if (component == "all") {
    if (is.null(names(sbml_list[["sbml"]][["model"]])[1])) stop("Invalid file format: cannot find 'sbml' or 'model' elements in list object")
    sbml_list
  } else if (component == "species") {
    if (is.null(names(sbml_list[["sbml"]][["model"]][["listOfSpecies"]])[1])) stop("Invalid file format: cannot find 'listOfSpecies' in sbml's model")
    sbml_list[["sbml"]][["model"]][["listOfSpecies"]]
  } else if (component == "reactions") {
    if (is.null(names(sbml_list[["sbml"]][["model"]][["listOfReactions"]])[1])) stop("Invalid file format: cannot find 'listOfReactions' in sbml's model")
    sbml_list[["sbml"]][["model"]][["listOfReactions"]]
  } else if (component == "compartments") {
    if (is.null(names(sbml_list[["sbml"]][["model"]][["listOfCompartments"]])[1])) stop("Invalid file format: cannot find 'listOfCompartments' in sbml's model")
    sbml_list[["sbml"]][["model"]][["listOfCompartments"]]
  } else {
    stop("Invalid 'component' argument: it must be either 'all', 'species', 'compartments' or 'reactions'")
  }
}
