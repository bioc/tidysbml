#' Create R list from sbml file
#'
#' This function converts a sbml file into a R list.
#'
#' By selecting the argument 'component' as 'all' it returns the sbml-converted list looking at the highest level (i.e. 'sbml' tag), whereas if it equals either 'species', 'reactions' or 'compartments', the list can be zoomed to species/reactions/compartments level.
#' The first option permits to use the resulting list as input for as_dfs(), while the other options provide the input for all the other functions belonging to this package (functions as_df-like).
#'
#' @param file path for the sbml file to be loaded
#' @param component description of which entities (i.e. sbml level) have to be extracted into a list, a character string
#'
#' @return sbml converted into a R list
#' @export
#'
#' @examples
#' filepath = system.file("extdata", "R-HSA-8937144.sbml", package = "tidysbml")
#'
#' sbml_list = sbml_as_list(filepath, "all")   ## returns the R list with the entire sbml model
#'
#' sbml_as_list(filepath, "species")   ## returns the R list restricted to the list of species
#'
#' sbml_as_list(filepath, "reactions")   ## returns the R list restricted to the list of reactions
#'
#' sbml_as_list(filepath, "compartments")   ## returns the R list restricted to the list of components

sbml_as_list <- function(file, component="all"){
  sbml_file <- xml2::read_xml(file)
  sbml_list <- xml2::as_list(sbml_file)

  if(component=="all"){
    if(is.null(names(sbml_list[["sbml"]][["model"]])[1])) stop("Invalid file format: cannot find 'sbml' or 'model' elements in list object" )
    sbml_list
  }else if(component=="species"){
    if(is.null(names(sbml_list[["sbml"]][["model"]][["listOfSpecies"]])[1])) stop("Invalid file format: cannot find 'listOfSpecies' in sbml's model")
    sbml_list[["sbml"]][["model"]][["listOfSpecies"]]
  }else if(component=="reactions"){
    if(is.null(names(sbml_list[["sbml"]][["model"]][["listOfReactions"]])[1])) stop("Invalid file format: cannot find 'listOfReactions' in sbml's model")
    sbml_list[["sbml"]][["model"]][["listOfReactions"]]
  }else if(component=="compartments"){
    if(is.null(names(sbml_list[["sbml"]][["model"]][["listOfCompartments"]])[1])) stop("Invalid file format: cannot find 'listOfCompartments' in sbml's model")
    sbml_list[["sbml"]][["model"]][["listOfCompartments"]]
  }else{
    stop("Invalid 'component' argument: it must be either 'all', 'species', 'compartments' or 'reactions'")
  }
}
