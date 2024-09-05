#' Create a list of dataframes (full extraction)
#'
#' It takes in input a sbml file (or sbml list) and returns a list of maximum 3 components, depending on which entities are actually reported in the sbml document.    The first component contains the dataframe for listOfCompartments data, the second one for listOfSpecies data while the third component contains maximum two dataframes about reactions info (one with reactions data and the other one with data about the species involved in each reaction).
#'
#' This is the main function of the package. It comprehends all the other functions.
#' Since it incorporates also the sbml_as_list() function it is possible to insert as input either the sbml path or the sbml already converted into a list. This is set up using the argument 'type' which has to be 'file' or 'list', respectively.
#'
#' @param sbml either the path of the sbml file or the sbml already converted into a list, that is either a character string or a list respectively
#' @param type description about the type of the first argument (i.e. 'file' or 'list'), a character string
#'
#' @return a list of dataframes
#' @export
#'
#' @examples
#' filepath <- system.file("extdata", "R-HSA-8937144.sbml", package = "tidysbml")
#'
#' sbml_list <- sbml_as_list(filepath)
#' as_dfs(sbml_list, type = "list") ## returns a list of dataframes, giving in input the sbml already converted into a list
#'
#' as_dfs(filepath, type = "file") ## returns a list of dataframes, giving in input the sbml path
as_dfs <- function(sbml, type = "file") {
  if (type == "file") {
    sbml <- sbml_as_list(sbml)
  } else if (type != "list") {
    stop("invalid argument 'type': it must be either 'file' or 'list'")
  }

  if (is.null(names(sbml[["sbml"]][["model"]])[1])) stop("invalid list format: extraction requires 'sbml' and 'model' elements within the input list")

  list_dfs <- lapply(c("listOfCompartments", "listOfSpecies", "listOfReactions"), FUN = function(component_name) {
    list_name_model <- sbml[["sbml"]][["model"]][[component_name]]
    if (length(list_name_model)) {
      as_df(list_name_model)
    } else {
      NA
    }
  })

  if (all(is.na(list_dfs))) {
    message("empty list of dataframes in output: neither 'listOfCompartments','listOfSpecies' nor 'listOfReactions' element found")
    c()
  } else {
    names(list_dfs) <- c("df_compartments", "df_species", "about_reactions")
    names(list_dfs[[3]]) <- c("df_reactions", "df_species_in_reactions")
    list_dfs
  }
}
