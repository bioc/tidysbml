#' Create a dataframe for the type of entity selected
#'
#' It takes in input a sbml-converted list zoomed at one 'listOf' level and produces one dataframe containing one row for each entity (i.e. either species, reaction or compartment) and columns for the attributes, notes and annotation content.
#'
#' Input list may be obtained using sbml_as_list() with the argument 'component' equal to 'species', 'reactions' or 'compartments', which returns a list zoomed at 'listOfSpecies', 'listOfReactions' or 'listOfCompartments' level, respectively.
#' If the 'listOf' selected is 'listOfReactions' it returns one more dataframe: the first one with one row for each reaction, and the extra one with one row for each species involved in each reaction (i.e. entities called 'speciesReference' in the sbml).
#' Each dataframe contains at least one column, since some attributes are mandatory, and also notes and annotation columns, if their content is not empty. If the dataframe is missing it means the sbml does not contain such information (i.e. no data about those entities).
#'
#' @param sbml_list_listOf sbml converted into a list restricted to a 'listOf' level (e.g. listOfSpecies), a list
#'
#' @importFrom methods is
#'
#' @return one dataframe (or a list of two dataframes) containing information about the entities in the input list
#' @export
#'
#' @examples
#' filepath <- system.file("extdata", "R-HSA-8937144.sbml", package = "tidysbml")
#'
#' sbml_list <- sbml_as_list(filepath, "species")
#' df <- as_df(sbml_list) ## returns one dataframe with data about the species saved in the sbml document
#'
#' sbml_list <- sbml_as_list(filepath, "reactions")
#' df <- as_df(sbml_list) ## returns one list containing two dataframe with data about the reactions and the speciesReferences listed in the sbml document
as_df <- function(sbml_list_listOf) {
  # Check input conditions:
  if (missing(sbml_list_listOf)) stop("no argument inserted. You must provide one list in input.")
  if (!typeof(sbml_list_listOf) == "list") stop("argument is not a list")
  if (!length(sbml_list_listOf)) stop("empty output. List of length 0 inserted.")
  if (length(unique(names(sbml_list_listOf))) != 1 || !all(unique(names(sbml_list_listOf)) %in% c("species", "reaction", "compartment"))) stop("invalid format for list in input : it must contain only one type of element, that is either 'species','reaction' or 'compartment' type")

  df_attributes <- as_df.attributes(sbml_list_listOf)
  df_notes <- as_df.notes(sbml_list_listOf)
  df_annotation <- as_df.annotation(sbml_list_listOf)
  subdf <- as_subdf(sbml_list_listOf)

  if (length(df_attributes)) {
    df <- data.frame(df_attributes)
    if (methods::is(df_notes, "data.frame")) df <- data.frame(df, df_notes)
    if (methods::is(df_annotation, "data.frame")) df <- data.frame(df, df_annotation)
  }

  if (methods::is(subdf, "data.frame")) {
    list(df, subdf)
  } else {
    df
  }
}
