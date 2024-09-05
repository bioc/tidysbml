# Create a dataframe with attributes content
#
# It returns a dataframe containing information about attributes for each entity reported in the 'listOf' class selected. It has at least one column. Otherwise, the format of the input list is not appropriate for this type of extraction.
#
# The input list may be obtained using sbml_as_list() function, by selecting as second argument the type of entity to be selected (i.e. either 'species', 'reactions' or 'compartments').
#
#
# @param sbml_list_listOf sbml converted into a list restricted to a 'listOf' level (e.g. listOfSpecies), a list
#
# @return a data frame with one row for each entity inside the 'listOf' chosen and one column for each attribute reported for them
#
# @examples
# sbml_list <- sbml_as_list(system.file("extdata", "R-HSA-8937144.sbml", package = "tidysbml"), "species")
# df <- as_df.attributes(sbml_list)   ## returns one dataframe with one row for each species and one column for each attribute
as_df.attributes <- function(sbml_list_listOf) {
  vec_names_attr <- c()

  for (i in seq_along(sbml_list_listOf)) {
    for (j in seq_along(names(attributes(sbml_list_listOf[[i]])))) {
      name_attr <- names(attributes(sbml_list_listOf[[i]]))[[j]]
      name_var_attr <- paste("attr_", name_attr, sep = "")

      if (!name_attr %in% vec_names_attr) {
        assign(name_var_attr, rep(NA, length(sbml_list_listOf)))
        vec_names_attr <- c(vec_names_attr, name_attr)
      }

      vec_temp <- get(name_var_attr)
      vec_temp[i] <- paste(attributes(sbml_list_listOf[[i]])[[j]], collapse = ", ")
      assign(name_var_attr, vec_temp)
    }
  }

  if (length(vec_names_attr)) {
    df <- data.frame(
      lapply(
        seq_along(vec_names_attr),
        function(n_col) get(paste("attr_", vec_names_attr[n_col], sep = ""))
      )
    )
    colnames(df) <- vec_names_attr
    df
  } else {
    stop("Extraction of 'attributes' as dataframe not available : argument with invalid list format")
  }
}
