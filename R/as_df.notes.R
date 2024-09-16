# Character value inside notes
#
# Helper-function for as_df.notes() and as_subdf(). It returns the notes value to be inserted in the dataframe's column called 'notes'.
#
# @param list_notes list object where the notes' values have to be extracted from, a list
#
# @return notes' value to be inserted in one dataframe's cell, a character string
as_value.notes <- function(list_notes, sep = "|") {
  
  if (length(list_notes) == 0) {
    return("Empty")
  } else {
    vec_notes_i <- vapply(names(list_notes), function(x) {
      if (x == "p") {
        list_temp <- list_notes[[x]]
        while (!typeof(list_temp[[1]]) == "character") list_temp <- list_temp[[1]]
        if (!is.null(list_temp)) {
          list_temp[[1]]
        } else { # empty paragraph
          ""
        }
      } else if (x == "html") {
        paste(xml2::as_xml_document(list_notes))
      } else if (x == "body") {
        paste(xml2::as_xml_document(list_notes))
      } else {
        warning("invalid argument for 'element' contained in the input list : it must be either 'p', 'html' or 'body'. Inserted 'extraction not available' value in the dataframe")
        "extraction not available"
      }
    }, character(1))
    
    return(paste(vec_notes_i, collapse = sep))
  }
}


# Create a dataframe with notes content
#
# It returns a dataframe with either zero or one column. It is zero if the notes content is empty for all the entities inside the 'listOf' class selected.
#
# @param sbml_list_listOf sbml-converted list restricted to a 'listOf' level (e.g. listOfSpecies), a list
#
# @return a data frame with one row for each entity inside the 'listOf' chosen and one column for the 'notes' content. Empty vector if the 'notes' content is empty for each entity.
#
# @examples
# sbml_list <- sbml_as_list(system.file("extdata", "R-HSA-8937144.sbml", package = "tidysbml"), "species")
# df <- as_df.notes(sbml_list)  ## returns the dataframe with only one column containing the notes' info
as_df.notes <- function(sbml_list_listOf) {
  vec_notes <- unlist(lapply(seq_along(sbml_list_listOf), function(i) {
    vec_which_notes <- which(names(sbml_list_listOf[[i]]) == "notes")
    if (length(which(names(sbml_list_listOf[[i]]) == "notes")) == 1) {
      as_value.notes(sbml_list_listOf[[i]][[which(names(sbml_list_listOf[[i]]) == "notes")]])
    } else { # if length 0
      NA
    }
  }))
  
  if (all(is.na(vec_notes))) {
    message("Empty notes' column for '", unique(names(sbml_list_listOf)), "' elements")
    c()
  } else {
    df <- data.frame(vec_notes)
    colnames(df) <- c("notes")
    df
  }
}
