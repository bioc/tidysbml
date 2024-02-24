# Character value inside notes
#
# Helper-function for as_value.notes(). It returns the value contained inside the 'p', 'html' or 'body' tag for each entity.
#
# @param vec_notes_i vector's component where the notes values for one entity are saved
# @param element tag's name to be exported inside the sbml file (e.g. 'p')
# @param list_value list containing the value to be extracted
# @param sep delimiter to be used to separate multiple values associated to only one entity, a character string
#
# @return value extracted from the 'notes' subelement, a character string
as_value.notes.value <- function(vec_notes_i, element, list_value=NULL, sep="|"){
  if(!element %in% c("p","html","body")) warning("invalid argument for 'element' contained in the input list : it must be either 'p', 'html' or 'body'. Inserted 'ERROR' value in the data frame")

  if(element=="p"){
    if(!is.null(list_value)){
      value <- list_value[[1]]
    }else{
      warning("required list in 'list_value' argument. Inserted 'NA' in the data frame")
      value <- "NA"
    }
  }else if(element=="html"){
    value <- "html document"
  }else if(element=="body"){
    value <- "body element"
  }else{
    value <- "ERROR"
  }

  if(is.na(vec_notes_i)||!length(vec_notes_i)){
    value
  }else if(is.character(vec_notes_i) && length(vec_notes_i)==1){
    paste(vec_notes_i, value, sep = sep)
  }else{
    warning("invalid argument for 'vec_notes_i' : it must be a vector's component with either NA or a character value. It will be assumed as an empty component and only one value will be inserted")
    value
  }
}


# Character value inside notes
#
# Helper-function for as_df.notes() and as_subdf(). It returns the notes value to be inserted in the dataframe's column called 'notes'.
#
# @param list_notes list object where the notes' values have to be extracted from, a list
#
# @return notes' value to be inserted in one dataframe's cell, a character string
as_value.notes <- function(list_notes){    # , vec_notes_i){    # I think you cannot have multiple notes elements within on entity

  #check input conditions:
  if(missing(list_notes)) stop("no argument inserted. You must provide one list in input.")
  if(!typeof(list_notes)=="list") stop("argument is not a list")

  if(length(list_notes)==0){
    return("Empty")
  }else{
    vec_notes_i <- NA
    for(l in seq_along(names(list_notes))){
      list_temp <- list_notes
      if(names(list_temp)[l]=="p"){
        list_temp <- list_temp[[l]]
        while(!typeof(list_temp[[1]])=="character") list_temp <- list_temp[[1]]
        vec_notes_i <- as_value.notes.value(vec_notes_i, "p", list_temp)
      }else if(names(list_temp)[l]=="html"){
        vec_notes_i <- as_value.notes.value(vec_notes_i, "html")
      }else if(names(list_temp)[l]=="body"){
        vec_notes_i <- as_value.notes.value(vec_notes_i, "body")
      }else{
        vec_notes_i <- as_value.notes.value(vec_notes_i, "error")
      }
    }
    return(vec_notes_i)
  }
}


#' Create a dataframe with notes content
#'
#' It returns a dataframe with either zero or one column. It is zero if the notes content is empty for all the entities inside the 'listOf' class selected.
#'
#' @param sbml_list_listOf sbml-converted list restricted to a 'listOf' level (e.g. listOfSpecies), a list
#'
#' @return a data frame with one row for each entity inside the 'listOf' chosen and one column for the 'notes' content. Empty vector if the 'notes' content is empty for each entity.
#' @export
#'
#' @examples
#' sbml_list <- sbml_as_list(system.file("extdata", "R-HSA-8937144.sbml", package = "tidysbml"), "species")
#' df <- as_df.notes(sbml_list)  ## returns the dataframe with only one column containing the notes' info
as_df.notes <- function(sbml_list_listOf){

  # Check input conditions:
  if(missing(sbml_list_listOf)) stop("no argument inserted. You must provide one list in input.")
  if(!typeof(sbml_list_listOf)=="list") stop("argument is not a list")
  len=length(sbml_list_listOf)
  if(!len>0) stop("empty output. List in input must have length greater than 0.")   # messo len<=0 perche non so se puo esserci il caso len<0, in caso darebbe errore in vec_notes=rep()
  if(length(unique(names(sbml_list_listOf)))!=1 && !unique(names(sbml_list_listOf)) %in% c("species", "reaction", "compartment")) stop("invalid format for list in input : it must contain only one type of element, that is either 'species','reaction' or 'compartment' type")

  vec_notes=rep(NA,len)

  for (i in seq_len(len)) {
    for(j in seq_along(names(sbml_list_listOf[[i]]))){
      if(names(sbml_list_listOf[[i]])[j]=="notes"){
        vec_notes[i] <- as_value.notes(sbml_list_listOf[[i]][[j]])
      }
    }
  }

  if(sum(is.na(vec_notes))<len){
    df <- data.frame(vec_notes)
    colnames(df) <- c("notes")
    df
  }else{
    message("Empty notes' column for '", unique(names(sbml_list_listOf)) ,"' elements")
    c()
  }
}
