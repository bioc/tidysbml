#' Create a list of dataframes (full extraction)
#'
#' It takes in input a sbml file (or sbml list) and returns a list of maximum 3 components, depending on which entities are actually reported in the sbml document.    The first component contains the dataframe for listOfCompartments data, the second one for listOfSpecies data while the third component contains maximum two dataframes about reactions info (one with reactions data and the other one with data about the species involved in each reaction).
#'
#' This is the main function of the package. It comprehends all the other functions.
#' Since it incorporates also the sbml_as_list() function it is possible to insert as input either the sbml path or the sbml already converted into a list. This is set up using the argument 'type' which has to be 'file' or 'list' (default), respectively.
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
#' as_dfs(sbml_list)  ## returns a list of dataframes, giving in input the sbml already converted into a list
#'
#' as_dfs(filepath, type = "file")  ## returns a list of dataframes, giving in input the sbml path
as_dfs <-function(sbml, type="list"){

  if(type=="file"){
    sbml <- sbml_as_list(sbml)
  } else if(type != "list"){
    stop("invalid argument 'type': it must be either 'file' or 'list'")
  }

  if(is.null(names(sbml[["sbml"]][["model"]])[1])) stop("invalid list format: extraction requires 'sbml' and 'model' elements within the input list")

  out_list <- vector(mode = "list", length = 3)
  vec_names_dfs <- rep(NA,3)
  pos <- 0
  vec_names_model <- names(sbml[["sbml"]][["model"]])
  for (i in seq_along(vec_names_model)) {
    list_name_model <- sbml[["sbml"]][["model"]][[ vec_names_model[i] ]]
    if(vec_names_model[i] %in% c("listOfCompartments","listOfSpecies","listOfReactions")){
      pos <- pos+1

      name_df <- paste("df_", vec_names_model[i], sep = "" )
      output <- as_df(list_name_model)

      if(is.data.frame(output)){
        out_list[[pos]] <- output
        vec_names_dfs[pos] <- name_df
      }else if(length(output)==2){
        name_subdf <- "df_speciesWithinReactions"
        vec_names_dfs[pos] <- "aboutReactions"
        out_list[[pos]] <- output
        names(out_list[[pos]]) <- c(name_df,name_subdf)
      }
    }
  }

  if(!pos){
    warning("empty dataframe in output: neither 'listOfCompartments','listOfSpecies' nor 'listOfReactions' element found within the input list")
    c()
  }else{
    names(out_list) <- vec_names_dfs
    out_list
  }
}
