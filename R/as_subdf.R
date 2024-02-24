#' Create a dataframe for speciesReference entities
#'
#' It returns a dataframe containing data (i.e. attributes, notes and annotation) about the species involved in each reaction described in listOfReactions.
#'
#' It will create the data frame if and only if listOfReactions is present within the sbml in argument and is not empty (i.e. it contains 'speciesReference' entities).
#' The output dataframe has at least two columns, one with the id for the reaction taken into account, one with the type of container the species belongs to (i.e. listOfReactants, listOfProduct, listOfModifiers) and others for the mandatory attributes.
#' Otherwise, the format of the input list is not appropriate for this type of extraction.
#'
#' @param sbml_list_listOf sbml-converted list restricted to the 'listOfReactions' level, a list
#'
#' @return a data frame with one row for each speciesReference entity and one column for each attribute, notes, annotation reported for them
#' @export
#'
#' @examples
#' sbml_list <- sbml_as_list(system.file("extdata", "R-HSA-8937144.sbml", package = "tidysbml"), "reactions")
#' df <- as_subdf(sbml_list)   ## returns one dataframe with one row for each species involved in each reaction
as_subdf <- function(sbml_list_listOf){

  # Check input conditions:
  if(missing(sbml_list_listOf)) stop("no argument inserted. You must provide one list in input.")
  if(!typeof(sbml_list_listOf)=="list") stop("argument is not a list")
  len=length(sbml_list_listOf)
  if(!len) stop("Empty output : list of length 0 inserted")
  if(length(unique(names(sbml_list_listOf)))!=1) stop("invalid format for list in input : it must contain only one type of element, that is either 'species','reaction' or 'compartment' type")
  if(!unique(names(sbml_list_listOf))=="reaction") return(c())

  vec_names_subattr <- c()
  col_reactionId <- c()
  col_type_listOf <- c()
  vec_notes <- c()

  vec_names_attr <- c()
  vec_annotation <- c()
  vec_colname_annotation <- c()
  vec_colname_ann_final <- c()

  r <- 0  # row index for subdf

  for (i in seq_len(len)) {
    for(j in seq_along(names(sbml_list_listOf[[i]]))){

      if(grepl("listOf",names(sbml_list_listOf[[i]])[j])){

        reactId <- attributes(sbml_list_listOf[[i]])[["id"]]
        type_listOf <- names(sbml_list_listOf[[i]])[j]

        for(k in seq_along(sbml_list_listOf[[i]][[j]])){
          r=r+1

          # extraction of attributes
          list_speciesref <- sbml_list_listOf[[i]][[j]][[k]]
          for(l in seq_along(names(attributes(list_speciesref)))){
            name_subattr <- names(attributes(list_speciesref))[[l]]
            name_var_subattr <- paste("subattr_", name_subattr ,sep="")
            if(!name_subattr %in% vec_names_subattr){
              assign(name_var_subattr, c())
              vec_names_subattr <- c(vec_names_subattr, name_subattr)
            }
            vec_tmp <- get(name_var_subattr)
            vec_tmp[r] <- paste(attributes(list_speciesref)[[l]], collapse=", ")
            assign(name_var_subattr,vec_tmp)
          }

          # extraction of notes&annotation
          if(length(list_speciesref)){
            for (l in seq_along(list_speciesref)) {
              # extraction of notes
              if(names(list_speciesref)[l]=="notes"){
                vec_notes[r] <- as_value.notes(list_speciesref[[l]])
              }
              # extraction of annotation
              if(names(list_speciesref)[l]=="annotation"){

                if(length(list_speciesref[[l]])>0){

                  vec_names_descriptor <- c()

                  for(m in seq_along(list_speciesref[[l]])){

                    if(length(list_speciesref[[l]][[m]])>0){

                      if(names(list_speciesref[[l]])[m]=="RDF"){   # 'RDF' case

                        for(n_descr in seq_along(names(list_speciesref[[l]][[m]]))){

                          if(names(list_speciesref[[l]][[m]])[n_descr]=="Description"){
                            list_descr <- list_speciesref[[l]][[m]][[n_descr]]

                            for(n in seq_along(list_descr)){
                              name_descriptor <- names(list_descr)[n]
                              name_col_descriptor <- paste("annotation_", name_descriptor ,"__",sep="")

                              name_col_tosearch <- substr(name_col_descriptor, 1, nchar(name_col_descriptor)-1)
                              name_col_tosave <- substr(name_col_descriptor, 1, nchar(name_col_descriptor)-2)

                              if(!name_col_descriptor %in% vec_colname_annotation ){
                                assign(name_col_descriptor, rep(NA,length(sbml_list_listOf)) )
                                vec_colname_ann_final <- c(vec_colname_ann_final, name_col_tosave)
                                vec_colname_annotation <- c(vec_colname_annotation, name_col_descriptor)
                                vec_names_descriptor <- c(vec_names_descriptor, name_col_descriptor)
                              }else if(!name_col_descriptor %in% vec_names_descriptor){
                                vec_names_descriptor <- c(vec_names_descriptor, name_col_descriptor)
                              }else{ # case with column's name repetead
                                string_tosearch <- paste("^",name_col_tosearch,".+", sep = "")
                                number <- sum(grepl(string_tosearch, vec_names_descriptor))
                                name_new_col <- paste(name_col_tosearch, number, sep = "")
                                if(name_new_col %in% vec_colname_annotation){
                                  vec_names_descriptor <- c(vec_names_descriptor, name_new_col)
                                }else{
                                  assign(name_new_col , rep(NA,length(sbml_list_listOf)) )
                                  vec_colname_ann_final <- c(vec_colname_ann_final, name_new_col)
                                  vec_colname_annotation <- c(vec_colname_annotation, name_new_col)
                                  vec_names_descriptor <- c(vec_names_descriptor, name_new_col)
                                }
                                name_col_descriptor <- name_new_col
                              }

                              list_tmp <- list_descr[[n]]

                              vec_tmp <- get(name_col_descriptor)

                              vec_tmp[r] <- as_value.annotation(list_tmp, vec_tmp[r])

                              assign(name_col_descriptor,vec_tmp)

                            }
                          }
                        }
                      }else{ #case different from 'RDF' within annotation
                        warning("Extraction of 'annotation' for nested dataframe is not totally available for '",names(sbml_list_listOf)[i]," ",i,"': found element '",names(sbml_list_listOf[[i]][[j]])[k],"' different from 'RDF' within the input list")
                      }
                    }
                  }
                }
              }
            }
          }

          col_reactionId[r] <- reactId
          col_type_listOf[r] <- type_listOf

        }
      }
    }
  }

  if(length(vec_names_subattr)){
    real_len <- length(col_reactionId)

    subdf=data.frame(col_reactionId, col_type_listOf,
                     lapply(seq_along(vec_names_subattr), function(n_subatt) {
                       temp_len=length(get(paste("subattr_",vec_names_subattr[n_subatt],sep = "")))

                       if(!temp_len==real_len){      # check length and if needed fills with NA values
                         vec_na <- rep(NA,real_len-temp_len)
                         assign(paste("subattr_",vec_names_subattr[n_subatt],sep = ""), c(get(paste("subattr_",vec_names_subattr[n_subatt],sep = "")),vec_na))
                       }

                       get(paste("subattr_",vec_names_subattr[n_subatt],sep = ""))
                     }))

    vec_col_names <- c("reaction_id","container_list",vec_names_subattr)

    if(length(vec_notes)){
      if(length(vec_notes)<nrow(subdf)) vec_notes[nrow(subdf)]=NA
      subdf=data.frame(subdf, vec_notes)
      vec_col_names <- c(vec_col_names, "notes")
    }

    if(length(vec_colname_annotation)){
      vec_colname_ann_final <- sort(vec_colname_ann_final)
      vec_colname_annotation <- sort(vec_colname_annotation)
      subdf=data.frame(subdf,
                       lapply(seq_along(vec_colname_annotation), function(n_colannotation){
                         if(length(get(vec_colname_annotation[n_colannotation]))!=nrow(subdf)){
                           vec_temp <- get(vec_colname_annotation[n_colannotation])
                           vec_temp[nrow(subdf)] <- NA
                           assign(vec_colname_annotation[n_colannotation], vec_temp)
                         }
                         get(vec_colname_annotation[n_colannotation])
                       }))
      vec_col_names <- c(vec_col_names, vec_colname_ann_final)
    }

    colnames(subdf) <- vec_col_names
    subdf
  }else{
    message("Empty dataframe in output : input list is not valid for creation of a nested data frame which contains information about the species within each reaction")
    c()
  }
}
