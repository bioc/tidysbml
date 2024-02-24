# Character value inside annotation creator
#
# Helper function for as_value.annotation(). It returns the content for descriptors with nested content (i.e. creator).
#
# @param list_creator sbml-converted list looking at 'creator' tag level, a list
# @param vec_value_i value to be inserted in the annotation_creator column for the i-th entity, a character string
# @param descriptor_content_idx index for creator's content, an integer
#
# @return value for creator's content, a character string
as_value.annotation.creator <- function(list_creator=list_app, vec_value_i=vec_tmp_i, descriptor_content_idx=m){
  if(descriptor_content_idx!=1) vec_value_i <- paste(vec_value_i, "||", sep = "")

  for(p in seq_along(list_creator[[descriptor_content_idx]])){
    str_name <- attributes(list_creator[[descriptor_content_idx]])[["names"]][[p]]   # N,ORG,EMAIL
    sub_names <- names(list_creator[[descriptor_content_idx]][[p]])

    if(descriptor_content_idx!=1||p!=1){
      if(p!=1) vec_value_i=paste(vec_value_i, "; ", sep = "")
      vec_value_i <- paste(vec_value_i,str_name,"(",paste(sub_names, collapse=" "),"):", sep = "")
    }else{
      vec_value_i <- paste(str_name,"(",paste(sub_names, collapse=" "),"):", sep = "")
    }

    for(q in seq_along(list_creator[[descriptor_content_idx]][[p]])){
      if(length(list_creator[[descriptor_content_idx]][[p]][[q]])==0) {
        vec_value_i <- paste(vec_value_i, " , ", sep = " ")
      }else{
        vec_value_i <- paste(vec_value_i, list_creator[[descriptor_content_idx]][[p]][[q]][[1]][[1]], sep = " ")
      }
    }
  }
  vec_value_i
}


# Character value inside one annotation descriptor
#
# Helper function for as_df.annotation(). It returns the value containing one annotation info to be inserted in one cell within the data frame.
#
# @param list_annotation_descriptor sbml-converted list looking at one annotation's descriptor level, a list
# @param vec_tmp_i value to be inserted in the annotation_descriptorname column for the i-th entity, a character string
# @param sep delimiter to be used to separate multiple values associated to only one entity, a character string
#
# @return value contained inside one annotation descriptor tag, a character string
as_value.annotation <- function(list_annotation_descriptor, vec_tmp_i, sep = "||"){

  if(length(list_annotation_descriptor)==1 && names(list_annotation_descriptor)=="W3CDTF"){
    if(length(list_annotation_descriptor[[1]])==1){
      vec_tmp_i <- list_annotation_descriptor[[1]][[1]]
    }else{
      vec_tmp_i <- paste(list_annotation_descriptor[[1]][[1]], " and other elements", sep="")
    }
  }else if(length(list_annotation_descriptor)==1 && names(list_annotation_descriptor) %in% c("Bag","Seq","Alt")){
    if(length(list_annotation_descriptor[[1]])==0){ # case with empty Bag/Seq/Alt
      vec_tmp_i <- paste("Empty",names(list_annotation_descriptor), seq=" ")
    }else{
      list_app <- list_annotation_descriptor[[1]]

      for(m in seq_along(list_app)){

        if(length(list_app[[m]])==0){
          if(m==1){
            vec_tmp_i <- attributes(list_app[[m]])[[1]]
          }else{
            vec_tmp_i <- paste(vec_tmp_i,attributes(list_app[[m]])[[1]], sep = sep)
          }
        }else{
          vec_tmp_i <- as_value.annotation.creator(list_app, vec_tmp_i, m)
        }
      }
    }
  }else{
    vec_tmp_i <- "ERROR"
    warning("Extraction of 'annotation' not available : different case for descriptor's extraction (neither 'W3CDTF' nor 'Bag'/'Seq'/'Alt' element)")
  }
  vec_tmp_i
}


#' Create a dataframe with annotation content
#'
#' It returns a dataframe with one row for each entity and 0 or more columns. If zero, it means the annotation content for each entity inside the 'listOf' selected is empty.
#'
#' Each column is named as 'annotation_' concatenated with the 'descriptor name' (e.g. is, hasPart, isVersionOf). If the name is repeated (i.e. there are multiple equal descriptors' name for the same entity), it concatenates also a number to the name, starting from '_1' and leaving the first name without any number.
#'
#' @param sbml_list_listOf sbml-converted list restricted to a 'listOf' level (e.g. listOfSpecies), a list
#'
#' @return one data frame with one row for each entity inside the 'listOf' selected and one column for each annotation's descriptor reported for them.  Empty vector if the 'annotation' content is empty for each entity.
#' @export
#'
#' @examples
#' sbml_list <- sbml_as_list(system.file("extdata", "R-HSA-8937144.sbml", package = "tidysbml"), "species")
#' df <- as_df.annotation(sbml_list)  ## returns one dataframe with one row for each species and one column for each descriptor in annotation
as_df.annotation <- function(sbml_list_listOf){

  # Check input conditions:
  if(missing(sbml_list_listOf)) stop("no argument inserted. You must provide one list in input.")
  if(!typeof(sbml_list_listOf)=="list") stop("argument is not a list")
  len=length(sbml_list_listOf)
  if(!len) stop("Empty output : list of length 0 inserted")
  if(length(unique(names(sbml_list_listOf)))!=1 && !unique(names(sbml_list_listOf)) %in% c("species", "reaction", "compartment")) stop("invalid format for list in input : it must contain only one type of element, that is either 'species','reaction' or 'compartment' type")

  vec_names_attr <- c()
  vec_annotation <- c()
  vec_colname_annotation <- c()
  vec_colname_ann_final <- c()

  for (i in seq_len(len)) {
    for(j in seq_along(names(sbml_list_listOf[[i]]))){

      if(names(sbml_list_listOf[[i]])[j]=="annotation"){
        if(length(sbml_list_listOf[[i]][[j]])>0){

          vec_names_descriptor <- c()

          for(k in seq_along(sbml_list_listOf[[i]][[j]])){

            if(length(sbml_list_listOf[[i]][[j]][[k]])>0){

              if(names(sbml_list_listOf[[i]][[j]])[k]=="RDF"){   # case with 'RDF'

                for(n_descr in seq_along(sbml_list_listOf[[i]][[j]][[k]])){

                  if(names(sbml_list_listOf[[i]][[j]][[k]])[n_descr]=="Description"){
                    list_temp <- sbml_list_listOf[[i]][[j]][[k]][[n_descr]]

                    for(l in seq_along(list_temp)){
                      # create one column for each name in Description
                      name_descriptor <- names(list_temp)[l]
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
                      }else{  # case with name repeated
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

                      list_temp1 <- list_temp[[l]]

                      # fill with value
                      vec_app <- get(name_col_descriptor)
                      if(!is.null(as_value.annotation(list_temp1, vec_app[i]))){ # returns NA otherwise
                        vec_app[i] <- as_value.annotation(list_temp1, vec_app[i])
                      }

                      assign(name_col_descriptor,vec_app)

                    }
                  }
                }

              }else{ #case different from 'RDF' within annotation
                warning("Extraction of 'annotation' is not totally available for '",names(sbml_list_listOf)[i]," ",i,"': found element '",names(sbml_list_listOf[[i]][[j]])[k],"' different from 'RDF' within the input list")
              }
            }
          }
        }
      }

    }
  }

  if(length(vec_colname_ann_final)==length(vec_colname_annotation) && length(vec_colname_annotation)){
    vec_colname_ann_final <- sort(vec_colname_ann_final)
    vec_colname_annotation <- sort(vec_colname_annotation)
    df=data.frame(
      lapply(
        seq_along(vec_colname_annotation),
        function(n_col)  get(vec_colname_annotation[n_col])
      )
    )
    colnames(df) <- vec_colname_ann_final
    df
  }else{
    message("Empty annotation's columns for '", unique(names(sbml_list_listOf)) ,"' elements")
    c()
  }
}
