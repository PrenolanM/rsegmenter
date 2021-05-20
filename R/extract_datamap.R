#' extract_datamap returns a dataframe containing a mapping between variables, value codes and value labels.
#' 
#' @param df must be a .sav file stored as data.frame. 
#' Importing the .sav file using haven::read_sav or 
#' rsegmenter::read_data is the preferred methods 
#' @export

extract_datamap <- function(df){
  
  # ensuring df is provided
  if (missing(df)){
    stop("df is compulsory")
  }
  
  # df must be a data.frame or tibble
  if (!(c("data.frame") %in% class(df))){
    stop("df must be a data.frame or tibble")
  }
  
  mynames <- colnames(df)
  
  data_map_list <- lapply(mynames,
                          function(x){
                            t(rbind(x,
                                    attr(df[[x]],"label"),
                                    attr(df[[x]],"labels"),
                                    names(attr(df[[x]],"labels"))))
                          }
  )
  
  vars_to_keep <- unlist(lapply(data_map_list,function(x)dim(x)[2]))
  
  data_map_list <- data_map_list[vars_to_keep==4]
  
  data_map_df <- do.call("rbind.data.frame",data_map_list)
  
  rownames(data_map_df) <- NULL
  colnames(data_map_df) <- c("Variable_Name","Variable_Label","Value_Code","Value_Label")
  
  data_map_df[["Value_Code"]] <- as.numeric(as.character(data_map_df[["Value_Code"]]))
  
  data_map_df[["Variable_Order"]] <- seq_along(data_map_df[["Variable_Name"]])
  
  return(data_map_df[c("Variable_Order","Variable_Name","Variable_Label","Value_Code","Value_Label")])
}