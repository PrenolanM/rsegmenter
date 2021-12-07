#' Add variable labels.
#' @param var variable to add variable label to
#' @param df data.frame containing the variable
#' @param stored_attribs list object that is the result of calling 
#' rsegmenter::store_spss_attr
#' @export

add_spss_labels <- function(var, df, stored_attribs){
  
  attr(df[[var]],"label") <<- stored_attribs[["label"]][[var]]
  
}

#' Add variable format.
#' @param var variable to add format to
#' @param df data.frame containing the variable
#' @param stored_attribs list object that is the result of calling 
#' rsegmenter::store_spss_attr
#' @export
#' 
add_spss_format <- function(var, df, stored_attribs){
  
  attr(df[[var]],"format.spss") <<- stored_attribs[["format"]][[var]]
  
}

#' Add display width.
#' @param var variable to add display width to
#' @param df data.frame containing the variable
#' @param stored_attribs list object that is the result of calling 
#' rsegmenter::store_spss_attr
#' @export
#' 
add_spss_display <- function(var, df, stored_attribs){
  
  attr(df[[var]],"display_width") <<- stored_attribs[["display"]][[var]]
  
}

#' Add value codes.
#' @param var variable to add value codes to
#' @param df data.frame containing the variable
#' @param data_map data map that is the result of calling 
#' rsegmenter::extract_datamap
#' @export
#' 
add_spss_value_codes <- function(var, df, data_map){
  
  attr(df[[var]], "labels") <<- data_map %>%
    filter(Variable_Name == var) %>%
    pull(Value_Code)
  
}

#' Add value labels. This function should always be called after calling 
#' rsegmenter::add_spss_value_codes
#' @param var variable to add value labels to
#' @param df data.frame containing the variable
#' @param data_map data map that is the result of calling 
#' rsegmenter::extract_datamap
#' @export
#' 
add_spss_value_labels <- function(var, df, data_map){
  
  attr(attr(df[[var]],"labels"),"names") <<- data_map %>%
    filter(Variable_Name == var) %>%
    pull(Value_Label)
  
}