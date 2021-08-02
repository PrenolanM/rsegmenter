#' create a segment variable by assigning a unique code to every combination of the input variables. input variables are sorting in
#' descending order, from the left most column to the right most column
#' @param df data.frame of numeric variables
#' @param vars variables to be crosstabbed
#' @export
#' 
crosstab_segmentation <- function(df,vars){
  
  df <- df %>% 
    dplyr::select(dplyr::all_of(vars))
  
  # check that all variables are numeric
  if (check_all_numeric(df)){
    stop("at least one of the input variables is not numeric")
  }
  
  # check that there are no missing values
  if (sum(is.na(df))>0){
    stop("data has NA's. please address these before segmenting")
  }
  
  segment_map <- df %>% 
    dplyr::distinct(dplyr::all_of(vars)) %>% 
    dplyr::arrange(dplyr::desc(.,FALSE)) %>% 
    dplyr::mutate(segment = dplyr::row_number())
  
  return(df %>% 
           dplyr::left_join(segment_map) %>% 
           dplyr::select(.data[["segment"]])
    )
  
}