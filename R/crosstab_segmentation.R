#' Create a segment variable by assigning a unique code to every combination of the input variables.
#' Input variables are sorted in descending order, from the left most column to the right most column.
#' @param df data.frame of numeric variables
#' @param vars variables to be used in the crosstab segmentation.
#' 
#' @examples
#' df <- rsegmenter::test_seg_unlabelled
#' crosstab_segmentation(df = df, vars = c("seg1","seg2","seg3"))
#' 
#' @export
#' 
crosstab_segmentation <- function(df,vars){
  
  df <- df[,vars,drop=FALSE]
  
  # check that all variables are numeric
  if (check_all_numeric(df)){
    stop("at least one of the input variables is not numeric")
  }
  
  # check that there are no missing values
  if (sum(is.na(df))>0){
    stop("data has NA's. please address these before segmenting")
  }
  
  segment_map <- df %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(dplyr::desc(.)) %>% 
    dplyr::mutate(Crosstab_Segment = dplyr::row_number())
  
  return(df %>% 
           dplyr::left_join(segment_map) %>% 
           dplyr::select(.data[["Crosstab_Segment"]])
    )
  
}