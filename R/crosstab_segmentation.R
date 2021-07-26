#' create a segment variable by assigning a unique code to every combination of the input variables. input variables are sorting in
#' descending order, from the left most column to the right most column
#' @param df data.frame of numeric variables
#' @param ... variables to be crosstabbed
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1),myweight=c(1,2,1))
#' crosstab_segmentation(df = mydf,col1,col2,col3)
#' @export
#' 
crosstab_segmentation <- function(df,...){
  
  df <- df %>% 
    select(...)
  
  # check that all variables are numeric
  if (check_all_numeric(df)){
    stop("at least one of the input variables is not numeric")
  }
  
  # check that there are no missing values
  if (sum(is.na(df))>0){
    stop("data has NA's. please address these before segmenting")
  }
  
  segment_map <- df %>% 
    distinct(...) %>% 
    arrange(desc(.,FALSE)) %>% 
    mutate(segment = row_number())
  
  return(df %>% 
    left_join(segment_map)
    )
  
}