#' gets the number of responses that are the same per person
#' @export
#' @param df should be a dataframe of numeric variables

check_flatline <- function(df){
  
  # ensuring df is provided
  if (missing(df)){
    stop("df is compulsory")
  }
  
  # df must be a data.frame or tibble
  if (!(c("data.frame") %in% class(df))){
    stop("df must be a data.frame or tibble")
  }
  
  # df must have at least 1 row
  if (is.null(nrow(df))){
    stop("df must be a data.frame of at least 1 row")
  }
  
  # df must have at least 1 col
  if (is.null(ncol(df))){
    stop("df must be a data.frame of at least 1 column")
  }
  
  return_df <- as.data.frame(apply(df,1,
                                   function(x){
                                     max(table(x,useNA = "always"))
                                     }
                                   )
                             )
  
  colnames(return_df) <- "Number_Flatline"
    
  return(return_df)
  
}
