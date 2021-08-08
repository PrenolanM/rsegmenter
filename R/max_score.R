#' assigns segments based on max factor score
#' @param df data.frame that was input to factor analysis
#' @param myfacscores factor scores
#' 
max_score <- function(df,myfacscores){
  
  assigned_segment <- max.col(myfacscores)  
}

