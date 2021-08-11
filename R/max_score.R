#' assigns segments based on max factor score
#' @param myfacscores factor scores
#' 
max_score <- function(myfacscores){
  set.seed(123456)
  assigned_segment <- max.col(myfacscores)
  return(assigned_segment)
}

