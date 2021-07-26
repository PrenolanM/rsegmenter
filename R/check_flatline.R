#' Returns the max percentage of responses that are the same per row.
#' 
#' Returns the max percentage of responses that are the same per row.
#' The closer this return value is to 1, the more serious the flatline problem 
#' is for that row.
#' @param myvar must be a numeric vector to operate on.
#' 
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' check_flatline(mydf$col1)
#' @export

check_flatline <- function(myvar){
  
  # ensuring myvar is provided
  if (missing(myvar)){
    
    stop("myvar is compulsory")
    
  }
  
  # check that variable is numeric
  if (!is.numeric(myvar)){
    
    stop("non numeric variable supplied")
    
  }
  
  if (sum(is.na(myvar))){
    
    stop("there are missing values in your data")
    
  }
  
  max(table(myvar))/length(myvar)
  
}
