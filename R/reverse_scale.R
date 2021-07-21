#' Reverses the coding of each input variable.
#' @param myvar must be a numeric vector to operate on.
#' 
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' reverse_scale(mydf$col1)
#' @export

reverse_scale <- function(myvar){
  
  # ensuring var is provided
  if (missing(myvar)){
    
    stop("myvar is compulsory")
    
  }
  
  # check that all variables are numeric
  if (!is.numeric(myvar)){
    
    stop("non numeric variable supplied")
    
  }
  
  if (sum(is.na(myvar))){
    
    stop("there are missing values in your data")
    
  }

  if (min(myvar)<0){
    
    stop("there are negative values in your data")
    
  }
  
  if (min(unique(myvar))==0)
  {
    return(abs(myvar - max(myvar)))
  } else if (min(unique(myvar))>=1)
  {
    return(abs(myvar - max(myvar)) + min(myvar))
  }
}




