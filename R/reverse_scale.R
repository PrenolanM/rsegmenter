#' Reverses the coding of each input variable.
#' @param df must be a data.frame
#' 
#' @param vars must be a string of variable names to operate on.
#' Variables must be numeric
#' 
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' reverse_scale(df = mydf, vars = c("col1","col2","col3"))
#' @export

reverse_scale <- function(df,vars){
  
  # ensuring df is provided
  if (missing(df)){
    stop("df is compulsory")
  }
  
  # ensuring vars is provided
  if (missing(vars)){
    stop("vars is compulsory")
  }
  
  # df must be a data.frame or tibble
  if (!(c("data.frame") %in% class(df))){
    stop("df must be a data.frame or tibble")
  }
  
  # df must have at least 1 row
  # this implies df will have at least 1 col as well
  if (is.null(nrow(df))){
    stop("df must be a data.frame of at least 1 row")
  }
  
  df <- df[,vars,drop=FALSE]
  
  # check that we don't have variables with all NA's
  if (check_all_na(df)){
    stop("at least one of the input variables contain all NA's")
  }
  
  # check that all variables are numeric
  if (check_all_numeric(df)){
    stop("at least one of the input variables is not numeric")
  }
  
  if (sum(is.na(df))){
    
    warning("there are still missing values in your data")
    
  }

  return_df <- as.data.frame(lapply(df,
                                    function(x)
                                      {
                                      if (min(unique(x))==0){
                                        return(abs(x - max(x)))
                                        } else if (min(unique(x))>=1){
                                          return(abs(x - max(x)) + min(x))
                                        }
                                      }
                                    )
                             )

  colnames(return_df) <- paste0(colnames(df),"_reverse_scale")

  return(return_df)

}




