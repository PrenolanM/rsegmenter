#' check_flatine returns the max percentage of responses that are the same per row.
#' 
#' The closer this return value is to 1, the more serious the flatline problem is for that row.
#' 
#' per variable, missing values can be replaced with one of the following:
#' 1. mean
#' 2. min
#' 3. max

#' @export
#' @param df must be a data.frame
#' 
#' @param vars must be a string of variable names to operate on.
#' These variables must be numeric
#' 
#' @param impute_type must be a string of one of "mean","min","max"
#' 
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' check_flatline(df = mydf, vars = c("col1","col2","col3"), impute_type = "mean")

check_flatline <- function(df,vars,impute_type="mean"){
  
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
  
  df <- df[vars]
  
  # check that we don't have variables with all NA's
  if (check_all_na(df)){
    stop("at least one of the input variables contain all NA's")
  }
  
  # check that all variables are numeric
  if (check_all_numeric(df)){
    stop("at least one of the input variables is not numeric")
  }
  
  for (i in seq_along(vars)){
    df[is.na(df[,i]),i] <- impute_values(df)[i]
  }
  
  if (sum(is.na(df))){
    
    warning("there are still missing values in your data")
    
  }
  
  return_df <- as.data.frame(apply(df,1,
                                   function(x){
                                     max(table(x))/length(vars)
                                     }
                                   )
                             )
  
  colnames(return_df) <- "Prop_Flatline"
    
  return(return_df)
  
}
