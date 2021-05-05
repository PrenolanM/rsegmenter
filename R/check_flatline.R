#' check_flatine returns the max percentage of responses that are the same per row.
#' the closer this value is to 1, the more serious the flatline problem is for that row.
#' missing values can be replaced with one of the mean,mode,min,max per variable.

#' @export
#' @param df must be a data.frame of numeric variables
#' @param vars must be a string of variable names to operate on
#' @param impute_type must be a string of one of "mean","min","max"

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
  
  # basic imputation using one of the min, max or mean
  impute_values <- apply(df,2,
                         function(x){
                           if (impute_type=="min"){
                             min(x,na.rm = TRUE)  
                             } else if (impute_type=="max"){
                             max(x,na.rm = TRUE)
                             } else if (impute_type=="mean"){
                             mean(x,na.rm = TRUE)
                             }
                           }
                         )
  
  for (i in seq_along(vars)){
    df[is.na(df[,i]),i] <- impute_values[i]
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
