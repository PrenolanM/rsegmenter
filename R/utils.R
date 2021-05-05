
# check that we don't have variables with all NA's
check_all_na <- function(df){
  if (any(apply(df,2,function(x)all(is.na(x))))){
    return(TRUE)
  }  
}

# check that we don't have variables that are not numeric
check_all_numeric <- function(df){
  if (all(apply(df,2,is.numeric))){
    return(TRUE)
  }  
}