
# check that we don't have variables with all NA's
check_all_na <- function(df){
  return(
    any(apply(df,2,
              function(x){
                all(is.na(x))
              }
    )
    )
  )
}

# check that we don't have variables that are not numeric
check_all_numeric <- function(df){
  return(!all(apply(df,2,is.numeric)))
}

# basic imputation using one of the min, max or mean
impute_values <- function(df,impute_type){
  apply(df,2,
        function(x){
          if (impute_type=="mode"){
            mode_fn(x)
          } else if (impute_type=="mean"){
            mean(x,na.rm = TRUE)
          } else if (impute_type=="max"){
            max(x,na.rm = TRUE)
          } else if (impute_type=="min"){
            min(x,na.rm = TRUE)
          }
        })  
}
  
mode_fn <- function(x) {
  
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
  
}
