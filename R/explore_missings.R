#' Explore missing values at variable and respondent levels
#' @param df data.frame of numeric variables.
#' @param vars variables to be explore missings for.
#' @export

explore_missings <- function(df,vars){
  
  var_missings <- data.frame(Number_Missings = apply(apply(df[,vars,drop=FALSE],
                                                           2,
                                                           is.na),
                                                     2,
                                                     sum))
  
  resp_missings <- as.data.frame(df[!stats::complete.cases(df[,vars,drop=FALSE]),])
  
  return(list(var_missings,resp_missings))
  
}