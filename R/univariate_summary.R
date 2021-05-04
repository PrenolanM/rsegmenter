
univariate_summary <- function(df,max_vals = 10){
  return(summarytools::dfSummary(df,max.distinct.values = max_vals))
}