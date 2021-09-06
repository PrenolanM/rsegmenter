#' Determine outliers based on mahalanobis distance
#' @param df data.frame of numeric variables.
#' @param vars variables to be used in the outlier calculation.
#' @param pval_cutoff cutoff to classify an outlier. Any pvalue less than the cutoff
#' will be classified as an outlier
#' @export

mahalanobis_outlier <- function(df,
                                vars,
                                pval_cutoff = 0.001){
  
  mycenter <- colMeans(df[,vars,drop=FALSE])
  
  mycov <- cov(df[,vars,drop=FALSE])
  
  mymahalanobis <- stats::mahalanobis(x = df[,vars,drop=FALSE],
                                      center = mycenter,
                                      cov = mycov)
  
  pvalue <- stats::pchisq(mymahalanobis, df=3, lower.tail=FALSE)
  
  mahalanobis_outlier = ifelse(pvalue<pval_cutoff,1,0)
  
  return(mahalanobis_outlier)
  
}

