#' runs hierarchical clustering using hclust
#' @param df data.frame of numeric variables
#' @param vars character vector of variable names
#' @param dist_metric name of distance metric to use. see stats::dist for more distance options
#' @param linkage name of linkage method to use. see stats::hclust for more linkage options
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' hierarchical_segmentation(df = mydf, vars = c("col1","col2","col3"))
#' @export
#'
hierarchical_segmentation <- function(df,
                                      vars,
                                      dist_metric = "euclidean",
                                      linkage = "ward.D2"){
  
  df <- df[,vars,drop=FALSE]
  
  # Dissimilarity matrix
  dist_mat <- dist(t(df), method = dist_metric)
  
  # Hierarchical clustering using Complete Linkage
  hierarchical_segs <- hclust(dist_mat, method = linkage)
  
  # Plot the obtained dendrogram
  dendro <- plot(hierarchical_segs,
                 cex = 0.6,
                 hang = -1)
  
  return(list(hierarchical_segs,dendro))
  
}


