#' runs kmeans using the stats package
#' @param df data.frame of numeric variables
#' @param vars character vector of variable names
#' @param num_sols numeric vector specifying the minimum and maximum number of segments to extract
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' kmeans_segmentation(df = mydf, vars = c("col1","col2","col3"), num_sols = c(2,3))
#' @export
#'
kmeans_segmentation <- function(df,
                                vars,
                                num_sols,
                                iter.max = 10,
                                nstart = 1,
                                algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                                              "MacQueen")){

  df <- df[,vars,drop=FALSE]
  
  kmeans_segs <- lapply(num_sols,
                        function(x){

                          set.seed(123456)

                          stats::kmeans(df,
                                        centers = x,
                                        iter.max = iter.max,
                                        nstart = nstart,
                                        algorithm = "Hartigan-Wong"
                                        )
                          }
                        )

  return(kmeans_segs)

}
