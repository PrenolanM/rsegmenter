#' runs kmeans using the stats package
#' @param df data.frame of numeric variables
#' @param vars character vector of variable names
#' @param num_sols numeric vector specifying the minimum and maximum number of segments to extract
#' @param iter.max the maximum number of iterations allowed
#' @param nstart if centers is a number, how many random sets should be chosen?
#' @param algorithm character: may be abbreviated. Note that "Lloyd" and "Forgy" are alternative names for one algorithm.
#' 
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
