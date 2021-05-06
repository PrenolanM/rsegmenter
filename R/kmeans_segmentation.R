#' runs kmeans

#' @param dist_mat should be a distance matrix
#' @param num_sols should be a numeric vector specifying the minimum and maximum number of factors to extract

kmeans_segmentation <- function(dist_mat,num_sols){

  # need to test that dist_mat is a distance matrix

  kmeans_segs <- lapply(num_sols,
                        function(x){

                          set.seed(12345)

                          stats::kmeans(dist_mat,
                                        centers =x
                                        #iter.max = 100,
                                        #nstart = 10
                                        )

                          }
                        )


  return(kmeans_segs)

}
