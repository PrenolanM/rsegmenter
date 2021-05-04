#' runs kmediods
#' @export
#' @param dist_mat should be a distance matrix
#' @param num_sols should be a numeric vector specifying the minimum and maximum number of segments to extract

kmediods_segmentation <- function(dist_mat,num_sols){

  # need to test that the input is a distance matrix

  kmediods_segs <- lapply(num_sols,
                          function(x){

                            set.seed(12345)

                            cluster::pam(dist_mat,
                                         k=x,
                                         keep.diss=FALSE,
                                         keep.data=FALSE)
                          }
                        )


  return(kmediods_segs)

}
