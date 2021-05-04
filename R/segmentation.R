
#' runs kmeans, factor analsysis, kmediods
#' @export
#' @param df should be a dataframe of numeric variables
#' @param min_segs should be an integer specifying the minimum number of segments to extract, defaults to 3
#' @param max_segs should be an integer specifying the maximum number of segments to extract, defaults to 5

segmentation <- function(df, min_segs = 3, max_segs = 5,weight_var){

  dist_start <- base::Sys.time()
  print(paste0("calculating distance matrix: ",dist_start))

  segmentation_out <- vector("list",length = 2)
  num_sols <- seq(min_segs,max_segs)

  gower_dist <- cluster::daisy(df,metric = "gower")

  kmeans_start <- base::Sys.time()
  print(paste0("starting kmeans segmentation: ",kmeans_start))

  # kmeans
  segmentation_out[[1]] <- kmeans_segmentation(gower_dist,num_sols)

  fac_start <- base::Sys.time()
  print(paste0("starting factor segmentation: ",fac_start))

  # factor analysis
  segmentation_out[[2]] <- factor_segmentation(df,num_sols,weight_var)

  kmediods_start <- base::Sys.time()
  print(paste0("starting kmediods segmentation: ",kmediods_start))

  # kmediods
  segmentation_out[[3]] <- kmediods_segmentation(gower_dist,num_sols)

  timeend <- base::Sys.time()
  print(paste0("segmentation complete: ",timeend))

  print(paste0("duration: ",timeend-dist_start))

  return(segmentation_out)
}


