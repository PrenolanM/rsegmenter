
#' runs factor analsysis and lca

#' @param df should be a data.frame of numeric variables
#' 
#' @param vars must be a string of variable names to operate on.
#' These variables must be numeric
#' 
#' @param impute_type must be a string of one of "none","mode","mean","min","max"
#' @param min_segs should be an integer specifying the minimum number of segments to extract, defaults to 3
#' @param max_segs should be an integer specifying the maximum number of segments to extract, defaults to 5

#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' segmentation(df = mydf, vars = c("col1","col2","col3"), impute_type = "none",min_segs = 3, max_segs = 5)
 
#' @export

segmentation <- function(df, vars, impute_type="none", min_segs = 3, max_segs = 5, weight_var = NULL){

  #dist_start <- base::Sys.time()
  #print(paste0("calculating distance matrix: ",dist_start))

  segmentation_out <- vector("list",length = 2)
  num_sols <- seq(min_segs,max_segs)

  #gower_dist <- cluster::daisy(df,metric = "gower")

  #kmeans_start <- base::Sys.time()
  #print(paste0("starting kmeans segmentation: ",kmeans_start))

  # kmeans
  #segmentation_out[["kmeans"]] <- kmeans_segmentation(gower_dist,num_sols)

  #fac_start <- base::Sys.time()
  #print(paste0("starting factor segmentation: ",fac_start))

  # factor analysis
  segmentation_out[["factor_analysis"]] <- factor_segmentation(df,vars,impute_type,num_sols,weight_var)

  #kmediods_start <- base::Sys.time()
  #print(paste0("starting kmediods segmentation: ",kmediods_start))

  # kmediods
  #segmentation_out[["kmediods"]] <- kmediods_segmentation(gower_dist,num_sols)

  #timeend <- base::Sys.time()
  #print(paste0("segmentation complete: ",timeend))

  #print(paste0("duration: ",timeend-dist_start))

  return(segmentation_out)
}


