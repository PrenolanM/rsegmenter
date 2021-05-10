#' extract segments from factor analysis segment solutions and stores in a dataframe
#' @export
#' @param seglist must be an output from the kantarsegmentr::segmentation() function

extract_factor_segments <- function(seglist){
  return_df <- as.data.frame(lapply(seq(1,length(seglist[["factor_analysis"]])),
                                    function(x){
                                      seglist[["factor_analysis"]][[x]][[2]]
                                      }
                                    )
                             )

  return_df <- as.data.frame(lapply(return_df,
                                    function(x){
                                      as.factor(x)
                                    }
                                    )
                             )

  colnames(return_df) <- paste0("Factor_Cluster_Soln_",seq(1,ncol(return_df)))

  return(return_df)
}

#' extract rotated components from factor analysis
#' @export
#' @param seglist must be an output from the kantarsegmentr::segmentation() function
#' @param var_labels must be a data.frame. 
#' First column must be names of the variables run in the factor analysis.
#' Second column must be the corresponding label

extract_rotated_components <- function(seglist,var_labels){
  
  return_list <- vector(mode="list",length(seglist[["factor_analysis"]]))
  
  return_list <- lapply(seq(1,length(seglist[["factor_analysis"]])),
                        function(x){
                          rc <- seglist[["factor_analysis"]][[x]][[1]]
                          rownames(rc) <- var_labels[match(var_labels[["Variable_Name"]],rownames(return_list[[1]])),2]
                          return(rc)
                          }
                        )
                             
  
  return(return_list)
  
}

#' extract segments from lca segment solutions and stores in a dataframe
#' @export
#' @param seglist must be an output from the kantarsegmentr::segmentation() function

extract_lca_segments <- function(seglist){
  return_df <- as.data.frame(lapply(seq(1,length(seglist[["lca"]])),
                                    function(x){
                                      seglist[["lca"]][[x]][["predclass"]]
                                      }
                                    )
                             )
  
  return_df <- as.data.frame(lapply(return_df,
                                    function(x){
                                      as.factor(x)
                                      }
                                    )
                             )
  
  colnames(return_df) <- paste0("LCA_Cluster_Soln_",seq(1,ncol(return_df)))
  
  return(return_df)
}

#' extract segments from kmeans segment solutions and stores in a dataframe
#' @export
#' @param df should be an output from the kantarsegmentr::segmentation() function

extract_kmeans_segments <- function(df){
  return_df <- as.data.frame(lapply(seq(1,length(df[[1]])),
                                    function(x){
                                      df[[1]][[x]][[1]]
                                      }
                                    )
                             )

  return_df <- as.data.frame(lapply(return_df,
                                    function(x){
                                      as.factor(x)
                                      }
                                    )
                             )

  colnames(return_df) <- paste0("Kmeans_Cluster_Soln_",seq(1,ncol(return_df)))

  return(return_df)
}

#' extract segments from kmediods segment solutions and stores in a dataframe
#' @export
#' @param df should be an output from the kantarsegmentr::segmentation() function

extract_kmeans_segments <- function(df){
  return_df <- as.data.frame(lapply(seq(1,length(df[[1]])),
                                    function(x){
                                      df[[3]][[x]][[1]]
                                    }
  )
  )

  return_df <- as.data.frame(lapply(return_df,
                                    function(x){
                                      as.factor(x)
                                    }
  )
  )

  colnames(return_df) <- paste0("Kmediods_Cluster_Soln_",seq(1,ncol(return_df)))

  return(return_df)
}
