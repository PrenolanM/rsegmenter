#' Extract segments from factor analysis segment solutions and stores each segment as a variable in a data.frame.
#' @param seglist an output from the rsegmenter::factor_segmentation() function. See more details about factor_segmentation() 
#' in the package documentation.
#' @export
#' 
extract_factor_segments <- function(seglist,suffix=NULL){
  
  # need to check that seglist is not empty
  
  return_df <- as.data.frame(lapply(seq_along(seglist),
                                    function(x){
                                      seglist[[x]][["segments"]]
                                      }
                                    )
                             )

  
  min_sol_num <- max(return_df[,1])
  max_sol_num <- max(return_df)
  colnames(return_df) <- paste0("Factor_Cluster_Soln_",
                                ifelse(is.null(suffix),"",paste0(suffix,"_")),
                                seq(min_sol_num,max_sol_num))

  return(return_df)
  
}

#' Extract factor scores for each segment solution from factor analysis stores each set of scores as a data.frame as element of a list.
#' @param seglist an output from the rsegmenter::factor_segmentation() function. See more details about factor_segmentation() 
#' in the package documentation.
#' @export
#' 
extract_factor_scores <- function(seglist,suffix=NULL){
  
  return_list <- (lapply(seq_along(seglist),
                         function(x){
                           scores <- seglist[[x]][["scores"]]
                           colnames(scores) <- paste0("Factor_Cluster_Soln_",
                                                      ifelse(is.null(suffix),"",paste0(suffix,"_")),
                                                      ncol(scores),
                                                      "_Scores_",
                                                      seq(1,ncol(scores))) 
                           return(scores)
                           }
                         )
                  )
  
  min_sol_num <- ncol(return_list[[1]])
  max_sol_num <- ncol(return_list[[length(return_list)]])
  names(return_list) <- paste0("Factor_Cluster_Soln_",
                               ifelse(is.null(suffix),"",paste0(suffix,"_")),
                               seq(min_sol_num,max_sol_num))
  
  return(return_list)
  
}

#' Extract segments from lca segment solutions and stores each segment as a variable in a data.frame.
#' @param seglist an output from the rsegmenter::lca_segmentation() function. See more details about lca_segmentation() 
#' in the package documentation.
#' @export
#' 
extract_lca_segments <- function(seglist){
  return_df <- as.data.frame(lapply(seq_along(seglist),
                                    function(x){
                                      seglist[[x]][["segments"]]
                                      }
                                    )
                             )

  min_sol_num <- max(return_df[,1])
  max_sol_num <- max(return_df)  
  colnames(return_df) <- paste0("LCA_Cluster_Soln_",seq(min_sol_num,max_sol_num))
  
  return(return_df)
}

#' Extract segments from lca segment solutions and stores each segment as a variable in a data.frame.
#' @param seglist an output from the rsegmenter::lca_segmentation() function. See more details about lca_segmentation() 
#' in the package documentation.
#' @export
#' 
extract_kmeans_segments <- function(seglist){
  return_df <- as.data.frame(lapply(seq_along(seglist),
                                    function(x){
                                      seglist[[x]][["cluster"]]
                                    }
  )
  )
  
  min_sol_num <- max(return_df[,1])
  max_sol_num <- max(return_df)  
  colnames(return_df) <- paste0("Kmeans_Cluster_Soln_",seq(min_sol_num,max_sol_num))
  
  return(return_df)
}