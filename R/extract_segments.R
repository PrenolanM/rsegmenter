#' Extract segments from factor analysis segment solutions and stores each segment as a variable in a data.frame.
#' @param seglist an output from the rsegmenter::factor_segmentation() function. See more details about factor_segmentation() 
#' in the package documentation.
#' @examples 
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' segment_solutions <- factor_segmentation(mydf,vars = c("col1","col2","col3"),
#' weight_var = NULL, num_sols = c(2:3), rotate = "varimax",
#' scores = TRUE, fac_assign = "avg_loading")
#' extract_factor_segments(segment_solutions)
#' @export
#' 
extract_factor_segments <- function(seglist){
  
  return_df <- as.data.frame(lapply(seq_along(seglist),
                                    function(x){
                                      seglist[[x]][["segments"]]
                                      }
                                    )
                             )

  
  min_sol_num <- max(return_df[,1])
  max_sol_num <- max(return_df)
  colnames(return_df) <- paste0("Factor_Cluster_Soln_",seq(min_sol_num,max_sol_num))

  return(return_df)
  
}

#' Extract factor scores for each segment solution from factor analysis stores each set of scores as a data.frame as element of a list.
#' @param seglist an output from the rsegmenter::factor_segmentation() function. See more details about factor_segmentation() 
#' in the package documentation.
#' @examples 
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' segment_solutions <- factor_segmentation(mydf,vars = c("col1","col2","col3"),
#' weight_var = NULL, num_sols = c(2:3), rotate = "varimax",
#' scores = TRUE, fac_assign = "avg_loading")
#' extract_factor_scores(segment_solutions)
#' @export
#' 
extract_factor_scores <- function(seglist){
  
  return_list <- (lapply(seq_along(seglist),
                         function(x){
                           scores <- seglist[[x]][["scores"]]
                           colnames(scores) <- paste0("Factor_Cluster_Soln_",
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
                               seq(min_sol_num,max_sol_num))
  
  return(return_list)
  
}

#' Extract segments from lca segment solutions and stores each segment as a variable in a data.frame.
#' @param seglist an output from the rsegmenter::lca_segmentation() function. See more details about lca_segmentation() 
#' in the package documentation.
#' @examples 
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' segment_solutions <- lca_segmentation(mydf,vars = c("col1","col2","col3"),
#' num_sols = c(2:3))
#' extract_lca_segments(segment_solutions)
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
