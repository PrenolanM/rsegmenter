#' extract segments from factor analysis segment solutions and stores in a dataframe
#' @param seglist must be an output from the rsegmenter::segmentation() function
#' @examples 
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' segment_solutions <- segmentation(mydf,c("col1","col2","col3"))
#' extract_factor_segments(segment_solutions)
#' @export
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

#' extract factor scores segments from factor analysis segment solutions and stores in a dataframe
#' @param seglist must be an output from the rsegmenter::segmentation() function
#' @examples 
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' segment_solutions <- segmentation(mydf,c("col1","col2","col3"))
#' extract_factor_scores(segment_solutions)
#' @export
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

#' Extract rotated components from factor analysis.
#' @param seglist must be an output from the rsegmenter::segmentation() function
#' @param var_labels must be a data.frame. 
#' First column must be names of the variables run in the factor analysis.
#' Second column must be the corresponding labels
#' @examples 
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' segment_solutions <- segmentation(mydf,c("Col1","Col2"))
#' extract_rotated_components(segment_solutions,var_labels)
#' @export

extract_rotated_components <- function(seglist,var_labels){
  
  return_list <- vector(mode="list",length(seglist[["factor_analysis"]]))
  
  return_list <- lapply(seq_along(seglist[["factor_analysis"]]),
                        function(x){
                          rc <- seglist[["factor_analysis"]][[x]][[1]]
                          rownames(rc) <- var_labels[match(var_labels[["Variable_Name"]],rownames(rc)),2]
                          return(rc)
                          }
                        )
                             
  
  return(return_list)
  
}

#' extract segments from lca segment solutions and stores in a dataframe
#' @param seglist must be an output from the rsegmenter::segmentation() function
#' @examples 
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' segment_solutions <- segmentation(mydf,c("Col1","Col2"))
#' extract_lca_segments(segment_solutions)
#' @export
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

#' extract segments from kmeans segment solutions and stores in a dataframe
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
