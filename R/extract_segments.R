#' extract segments from factor analysis segment solutions and stores in a dataframe
#' @export
#' @param df should be an output from the kantarsegmentr::segmentation() function

extract_factor_segments <- function(df){
  return_df <- as.data.frame(lapply(seq(1,length(df[[2]])),
                                    function(x){
                                      df[[2]][[x]][[2]]
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
