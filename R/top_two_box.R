#' creates a 0,1 coded variable. The two highest values become a 1 and the rest becomes a 0
#' @export
#' @param df should be a dataframe of numeric variables

top_two_box <- function(df){

  # ensure correct data type is used before being recoded
  df <- as.data.frame(lapply(df,
                             function(x){
                               as.numeric(as.character(x))
                               }
                             )
                      )

  return_df <- as.data.frame(lapply(df,
                                    function(x){

                                      max_val <- max(x)

                                      second_high_val <- sort(unique(x),decreasing = TRUE)[2]

                                      return(ifelse(x==max_val | x == second_high_val,1,0))
                                      }
                                    )
                             )

  return_df <- as.data.frame(lapply(return_df,
                                    function(x){
                                      as.factor(x)
                                      }
                                    )
                             )

  colnames(return_df) <- paste0(colnames(df),"_top_2_box")

  return(return_df)

}
