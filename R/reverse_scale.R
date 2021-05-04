
#' reverses the coding of the numeric variables provided.
#' @export
#' @param df should be a dataframe of numeric variables


reverse_scale <- function(df){
  # ensure correct data type is used before being reverse scaled
  df <- as.data.frame(lapply(df,
                             function(x){
                               as.numeric(as.character(x))
                               }
                             )
                      )

  return_df <- as.data.frame(lapply(df,
                                    function(x)
                                      {
                                      if (min(unique(x))==0){
                                        return(abs(x - max(x)))
                                        } else if (min(unique(x))==1){
                                          return(abs(x - max(x)) + 1)
                                        }
                                      }
                                    )
                             )

  colnames(return_df) <- paste0(colnames(df),"_reverse_scale")

  return(return_df)

}




