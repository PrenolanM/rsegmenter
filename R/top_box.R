#' creates a 0,1 coded variable. The highest value become a 1 and the rest becomes a 0
#' @export
#' @param df should be a dataframe of numeric variables

top_box <- function(df){

  # ensure correct data type is used before being recoded
  df <- as.data.frame(lapply(df,
                             function(x){
                               as.numeric(as.character(x))
                               }
                             )
                      )

  return_df <- as.data.frame(lapply(df,
                                    function(x){
                                      return(ifelse(x==max(x),1,0))
                                      }
                                    )
                             )

  return_df <- as.data.frame(lapply(return_df,
                                    function(x){
                                      as.factor(x)
                                      }
                                    )
                             )

  colnames(return_df) <- paste0(colnames(df),"_top_box")

  return(return_df)

}
