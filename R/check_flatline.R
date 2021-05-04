#' gets the number of responses that are the same per person
#' @export
#' @param df should be a dataframe of numeric variables

check_flatline <- function(df){

  return_df <- as.data.frame(apply(df,1,
                                   function(x){
                                     max(table(x))
                                     }
                                   )
                             )

  colnames(return_df) <- "Number_Flatline"

  return(return_df)
}
