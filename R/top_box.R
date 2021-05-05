#' creates a 0,1 coded variable. The highest value becomes a 1 and the rest becomes a 0

#' @param df must be a data.frame
#' 
#' @param vars must be a string of variable names to operate on.
#' These variables must be numeric
#' 
#' @param impute_type must be a string of one of "mean","min","max"
#' 
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' top_box(df = mydf, vars = c("col1","col2","col3"), impute_type = "mean")

#' @export

top_box <- function(df,vars,impute_type="mode"){

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
