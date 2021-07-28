#' assigns segments based on average factor loading
#' @param myloadings factor loadings
#' 
avg_loading <- function(df,myloadings){
  
  # getting the factor that each variable loads highest on
  max_loading <- t(apply(myloadings,1,
                         function(x){
                           ifelse(max(x)==x,1,0)
                           }
                         )
                   )
  
  
  # assigning the variable number to the factor it loads highest on
  # variables are in the order in which they were entered into the factoring
  max_loading <- as.data.frame(ifelse(max_loading==0,0,row(max_loading)))
  
  rowmeans_df <- as.data.frame(lapply(seq_along(max_loading),
                                      function(x){
                                        if(sum(max_loading[,x]>0)<2){
                                          df[,max_loading[,x]>0]
                                          } else {
                                            rowMeans(df[,max_loading[,x]>0])
                                          }
                                        }
                                      )
                               )
  
  assigned_segment <- max.col(rowmeans_df)  
}

