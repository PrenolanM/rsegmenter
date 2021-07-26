#' runs factor analysis with varimax rotation using the psych package
#' @param df data.frame of numeric variables
#' @param ... variables for the factor analysis
#' @param num_sols numeric vector specifying the minimum and maximum number of factors to extract
#' @param weight_var numeric vector of row weights 
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1),myweight=c(1,2,1))
#' factor_segmentation(df = mydf,col1,col2,col3,num_sols=c(3,5),myweight)
#' @export
#' @importFrom magrittr %>%
factor_segmentation <- function(df,...,num_sols,weight_var){

  factor_segs <- vector("list",length = max(num_sols)-min(num_sols) + 1)

  if (missing(weight_var)){

    resp_weight <- rep(1,nrow(df))

  } else{

    resp_weight <- df %>% 
      select({{weight_var}}) %>% 
      unlist()
    
  }

  df <- df %>% 
    select(...)
  
  # this will run all factor solutions in order to get the loadings and factor scores
  factor_segs <- lapply(num_sols,
                      function(x){

                        set.seed(123456)

                        factor_soln <- psych::principal(df,
                                                        nfactors = x,
                                                        rotate = "varimax",
                                                        scores = FALSE,
                                                        weight = resp_weight)

                        rcloadings <- as.data.frame(unclass(factor_soln$loadings))

                        # getting the variables per factor that load highest on the factor
                        # this will be used to work out respondent row level means using the set of variables
                        max_loading <- (t(apply(rcloadings,1,
                                                function(x){
                                                  ifelse(max(x)==x,1,0)
                                                  }
                                                )
                                          )
                                        )

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

                        #assigned_segment <- apply(rowmeans_df,1,which.max)

                        assigned_segment <- max.col(rowmeans_df)
                        
                        return(list(assigned_segment,rowmeans_df,rcloadings))
                        }
                      )

  return(factor_segs)

}
