#' runs factor analysis with varimax rotation using the psych package
#' @export
#' @param df should be a dataframe of numeric variables
#' @param num_sols should be a numeric vector specifying the minimum and maximum number of factors to extract

factor_segmentation <- function(df,num_sols,weight_var=NULL){

  factor_segs <- vector("list",length = max(num_sols)-min(num_sols) + 1)

  # ensure correct data type is used in factoring
  df <- as.data.frame(lapply(df,
                             function(x){
                               as.numeric(as.character(x))
                               }
                             )
                      )

  if (missing(weight_var)){

    resp_weight <- rep(1,nrow(df))

  } else{

    resp_weight <- df[[weight_var]]
    df[[weight_var]] <- NULL

  }


  # this will run all factor solutions in order to get the loadings and factor scores
  factor_segs <- lapply(num_sols,
                      function(x){

                        set.seed(12345)

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

                        max_loading <- ifelse(max_loading==0,0,row(max_loading))

                        rowmeans_df <- as.data.frame(lapply(seq(1,ncol(max_loading)),
                                                            function(x){
                                                              if(sum(max_loading[,x]>0)<2){
                                                                df[,max_loading[,x]>0]
                                                                } else {
                                                                  rowMeans(df[,max_loading[,x]>0])
                                                                }
                                                              }
                                                            )
                                                     )

                        assigned_segment <- apply(rowmeans_df,1,which.max)

                        return(list(rcloadings,assigned_segment))
                        }
                      )

  return(factor_segs)

}
