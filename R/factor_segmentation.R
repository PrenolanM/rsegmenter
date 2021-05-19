#' runs factor analysis with varimax rotation using the psych package
#' @param df must be a data.frame of numeric variables
#' 
#' @param vars must be a string of variable names to operate on.
#' @param num_sols should be a numeric vector specifying the minimum and maximum number of factors to extract
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' factor_segmentation(df = mydf, vars = c("col1","col2","col3"),num_sols=c(3,5))


factor_segmentation <- function(df,vars,num_sols,weight_var){
  
  # ensuring df is provided
  if (missing(df)){
    stop("df is compulsory")
  }
  
  # ensuring vars is provided
  if (missing(vars)){
    stop("vars is compulsory")
  }
  
  # ensuring num_sols is provided
  if (missing(num_sols)){
    stop("num_sols is compulsory")
  }
  
  factor_segs <- vector("list",length = max(num_sols)-min(num_sols) + 1)
  
  # df must be a data.frame or tibble
  if (!(c("data.frame") %in% class(df))){
    stop("df must be a data.frame or tibble")
  }
  
  # df must have at least 1 row
  # this implies df will have at least 1 col as well
  if (is.null(nrow(df))){
    stop("df must be a data.frame of at least 1 row")
  }
  
  # check that we don't have variables with all NA's
  if (check_all_na(df)){
    stop("at least one of the input variables contain all NA's")
  }
  
  # check that all variables are numeric
  if (check_all_numeric(df)){
    stop("at least one of the input variables is not numeric")
  }
  
  df <- df[,vars,drop=FALSE]
  
  if (sum(is.na(df))){
    
    warning("there are missing values in your data")
    
  }

  if (is.null(weight_var)){

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

                        assigned_segment <- apply(rowmeans_df,1,which.max)

                        return(list(rcloadings,assigned_segment))
                        }
                      )

  return(factor_segs)

}
