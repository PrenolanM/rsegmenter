#' runs factor analysis with varimax rotation using the psych package
#' @param df data.frame of numeric variables
#' @param vars variables for the factor analysis
#' @param weight_var numeric vector of row weights
#' @param num_sols numeric vector specifying the minimum and maximum number of factors to extract
#' @param rotate method of rotation for factor analysis. Options are the same as psych::principal()
#' @param scores TRUE/FALSE to include scores in the output or not. if fac_assign="max_score", scores must be set to TRUE
#' @param fac_assign method to use to assign segments to rows. options are one of c("avg_loading","max_score")
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1),myweight=c(1,2,1))
#' factor_segmentation(df = mydf,col1,col2,col3,myweight,num_sols=c(3,5),rotate="varimax",scores=FALSE,fac_assign="avg_loading")
#' @export
#' 
factor_segmentation <- function(df,
                                vars,
                                weight_var,
                                num_sols,
                                rotate="varimax",
                                scores=FALSE,
                                fac_assign="avg_loading"){
  
  # ensuring df is provided
  if (missing(df)){
    stop("df is compulsory")
  }
  
  # ensuring variables to factor analyse is provided
  if(length(vars)==0){
    stop("numeric variables to factor must be provided")
  }
  
  # ensure if fac_assign = "max_score" then scores=TRUE
  if (fac_assign=="max_score" & !scores){
    stop("If fac_assign=max_score, scores must be TRUE")
  }
  
  factor_segs <- vector("list",length = max(num_sols)-min(num_sols) + 1)

  if (missing(weight_var)){

    resp_weight <- rep(1,nrow(df))

  } else{

    resp_weight <- df[[weight_var]]
    
  }

  df <- df[,vars,drop=FALSE]
  
  # check that there are no missing values
  if (sum(is.na(df))>0){
    stop("data has NA's. please address these before segmenting")
  }
  
  # this will run all factor solutions in order to get the loadings and factor scores
  factor_segs <- lapply(num_sols,
                      function(x){

                        set.seed(123456)

                        factor_soln <- psych::principal(df,
                                                        nfactors = x,
                                                        rotate = rotate,
                                                        scores = scores,
                                                        weight = resp_weight)

                        rcloadings <- as.data.frame(unclass(factor_soln[["loadings"]]))

                        if (fac_assign=="avg_loading"){
                          
                          assigned_segment <- avg_loading(df,rcloadings)
                          
                        } else {
                          
                          assigned_segment <- max_score(rcloadings)
                          
                        }
                        
                        if (!scores){
                          
                          return(list(segments=assigned_segment,
                                      loadings=rcloadings))
                          
                        } else if (scores){
                          
                          rcscores <- as.data.frame(factor_soln[["scores"]])
                          return(list(segments=assigned_segment,
                                      loadings=rcloadings,
                                      scores=rcscores))
                          
                        }
                        
                        }
                      )
  
  return(factor_segs)

}
