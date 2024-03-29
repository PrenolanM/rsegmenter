#' Runs factor analysis with varimax rotation using the psych package.
#' @param df data.frame of numeric variables.
#' @param vars variables to be used in the factor analysis.
#' @param weight_var if not NULL, a vector that contains weights for each observation. The NULL
#' case is equivalent to all cases being weighted 1.
#' @param num_sols number of segment solutions to run.
#' @param rotate method of rotation for factor analysis. See psych::principal() for more details.
#' @param fac_assign method to use to assign segments to rows. Options are one of c("avg_loading","max_score").
#' 
#' @examples
#' df <- rsegmenter::test_seg_unlabelled
#' segment_input_vars <- c("seg1","seg2","seg3","seg4","seg5","seg6","seg7","seg8","seg9","seg10")
#' factor_segmentation(df = df, vars = segment_input_vars, weight_var = "weight", num_sols=c(2:3),
#' rotate="varimax", fac_assign="avg_loading")
#' 
#' @export
#' 
factor_segmentation <- function(df,
                                vars,
                                weight_var=NULL,
                                num_sols,
                                rotate="varimax",
                                fac_assign="avg_loading"){
  
  # need to check that number of vars is less than equal to max num_sols
  if (length(vars) < max(num_sols)){
    stop("Number of variables is < max(num_sols)")
  }
  
  factor_segs <- vector("list",length = max(num_sols)-min(num_sols) + 1)

  if (is.null(weight_var)){

    resp_weight <- rep(1,nrow(df))

  } else{

    resp_weight <- df[[weight_var]]
    
  }

  df <- df[,vars,drop=FALSE]
  
  # check that there are no missing values
  if (sum(is.na(df))>0){
    stop("data has NA's. please address these before segmenting")
  }
  
  if (sum(is.na(resp_weight))>0){
    stop("weight has NA's. please address these before segmenting")
  }
  
  # this will run all factor solutions in order to get the loadings and factor scores
  factor_segs <- lapply(num_sols,
                      function(x){

                        set.seed(123456)

                        factor_soln <- psych::principal(df,
                                                        nfactors = x,
                                                        rotate = rotate,
                                                        scores = TRUE,
                                                        weight = resp_weight)

                        rcloadings <- as.data.frame(unclass(factor_soln[["loadings"]]))
                        full_rcloadings <- rbind(as.data.frame(unclass(factor_soln[["loadings"]])),
                                                 as.data.frame(unclass(factor_soln[["Vaccounted"]])))
                        
                        rcscores <- as.data.frame(factor_soln[["scores"]])
                        
                        if (fac_assign=="avg_loading"){
                          
                          assigned_segment <- avg_loading(df,rcloadings)
                          
                        } else {
                          
                          assigned_segment <- max_score(rcscores)
                          
                        }
                        
                        return(list(segments=assigned_segment,
                                    loadings=full_rcloadings,
                                    scores=rcscores))
                        
                        }
                      )
  
  return(factor_segs)

}
