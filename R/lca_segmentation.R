#' runs lca using the poLCA package
#' @param df data.frame of numeric variables
#' @param vars character vector of variable names
#' @param num_sols numeric vector specifying the minimum and maximum number of factors to extract
#' @param maxiter The maximum number of iterations through which the estimation algorithm will cycle
#' @param tol A tolerance value for judging when convergence has been reached. When the one-iteration change in the estimated 
#' log-likelihood is less than tol, the estimation algorithm stops updating and considers the maximum log-likelihood to have been 
#' found
#' @param na.rm Logical, for how poLCA handles cases with missing values on the manifest variables. If TRUE, those cases are 
#' removed (listwise deleted) before estimating the model. If FALSE, cases with missing values are retained. 
#' Cases with missing covariates are always removed. The default is TRUE.
#' @param nrep Number of times to estimate the model, using different values of probs.start. The default is one. 
#' Setting nrep>1 automates the search for the global—rather than just a local—maximum of the log-likelihood function. 
#' poLCA returns the parameter estimates corresponding to the model with the greatest log-likelihood.
#' 
#' #' @examples
#' df <- rsegmenter::test_seg_unlabelled
#' segment_input_vars <- c("seg1","seg2","seg3","seg4","seg5","seg6","seg7","seg8","seg9","seg10")
#' lca_segmentation(df = df, vars = segment_input_vars, num_sols=c(2,3))
#' 
#' @importFrom MASS ginv
#' @export
#' 
lca_segmentation <- function(df,
                             vars,
                             num_sols,
                             maxiter=1000,
                             tol=1e-10,
                             na.rm=TRUE,
                             nrep=1){
  
  # ensuring df is provided
  if (missing(df)){
    stop("df is compulsory")
  }
  
  # ensuring variables to run lca on is provided
  if(length(vars)==0){
    stop("numeric variable to factor must be provided")
  }
  
  # ensuring num_sols is provided
  if (missing(num_sols)){
    stop("num_sols is compulsory")
  }
  
  lca_segs <- vector("list",length = max(num_sols)-min(num_sols) + 1)
  
  # df must be a data.frame or tibble
  if (!(c("data.frame") %in% class(df))){
    stop("df must be a data.frame or tibble")
  }
  
  
  df <- df[,vars,drop=FALSE]
  
  # df must have at least 1 row
  # this implies df will have at least 1 col as well
  if (is.null(nrow(df))){
    stop("df must be a data.frame of at least 1 row")
  }
  
  # check that all variables are numeric
  if (check_all_numeric(df)){
    stop("at least one of the input variables is not numeric")
  }
  
  # check that there are no missing values
  if (sum(is.na(df))>0){
    stop("data has NA's. please address these before segmenting")
  }
  
  # cannot have 0's in our data for this algorithm
  # if 0's are present, increment the affected variable by 1
  df <- df %>% 
    dplyr::mutate(dplyr::across(tidyselect::where(~ min(.)==0), ~ . + 1))
  
  # this will run all factor solutions in order to get the loadings and factor scores
  lca_segs <- lapply(num_sols,
                     function(x){
                       
                       set.seed(123456)
                       
                       # building the formula to pass through to poLCA
                       f <- stats::as.formula(
                         paste(paste("cbind(",
                                     paste(vars,collapse = ","),
                                     ")"
                                     ),
                               1,
                               sep = "~")
                         )
                       
                       lca_soln <-  poLCA::poLCA(f,                 #A formula expression of the form response ~ predictors
                                                 df,                #Data frame
                                                 nclass=x,          #Number of latent classes to assume in the model
                                                 maxiter=maxiter,   #The maximum number of iterations through which the estimation algorithm will cycle
                                                 graphs=FALSE,      #Should poLCA graphically display the parameter estimates at the completion of the estimation algorithm
                                                 tol=tol,           #A tolerance value for judging when convergence has been reached. When the one-iteration change in the estimated log-likelihood is less than tol, the estimation algorithm stops updating and considers the maximum log-likelihood to have been found
                                                 na.rm=na.rm,       #How to handle missing values, If TRUE, those cases are removed (listwise deleted) before estimating the model. If FALSE, cases with missing values are retained. Cases with missing covariates are always removed
                                                 probs.start=NULL,  #A list of matrices of class-conditional response probabilities to be used as the starting values for the estimation algorithm. NULL = producing random starting values
                                                 nrep=nrep,         #Number of times to estimate the model, using different values of probs.start. The default is one. Setting nrep>1 automates the search for the global---rather than just a local---maximum of the log-likelihood function. poLCA returns the parameter estimates corresponding to the model with the greatest log-likelihood
                                                 verbose=FALSE,     #To should output of the model = TRUE
                                                 calc.se=TRUE)      #Calculate the standard errors of the estimated class-conditional response probabilities and mixing proportions
                       
                       return(list(segments = lca_soln[["predclass"]],
                                   posterior_probs = lca_soln[["posterior"]],
                                   conditional_probs = lca_soln[["probs"]]
                                   )
                              )
                     }
  )
  
  return(lca_segs)
  
}