#' runs lca using the polca package
#' @param df should be a data.frame of numeric variables
#' 
#' @param vars must be a string of variable names to operate on.
#' These variables must be numeric
#' 
#' @param impute_type must be a string of one of "none","mode","mean","min","max"
#' @param num_sols should be a numeric vector specifying the minimum and maximum number of factors to extract

#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' lca_segmentation(df = mydf, vars = c("col1","col2","col3"), impute_type = "none",num_sols=c(3,5))

lca_segmentation <- function(df,vars,impute_type="none",num_sols){
  
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
  
  lca_segs <- vector("list",length = max(num_sols)-min(num_sols) + 1)
  
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
  
  df <- df[vars]
  
  # vars can't have 0's
  # we check for this and if present, we increment the whole variable by the second highest value
  col_with_zero <- apply(df,2,
                         function(x){
                           any(x==0)
                           }
                         )
  
  
  if (any(col_with_zero)){
    df[col_with_zero] <- as.data.frame(apply(df[col_with_zero],2,
                                             function(x){
                                               x + 1
                                               }
                                             )
                                       )
    }
  
  
  if (impute_type!="none"){
    for (i in seq_along(vars)){
      df[is.na(df[,i]),i] <- impute_values(df,impute_type)[i]
    }  
  }
  
  if (sum(is.na(df))){
    
    warning("there are missing values in your data")
    
  }
  
  # if (missing(weight_var)){
  #   
  #   resp_weight <- rep(1,nrow(df))
  #   
  # } else{
  #   
  #   resp_weight <- df[[weight_var]]
  #   df[[weight_var]] <- NULL
  #   
  # }
  
  
  # this will run all factor solutions in order to get the loadings and factor scores
  lca_segs <- lapply(num_sols,
                     function(x){
                       
                       set.seed(12345)
                       
                       f <- as.formula(paste(paste("cbind(",paste(vars,collapse = ","),")"),1,sep = "~"))
                       
                       lca_soln <-  poLCA::poLCA(f,                 #A formula expression of the form response ~ predictors
                                                 df,                #Data frame
                                                 nclass=x,          #Number of latent classes to assume in the model
                                                 maxiter=1000,      #The maximum number of iterations through which the estimation algorithm will cycle
                                                 graphs=FALSE,      #Should poLCA graphically display the parameter estimates at the completion of the estimation algorithm
                                                 tol=1e-10,         #A tolerance value for judging when convergence has been reached. When the one-iteration change in the estimated log-likelihood is less than tol, the estimation algorithm stops updating and considers the maximum log-likelihood to have been found
                                                 na.rm=TRUE,        #How to handle missing values, If TRUE, those cases are removed (listwise deleted) before estimating the model. If FALSE, cases with missing values are retained. Cases with missing covariates are always removed
                                                 probs.start=NULL,  #A list of matrices of class-conditional response probabilities to be used as the starting values for the estimation algorithm. NULL = producing random starting values
                                                 nrep=1,            #Number of times to estimate the model, using different values of probs.start. The default is one. Setting nrep>1 automates the search for the global---rather than just a local---maximum of the log-likelihood function. poLCA returns the parameter estimates corresponding to the model with the greatest log-likelihood
                                                 verbose=FALSE,      #To should output of the model = TRUE
                                                 calc.se=TRUE)      #Calculate the standard errors of the estimated class-conditional response probabilities and mixing proportions  
                     }
  )
  return(lca_segs)
}