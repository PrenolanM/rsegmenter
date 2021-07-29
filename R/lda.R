#' Runs lda from the MASS package
#' 
#' Returns the lda model object from the MASS package with a additional
#' elements that holds the fishers classification function coefficients and the 
#' predicted outcome
#' 
#' @param df must be a data.frame
#' 
#' @param dep must be a string of the dependent variable name
#' 
#' @param indeps character vector of predictor variable names
#' 
#' @param prior must be a numeric vector of prior probabilities. 
#' length(prior) must equal number of unique values in the dependent variable
#' 
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' lda(df = mydf, dep = "col1", indeps = c("col2","col3"), prior = rep(1/3,3))
#' @export
#' 
lda <- function(df,dep,indeps,prior){
  
  # ensuring df is provided
  if (missing(df)){
    stop("df is compulsory")
  }
  
  # ensuring vars is provided
  if (missing(dep)){
    stop("dep is compulsory")
  }
  
  # ensuring vars is provided
  if (missing(indeps)){
    stop("indeps is compulsory")
  }
  
  # df must be a data.frame or tibble
  if (!(c("data.frame") %in% class(df))){
    stop("df must be a data.frame or tibble")
  }
  
  # df must have at least 1 row
  # this implies df will have at least 1 col as well
  if (is.null(nrow(df))){
    stop("df must be a data.frame of at least 1 row")
  }
  
  df <- df[,c(dep,indeps),drop=FALSE]
  
  ldamodel <- MASS::lda(as.formula(paste('as.factor(',dep,')','~.',
                                         sep = '')),
                        data = df, 
                        prior = prior)
  
  group_vals <- sort(unique(df[[dep]]),decreasing = FALSE)
  
  mydep <- df[[dep]]
  
  df[[dep]] <- NULL
  
  gr <- length(group_vals) ## groups might be factors or numeric
  v <- ncol(df) ## variables
  m <- ldamodel$means ## group means
  
  w <- array(NA, dim = c(v, v, gr))
  
  for(i in 1:gr){
    tmp <- scale(subset(df, 
                        as.numeric(as.character(mydep)) == 
                          as.numeric(as.character(group_vals[i]))),
                 scale = FALSE)
    w[,,i] <- t(tmp) %*% tmp
  }
  
  W <- w[,,1]
  for(i in 2:gr)
    W <- W + w[,,i]
  
  V <- W/(nrow(df) - gr)
  
  iV <- solve(V)
  
  class_funs <- matrix(NA, nrow = v + 1, ncol = gr)
  
  colnames(class_funs) <- paste('segment', 1:gr, sep='.')
  
  rownames(class_funs) <- c('constant', paste('var', 1:v, sep = '.'))
  
  for(i in 1:gr) {
    class_funs[1, i] <- -0.5 * t(m[i,]) %*% iV %*% (m[i,])
    class_funs[2:(v+1) ,i] <- iV %*% (m[i,])
  }
  
  rownames(class_funs) <- c("Constant",indeps)
  ldamodel$class_funs <- class_funs
  
  # adding predicted segments
  ldamodel$predictions <- pred_seg(df,indeps,class_funs)
  
  return(ldamodel)
  
}

pred_seg <- function(df,indeps,coefs){
  
  coefs_const <- coefs[1,]
  coefs_vars <- coefs[2:nrow(coefs),]
  
  print(dim(as.matrix(df[,indeps,drop=FALSE])))
  print(dim(coefs_vars))
  
  sum_prod <- as.matrix(df[,indeps,drop=FALSE]) %*% coefs_vars + coefs_const
  final_pred <- max.col(sum_prod)
  return (final_pred)
  
}
