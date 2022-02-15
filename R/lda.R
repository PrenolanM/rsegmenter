#' Runs Linear Discriminant Analysis using the MASS package.
#' Returns the lda model object from the MASS package with an additional
#' elements that holds the fishers classification function coefficients and the 
#' predicted outcome.
#' @param df data.frame of numeric variables.
#' @param id unique row identifier.
#' @param dep target variable to be predicted.
#' @param indeps predictor variables.
#' @param prior numeric vector of prior probabilities. If NULL, will default
#' to 1 divided by number of distinct categories in the dependent variable.
#' @param create_algorithm TRUE/FALSE. If TRUE, will produce an excel file containing the prediction
#' algorithm.
#' 
#' @examples
#' df <- rsegmenter::test_seg_unlabelled
#' lda(df, dep="seg1", indeps=c("seg2","seg3","seg4"),prior=rep(1/4,4))
#' 
#' @export
#' 
lda <- function(df,
                id,
                dep,
                indeps,
                prior = NULL,
                create_algorithm = FALSE){
  
  if(is.null(prior)){
    prior <- rep(1/length(unique(df[,dep,drop=TRUE])),
                 length(unique(df[,dep,drop=TRUE]))
                 )  
  }
  
  id_data <- df[,id,drop=FALSE]
  dep_data <- df[,dep,drop=FALSE]
  df <- df[,c(dep,indeps),drop=FALSE]
  
  ldamodel <- MASS::lda(stats::as.formula(paste('as.factor(',dep,')','~.',
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
  
  if(create_algorithm == TRUE){
    
    df <- cbind(id_data,dep_data,df)
    
    colnames(df) <- c(id,dep,indeps)
    
    create_algorithm(df,id,indeps,ldamodel)
    
  }
  
  return(ldamodel)
  
}

pred_seg <- function(df,indeps,coefs){
  
  coefs_const <- coefs[1,]
  coefs_vars <- coefs[2:nrow(coefs),]
  
  # sum_prod <- as.matrix(df[,indeps,drop=FALSE]) %*% coefs_vars + coefs_const
  sum_prod <- (as.matrix(df[,indeps,drop=FALSE]) %*% coefs_vars) + matrix(rep(coefs_const,each = nrow(df)),nrow = nrow(df))
  
  final_pred <- max.col(sum_prod)
  return (final_pred)
  
}
