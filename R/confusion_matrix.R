#' creates confusion matrix
#' @param df data.frame of numeric variables
#' @param actual ground truth data
#' @param predicted predicted data
#' @param weight_var if not NULL, a vector that contains weights for each observation. The NULL
#' case is equivalent to all cases being weighted 1.
#' @export
#' 
confusion_matrix <- function(df,
                             actual,
                             predicted,
                             weight_var=NULL){
  
  # if weight is NULL, create a variable of all 1's and use this
  if (is.null(weight_var)){
    df[["weight_var"]] <- rep(1,nrow(df))
    weight_var <- "weight_var"
  }
  
  conf_table_raw <- df %>% 
    dplyr::select(dplyr::all_of(c(actual,predicted,weight_var))) %>% 
    dplyr::group_by(.data[[actual]],.data[[predicted]]) %>% 
    dplyr::summarise(mycount = sum(.data[[weight_var]])) %>% 
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(names_from = predicted,
                       names_prefix = predicted,
                       values_from = .data[["mycount"]]) %>% 
    dplyr::select(-actual)
    
  conf_table_raw[is.na(conf_table_raw)] <- 0

  conf_table_raw <- 
    conf_table_raw[,as.numeric(colnames(conf_table_raw))]
  
  conf_table_raw <- as.matrix(conf_table_raw)
  
  conf_table_raw <- 
    conf_table_raw[,as.numeric(colnames(conf_table_raw))]
  
  colnames(conf_table_raw) <- paste0("Predicted:",seq(ncol(conf_table_raw)))
  rownames(conf_table_raw) <- paste0("Actual:",seq(ncol(conf_table_raw)))
  
  conf_table_col <- conf_table_raw/colSums(conf_table_raw)

  conf_table_row <- conf_table_raw/rowSums(conf_table_raw)

  accuracy <- round(sum(diag(conf_table_raw))/sum(conf_table_raw),4) * 100

  sensitivity <- round(diag(conf_table_row),4)*100

  specificity <- unlist(lapply(seq(ncol(conf_table_raw)),function(x){
    round(sum(conf_table_raw[-x,-x])/sum(conf_table_raw[-x,]),4)*100
  }))

  pos_pred_val <- round(diag(conf_table_col),4)*100

  neg_pred_val <- unlist(lapply(seq(ncol(conf_table_raw)),function(x){
    round(sum(conf_table_raw[-x,-x])/sum(conf_table_raw[,-x]),4)*100
  }))

  detection_rate <- round(diag(conf_table_raw)/sum(conf_table_raw),4)*100

  outdf <- as.matrix(rbind(sensitivity,
                           specificity,
                           pos_pred_val,
                           neg_pred_val,
                           detection_rate))

  colnames(outdf) <- paste0("Class:",seq(ncol(conf_table_raw)))

  rownames(outdf) <- c("Sensitivity",
                       "Specificity",
                       "Positive_Prediction_Value",
                       "Negative_Prediction_Value",
                       "Detection_Rate")
  
  #f1 <- 2*((t(pos_pred_val)%*%sensitivity)/(sum(pos_pred_val,sensitivity)))
  
  return(list(accuracy = accuracy,
              #f1 = f1,
              outdf,
              confusion_matrix = round(conf_table_raw,0))
         )
  
}