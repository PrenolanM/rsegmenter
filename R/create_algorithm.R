#' Creates an excel workbook with two worksheets. The first sheet contains the classification 
#' function coefficients from a model created using rsegmenter::lda. The second sheet contains the training data
#' and excel formulae to assign the training data to a segment, using the classification function
#' coefficients.
#' 
#' @param df data.frame of numeric variables.
#' @param id unique row identifier.
#' @param indeps predictor variables.
#' @param ldamodel lda model object.

create_algorithm <- function(df,
                             id,
                             indeps,
                             ldamodel){
  
  df <- df[,c(id,indeps),drop=FALSE]
  coefs <- ldamodel[["class_funs"]]
  
  # create new workbook
  algorithm_wb <- openxlsx::createWorkbook()
  
  # add worksheet for classification function coefficients
  openxlsx::addWorksheet(algorithm_wb, "coefs")
  
  # add worksheet for input data
  openxlsx::addWorksheet(algorithm_wb, "input_data")
  
  # writing the function classification function coefficients to the sheet
  openxlsx::writeData(algorithm_wb, 
                      "coefs",
                      t(ldamodel$class_funs),
                      colNames = TRUE,
                      rowNames = TRUE,
                      startCol = 1,
                      startRow = 1)
  
  # writing input data with 
  openxlsx::writeData(algorithm_wb, 
                      "input_data",
                      df[1,],
                      colNames = TRUE,
                      rowNames = FALSE,
                      startCol = 1,
                      startRow = 1)
  
  # getting different starting columns to be used to write to excel
  num_pred_vars <- length(indeps)
  sum_prod_start <- num_pred_vars + 2
  sum_prod_end <- sum_prod_start + ncol(coefs)
  
  lapply(seq(ncol(coefs)),function(x){
    
    openxlsx::writeData(algorithm_wb,
                        "input_data",
                        paste0("Segment_",x,"_Score"),
                        startCol = sum_prod_start + x,
                        startRow = 1)
    
    # variables used as predictors start in column B since the first column must be the resp id
    # data always starts in row 2
    sum_prod <- paste0("=SUMPRODUCT($",
                       openxlsx::int2col(2),"2:$",openxlsx::int2col(num_pred_vars + 1),"2",
                       ",coefs!$",openxlsx::int2col(3),"$",x + 1,":$",openxlsx::int2col(num_pred_vars + 2),"$",x + 1,")",
                       "+coefs!$",openxlsx::int2col(2),"$",x + 1)
    
    openxlsx::writeFormula(algorithm_wb,
                           "input_data",
                           sum_prod,
                           startCol = sum_prod_start + x,
                           startRow = 2)
    
  })
  
  match_max <- paste0("=MATCH(MAX($",
                      openxlsx::int2col(sum_prod_start + 1),"2:$",openxlsx::int2col(sum_prod_end),"2",
                      "),$",
                      openxlsx::int2col(sum_prod_start + 1),"2:$",openxlsx::int2col(sum_prod_end),"2",
                      ",",
                      0,")")
  
  openxlsx::writeData(algorithm_wb,
                      "input_data",
                      "Predicted_Segment",
                      startCol = sum_prod_end + 1 + 1,
                      startRow = 1)
  
  openxlsx::writeFormula(algorithm_wb,
                         "input_data",
                         match_max,
                         startCol = sum_prod_end + 1 + 1,
                         startRow = 2)
  
  openxlsx::saveWorkbook(algorithm_wb,
                         "LDA Algorithm.xlsx",
                         TRUE)  
}

