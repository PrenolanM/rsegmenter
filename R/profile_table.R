
#' create raw count profile tables of all specified variables by segment variable.
#' @param df data.frame of input variables
#' @param factor_vars character vector of variable names that are to be treated as factor.
#' Factor variables will have counts shown for each level of each variable.
#' @param numeric_vars character vector of variable names that are to be treated as numeric.
#' Numeric variables will have means shown for variable.
#' @param weight_var name of the variable holding case/row weights.
#' If data is un-weighted, leave as NULL
#' @param segment_var name of the variable holding the segment variable

profile_table_raw <- function(df,
                              factor_vars = NULL,
                              numeric_vars = NULL,
                              weight_var = NULL,
                              segment_var){

  # if weight is NULL, create a variable of all 1's and use this
  if (is.null(weight_var)){
    df[["weight_var"]] <- rep(1,nrow(df))
    weight_var <- "weight_var"
  }
  
  if (!is.null(factor_vars)){

    # factor variables will have weighted counts
    temp_fac <- df %>%
      tidyr::pivot_longer(cols = dplyr::all_of(factor_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>%
      dplyr::group_by(Variable_Name,Value_Code,.data[[segment_var]]) %>%
      dplyr::summarise(mycount = sum(.data[[weight_var]])) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[segment_var]]) %>%
      tidyr::pivot_wider(names_from = .data[[segment_var]],
                         names_prefix = "Cluster_",
                         values_from = mycount)

    temp_fac[is.na(temp_fac)] <- 0

    temp_fac[["Total"]] <- rowSums(temp_fac[,3:ncol(temp_fac)])

    }

  if (!is.null(numeric_vars)){

    # numeric variables will have weighted means
    temp_num <- df %>%
      group_by(.data[[segment_var]]) %>%
      summarise(across(all_of(numeric_vars),
                       ~ weighted.mean(.x,.data[[weight_var]])
                       )
                ) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[segment_var]]) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(numeric_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>% 
      tidyr::pivot_wider(names_from = .data[[segment_var]],
                         names_prefix = "Cluster_",
                         values_from = Value_Code)

    temp_num[is.na(temp_num)] <- 0

    temp_num[["Total"]] <- 1

  }

  # need to add a total column for numeric variables -
  if (!is.null(numeric_vars) & !is.null(factor_vars)){
    
    return(dplyr::bind_rows(temp_fac,temp_num))
    
  } else if (is.null(numeric_vars) & !is.null(factor_vars)){

    return(dplyr::bind_rows(temp_fac))

  } else if (!is.null(numeric_vars) & is.null(factor_vars)){

    return(dplyr::bind_rows(temp_num))

  }

}

#' create col % profile tables of all specified variables by segment variable.
#' @param df data.frame of input variables
#' @param factor_vars character vector of variable names that are to be treated as factor.
#' Factor variables will have counts shown for each level of each variable.
#' @param numeric_vars character vector of variable names that are to be treated as numeric.
#' Numeric variables will have means shown for variable.
#' @param weight_var name of the variable holding case/row weights.
#' If data is un-weighted, leave as NULL
#' @param segment_var name of the variable holding the segment variable

profile_table_col_perc <- function(df,
                                   factor_vars = NULL,
                                   numeric_vars = NULL,
                                   weight_var,
                                   segment_var){

  # get base sizes of each segment
  segment_size <- df %>%
    select(.data[[segment_var]],.data[[weight_var]]) %>%
    group_by(.data[[segment_var]]) %>%
    summarise(mycount = sum(.data[[weight_var]])) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data[[segment_var]])
  
  if (!is.null(factor_vars)){

    # factor variables will have weighted counts
    temp_fac <- df %>%
      tidyr::pivot_longer(cols = dplyr::all_of(factor_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>%
      dplyr::group_by(Variable_Name,Value_Code,.data[[segment_var]]) %>%
      dplyr::summarise(mycount = sum(.data[[weight_var]])) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[segment_var]]) %>%
      tidyr::pivot_wider(names_from = .data[[segment_var]],
                         names_prefix = "Cluster_",
                         values_from = mycount)

    temp_fac[is.na(temp_fac)] <- 0

    temp_fac_col_perc <- temp_fac

    temp_fac_col_perc[,3:ncol(temp_fac_col_perc)] <- t(apply(temp_fac[,3:ncol(temp_fac)],1,
                                                             function(x){
                                                               as.numeric(x)/as.numeric(segment_size$mycount)
                                                               }
                                                             )
                                                       )

    temp_fac_col_perc[["Total"]] <- rowSums(temp_fac[,3:ncol(temp_fac)])/sum(segment_size$mycount)

  }

  if (!is.null(numeric_vars)){
    
    # numeric variables will have weighted means
    temp_num <- df %>%
      group_by(.data[[segment_var]]) %>%
      summarise(across(all_of(numeric_vars),
                       ~ weighted.mean(.x,.data[[weight_var]])
                       )
                ) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[segment_var]]) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(numeric_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>% 
      tidyr::pivot_wider(names_from = .data[[segment_var]],
                         names_prefix = "Cluster_",
                         values_from = Value_Code)
    
    temp_num[is.na(temp_num)] <- 0
    
    temp_num[["Total"]] <- 1
    
  }

  # need to add a total column for numeric variables -
  if (!is.null(numeric_vars) & !is.null(factor_vars)){
    
    return(dplyr::bind_rows(temp_fac_col_perc,temp_num))
    
  } else if (is.null(numeric_vars) & !is.null(factor_vars)){
    
    return(dplyr::bind_rows(temp_fac_col_perc))
    
  } else if (!is.null(numeric_vars) & is.null(factor_vars)){
    
    return(dplyr::bind_rows(temp_num))
    
  }

}

#' create row % profile tables of all specified variables by segment variable.
#' @param df data.frame of input variables
#' @param factor_vars character vector of variable names that are to be treated as factor.
#' Factor variables will have counts shown for each level of each variable.
#' @param numeric_vars character vector of variable names that are to be treated as numeric.
#' Numeric variables will have means shown for variable.
#' @param weight_var name of the variable holding case/row weights.
#' If data is un-weighted, leave as NULL
#' @param segment_var name of the variable holding the segment variable

profile_table_row_perc <- function(df,
                                   factor_vars = NULL,
                                   numeric_vars = NULL,
                                   weight_var,
                                   segment_var){

  if (!is.null(factor_vars)){

    # factor variables will have weighted counts
    temp_fac <- df %>%
      tidyr::pivot_longer(cols = dplyr::all_of(factor_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>%
      dplyr::group_by(Variable_Name,Value_Code,.data[[segment_var]]) %>%
      dplyr::summarise(mycount = sum(.data[[weight_var]])) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[segment_var]]) %>%
      tidyr::pivot_wider(names_from = .data[[segment_var]],
                         names_prefix = "Cluster_",
                         values_from = mycount)

    temp_fac[is.na(temp_fac)] <- 0

    temp_fac_row_perc <- temp_fac

    temp_fac_row_perc[,3:ncol(temp_fac_row_perc)] <- t(apply(temp_fac,1,
                                                             function(x){
                                                               as.numeric(x[3:ncol(temp_fac)])/sum(as.numeric(x[3:ncol(temp_fac)]))
                                                               }
                                                             )
                                                       )
  }

  if (!is.null(numeric_vars)){
    
    # numeric variables will have weighted means
    temp_num <- df %>%
      group_by(.data[[segment_var]]) %>%
      summarise(across(all_of(numeric_vars),
                       ~ weighted.mean(.x,.data[[weight_var]])
                       )
                ) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[segment_var]]) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(numeric_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>% 
      tidyr::pivot_wider(names_from = .data[[segment_var]],
                         names_prefix = "Cluster_",
                         values_from = Value_Code)
    
    temp_num[is.na(temp_num)] <- 0
    
    #temp_num[["Total"]] <- 1
    
  }

  # need to add a total column for numeric variables -
  if (!is.null(numeric_vars) & !is.null(factor_vars)){
    
    return(dplyr::bind_rows(temp_fac_row_perc,temp_num))
    
  } else if (is.null(numeric_vars) & !is.null(factor_vars)){
    
    return(dplyr::bind_rows(temp_fac_row_perc))
    
  } else if (!is.null(numeric_vars) & is.null(factor_vars)){
    
    return(dplyr::bind_rows(temp_num))
    
  }

}

#' create col % index profile tables of all specified variables by segment variable.
#' @param df data.frame of input variables
#' @param factor_vars character vector of variable names that are to be treated as factor.
#' Factor variables will have counts shown for each level of each variable.
#' @param numeric_vars character vector of variable names that are to be treated as numeric.
#' Numeric variables will have means shown for variable.
#' @param weight_var name of the variable holding case/row weights.
#' If data is un-weighted, leave as NULL
#' @param segment_var name of the variable holding the segment variable
#' 
profile_table_col_index <- function(df,
                                    factor_vars = NULL,
                                    numeric_vars = NULL,
                                    weight_var,
                                    segment_var){

  # get base sizes of each segment
  segment_size <- df %>%
    select(.data[[segment_var]],.data[[weight_var]]) %>%
    group_by(.data[[segment_var]]) %>%
    summarise(mycount = sum(.data[[weight_var]])) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data[[segment_var]])

  if (!is.null(factor_vars)){

    # factor variables will have weighted counts
    temp_fac <- df %>%
      tidyr::pivot_longer(cols = dplyr::all_of(factor_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>%
      dplyr::group_by(Variable_Name,Value_Code,.data[[segment_var]]) %>%
      dplyr::summarise(mycount = sum(.data[[weight_var]])) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[segment_var]]) %>%
      tidyr::pivot_wider(names_from = .data[[segment_var]],
                         names_prefix = "Cluster_",
                         values_from = mycount)

    temp_fac[is.na(temp_fac)] <- 0

    temp_fac_col_perc <- temp_fac

    temp_fac_col_perc[,3:ncol(temp_fac_col_perc)] <- t(apply(temp_fac[,3:ncol(temp_fac)],1,
                                                             function(x){
                                                               as.numeric(x)/as.numeric(segment_size$mycount)
                                                             }
    )
    )

    Total <- rowSums(temp_fac[,3:ncol(temp_fac)])/sum(segment_size$mycount)

    temp_fac_col_perc[,3:ncol(temp_fac_col_perc)] <-
      100 * (temp_fac_col_perc[,3:ncol(temp_fac_col_perc)]/Total)

  }

  if (!is.null(numeric_vars)){
    
    # numeric variables will have weighted means
    temp_num <- df %>%
      group_by(.data[[segment_var]]) %>%
      summarise(across(all_of(numeric_vars),
                       ~ weighted.mean(.x,.data[[weight_var]])
                       )
                ) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[segment_var]]) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(numeric_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>% 
      tidyr::pivot_wider(names_from = .data[[segment_var]],
                         names_prefix = "Cluster_",
                         values_from = Value_Code)
    
    temp_num[is.na(temp_num)] <- 0
    
    #temp_num[["Total"]] <- 1
    
  }

  # need to add a total column for numeric variables -
  if (!is.null(numeric_vars) & !is.null(factor_vars)){
    
    return(dplyr::bind_rows(temp_fac_col_perc,temp_num))
    
  } else if (is.null(numeric_vars) & !is.null(factor_vars)){
    
    return(dplyr::bind_rows(temp_fac_col_perc))
    
  } else if (!is.null(numeric_vars) & is.null(factor_vars)){
    
    return(dplyr::bind_rows(temp_num))
    
  }

}

#' Creates a data.frame of 4 sets of cross tables. These are raw count, col %, row % and col % index tables
#' @param df data.frame of input variables
#' @param factor_vars character vector of variable names that are to be treated as factor.
#' Factor variables will have counts shown for each level of each variable.
#' @param numeric_vars character vector of variable names that are to be treated as numeric.
#' Numeric variables will have means shown for variable.
#' @param weight_var name of the variable holding case/row weights.
#' If data is un-weighted, specify a vector of 1's with length equal to number of rows of df
#' @param segment_vars character vector segment variables to profile by
#' @export
#' 
profile_table <- function(df,
                          factor_vars = NULL,
                          numeric_vars = NULL,
                          weight_var,
                          segment_vars,
                          table_labels){

  segment_vars <- as.list(segment_vars)
  
  segment_vars_2 <- purrr::map(segment_vars, function(var) {

    df1 <- profile_table_raw(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = weight_var,var)
    df2 <- profile_table_col_perc(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = weight_var,var)
    df3 <- profile_table_row_perc(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = weight_var,var)
    df4 <- profile_table_col_index(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = weight_var,var)


    df1 <- df1 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Value_Code")) %>%
      dplyr::select(Variable_Name,Value_Label,everything(),-Variable_Label,-Value_Code) %>%
      dplyr::arrange(Variable_Order) %>%
      dplyr::select(-Variable_Order)

    df2 <- df2 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Value_Code")) %>%
      dplyr::select(Variable_Name,Value_Label,everything(),-Variable_Label,-Value_Code) %>%
      dplyr::arrange(Variable_Order) %>%
      dplyr::select(-Variable_Order)

    df3 <- df3 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Value_Code")) %>%
      dplyr::select(Variable_Name,Value_Label,everything(),-Variable_Label,-Value_Code) %>%
      dplyr::arrange(Variable_Order) %>%
      dplyr::select(-Variable_Order)

    df4 <- df4 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Value_Code")) %>%
      dplyr::select(Variable_Name,Value_Label,everything(),-Variable_Label,-Value_Code) %>%
      dplyr::arrange(Variable_Order) %>%
      dplyr::select(-Variable_Order)

    df_final <- bind_cols(df1,"",df2,"",df3,"",df4)

    colnames(df_final) <- c(colnames(df1),"empty",colnames(df2),"empty",colnames(df3),"empty",colnames(df4))

    return(df_final)
  })

  return(segment_vars_2)
}

#' Exports cross tables to .xlsx format.
#' @param prof_table should be the output from rsegmenter::profile_table
#' @param min_index numeric value for highlighting under-indexing
#' @param max_index numeric value for highlighting over-indexing
#' @param filename character string naming a file
#' @examples 
#' df <- data.frame(var1=c(1,2,3),var2=c(2,1,3),var3=c(3,2,1))
#' segments <- factor_segmentation(df,c(2:3))
#' profile_table <- profile_table(df,factor_vars = c("var1","var2","var3"),numeric_vars = NULL,segment_vars,table_labels)
#' export_profile_tables(profile_table,min_index=80,max_index=120,"profile_tables.xlsx")
#' @export
#' 
export_profile_tables <- function(prof_table,
                                  min_index=80,
                                  max_index=120,
                                  filename){

  num_segs <- length(prof_table)

  wb <- openxlsx::createWorkbook()

  sheet_names <- (lapply(seq(1,num_segs),function(x){

    openxlsx::addWorksheet(wb, paste0("Solution_",x))

    openxlsx::writeData(wb, 
                        paste0("Solution_",x),
                        prof_table[[x]],
                        colNames = TRUE,
                        startCol = 1,
                        startRow = 1)
    # openxlsx::conditionalFormatting(wb,
    #                                 paste0("Solution_",x),
    #                                 cols = (ncol(prof_table[[x]])-1):ncol(prof_table[[x]]),
    #                                 rows = 2:(nrow(prof_table[[x]])+1),
    #                                 style = openxlsx::createStyle(bgFill = c("red")),
    #                                 rule = c(paste0("<=",min_index)),
    #                                 type = "expression")
    #
    # openxlsx::conditionalFormatting(wb,
    #                                 paste0("Solution_",x),
    #                                 cols = (ncol(prof_table[[x]])-1):ncol(prof_table[[x]]),
    #                                 rows = 2:(nrow(prof_table[[x]])+1),
    #                                 style = openxlsx::createStyle(bgFill = c("green")),
    #                                 rule = c(paste0(">=",max_index)),
    #                                 type = "expression")
  }))

  openxlsx::saveWorkbook(wb, filename, TRUE)
}


#' create row percent profile tables of all specified variables by segment variable

#' @param df should be a dataframe of numeric variables
#' @param factor_vars should be a character vector of variable names
#' @param numeric_vars should be a character vector of variable names
#' @param ... segment variables

add_table_labels <- function(prof_table,table_labels){
  return(dplyr::left_join(prof_table,
                          prof_table,
                          by=c("Variable_Name","Value_Code")))
}
