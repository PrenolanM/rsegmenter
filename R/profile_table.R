
#' create raw count profile tables of all specified variables by segment variable

#' @param df should be a dataframe of numeric variables
#' @param factor_vars a character vector of variable names
#' @param numeric_vars a character vector of variable names
#' @param weight_var name of weight variable
#' @param segment_var a character vector of segment variable name

profile_table_raw <- function(df,factor_vars = NULL,numeric_vars = NULL,weight_var,segment_var){

  segment_var <- dplyr::enquo(segment_var)

  weight_var <- dplyr::enquo(weight_var)

  if (!is.null(factor_vars)){

    # factor variables will have weighted counts
    temp_fac <- df %>%
      tidyr::pivot_longer(cols = all_of(factor_vars),
                          names_to = "Variable_Name",
                          values_to = "Variable_Code") %>%
      dplyr::group_by(Variable_Name,Variable_Code,!!(segment_var)) %>%
      dplyr::summarise(mycount = sum(!!(weight_var))) %>%
      ungroup() %>%
      arrange(!!(segment_var)) %>%
      tidyr::pivot_wider(names_from = !!(segment_var),
                         names_prefix = "Cluster_",
                         values_from = mycount)

    temp_fac[is.na(temp_fac)] <- 0

    temp_fac[["Total"]] <- rowSums(temp_fac[,3:ncol(temp_fac)])



    }

  if (!is.null(numeric_vars)){

    # numeric variables will have weighted means
    temp_num <- df %>%
      tidyr::pivot_longer(cols = all_of(numeric_vars),
                          names_to = "Variable_Name",
                          values_to = "Variable_Code") %>%
      dplyr::group_by(Variable_Name,Variable_Code,!!(segment_var)) %>%
      dplyr::summarise(mycount = weighted.mean(!!(weight_var))) %>%
      ungroup() %>%
      arrange(!!(segment_var)) %>%
      tidyr::pivot_wider(names_from = !!(segment_var),
                         names_prefix = "Cluster_",
                         values_from = mycount)

    temp_num[is.na(temp_num)] <- 0

    temp_num[["Total"]] <- 1



  }



  # need to add a total column for numeric variables -
  if (is.null(numeric_vars)){

    return(dplyr::bind_rows(temp_fac))

  } else if (is.null(factor_vars)){

    return(dplyr::bind_rows(temp_num))

  } else{

    return(dplyr::bind_rows(temp_fac,temp_num))

  }

}

#' create column percent profile tables of all specified variables by segment variable

#' @param df should be a dataframe of numeric variables
#' @param factor_vars should be a character vector of variable names
#' @param numeric_vars should be a character vector of variable names
#' @param segment_var should be a character vector of segment variable name

profile_table_col_perc <- function(df,factor_vars = NULL,numeric_vars = NULL,weight_var,segment_var){

  segment_var <- dplyr::enquo(segment_var)

  weight_var <- dplyr::enquo(weight_var)

  # get base sizes of each segment
  segment_size <- df %>%
    select(!!segment_var,!!weight_var) %>%
    group_by(!!segment_var) %>%
    summarise(mycount = sum(!!(weight_var))) %>%
    ungroup() %>%
    arrange(!!(segment_var))

  if (!is.null(factor_vars)){

    # factor variables will have weighted counts
    temp_fac <- df %>%
      tidyr::pivot_longer(cols = all_of(factor_vars),
                          names_to = "Variable_Name",
                          values_to = "Variable_Code") %>%
      dplyr::group_by(Variable_Name,Variable_Code,!!(segment_var)) %>%
      dplyr::summarise(mycount = sum(!!(weight_var))) %>%
      ungroup() %>%
      arrange(!!(segment_var)) %>%
      tidyr::pivot_wider(names_from = !!(segment_var),
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
      tidyr::pivot_longer(cols = all_of(numeric_vars),
                          names_to = "Variable_Name",
                          values_to = "Variable_Code") %>%
      dplyr::group_by(Variable_Name,Variable_Code,!!(segment_var)) %>%
      dplyr::summarise(mycount = weighted.mean(!!(weight_var))) %>%
      ungroup() %>%
      arrange(!!(segment_var)) %>%
      tidyr::pivot_wider(names_from = !!(segment_var),
                         names_prefix = "Cluster_",
                         values_from = mycount)

    temp_num[is.na(temp_num)] <- 0

    temp_num[["Total"]] <- 1

  }



  # need to add a total column for numeric variables -
  if (is.null(numeric_vars)){

    return(dplyr::bind_rows(temp_fac_col_perc))

  } else if (is.null(factor_vars)){

    return(dplyr::bind_rows(temp_num))

  } else{

    return(dplyr::bind_rows(temp_fac_col_perc,temp_num))

  }


}

#' create row percent profile tables of all specified variables by segment variable

#' @param df should be a dataframe of numeric variables
#' @param factor_vars should be a character vector of variable names
#' @param numeric_vars should be a character vector of variable names
#' @param segment_var should be a character vector of segment variable name

profile_table_row_perc <- function(df,factor_vars = NULL,numeric_vars = NULL,weight_var,segment_var){

  segment_var <- dplyr::enquo(segment_var)

  weight_var <- dplyr::enquo(weight_var)

  if (!is.null(factor_vars)){

    # factor variables will have weighted counts
    temp_fac <- df %>%
      tidyr::pivot_longer(cols = all_of(factor_vars),
                          names_to = "Variable_Name",
                          values_to = "Variable_Code") %>%
      dplyr::group_by(Variable_Name,Variable_Code,!!(segment_var)) %>%
      dplyr::summarise(mycount = sum(!!(weight_var))) %>%
      ungroup() %>%
      arrange(!!(segment_var)) %>%
      tidyr::pivot_wider(names_from = !!(segment_var),
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
      tidyr::pivot_longer(cols = all_of(numeric_vars),
                          names_to = "Variable_Name",
                          values_to = "Variable_Code") %>%
      dplyr::group_by(Variable_Name,Variable_Code,!!(segment_var)) %>%
      dplyr::summarise(mycount = weighted.mean(!!(weight_var))) %>%
      ungroup() %>%
      arrange(!!(segment_var)) %>%
      tidyr::pivot_wider(names_from = !!(segment_var),
                         names_prefix = "Cluster_",
                         values_from = mycount)

    temp_num[is.na(temp_num)] <- 0

    temp_num[["Total"]] <- 1

  }



  # need to add a total column for numeric variables -
  if (is.null(numeric_vars)){

    return(dplyr::bind_rows(temp_fac_row_perc))

  } else if (is.null(factor_vars)){

    return(dplyr::bind_rows(temp_num))

  } else{

    return(dplyr::bind_rows(temp_fac_row_perc,temp_num))

  }

}

#' create row percent profile tables of all specified variables by segment variable

#' @param df should be a dataframe of numeric variables
#' @param factor_vars should be a character vector of variable names
#' @param numeric_vars should be a character vector of variable names
#' @param segment_var should be a character vector of segment variable name
#'
profile_table_col_index <- function(df,factor_vars = NULL,numeric_vars = NULL,weight_var,segment_var){

  segment_var <- dplyr::enquo(segment_var)

  weight_var <- dplyr::enquo(weight_var)

  # get base sizes of each segment
  segment_size <- df %>%
    select(!!segment_var,!!weight_var) %>%
    group_by(!!segment_var) %>%
    summarise(mycount = sum(!!(weight_var))) %>%
    ungroup() %>%
    arrange(!!(segment_var))

  if (!is.null(factor_vars)){

    # factor variables will have weighted counts
    temp_fac <- df %>%
      tidyr::pivot_longer(cols = all_of(factor_vars),
                          names_to = "Variable_Name",
                          values_to = "Variable_Code") %>%
      dplyr::group_by(Variable_Name,Variable_Code,!!(segment_var)) %>%
      dplyr::summarise(mycount = sum(!!(weight_var))) %>%
      ungroup() %>%
      arrange(!!(segment_var)) %>%
      tidyr::pivot_wider(names_from = !!(segment_var),
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
      tidyr::pivot_longer(cols = all_of(numeric_vars),
                          names_to = "Variable_Name",
                          values_to = "Variable_Code") %>%
      dplyr::group_by(Variable_Name,Variable_Code,!!(segment_var)) %>%
      dplyr::summarise(mycount = weighted.mean(!!(weight_var))) %>%
      ungroup() %>%
      arrange(!!(segment_var)) %>%
      tidyr::pivot_wider(names_from = !!(segment_var),
                         names_prefix = "Cluster_",
                         values_from = mycount)

    temp_num[["Total"]] <- 1

  }



  # need to add a total column for numeric variables -
  if (is.null(numeric_vars)){

    return(dplyr::bind_rows(temp_fac_col_perc))

  } else if (is.null(factor_vars)){

    return(dplyr::bind_rows(temp_num))

  } else{

    return(dplyr::bind_rows(temp_fac_col_perc,temp_num))

  }


}

#' create profile raw count, col %, row %, col % index tables of all specified variables by segment variable
#' @export
#' @param df must be a dataframe of numeric variables
#' @param factor_vars must be a character vector of variable names
#' @param numeric_vars must be a character vector of variable names
#' @param weight_var must be a numeric vector of row weights. if data is unweighted, specify a vector of 1's with length equal to number of rows of df
#' @param ... segment variables

profile_table <- function(df,factor_vars = NULL,numeric_vars = NULL,weight_var,table_labels,...){

  weight_var <- dplyr::enquo(weight_var)

  segment_vars <- enquos(..., .named = TRUE)

  segment_vars_2 <- purrr::map(segment_vars, function(var) {

    df1 <- profile_table_raw(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = !!weight_var,!!var)
    df2 <- profile_table_col_perc(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = !!weight_var,!!var)
    df3 <- profile_table_row_perc(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = !!weight_var,!!var)
    df4 <- profile_table_col_index(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = !!weight_var,!!var)


    df1 <- df1 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Variable_Code")) %>%
      dplyr::select(Variable_Name,Variable_Label,everything(),-Variable_Code) %>%
      dplyr::arrange(Variable_Order) %>%
      dplyr::select(-Variable_Order)

    df2 <- df2 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Variable_Code")) %>%
      dplyr::select(Variable_Name,Variable_Label,everything(),-Variable_Code) %>%
      dplyr::arrange(Variable_Order) %>%
      dplyr::select(-Variable_Order)

    df3 <- df3 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Variable_Code")) %>%
      dplyr::select(Variable_Name,Variable_Label,everything(),-Variable_Code) %>%
      dplyr::arrange(Variable_Order) %>%
      dplyr::select(-Variable_Order)

    df4 <- df4 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Variable_Code")) %>%
      dplyr::select(Variable_Name,Variable_Label,everything(),-Variable_Code) %>%
      dplyr::arrange(Variable_Order) %>%
      dplyr::select(-Variable_Order)

    df_final <- bind_cols(df1,"",df2,"",df3,"",df4)

    colnames(df_final) <- c(colnames(df1),"empty",colnames(df2),"empty",colnames(df3),"empty",colnames(df4))

    return(df_final)
  })

  return(segment_vars_2)
}

#' create row percent profile tables of all specified variables by segment variable
#' @export
#' @param df should be a dataframe of numeric variables
#' @param factor_vars should be a character vector of variable names
#' @param numeric_vars should be a character vector of variable names
#' @param ... segment variables

export_profile_tables <- function(prof_table,min_index=80,max_index=120,filename){

  num_segs <- length(prof_table)

  wb <- openxlsx::createWorkbook()

  sheet_names <- (lapply(seq(1,num_segs),function(x){

    openxlsx::addWorksheet(wb, paste0("Solution_",x))

    openxlsx::writeData(wb, paste0("Solution_",x),prof_table[[x]], colNames = TRUE, startCol = 1, startRow = 1)
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
  return(dplyr::left_join(prof_table,prof_table,by=c("Variable_Name","Variable_Code")))
}
