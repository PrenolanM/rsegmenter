#' Create raw count profile tables of all specified variables by banner variable.
#' @param df data.frame of input variables
#' @param factor_vars variables that are to be treated as factors in the table output.
#' Will produce counts for each value for each variable. If weight_var is provided, will 
#' produce weighted counts.
#' @param numeric_vars variables that are to be treated as numeric in the table output.
#' Will produce means for each variable. if weight_var is provided, will produce weighted means.
#' @param weight_var if not NULL, a vector that contains weights for each observation. The NULL
#' case is equivalent to all cases being weighted 1.
#' @param banner_var name of the variable to appear along the top of the table.
#'
#' @importFrom rlang .data
#' 
profile_table_raw <- function(df,
                              factor_vars = NULL,
                              numeric_vars = NULL,
                              weight_var = NULL,
                              banner_var){

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
      dplyr::group_by(.data[["Variable_Name"]],.data[["Value_Code"]],.data[[banner_var]]) %>%
      dplyr::summarise(mycount = sum(.data[[weight_var]])) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[banner_var]]) %>%
      tidyr::pivot_wider(names_from = .data[[banner_var]],
                         names_prefix = "Cluster_",
                         values_from = .data[["mycount"]])

    temp_fac[is.na(temp_fac)] <- 0

    temp_fac[["Total"]] <- rowSums(temp_fac[,3:ncol(temp_fac)])

    }

  if (!is.null(numeric_vars)){

    # numeric variables will have weighted means
    temp_num <- df %>%
      dplyr::group_by(.data[[banner_var]]) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(numeric_vars),
                       ~ weighted.mean(.x,.data[[weight_var]])
                       )
                ) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[banner_var]]) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(numeric_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>% 
      tidyr::pivot_wider(names_from = .data[[banner_var]],
                         names_prefix = "Cluster_",
                         values_from = .data[["Value_Code"]])

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

#' Create col % profile tables of all specified variables by banner variable.
#' @param df data.frame of input variables
#' @param factor_vars variables that are to be treated as factors in the table output.
#' Will produce counts for each value for each variable. If weight_var is provided, will 
#' produce weighted counts.
#' @param numeric_vars variables that are to be treated as numeric in the table output.
#' Will produce means for each variable. if weight_var is provided, will produce weighted means.
#' @param weight_var if not NULL, a vector that contains weights for each observation. The NULL
#' case is equivalent to all cases being weighted 1.
#' @param banner_var name of the variable to appear along the top of the table.

profile_table_col_perc <- function(df,
                                   factor_vars = NULL,
                                   numeric_vars = NULL,
                                   weight_var = NULL,
                                   banner_var){

  # if weight is NULL, create a variable of all 1's and use this
  if (is.null(weight_var)){
    df[["weight_var"]] <- rep(1,nrow(df))
    weight_var <- "weight_var"
  }
  
  # get base sizes of each segment
  segment_size <- df %>%
    dplyr::select(.data[[banner_var]],.data[[weight_var]]) %>%
    dplyr::group_by(.data[[banner_var]]) %>%
    dplyr::summarise(mycount = sum(.data[[weight_var]])) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data[[banner_var]])
  
  if (!is.null(factor_vars)){

    # factor variables will have weighted counts
    temp_fac <- df %>%
      tidyr::pivot_longer(cols = dplyr::all_of(factor_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>%
      dplyr::group_by(.data[["Variable_Name"]],.data[["Value_Code"]],.data[[banner_var]]) %>%
      dplyr::summarise(mycount = sum(.data[[weight_var]])) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[banner_var]]) %>%
      tidyr::pivot_wider(names_from = .data[[banner_var]],
                         names_prefix = "Cluster_",
                         values_from = .data[["mycount"]])

    temp_fac[is.na(temp_fac)] <- 0

    temp_fac_col_perc <- temp_fac

    temp_fac_col_perc[,3:ncol(temp_fac_col_perc)] <- 
      t(apply(temp_fac[,3:ncol(temp_fac)],1,
              function(x){
                as.numeric(x)/as.numeric(segment_size$mycount)
                })
        )

    temp_fac_col_perc[["Total"]] <- rowSums(temp_fac[,3:ncol(temp_fac)])/sum(segment_size$mycount)

    temp_fac_col_perc[,3:ncol(temp_fac_col_perc)] <- round(temp_fac_col_perc[,3:ncol(temp_fac_col_perc)]*100,2)
  }

  if (!is.null(numeric_vars)){
    
    # numeric variables will have weighted means
    temp_num <- df %>%
      dplyr::group_by(.data[[banner_var]]) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(numeric_vars),
                       ~ weighted.mean(.x,.data[[weight_var]])
                       )
                ) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[banner_var]]) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(numeric_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>% 
      tidyr::pivot_wider(names_from = .data[[banner_var]],
                         names_prefix = "Cluster_",
                         values_from = .data[["Value_Code"]])
    
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

#' Create row % profile tables of all specified variables by banner variable.
#' @param df data.frame of input variables
#' @param factor_vars variables that are to be treated as factors in the table output.
#' Will produce counts for each value for each variable. If weight_var is provided, will 
#' produce weighted counts.
#' @param numeric_vars variables that are to be treated as numeric in the table output.
#' Will produce means for each variable. if weight_var is provided, will produce weighted means.
#' @param weight_var if not NULL, a vector that contains weights for each observation. The NULL
#' case is equivalent to all cases being weighted 1.
#' @param banner_var name of the variable to appear along the top of the table.

profile_table_row_perc <- function(df,
                                   factor_vars = NULL,
                                   numeric_vars = NULL,
                                   weight_var = NULL,
                                   banner_var){

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
      dplyr::group_by(.data[["Variable_Name"]],.data[["Value_Code"]],.data[[banner_var]]) %>%
      dplyr::summarise(mycount = sum(.data[[weight_var]])) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[banner_var]]) %>%
      tidyr::pivot_wider(names_from = .data[[banner_var]],
                         names_prefix = "Cluster_",
                         values_from = .data[["mycount"]])

    temp_fac[is.na(temp_fac)] <- 0

    temp_fac_row_perc <- temp_fac

    temp_fac_row_perc[,3:ncol(temp_fac_row_perc)] <- 
      t(apply(temp_fac,1,
              function(x){
                as.numeric(x[3:ncol(temp_fac)])/sum(as.numeric(x[3:ncol(temp_fac)]))
                })
        )
    
    temp_fac_row_perc[,3:ncol(temp_fac_row_perc)] <- round(temp_fac_row_perc[,3:ncol(temp_fac_row_perc)]*100,2)
  }

  if (!is.null(numeric_vars)){
    
    # numeric variables will have weighted means
    temp_num <- df %>%
      dplyr::group_by(.data[[banner_var]]) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(numeric_vars),
                       ~ weighted.mean(.x,.data[[weight_var]])
                       )
                ) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[banner_var]]) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(numeric_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>% 
      tidyr::pivot_wider(names_from = .data[[banner_var]],
                         names_prefix = "Cluster_",
                         values_from = .data[["Value_Code"]])
    
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

#' Create col % index profile tables of all specified variables by banner variable.
#' @param df data.frame of input variables
#' @param factor_vars variables that are to be treated as factors in the table output.
#' Will produce counts for each value for each variable. If weight_var is provided, will 
#' produce weighted counts.
#' @param numeric_vars variables that are to be treated as numeric in the table output.
#' Will produce means for each variable. if weight_var is provided, will produce weighted means.
#' @param weight_var if not NULL, a vector that contains weights for each observation. The NULL
#' case is equivalent to all cases being weighted 1.
#' @param banner_var name of the variable to appear along the top of the table.
#' 
profile_table_col_index <- function(df,
                                    factor_vars = NULL,
                                    numeric_vars = NULL,
                                    weight_var = NULL,
                                    banner_var){

  # if weight is NULL, create a variable of all 1's and use this
  if (is.null(weight_var)){
    df[["weight_var"]] <- rep(1,nrow(df))
    weight_var <- "weight_var"
  }
  
  # get base sizes of each segment
  segment_size <- df %>%
    dplyr::select(.data[[banner_var]],.data[[weight_var]]) %>%
    dplyr::group_by(.data[[banner_var]]) %>%
    dplyr::summarise(mycount = sum(.data[[weight_var]])) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data[[banner_var]])

  if (!is.null(factor_vars)){

    # factor variables will have weighted counts
    temp_fac <- df %>%
      tidyr::pivot_longer(cols = dplyr::all_of(factor_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>%
      dplyr::group_by(.data[["Variable_Name"]],.data[["Value_Code"]],.data[[banner_var]]) %>%
      dplyr::summarise(mycount = sum(.data[[weight_var]])) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[banner_var]]) %>%
      tidyr::pivot_wider(names_from = .data[[banner_var]],
                         names_prefix = "Cluster_",
                         values_from = .data[["mycount"]])

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
      round(100 * (temp_fac_col_perc[,3:ncol(temp_fac_col_perc)]/Total),2)

  }

  if (!is.null(numeric_vars)){
    
    # numeric variables will have weighted means
    temp_num <- df %>%
      dplyr::group_by(.data[[banner_var]]) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(numeric_vars),
                       ~ weighted.mean(.x,.data[[weight_var]])
                       )
                ) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[banner_var]]) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(numeric_vars),
                          names_to = "Variable_Name",
                          values_to = "Value_Code") %>% 
      tidyr::pivot_wider(names_from = .data[[banner_var]],
                         names_prefix = "Cluster_",
                         values_from = .data[["Value_Code"]])
    
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

#' Create total summary tables.
#' @param df data.frame of input variables
#' @param weight_var if not NULL, a vector that contains weights for each observation. The NULL
#' case is equivalent to all cases being weighted 1.
#' @param banner_var name of the variable to appear along the top of the table.
#' 
profile_table_totals <- function(df,
                                 weight_var = NULL,
                                 banner_var){
  
  # if weight is NULL, create a variable of all 1's and use this
  if (is.null(weight_var)){
    df[["weight_var"]] <- rep(1,nrow(df))
    weight_var <- "weight_var"
  }
  
  # get base sizes of each segment
  segment_size <- df %>%
    dplyr::select(.data[[banner_var]],.data[[weight_var]]) %>%
    dplyr::group_by(.data[[banner_var]]) %>%
    dplyr::summarise(mycount = sum(.data[[weight_var]])) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data[[banner_var]])
  
  t(segment_size)
}

#' Creates a list of data.frame elements. Each data.frame consists of 4 sets of cross tables. 
#' These are raw count, col %, row % and col % index tables. There will be as many list elements as there are banner_vars.
#' @param df data.frame of input variables
#' @param factor_vars variables that are to be treated as factors in the table output.
#' Will produce counts for each value for each variable. If weight_var is provided, will 
#' produce weighted counts.
#' @param numeric_vars variables that are to be treated as numeric in the table output.
#' Will produce means for each variable. if weight_var is provided, will produce weighted means.
#' @param weight_var if not NULL, a vector that contains weights for each observation. The NULL
#' case is equivalent to all cases being weighted 1.
#' @param banner_vars name of the variables to appear along the top of the table. The number of cross
#' tables produced is equal to the number of banner variables.
#' @param table_labels datamap with variable names and labels. See rsegmenter::extract_datamap
#' 
#' @examples
#' df <- rsegmenter::test_seg_unlabelled
#' 
#' table_labels <- rsegmenter::extract_datamap(rsegmenter::test_seg_labelled)
#' 
#' profile_table(df, factor_vars = c("demog1","demog2"), numeric_vars = NULL,
#' weight_var = "weight", banner_vars = "seg1", table_labels = table_labels)
#' 
#' 
profile_table <- function(df,
                          factor_vars = NULL,
                          numeric_vars = NULL,
                          weight_var = NULL,
                          banner_vars,
                          table_labels){

  banner_vars <- as.list(banner_vars)
  
  banner_vars_2 <- purrr::map(banner_vars, function(var) {

    df1 <- profile_table_raw(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = weight_var,var)
    df2 <- profile_table_col_perc(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = weight_var,var)
    df3 <- profile_table_row_perc(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = weight_var,var)
    df4 <- profile_table_col_index(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = weight_var,var)


    df1 <- df1 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Value_Code")) %>%
      dplyr::select(.data[["Variable_Name"]],.data[["Value_Label"]],dplyr::everything(),-.data[["Variable_Label"]],-.data[["Value_Code"]]) %>%
      dplyr::arrange(.data[["Variable_Order"]]) %>%
      dplyr::select(-.data[["Variable_Order"]])

    df2 <- df2 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Value_Code")) %>%
      dplyr::select(.data[["Variable_Name"]],.data[["Value_Label"]],dplyr::everything(),-.data[["Variable_Label"]],-.data[["Value_Code"]]) %>%
      dplyr::arrange(.data[["Variable_Order"]]) %>%
      dplyr::select(-.data[["Variable_Order"]])

    df3 <- df3 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Value_Code")) %>%
      dplyr::select(.data[["Variable_Name"]],.data[["Value_Label"]],dplyr::everything(),-.data[["Variable_Label"]],-.data[["Value_Code"]]) %>%
      dplyr::arrange(.data[["Variable_Order"]]) %>%
      dplyr::select(-.data[["Variable_Order"]])

    df4 <- df4 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Value_Code")) %>%
      dplyr::select(.data[["Variable_Name"]],.data[["Value_Label"]],dplyr::everything(),-.data[["Variable_Label"]],-.data[["Value_Code"]]) %>%
      dplyr::arrange(.data[["Variable_Order"]]) %>%
      dplyr::select(-.data[["Variable_Order"]])

    df_final <- dplyr::bind_cols(df1,"",df2,"",df3,"",df4)

    colnames(df_final) <- c(colnames(df1),"empty",colnames(df2),"empty",colnames(df3),"empty",colnames(df4))

    return(df_final)
  })

  return(banner_vars_2)
}

#' Creates a list of data.frame elements. Each data.frame consists of 4 sets of cross tables. 
#' These are raw count, col %, row % and col % index tables. There will be as many list elements as there are banner_vars.
#' @param df data.frame of input variables
#' @param factor_vars variables that are to be treated as factors in the table output.
#' Will produce counts for each value for each variable. If weight_var is provided, will 
#' produce weighted counts.
#' @param numeric_vars variables that are to be treated as numeric in the table output.
#' Will produce means for each variable. if weight_var is provided, will produce weighted means.
#' @param weight_var if not NULL, a vector that contains weights for each observation. The NULL
#' case is equivalent to all cases being weighted 1.
#' @param banner_vars name of the variables to appear along the top of the table. The number of cross
#' tables produced is equal to the number of banner variables.
#' @param table_labels datamap with variable names and labels. See rsegmenter::extract_datamap
#' 
#' @examples
#' df <- rsegmenter::test_seg_unlabelled
#' 
#' table_labels <- rsegmenter::extract_datamap(rsegmenter::test_seg_labelled)
#' 
#' profile_table_2(df, factor_vars = c("demog1","demog2"), numeric_vars = NULL,
#' weight_var = "weight", banner_vars = "seg1", table_labels = table_labels)
#' 
#' @export
#' 
profile_table_2 <- function(df,
                            factor_vars = NULL,
                            numeric_vars = NULL,
                            weight_var = NULL,
                            banner_vars,
                            table_labels){
  
  banner_vars <- as.list(banner_vars)
  
  banner_vars_2 <- purrr::map(banner_vars, function(var) {
    
    df1 <- profile_table_raw(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = weight_var,var)
    df2 <- profile_table_col_perc(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = weight_var,var)
    df3 <- profile_table_row_perc(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = weight_var,var)
    df4 <- profile_table_col_index(df,factor_vars = factor_vars,numeric_vars = numeric_vars,weight_var = weight_var,var)
    
    total <- profile_table_totals(df,weight_var = weight_var, var) 
      
    df1 <- df1 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Value_Code")) %>%
      dplyr::select(.data[["Variable_Name"]],.data[["Value_Label"]],dplyr::everything(),-.data[["Variable_Label"]],-.data[["Value_Code"]]) %>%
      dplyr::arrange(.data[["Variable_Order"]]) %>%
      dplyr::select(-.data[["Variable_Order"]])
    
    df2 <- df2 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Value_Code")) %>%
      dplyr::select(.data[["Variable_Name"]],.data[["Value_Label"]],dplyr::everything(),-.data[["Variable_Label"]],-.data[["Value_Code"]]) %>%
      dplyr::arrange(.data[["Variable_Order"]]) %>%
      dplyr::select(-.data[["Variable_Order"]])
    
    df3 <- df3 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Value_Code")) %>%
      dplyr::select(.data[["Variable_Name"]],.data[["Value_Label"]],dplyr::everything(),-.data[["Variable_Label"]],-.data[["Value_Code"]]) %>%
      dplyr::arrange(.data[["Variable_Order"]]) %>%
      dplyr::select(-.data[["Variable_Order"]])
    
    df4 <- df4 %>%
      dplyr::left_join(table_labels,by=c("Variable_Name","Value_Code")) %>%
      dplyr::select(.data[["Variable_Name"]],.data[["Value_Label"]],dplyr::everything(),-.data[["Variable_Label"]],-.data[["Value_Code"]]) %>%
      dplyr::arrange(.data[["Variable_Order"]]) %>%
      dplyr::select(-.data[["Variable_Order"]])
    
    return(list(raw_tab = df1,
                col_perc_tab = df2,
                row_perc_tab = df3,
                col_index_tab = df4,
                total_tab = total))
  })
  
  return(banner_vars_2)
}

#' Exports cross tables to .xlsx format. Each table will be output on a separate worksheet.
#' @param prof_table output from rsegmenter::profile_table.
#' @param min_index numeric value for highlighting under-indexing.
#' @param max_index numeric value for highlighting over-indexing.
#' @param filename file name to use when saving.
#' @examples
#' df <- rsegmenter::test_seg_unlabelled
#' 
#' table_labels <- rsegmenter::extract_datamap(rsegmenter::test_seg_labelled)
#' 
#' prof_table <- profile_table(df, factor_vars = c("demog1","demog2"), numeric_vars = NULL,
#' weight_var = "weight", banner_vars = "seg1", table_labels = table_labels)
#' 
#' export_profile_tables(prof_table = prof_table, filename="profile_table.xlsx")
#' 
#' @export
#' 
export_profile_tables <- function(prof_table,
                                  min_index=80,
                                  max_index=120,
                                  filename){

  num_seg_sols <- length(prof_table)

  wb <- openxlsx::createWorkbook()

  sheet_names <- (lapply(seq(1,num_seg_sols),function(x){

    openxlsx::addWorksheet(wb, paste0("Solution_",x))
    
    # table 1 starts in column 1
    tab1_start <- 1
    
    tab2_start <- ncol(prof_table[[x]][["raw_tab"]]) + 2
    
    tab3_start <- ncol(prof_table[[x]][["raw_tab"]]) + ncol(prof_table[[x]][["col_perc_tab"]]) + 3
    
    tab4_start <- ncol(prof_table[[x]][["raw_tab"]]) + ncol(prof_table[[x]][["col_perc_tab"]]) + 
      ncol(prof_table[[x]][["row_perc_tab"]]) + 4
    
    startrow <- 5
    
    # table 1
    openxlsx::writeData(wb, 
                        paste0("Solution_",x),
                        prof_table[[x]][["raw_tab"]],
                        colNames = TRUE,
                        startCol = tab1_start,
                        startRow = startrow)
    
    openxlsx::writeData(wb, 
                        paste0("Solution_",x),
                        t(prof_table[[x]][["total_tab"]][2,]),
                        colNames = FALSE,
                        startCol = tab1_start+2,
                        startRow = 3)
    
    # table 2 starts in column ncol(table1) + 2
    openxlsx::writeData(wb, 
                        paste0("Solution_",x),
                        prof_table[[x]][["col_perc_tab"]],
                        colNames = TRUE,
                        startCol = tab2_start,
                        startRow = startrow)
    
    openxlsx::writeData(wb, 
                        paste0("Solution_",x),
                        t(prof_table[[x]][["total_tab"]][2,]/sum(prof_table[[x]][["total_tab"]][2,])),
                        colNames = FALSE,
                        startCol = tab2_start+2,
                        startRow = 3)
    
    # table 3 starts in column ncol(table1) + ncol(table2) + 3
    openxlsx::writeData(wb, 
                        paste0("Solution_",x),
                        prof_table[[x]][["row_perc_tab"]],
                        colNames = TRUE,
                        startCol = tab3_start,
                        startRow = startrow)
    
    openxlsx::writeData(wb, 
                        paste0("Solution_",x),
                        t(prof_table[[x]][["total_tab"]][2,]/sum(prof_table[[x]][["total_tab"]][2,])),
                        colNames = FALSE,
                        startCol = tab3_start+2,
                        startRow = 3)
    
    # table 4 starts in column ncol(table1) + ncol(table2) + ncol(table3) + 4
    openxlsx::writeData(wb, 
                        paste0("Solution_",x),
                        prof_table[[x]][["col_index_tab"]],
                        colNames = TRUE,
                        startCol = tab4_start,
                        startRow = startrow)
    
    openxlsx::writeData(wb, 
                        paste0("Solution_",x),
                        t(prof_table[[x]][["total_tab"]][2,]/sum(prof_table[[x]][["total_tab"]][2,])),
                        colNames = FALSE,
                        startCol = tab4_start+2,
                        startRow = 3)
    
    openxlsx::conditionalFormatting(wb,
                                    paste0("Solution_",x),
                                    cols = ((tab4_start+2):(tab4_start+ncol(prof_table[[x]][["col_index_tab"]])-1)),
                                    rows = (startrow+1):(nrow(prof_table[[x]][["col_index_tab"]])+startrow),
                                    style = openxlsx::createStyle(bgFill = c("#f589af")),
                                    rule = c(paste0("<=",min_index)),
                                    type = "expression")

    openxlsx::conditionalFormatting(wb,
                                    paste0("Solution_",x),
                                    cols = ((tab4_start+2):(tab4_start+ncol(prof_table[[x]][["col_index_tab"]])-1)),
                                    rows = (startrow+1):(nrow(prof_table[[x]][["col_index_tab"]])+startrow),
                                    style = openxlsx::createStyle(bgFill = c("#89f5b8")),
                                    rule = c(paste0(">=",max_index)),
                                    type = "expression")
    
  }))

  openxlsx::saveWorkbook(wb, filename, TRUE)
}


#' Adds user friendly labels to the profile tables
#' @param prof_table should be a dataframe of numeric variables
#' @param table_labels should be a character vector of variable names

add_table_labels <- function(prof_table,table_labels){
  return(dplyr::left_join(prof_table,
                          table_labels,
                          by=c("Variable_Name","Value_Code")))
}
