#' Creates a 0,1 coded variable. The two highest values become a 1 and the rest becomes a 0
#'
#' @param df must be a data.frame
#' 
#' @param vars must be a string of variable names to operate on.
#' These variables must be numeric
#' 
#' @examples
#' mydf <- data.frame(col1=c(1,2,3),col2=c(1,3,2),col3=c(1,2,1))
#' top_box(df = mydf, vars = c("col1","col2","col3"))

#' @export


top_two_box <- function(df,vars,impute_type="none"){

  # ensuring df is provided
  if (missing(df)){
    stop("df is compulsory")
  }
  
  # ensuring vars is provided
  if (missing(vars)){
    stop("vars is compulsory")
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
  
  df <- df[vars]
  
  # check that we don't have variables with all NA's
  if (check_all_na(df)){
    stop("at least one of the input variables contain all NA's")
  }
  
  # check that all variables are numeric
  if (check_all_numeric(df)){
    stop("at least one of the input variables is not numeric")
  }
  
  if (sum(is.na(df))){
    
    warning("there are still missing values in your data")
    
  }

  return_df <- as.data.frame(lapply(df,
                                    function(x){

                                      max_val <- max(x)

                                      second_high_val <- sort(unique(x),decreasing = TRUE)[2]

                                      return(ifelse(x==max_val | x == second_high_val,1,0))
                                      }
                                    )
                             )

  # return_df <- as.data.frame(lapply(return_df,
  #                                   function(x){
  #                                     as.factor(x)
  #                                     }
  #                                   )
  #                            )

  colnames(return_df) <- paste0(colnames(df),"_top_2_box")

  return(return_df)

}
