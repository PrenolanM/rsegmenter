#' Returns a list containing the label, format and display width attribute information
#' for all applicable variables
#' @param df data.frame.
#' @export

store_spss_attr <- function(df){
  
  spss_label <- lapply(colnames(df), function(var){
    attr(df[[var]],"label",exact = TRUE)
  })
  
  names(spss_label) <- colnames(df)
  
  spss_format <- lapply(colnames(df), function(var){
    attr(df[[var]],"format.spss",exact = TRUE)
  })
  
  names(spss_format) <- colnames(df)
  
  spss_display <- lapply(colnames(df), function(var){
    attr(df[[var]],"display_width",exact = TRUE)
  })
  
  names(spss_display) <- colnames(df)
  
  return(list(label = spss_label,
              format = spss_format,
              display = spss_display)
  )
}