

#' loads data from various sources
#' for now it is limited to .csv and .sav
#' uses vroom for fast loading of csv
#' uses haven for sav files
#' @export
#' @param infile should be the name of the file to be read in

read_data <- function(infile){

  # checking that a valid file name has been provided

  # if (!is.character(infile)){
  #   errorCondition("file name has to be a character")
  # }
  #
  # if (length(infile)<4){
  #   # show error message for invalid file name
  #   errorCondition("invalid file name")
  # }
  #
  # if (file_ext(infile)!="csv" | file_ext(infile)!="sav"){
  #   # show message saying only csv of sav files are allowed
  # }

  if (tools::file_ext(infile) == "csv"){

    return(readr::read_csv(infile))

  } else if (tools::file_ext(infile) == "sav"){

    return(haven::read_sav(infile))

  }
}

