#' This function reads in data given a csv in the format that qualtrics generates,
#' with the header row, a row of information about the variables, and then the data.
#' Make sure you export the data from qualtrics removing line breaks, or this function
#' won't work properly.
#' @return a data frame.
#' @export

read_qualtrics <- function(filePath){
  dHeader = read.csv(filePath,header = T,nrows = 1,stringsAsFactors = FALSE)
  dData = read.csv(filePath, header = F, skip = 3, stringsAsFactors = FALSE)
  names(dData) = names(dHeader)
  return(dData)
}
