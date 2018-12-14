#' This fucntion sets the working directory to the source file location, using
#' the RStudio API.
#' @return The working directory is printed to the console.
#' @export

setwd_toSource <- function() {
  currentPath = rstudioapi::getActiveDocumentContext()$path
  setwd(dirname(currentPath))
  rm(currentPath)
  getwd()
}
