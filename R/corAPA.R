#' Make a Correlation Matrix Suitable for an APA Publication
#'
#' This function creates a correlation matrix, given a data frame of variables,
#' and outputs it to the viewer in HTML Format to facilitate copyin and pasting. 
#' It uses stars to indicate significance.
#'
#' @param x a matrix containing the data
#' @param missingMethod how missing observations are handled: "pairwise" by default.
#' "complete" selects only complete cases
#' @param removeTriangle remove upper or lower triangle
#' @param result the format of the output
#' @param decimals number of decimals in the correlation matrix
#' @param cutoffs a vector of 3 cutoffs values. p values less than cutoff 1 get 3 stars, 
#' those less than cutoff 2 get 2 stars, and those less than cutoff 1 get one star.
#' @return The correlation matrix is printed to the viewer using the "Kable" package
#' @export

corAPA = function(x, missingMethod=c("pairwise","complete"), 
  removeTriangle=c("upper", "lower"),
  decimals=2, cutoffs=c(.001, .01, .05)){
  
  #Compute correlation matrix
  x <- as.matrix(x)
  correlation_matrix<-psych::corr.test(x, use = missingMethod[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$p # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < cutoffs[1], "***", ifelse(p < cutoffs[2], "** ", ifelse(p < cutoffs[3], "*  ", ifelse(p < .1, "    ", "    "))))
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < cutoffs[1], "***", ifelse(p < cutoffs[2], "** ", ifelse(p < .05, "*  ", ifelse(p < .1, "    ", "    "))))
  
  ## trunctuate the correlation matrix to number of decimals
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), decimals))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## return the correlation matrix
 knitr::kable(Rnew, format = 'html', booktabs = T) %>% kableExtra::kable_styling()
} 
