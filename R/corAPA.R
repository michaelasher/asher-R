#' Make a Correlation Matrix Suitable for an APA Publication
#'
#' This function creates a correlation matrix, given a data frame of variables,
#' and outputs it to the viewer in HTML Format to facilitate copying and pasting.
#' It uses stars to indicate significance.
#'
#' @param x a matrix containing the data
#' @param missingMethod how missing observations are handled: "pairwise" by default.
#' "complete" selects only complete cases
#' @param format returns as either a "Kable" or a data frame
#' @param descriptives adds means and standard deviations
#' @param removeTriangle remove upper or lower triangle
#' @param result the format of the output
#' @param decimals number of decimals in the correlation matrix
#' @param cutoffs a vector of 3 cutoffs values. p values less than cutoff 1 get 3 stars,
#' those less than cutoff 2 get 2 stars, and those less than cutoff 1 get one star
#' @return The correlation matrix in the desired format
#' @export
corAPA = function(x, missingMethod=c("pairwise","complete"),
                  format = c("kable", "df"),
                  descriptives = T,
  removeTriangle=c("upper", "lower"),
  decimals=2, cutoffs=c(.001, .01, .05)){

  df <- x
  format = format[1]
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
    Rnew <- as.data.frame(Rnew) %>% tibble::rownames_to_column(var = "measure")
  }

  ## remove lower triangle of correlation matrix
  if (removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew) %>% tibble::rownames_to_column(var = "measure")
  }

  ## add numbering
  Rnew <- Rnew %>% dplyr::mutate(measure = stringr::str_c(dplyr::row_number(), ". ", measure))

  ## add descriptives
  if(descriptives){
    msd <- df %>%
      tidyr::pivot_longer(dplyr::everything(), names_to = "name") %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(mean = mean(value, na.rm = T),
                sd = sd(value, na.rm = T)) %>%
      tidyr::pivot_longer(-name, names_to = "measure") %>%
      tidyr::pivot_wider(names_from = name, values_from = value) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, decimals))) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), as.character))

    Rnew <- dplyr::bind_rows(Rnew, msd)
  }

  ## return the correlation matrix
  if(format == "kable"){
    knitr::kable(Rnew, format = 'html', booktabs = T) %>% kableExtra::kable_styling()
  }

  else if(format == "df"){
    return(Rnew)
  }

}
