#' Summarize the output from multiple regression models with imputed data
#'
#' This function creates a summary table for a linear model with imputed data, and outputs it to the
#' viewer in HTML Format to facilitate copying and pasting by default. By default it outputs
#' standardized betas for regression coefficients, and it can accept
#' up to 8 models as arguments, which makes it ideal for displaying models with multiple
#' steps.
#'
#' @importFrom magrittr "%>%"
#' @param ... up to 8 linear models, produced by lm
#' @param Stars adds significance stars for the last model, if requested
#' @return The regression table
#' @export
multiSummaryImp = function(..., Stars = T){

  options(knitr.kable.NA = '')

  elipsis = list(...)
  l = length(elipsis)
  c = summary(mice::pool(elipsis[[1]])) %>% data.frame() %>% tibble::rownames_to_column() %>% data.frame()
  c = select(c, -starts_with('df'), -rowname)
  for(i in seq(from = 1, to = l)){
    if(i > 1){
      m = summary(mice::pool(elipsis[[i]])) %>% data.frame() %>% tibble::rownames_to_column() %>% data.frame()
      m = dplyr::select(m, -dplyr::starts_with('df'), -rowname)
      c = merge(c,m, by = 'term', all.x = TRUE, all.y = TRUE)
    }
    cNames = paste(rep(c('est','SE','t','p'),i),seq(2:ncol(c)), sep = '.')
    colnames(c)[2:ncol(c)] = cNames
  }
  c$Missing1 = 0
  c$Missing2 = 0
  c$Missing3 = 0
  c$Missing4 = 0
  c$Missing5 = 0
  c$Missing6 = 0
  c$Missing7 = 0
  c$Missing8 = 0
  c$Missing9 = 0
  c$Missing10 = 0
  c$Missing11 = 0
  c$Missing12 = 0
  c$Missing13 = 0
  c$Missing14 = 0
  c$Missing15 = 0

  c$Colon = stringr::str_count(c$term, ":")
  if(l > 1){
    c$Missing1 = ifelse(is.na(c$est.1),1,0)
  }
  if(l > 2){
    c$Missing2 = ifelse(is.na(c$est.5),1,0)
  }
  if(l > 3){
    c$Missing3 = ifelse(is.na(c$est.9),1,0)
  }
  if(l > 4){
    c$Missing4 = ifelse(is.na(c$est.13),1,0)
  }
  if(l > 5){
    c$Missing5 = ifelse(is.na(c$est.17),1,0)
  }
  if(l > 6){
    c$Missing6 = ifelse(is.na(c$est.21),1,0)
  }
  if(l > 7){
    c$Missing7 = ifelse(is.na(c$est.25),1,0)
  }
  if(l > 8){
    c$Missing8 = ifelse(is.na(c$est.29),1,0)
  }
  if(l > 9){
    c$Missing9 = ifelse(is.na(c$est.33),1,0)
  }
  if(l > 10){
    c$Missing10 = ifelse(is.na(c$est.37),1,0)
  }
  if(l > 11){
    c$Missing11 = ifelse(is.na(c$est.41),1,0)
  }
  if(l > 12){
    c$Missing12 = ifelse(is.na(c$est.45),1,0)
  }
  if(l > 13){
    c$Missing13 = ifelse(is.na(c$est.49),1,0)
  }
  if(l > 14){
    c$Missing14 = ifelse(is.na(c$est.53),1,0)
  }
  if(l > 15){
    c$Missing15 = ifelse(is.na(c$est.57),1,0)
  }
  c = dplyr::arrange(c, Missing1, Missing2,Missing3,Missing4,Missing5,Missing6,Missing7,Missing8,Missing9,Missing10,Missing11,Missing12,Missing13,Missing14,Missing15,Colon)
  c = dplyr::select(c, -dplyr::starts_with('Miss'), -dplyr::starts_with('Colon'),-dplyr::starts_with('SE'), -dplyr::starts_with('t.'))
  c = tibble::as.tibble(c)
  cNames = rep(c('b','sig.'),l)
  colnames(c)[2:ncol(c)] = cNames

  c$term = gsub(":", " x ", c$term)

  # Change first column name to say 'DV: ____'
  DVName <- format(eval(m1$call[[2]])) %>% stringr::str_extract(".*~") %>%
    stringr::str_replace("~","") %>% trimws()
  colnames(c)[1] = paste("DV: ", DVName,by="")

  # Add stars for significance if requested:
  if(Stars){
    c$stars = dplyr::case_when(
      c[,ncol(c)] < .001 ~ "***",
      c[,ncol(c)] < .01 ~ "**",
      c[,ncol(c)] < .05 ~ "*",
      c[,ncol(c)] < .10 ~ "†"
    )
    names(c)[length(c)] = ""
  }

  knitr::kable(c, digits = 3, format = 'html', booktabs = F) %>% kableExtra::kable_styling()

}
