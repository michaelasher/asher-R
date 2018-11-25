#' Make a Stepwise Regression Table
#'
#' This function creates a summary table for a linear model, and outputs it to the
#' viewer in HTML Format to facilitate copyin and pasting. By default it outputs
#' standardized betas for regression coefficients, and it can accept
#' up to 8 models as arguments, which makes it ideal for displaying stepwise models.
#'
#' @importFrom magrittr "%>%"
#' @param ... up to 8 linear models, produced by lm
#' @param Betas whether standardized betas should be printed
#' @param ModelStats whether r squared, etc. should be printed
#' @return The regression table is printed to the viewer using the "Kable" function from knitr
#' @export
stepTable = function(..., Betas = TRUE, ModelStats = FALSE){

  options(knitr.kable.NA = '')

  unstand = list(...)
  stand = list(...)
  elipsis = list(...)
  l = length(unstand)

  for(i in seq(1:l)){
    stand[[i]] = lm.beta::lm.beta(unstand[[i]])
  }
  if(Betas){
    elipsis = stand
  } else{
    elipsis = unstand
  }

  l = length(elipsis)

  c = summary(elipsis[[1]])$coefficients %>% data.frame() %>% tibble::rownames_to_column() %>% data.frame()

  if(Betas){c = dplyr::select(c, -dplyr::starts_with('Est'))}
  for(i in seq(from = 1, to = l)){
    if(i > 1){
      m = summary(elipsis[[i]])$coefficients %>% data.frame() %>% tibble::rownames_to_column() %>% data.frame()
      if(Betas){m = dplyr::select(m, -dplyr::starts_with('Est'))}
      c = merge(c,m, by = 'rowname', all.x = TRUE, all.y = TRUE)
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

  c$Colon = stringr::str_count(c$rowname, ":")
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
  c = dplyr::arrange(c, Missing1, Missing2,Missing3,Missing4,Missing5,Missing6,Missing7,Colon)
  c = dplyr::select(c, -dplyr::starts_with('Miss'), -dplyr::starts_with('Colon'),-dplyr::starts_with('SE'), -dplyr::starts_with('t'))
  if(Betas){c = dplyr::filter(c, rowname != '(Intercept)')}
  c = tibble::as.tibble(c)
  cNames = rep(c('B','sig.'),l)
  colnames(c)[2:ncol(c)] = cNames



  # Removing Names of Factors from Variable Names
  facs = dplyr::select_if(elipsis[[l]]$model, is.factor)


  for(i in seq(from = 1, to = ncol(facs))){
    if(ncol(facs)==0){
      break()
    }
    if(colnames(facs)[i] %in% as.character(levels(facs[,i]))){
      c$rowname = sub(paste(colnames(facs)[i],colnames(facs)[i],sep = ""), colnames(facs)[i], c$rowname)
    }
    else{
      c$rowname = sub(colnames(facs)[i], "", c$rowname)
    }
  }
  c$rowname = gsub(":", " x ", c$rowname)

  if(ModelStats){
    r2list = rep(NA,2*l+1)
    obslist = rep(NA,2*l+1)
    for(i in seq(from = 1, to = l)){
      r2 = summary(elipsis[[i]])$r.squared
      nobs = nobs(elipsis[[i]])
      r2list[2*i] = r2
      obslist[2*i] = nobs
    }
    c = rbind(c, obslist)
    c[nrow(c),1] = 'Observations:'
    c = rbind(c, r2list)
    c[nrow(c),1] = 'R-Squared:'
  }
  # Change first column name to say 'DV: ____'
  colnames(c)[1] = paste("DV: ",colnames(elipsis[[1]]$model)[1],by="")
  knitr::kable(c, digits = 3, format = 'html', booktabs = F) %>% kableExtra::kable_styling()
}
