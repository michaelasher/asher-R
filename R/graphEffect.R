#' Generate a table of estimates given a model
#'
#' This function generates estimates given a model, varying specified parameters.
#'
#' @importFrom magrittr "%>%"
#' @param ... up to 8 linear models, produced by lm
#' @param m A model
#' @param d The dataframe from which the model was generated
#' @param varying A vector of the model parameters that you wish to vary
#' @param xlevels The levels at which you want to vary any continuous parameters
#' @return A dataframe of model predictions, standard errors, and confidence intervals
#' @export
modelEffect <- function(m, d, varying, xlevels = c(-1,1)){
  nRows = nrow(d)
  dTemp = data.frame()[1:nRows,]
  rownames(dTemp) = seq(1,nRows)
  dNames = m$model %>% names()
  dIVs = dNames[2:length(dNames)]
  facNames = dplyr::select_if(m$model, is.factor) %>% names()

  ## Make New Data Set, "dTemp"
  for(name in dNames){
    eval(parse(text=paste("dTemp$", name, " = ", "d$", name, sep = "")))
  }

  ## Replace any factors that must be held constant with numeric versions
  for(name in names(dTemp)){
    if(name %in% facNames && ! name %in% varying){
      dest = eval(parse(text=paste("dTemp$", name, sep = "")))
      dTemp$tempVar = lmSupport::varRecode(dest, levels(dest), contrasts(dest)[1:length(contrasts(dest))]) %>% as.character() %>% as.numeric()
      dTemp = dTemp[,!(names(dTemp) %in% name)]
      names(dTemp)[ncol(dTemp)] = name
    }
  }

  ## Make Expand Grid of Things That Vary

  varList = list()

  for(i in 1:length(dIVs)){
    name = dIVs[i]
    if(name %in% facNames && name %in% varying){
      facLevels = levels(eval(parse(text=paste("d$",name,sep = ""))))
      varList[[name]] <- facLevels
    } else if(name %in% varying){
      varList[[name]] <- xlevels
    } else{
      varList[[name]] <- c(0)
    }
  }

  dInput = expand.grid(varList)

  # Create a model, mTemp, which is identical to m, but which uses dTemp
  modelFormula = eval(m$call[[2]])

  if(class(m)[1] == 'lm'){
    mTemp = lm(modelFormula, data = dTemp)
  }
  if(class(m)[1] == 'glm'){
    mTemp = glm(modelFormula, family = binomial(link='logit'), data = dTemp)
  }

  # Output model predictions
  dOut = lmSupport::modelPredictions(mTemp, dInput)

  return(dOut)
}

#' Generate a bar graph of a specified interaction
#'
#' This function generates a bar graph of an interaction, given a model,
#' one or two x variables, and one or two moderators.
#'
#' @importFrom magrittr "%>%"
#' @import ggplot2
#' @param m A model
#' @param d The dataframe from which the model was generated
#' @param x The variable that you want on the x axis
#' @param fill The variable that you want as a moderator
#' @param xlevels The levels at which you want to vary any continuous parameters
#' @param fill2 An optional second moderator, graphed as a fill
#' @param x2 An optional second independent variable, graphed on the x axis
#' @return A ggplot of of model predictions for the interaction
#' @export

graph_interaction <- function(d, m, x, fill, xlevels = c(-1,1), fill2, x2){

  threeWay = FALSE # Indicator of whether we'll pass three (vs. two) arguments into modelEffects.

  # Capturing inputs as quosure objects
  xv <- dplyr::enquo(x)
  mv <- dplyr::enquo(fill)
  vary1 = dplyr::quo_name(xv)
  vary2 = dplyr::quo_name(mv)

  # Throws error if there are 2 x vars and 2 fill vars, as
  # the package is not designed for 4-way interactions.
  if(!missing(fill2) && !missing(x2)){
    print("You can't have 2 Xs and 2 fills")
    break()
  }

  # Capturing x2 if it exists
  if(!missing(x2)){
    threeWay = TRUE
    xv2 <- dplyr::enquo(x2)
    vary3 = dplyr::quo_name(xv2)
  }

  # Capturing fill2 if it exists
  if(!missing(fill2)){
    threeWay = TRUE
    mv2 <- dplyr::enquo(fill2)
    vary3 = dplyr::quo_name(mv2)
  }

  # Generating model predictions for a 2-way interaction, or a 3-way interaction
  if(!threeWay){
    dOut = modelEffect(m, d, varying = c(vary1,vary2), xlevels = c(-1,1))
  }
  if(threeWay){
    dOut = modelEffect(m, d, varying = c(vary1,vary2,vary3), xlevels = c(-1,1))
  }

  # Set variables to determine type of interaction graph
  twoWay = TRUE
  threeWayTwoFill = FALSE
  threeWayTwoX = FALSE

  # Update variables if a 3-way interaction was requested
  if(!missing(fill2)){
    threeWayTwoFill = TRUE
    twoWay = FALSE
  }
  if(!missing(x2)){
    threeWayTwoX = TRUE
    twoWay = FALSE
  }

  # Graph Interactions:
  if(twoWay){
    out <- ggplot2::ggplot(dOut,
                  aes(x = !!xv %>% as.factor(),
                      y = Predicted,
                      fill = !!mv)) +
      ggplot2::geom_bar(stat = 'identity', position = 'dodge')
  }

  if(threeWayTwoFill){
    out <- ggplot2::ggplot(dOut,
                  aes(x = !!xv %>% as.factor(),
                      y = Predicted,
                      fill = interaction(!!mv, !!mv2))) +
      ggplot2::geom_bar(stat = 'identity', position = 'dodge')
  }

  if(threeWayTwoX){
    out <- ggplot2::ggplot(dOut,
                  aes(x = interaction(!!xv, !!xv2),
                      y = Predicted,
                      fill = !!mv %>% as.factor())) +
      ggplot2::geom_bar(stat = 'identity', position = 'dodge')
  }

  # Capture name of the y variable
  dNames = m$model %>% names()
  yLab = dNames[1]

  # Determine Y min and max for bar graph
  yvar = names(model.frame(m))[1]
  ySD = sd(eval(parse(text=paste("d$", yvar, sep = ""))), na.rm = T)
  yMin = min(eval(parse(text=paste("d$", yvar, sep = ""))), na.rm = T)
  yMax = max(eval(parse(text=paste("d$", yvar, sep = ""))), na.rm = T)
  ciMin = min(dOut$CILo)
  ciMax = max(dOut$CIHi)
  buffer = (ySD/2)
  graphMin = ciMin - buffer
  graphMax = ciMax + buffer
  graphMin = ifelse(yMin > graphMin, yMin, graphMin)
  graphMax = ifelse(yMax < graphMax, yMax, graphMax)

  # Add errobars, minimal theme, and labels
  out <- out + ggplot2::theme_minimal() +
    ggplot2::geom_errorbar(aes(ymin = CILo, ymax = CIHi), width = .25, position =  position_dodge(width = 0.90)) +
    ggplot2::labs(x = vary1, y = yLab, fill = NULL) + coord_cartesian(ylim = c(graphMin,graphMax))

  return(out)

}

#' Generate a bar graph, given a set of model predictions
#'
#' This function generates a bar graph of an interaction, given a dataframe of model
#' predictions.
#' @importFrom magrittr "%>%"
#' @import ggplot2
#' @param d A dataframe of model predictions, e.g. generated by the modelEffect function
#' @param x The variable that you want on the x axis
#' @param y The variable that you want on the y axis
#' @param fill The variable that you want as a moderator
#' @param fill2 An optional second moderator, graphed as a fill
#' @param x2 An optional second independent variable, graphed on the x axis
#' @return A ggplot of of model predictions for the interaction
#' @export
graph_effect <- function(d,x,y,fill,fill2,x2) {

  # Set variables to determine type of interaction graph
  twoWay = TRUE
  threeWayTwoFill = FALSE
  threeWayTwoX = FALSE

  # Capturing inputs as quosure objects
  xv <- dplyr::enquo(x)
  yv <- dplyr::enquo(y)
  mv <- dplyr::enquo(fill)

  # Throws error if there are 2 x vars and 2 fill vars, as
  # the package is not designed for 4-way interactions.
  if(!missing(fill2) && !missing(x2)){
    print("You can't have 2 xs and 2 fills")
    break()
  }

  # Update variables if a 3-way interaction was requested
  if(!missing(fill2)){
    threeWayTwoFill = TRUE
    twoWay = FALSE
    mv2 <- dplyr::enquo(fill2)
  }
  if(!missing(x2)){
    threeWayTwoX = TRUE
    twoWay = FALSE
    xv2 <- dplyr::enquo(x2)
  }

  # Graph interactions
  if(twoWay){
    out <- ggplot2::ggplot(d,
                  aes(x = !!xv %>% as.factor(),
                      y = !!yv,
                      fill = !!mv)) +
      ggplot2::geom_bar(stat = 'identity', position = 'dodge')
    }
  if(threeWayTwoFill){
    out <- ggplot2::ggplot(d,
                  aes(x = !!xv %>% as.factor(),
                      y = !!yv,
                      fill = interaction(!!mv, !!mv2))) +
      ggplot2::geom_bar(stat = 'identity', position = 'dodge')
  }
  if(threeWayTwoX){
    out <- ggplot2::ggplot(d,
                  aes(x = interaction(!!xv, !!xv2),
                      y = !!yv,
                      fill = !!mv %>% as.factor())) +
      ggplot2::geom_bar(stat = 'identity', position = 'dodge')
  }

  # Add errobars and minimal theme
  out <- out + ggplot2::theme_minimal() +
    ggplot2::geom_errorbar(aes(ymin = CILo, ymax = CIHi), width = .25, position =  position_dodge(width = 0.90))

  return(out)



}






