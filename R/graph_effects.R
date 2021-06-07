#' Graph all of the effects in a model that meet a significance threshold.
#'
#' This function is intended to quickly view and model output (e.g. interactions).
#' All factor variables should be given contrasts.
#'
#' @importFrom magrittr "%>%"
#' @param m A linear model. Standard OLS or logistic regression will work.
#' @param p_threshold All p values less than or equal to this threshold will be graphed.
#' @param x_pref An optional vector of preferences for the x axis.
#' @param fill_pref An optional vector of preferences for the fill of the graph.
#' @param facet_pref An optional vector of preferences for the facets of the graph.
#' @param continuous_x If true, a line graph will be created for any continuous variables, rather than bar graphs with predicted values at +1/-1 SD.
#' @param colors An optional vector of colors for the bars and lines of graphs.
#' @return A set of ggplot graphs.
#' @export
graph_all_effects <- function(m, p_threshold = .05,
                              x_pref = NULL, fill_pref = NULL, facet_pref = NULL,
                              continuous_x = FALSE, colors = NULL){
  # First, we'll want a table of all effects with a certain significance threshold.
  trends <- tibble::rownames_to_column(data.frame(summary(m)$coef))
  trends <- trends[trends[,5] <=  p_threshold, c(1,2,3,5)]
  trends <- trends %>% dplyr::mutate_if(is.numeric, ~round(.,3))
  trends <- dplyr::filter(trends, rowname != "(Intercept)")

  # Later, we'll loop over this table to generate our predictions

  # Next, interactions need to be split apart on ":" symbols so we can graph them.
  # We'll build this program to graph up to a 4-way interaction, but no further.
  if(sum(stringr::str_count(trends$rowname, ":") > 3)) stop("This program is not built for anything greater than a 4-way interaction")
  trends <- trends %>% tidyr::separate(rowname, c("var1","var2","var3","var4"), ":", fill = "right")

  # We also have to address the fact that factor variables get strange labels when we ask for a summary.
  # For instance, if we have a factor "FG" with the label "FG_vs_CG", the table outputs "FGFG_vs_CG"
  # Here's a table keeping track of the mappings between factor variable names and their possible
  # manifestations in our trends table. This will allow us know that "FGFG_vs_CG" corresponds to
  # the factor "FG"
  facs = dplyr::select_if(m$model, ~class(.) %in% c("factor", "character"))
  d_temp <- dplyr::select(m$model, names(facs)) %>% dplyr::mutate_if(is.character, as.factor)
  vars = names(m$model)[2:length(names(m$model))]

  fac_conversion <- tibble::tibble(table_name = character(), equation_name = character())


  for (var in vars){

    # If the variable is not a factor, it doesn't need to be converted
    if(!var %in% names(facs)){
      fac_conversion <- dplyr::bind_rows(fac_conversion,
                                  tibble::tibble(table_name = var, equation_name = var))
      next()
    }

    # If the variable is a factor, we'll add all of its possible conversions
    fac_conversion <- dplyr::bind_rows(
      fac_conversion,
      tibble::tibble(table_name = paste0(var, colnames(contrasts(d_temp[[var]]))), equation_name = var)
    )
  }

  # Now we'll do the graphing of all trends! Lots will happen in the loop below, where we go
  # row-by-row through our table of trends to graph.

  for(i in 1:nrow(trends)){

    # For each trend, we extract all of the variables that we will vary in our graph. After extracting
    # them, we will remove NA values and then match any factor labels to their corresponding factor names
    eff <- trends[i,c("var1","var2","var3","var4")]
    eff <- eff[!is.na(eff)]
    eff <- fac_conversion$equation_name[match(eff, fac_conversion$table_name)]

    # TODO: Determine if/which a numeric var should be graphed on a continuous scale (rather than +/- 1 sd)
    full_range_var <- NULL
    num_vars <- eff[eff %in% names(select_if(m$model, is.numeric))]
    if(continuous_x == TRUE & length(num_vars) > 0){
      ranks <- data.frame(var = num_vars, pref_rank = match(num_vars, x_pref))
      ranks$var <- as.character(ranks$var)
      ranks <- arrange(ranks, pref_rank, var)
      full_range_var <- ranks$var[1]
    }
    # Now we can get model predictions. We'll use a `get_predictions` function that to do so.
    model_predictions <- get_predictions(m, varying = c(eff), full_range_var)

    prefs <- set_x_fill_facet(model_predictions, x_pref, fill_pref, facet_pref)
    x <- prefs$x
    fill <- prefs$fill
    facet <- prefs$facet
    facet2 <- prefs$facet2


    # Next we'll do graphs.
    if(is.numeric(model_predictions[[x]])){
      geom <- "line"
    } else {
      geom <- "bar"
    }

    print(graph_predictions(model_predictions, geom = geom, x = x, fill = fill,
                            facet = facet, facet2 = facet2, colors = colors))

  }

}



#' Get predictions from a model
#'
#' This function returns a dataframe of model predictions, varying the variables you specify
#' and holding the others constant at zero. Accordingly, it's important to center continuous
#' variables and set up appropriate contrasts for factor variables before running this
#' function.
#'
#' @importFrom magrittr "%>%"
#' @param m A linear model. Standard OLS or logistic regression will work.
#' @param varying A vector of the names of the terms in the model to vary.
#' @param full_range_var Should one of the variables be graphed continuously on the x axis? If so,
#' you can write its name here.
#' @param sd_to_graph When making predictions for continuous variables, the function will make them
#' at +/- this many standard deviations.
#' @return A data frame of model predictions.
#' @export
get_predictions <- function(m, varying, full_range_var = NULL, sd_to_graph = 1){

  # Get a list of all terms in the model
  model_vars = m$model %>% names()
  num_vars = m$model[2:ncol(m$model)] %>% dplyr::select_if(is.numeric)
  d_model <- m$model

  if(!is.null(full_range_var)) {
    min_full_range_var <- min(d_model[[full_range_var]], na.rm = T)
    max_full_range_var <- max(d_model[[full_range_var]], na.rm = T)
    full_range_levels <- seq(min_full_range_var, max_full_range_var, length.out = 1000)
  }


  ## Make New Data Set, "d_temp", with only the appropriate variables
  d_temp <- d_model %>% dplyr::mutate_if(is.character, as.factor)


  ## Set aside a reference dataset for later if we need factor labels from anything
  # that we just converted to a factor.
  d_ref <- d_temp

  # Extract the factor variables we need to recode to numeric contrasts
  # We'll only extract those that aren't in our "varying" list.
  factor_vars <- dplyr::select_if(d_temp, is.factor)
  to_remove <- varying[varying %in% names(factor_vars)]
  factor_vars_to_recode <- dplyr::select_if(d_temp, is.factor) %>% dplyr::select(-all_of(to_remove))

  # Identify factor vars to be held at zero and recode them to numeric versions.
  # The code below creates explicit contrast factor variable and then binds them all with our temporary dataset
  mm_formula <- paste(names(factor_vars_to_recode), collapse = " + ")
  if(mm_formula != ""){
    regressors <- model.matrix(as.formula(paste("~", mm_formula)), model.frame(~ ., d_temp, na.action=na.pass))
    temp_names <- colnames(regressors)[2:ncol(regressors)]
    d_regressors = data.frame(regressors[, 2:ncol(regressors)])
    colnames(d_regressors) <- temp_names
    d_temp <- dplyr::bind_cols(d_temp, d_regressors)

    # Remove all of the now-redundant factor variables
    d_temp <- dplyr::select(d_temp, -all_of(names(factor_vars_to_recode)))
  }

  # Fix the formula for the model so it now incorporates all of the new factor variables
  model_formula <- m$call %>% as.character()
  model_formula <- model_formula[2]


  if(ncol(factor_vars_to_recode) > 0){
    for(i in 1:length(factor_vars_to_recode)){
      fac_name <- names(factor_vars_to_recode)[i]
      name_sub <- paste0(fac_name, colnames(contrasts(d_ref[[fac_name]])), collapse = " + ")
      name_sub <- paste0("(",name_sub,")")
      model_formula <- gsub(fac_name, name_sub, model_formula)
    }
  }


  var_list = list()

  for(i in 2:ncol(d_temp)){
    var_name <- names(d_temp)[i]
    if(var_name %in% varying && is.factor(d_temp[[var_name]])){
      fac_levels <- levels(d_temp[[var_name]])
      var_list[[var_name]] <- fac_levels
    } else if(var_name %in% varying){

      var_mean <- mean(d_temp[[var_name]], na.rm = T) %>% round(2)
      var_sd <- sd(d_temp[[var_name]], na.rm = T) %>% round(2)
      xmin <- var_mean - sd_to_graph*var_sd
      xmax <- var_mean + sd_to_graph*var_sd
      xlevels <- c(xmin, xmax)
      var_list[[var_name]] <- xlevels

    } else
      var_list[[var_name]] <- 0
  }

  if(!is.null(full_range_var)){
    var_list[[full_range_var]] <- full_range_levels
  }


  d_graph = expand.grid(var_list)

  if(class(m)[1] == 'lm'){
    m_graph = lm(model_formula, data = d_temp)
  }
  if(class(m)[1] == 'glm'){
    m_graph = glm(model_formula, family = binomial(link='logit'), data = d_temp)
  }

  # Return Model Predictions, removing all that were held constant
  d_out = get_model_predictions(m_graph, d_graph)
  d_out <- dplyr::select_if(d_out, ~length(unique(.)) > 1)
  names(d_out)[1] <- names(m$model)[1]

  # Convert numeric variables into character vars with labels like +1 SD, and -1 SD
  d_to_recode <- dplyr::select(d_out, all_of(varying)) %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::select_if(~length(unique(.)) < 3)

  for(var in names(d_to_recode)){
    d_out[[var]] <- stringr::str_c(
      round((d_out[[var]] - mean(d_temp[[var]], na.rm = T)) / sd(d_temp[[var]], na.rm = T),1),
      " SD ", var
    )
  }


  return(d_out)

}

#' Graph a dataframe of model predictions.
#'
#' This function returns a ggplot
#'
#' @importFrom magrittr "%>%"
#' @param predictions A dataframe of model predictions. The y variable must be in the first column.
#' @param x The variable to graph on the x axis.
#' @param fill The (optional) variable to graph as a fill.
#' @param facet The (optional) variable to graph as a facet
#' @param facet2 The (optional) variable to graph as a second facet (for 4-way interactions)
#' @param color An optional vector of colors for bars or lines.
#' @return A ggplot (bar or line graph).
#' @export
graph_predictions <- function(predictions, geom = "bar",
             x = NULL, fill = NULL, facet = NULL, facet2 = NULL,
             colors = NULL){

  y = names(predictions)[1]

  int_vars <- c(x,fill,facet,facet2)
  n_way <- length(int_vars)

  if(n_way == 4){
    facetLabel <- paste0(facet, "_x_", facet2)
    predictions[[facetLabel]] <- interaction(predictions[[facet]],predictions[[facet2]], sep = " & ")
    predictions[[facet]] <- NULL
    predictions[[facet2]] <- NULL
    facet <- facetLabel
  }

  if(geom == "bar" & n_way == 1){
    p <- ggplot2::ggplot(data = predictions, ggplot2::aes_string(x = x, y = y)) +
      ggplot2::geom_bar(stat = "identity", ggplot2::aes_string(fill = x)) +
      ggplot2::geom_errorbar(stat = "identity", ggplot2::aes(ymin = CILo, ymax = CIHi), width = .25) +
      ggplot2::theme(legend.position = "none")
  }

  if(geom == "line" & n_way == 1){
    p <- ggplot2::ggplot(data = predictions, ggplot2::aes_string(x = x, y = y)) +
      ggplot2::geom_smooth(stat = "identity", ggplot2::aes_string(ymin = "CILo", ymax = "CIHi"))
  }

  if(geom == "bar" & n_way == 2){
    p <- ggplot2::ggplot(data = predictions, ggplot2::aes_string(x = x, y = y, fill = fill)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::geom_errorbar(stat = "identity", ggplot2::aes(ymin = CILo, ymax = CIHi), position = ggplot2::position_dodge(.9), width = .25)
  }

  if(geom == "line" & n_way == 2){
    p <- ggplot2::ggplot(data = predictions, ggplot2::aes_string(x = x, y = y, color = fill)) +
      ggplot2::geom_smooth(stat = "identity", ggplot2::aes_string(ymin = "CILo", ymax = "CIHi"))
  }

  if(n_way %in% c(3,4) & geom == "bar"){
    p <- ggplot2::ggplot(data = predictions, ggplot2::aes_string(x = x, y = y, fill = fill)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::facet_wrap(as.formula(paste("~", facet))) +
      ggplot2::geom_errorbar(stat = "identity", ggplot2::aes(ymin = CILo, ymax = CIHi), position = ggplot2::position_dodge(.9), width = .25)
  }

  if(n_way %in% c(3,4) & geom == "line"){
    p <- ggplot2::ggplot(data = predictions, ggplot2::aes_string(x = x, y = y, color = fill)) +
      ggplot2::geom_smooth(stat = "identity", ggplot2::aes_string(ymin = "CILo", ymax = "CIHi")) +
      ggplot2::facet_wrap(as.formula(paste("~", facet)))
  }

  if(!is.null(colors)) {
    p <- p + scale_fill_manual(values=colors)
    p <- p + scale_color_manual(values=colors)
  }


  return(p)
}


#' Graph a dataframe of model predictions.
#'
#' This function determines the proper x, fill, facet, and facet2, given a set of
#' preferences. It is used by other graphing functions.
#'
#' @importFrom magrittr "%>%"
#' @param predictions A dataframe of model predictions. The y variable must be in the first column.
#' @param x_pref An optional vector of preferences for the x axis.
#' @param fill_pref An optional vector of preferences for the fill of the graph.
#' @param facet_pref An optional vector of preferences for the facets of the graph.
#' @return A list with x, fill, and facet decisions.
set_x_fill_facet <- function(predictions, x_pref, fill_pref, facet_pref){

  vars <- names(predictions)[5:ncol(predictions)]
  predictions <- dplyr::mutate_if(predictions, is.character, as.factor)

  out <- list(x = NULL, fill = NULL, facet = NULL, facet2 = NULL)

  # if(length(num_vars) > 1) stop("Cannot create a graph with more than 1 continuous input")

  ranks <- data.frame()

  for(var in vars){
    var_name <- as.character(var)
    var_preds <- predictions[[var]]
    is_numeric <- ifelse(class(var_preds) == "numeric", 1, 0)
    num_levels <- levels(var_preds) %>% length()
    x_pref_rank <- match(var, x_pref)
    fill_pref_rank <- match(var, fill_pref)
    facet_pref_rank <- match(var, facet_pref)

    ranks <- rbind(ranks, data.frame(var_name, is_numeric, num_levels,
                                     x_pref_rank, fill_pref_rank, facet_pref_rank))
  }

  ranks$var_name <- as.character(ranks$var_name)
  ranks <- dplyr::mutate_all(ranks, ~tidyr::replace_na(.,99))

  for(var in c("x", "fill", "facet", "facet2")[1:length(vars)]){
    if(var == "x"){
      ranks <- dplyr::arrange(ranks, -is_numeric, x_pref_rank, -fill_pref_rank,
                       -facet_pref_rank, var_name)
      out$x <- ranks$var_name[1] %>% as.character()
      ranks <- dplyr::filter(ranks, var_name != out$x)
    }

    if(var == "fill"){
      ranks <- dplyr::arrange(ranks, fill_pref_rank, -facet_pref_rank, -num_levels, var_name)
      out$fill <- ranks$var_name[1] %>% as.character()
      ranks <- dplyr::filter(ranks, var_name != out$fill)
    }

    if(var == "facet"){
      ranks <- dplyr::arrange(ranks, facet_pref_rank, var_name)
      out$facet <- ranks$var_name[1] %>% as.character()
      ranks <- dplyr::filter(ranks, var_name != out$facet)
    }

    if(var == "facet2"){
      out$facet2 <- ranks$var_name[1] %>% as.character()
    }


  }


  return(out)



}

#' Return a dataframe of model predictions
#'
#' This is an internal function used by other graphing functions.
#'
#' @importFrom magrittr "%>%"
#' @return A dataframe of model predictions.
get_model_predictions <- function(model, data = NULL, label = NULL, type = "response") {
  if (is.null(data) & class(model)[1] == "lm") {
    return(fitted.values(model))
  }
  else {
    if (is.null(label)) {
      predict_name = "Predicted"
      ci_lo_name = "CILo"
      ci_hi_name = "CIHi"
      se_name = "SE"
    }
    else {
      predict_name = paste0("Predicted", label)
      ci_lo_name = paste0("CILo", label)
      ci_hi_name = paste0("CIHi", label)
      se_name = paste0("SE", label)
    }
    predictions = matrix(data = NA, nrow = nrow(data), ncol = 4,
                         dimnames = list(1:nrow(data), c(predict_name, ci_lo_name,
                                                         ci_hi_name, se_name)))
    if (class(model)[1] == "lm") {
      ci_level = 1 - 2 * pt(c(1), df = model$df.residual,
                           lower.tail = FALSE)
      predictions[, 1:3] = predict(model, newdata = data,
                                   interval = "confidence", level = ci_level)
      predictions[, 4] = predictions[, 1] - predictions[,
                                                        2]
      predictions = as.data.frame(predictions)
    }
    if (class(model)[1] == "glm") {
      tmp_pred = predict(model, newdata = data, type = "link",
                        se.fit = TRUE)
      upr <- tmp_pred$fit + tmp_pred$se.fit
      lwr <- tmp_pred$fit - tmp_pred$se.fit
      fit <- tmp_pred$fit
      if (type == "response") {
        fit <- model$family$linkinv(fit)
        upr <- model$family$linkinv(upr)
        lwr <- model$family$linkinv(lwr)
      }
      predictions[, 1] = fit
      predictions[, 2] = lwr
      predictions[, 3] = upr
      predictions[, 4] = predictions[, 1] - predictions[,
                                                        2]
      predictions = as.data.frame(predictions)
    }
    if ((class(model)[1] == "lmerMod") || (class(model)[1] ==
                                           "glmerMod")) {
      predictions[, c(1, 4)] = predictSE(model, data, se.fit = TRUE,
                                         type = type, level = 0, print.matrix = TRUE)
      predictions[, 2] = predictions[, 1] - predictions[,
                                                        4]
      predictions[, 3] = predictions[, 1] + predictions[,
                                                        4]
    }
    if (any(names(data) == predict_name) || any(names(data) ==
                                               ci_lo_name) || any(names(data) == ci_hi_name) || any(names(data) ==
                                                                                                se_name)) {
      warning("Variable names (Predicted, CILo, CIHi, SE with label PostFix) used in data.  These variables removed before merging in predicted values")
      data[, c(predict_name, ci_lo_name, ci_hi_name, se_name)] = list(NULL)
    }
    data = data.frame(predictions, data)
    return(data)
  }
}

#' Graph a single effect, given a model.
#'
#' This function is intended to quickly graph a single set of predictions, given a model.
#'
#' @importFrom magrittr "%>%"
#' @param m A linear model. Standard OLS or logistic regression will work.
#' @param x the name of the variable that should be graphed on the x axis.
#' @param fill the (optional) name of the variable that should be graphed as a fill.
#' @param facet the (optional) name of the variable that should be graphed as a facet.
#' @param facet2 the (optional) name of the variable that should be graphed as a second facet.
#' @param continuous_x If true, a line graph will be created for any continuous variables, rather than bar graphs with predicted values at +1/-1 SD.
#' @param colors An optional vector of colors for the bars and lines of graphs.
#' @return A ggplot line or bar graph.
#' @export
graph_effect <- function(model, x, fill = NULL, facet = NULL, facet2 = NULL,
                         continuous_x = FALSE, colors = NULL){

  effs <- c(x, fill, facet, facet2)
  full_range_var <- NULL

  numeric_x <- class(model$model[[x]]) == "numeric"

  if(continuous_x & numeric_x){
    full_range_var <- x
  }

  preds <- get_predictions(model, effs, full_range_var = full_range_var)

  if(is.numeric(preds[[x]]) & continuous_x == TRUE){
    graph <- graph_predictions(preds, geom = "line", x, fill, facet, facet2,
                               colors)
  } else{
    graph <- graph_predictions(preds, geom = "bar", x, fill, facet, facet2,
                               colors)

  }

  return(graph)

}

