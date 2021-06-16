#' Get a data frame of correlations and p values
#'
#' This function is indented to store correlations and p values in an easy-to-
#' work-with dataframe.
#'
#' @importFrom magrittr "%>%"
#' @param df a data frame or tibble.
#' @return correlations and p values
#' @export
get_cor_df <- function(df) {
  cors <- Hmisc::rcorr(as.matrix(df))
  cor_df <- purrr::map(cors, ~data.frame(.x))
  return(cor_df)
}


#' Determines whether or not a value is in the upper or lower triangle
#' of a correlation matrix.
#'
#' This is a helper function for long_cors
#'
#' @importFrom magrittr "%>%"
#' @param df a data frame or tibble.
#' @return a data frame with labeled upper and lower triangles
add_triangle <- function(df){
  df %>%
    dplyr::group_by(measure1) %>%
    dplyr::mutate(m1_index = cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(measure2) %>%
    dplyr::mutate(m2_index = cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(triangle = case_when(
      m1_index == m2_index ~ "diag",
      m1_index > m2_index ~ "lower",
      TRUE ~ "upper"
    )) %>%
    dplyr::select(-contains("index"))
}

#' Get a data frame of correlations, in long format.
#'
#' A handy function to get lots of correlations in a tidy format
#'
#' @importFrom magrittr "%>%"
#' @param df a data frame or tibble.
#' @return correlations and p values
#' @export
long_cors <- function(df){
  get_cor_df(df) %>%
    purrr::map(~rownames_to_column(.x, var="measure1")) %>%
    purrr::map(~pivot_longer(.x, -measure1, "measure2")) %>%
    dplyr::bind_rows(.id = "id") %>%
    tidyr::pivot_wider(names_from = id, values_from = value) %>%
    dplyr::rename(p = P) %>%
    dplyr::mutate(sig_p = ifelse(p < .05, T, F),
           p_if_sig = ifelse(p <.05, p, NA),
           r_if_sig = ifelse(p <.05, r, NA)) %>%
    add_triangle()
}

#' Graph a data frame of correlations and p values, in long format.
#'
#' This is designed to work with long_cors in the pipe.
#'
#' @importFrom magrittr "%>%"
#' @param df a data frame or tibble.
#' @return a ggplot of correlations
#' @export
graph_cors <- function(df) {
  df %>%
    long_cors() %>%
    ggplot2::ggplot(aes(x = measure1, y = measure2, fill = r, label = round(r_if_sig,2))) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
    ggplot2::theme_classic() +
    ggplot2::scale_x_discrete(expand=c(0,0)) +
    ggplot2::scale_y_discrete(expand=c(0,0)) +
    ggplot2::geom_text(size = 3) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

# # Here's a demo of the long correlation dataframe the long_cors function makes
# d <- mtcars %>% select(cyl, hp, mpg, drat, wt, qsec)
#
# d %>%
#   long_cors()
#
# # You can graph this dataframe with geom_tile:
# d %>%
#   long_cors() %>%
#   ggplot(aes(x = measure1, y = measure2, fill = r, label = round(r_if_sig,2))) +
#   geom_tile() +
#   scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
#   theme_classic() +
#   scale_x_discrete(expand=c(0,0)) +
#   scale_y_discrete(expand=c(0,0)) +
#   geom_text(size = 3) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
#
# # You can filter the dataframe to exclude the upper triangle
# # (or lower, or diagonal, etc.)
#
# d %>%
#   long_cors() %>%
#   filter(triangle != "upper") %>%
#   ggplot(aes(x = measure1, y = measure2, fill = r, label = round(r_if_sig,2))) +
#   geom_tile() +
#   scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
#   theme_classic() +
#   scale_x_discrete(expand=c(0,0)) +
#   scale_y_discrete(expand=c(0,0)) +
#   geom_text(size = 3) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


