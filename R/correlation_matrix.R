#### Functions ####
get_cor_df <- function(df) {
  cors <- Hmisc::rcorr(as.matrix(df))
  cor_df <- purrr::map(cors, ~data.frame(.x))
  return(cor_df)
}


add_triangle <- function(df){
  df %>%
    group_by(measure1) %>%
    mutate(m1_index = cur_group_id()) %>%
    ungroup() %>%
    group_by(measure2) %>%
    mutate(m2_index = cur_group_id()) %>%
    ungroup() %>%
    mutate(triangle = case_when(
      m1_index == m2_index ~ "diag",
      m1_index > m2_index ~ "lower",
      TRUE ~ "upper"
    )) %>%
    select(-contains("index"))
}

long_cors <- function(df){
  get_cor_df(df) %>%
    purrr::map(~rownames_to_column(.x, var="measure1")) %>%
    purrr::map(~pivot_longer(.x, -measure1, "measure2")) %>%
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    rename(p = P) %>%
    mutate(sig_p = ifelse(p < .05, T, F),
           p_if_sig = ifelse(p <.05, p, NA),
           r_if_sig = ifelse(p <.05, r, NA)) %>%
    add_triangle()
}

graph_cors <- function(df) {
  df %>%
    long_cors() %>%
    ggplot(aes(x = measure1, y = measure2, fill = r, label = round(r_if_sig,2))) +
    geom_tile() +
    scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
    theme_classic() +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    geom_text(size = 3) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
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


