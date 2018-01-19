# place holder wk_view

wk_view_place_hold <- function(df, metric_name, num_wks_to_show = 4){

  wk_array <- df %>% filter(wk_end_date >= floor_date((Sys.Date() - (7 * num_wks_to_show)), 'week')) %>%
    arrange(wk_end_date) %>%
    mutate(wk_num_in_yr = wk_num_in_yr %>% as.character() %>% as_factor()) %>%
    select(wk_num_in_yr)

  wk_num_in_yr <- wk_array$wk_num_in_yr

  tibble(
    'metric' = c(metric_name, "Prior Year", "Variance vs. Prior Year", "OP2 Plan", "Variance vs. Plan")
  ) -> l

  t <- tibble(
    "wk_num_in_yr" = wk_num_in_yr,
    value = NA
  ) %>% spread(wk_num_in_yr, value)

  names(t)[1:ncol(t)] <- paste0("w", names(t)[1:ncol(t)])

  bind_rows(t,t,t,t,t) -> r

  bind_cols(l, r)

}
