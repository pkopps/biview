# place holder wk_view

wk_view_place_hold <- function(metric_name, num_wks_to_show = 4){

  wk_num_in_yr <- paste0("w", c((prev_wk - (num_wks_to_show - 1)):prev_wk))

  tibble(
    'metric' = c(metric_name, "Prior Year", "Variance vs. Prior Year", "OP2 Plan", "Variance vs. Plan")
  ) -> l

  t <- tibble(
    "wk_num_in_yr" = wk_num_in_yr,
    value = NA
  ) %>% spread(wk_num_in_yr, value)

  bind_rows(t,t,t,t,t) -> r

  bind_cols(l, r)

}
