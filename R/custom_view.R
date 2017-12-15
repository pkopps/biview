# custom view function

custom_view <- function(
  df,
  metric,
  fun = sum,
  spread_col,
  ...
){

  metric <- enquo(metric)
  grouping_cols <- quos(...)
  spread_dim <- enquo(spread_col)
  fun <- enquo(fun)

  df %>% group_by(!!!grouping_cols) %>%
    summarise_at(vars(!!metric), funs(!!fun)) %>%
    spread(!!spread_dim, !!metric)

}

