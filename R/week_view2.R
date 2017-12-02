#week_view2

week_view2 <- function(df,
                       metric,
                       show_type = FALSE,
                       num_wks_to_show = 4
                       ) {

  if(missing(df)){ stop("'df' argument is mandatory") }
  if(missing(metric)){ stop("'metric' argument is mandatory") }

  metric <- enquo(metric)
  metric_name <- quo_name(metric)

  metric_cur_yr_name <- as.name(paste(metric_name, "cur_yr", sep = "_"))
  metric_prev_yr_name <- as.name(paste(metric_name, "prev_yr", sep = "_"))

  metric_prev_yr_var_name <- paste(metric_name, "prev_yr_var", sep = "_")
  metric_prev_yr_var <- enquo(metric_prev_yr_var_name)

  metric_prev_yr_var_name <- paste(metric_name, "prev_yr_var", sep = "_")

  metric_op2 <- enquo(metric_op2)
  metric_op2_name <- quo_name(metric_op2)

  metric_op2_var_name <- paste(metric_name, "op2_var", sep = "_")
  metric_op2_var <- enquo(metric_op2_var_name)

  ordering_array <- c("type",
                      metric_cur_yr_name,
                      metric_prev_yr_name,
                      metric_prev_yr_var_name,
                      metric_op2_name,
                      metric_op2_var_name)

  df <- df %>%
    group_by(yr_num, wk_num_in_yr) %>%
    summarise_at(vars(!!metric), funs(round_sum)) %>%
    mutate(type = "actual") %>%
    filter(between(wk_num_in_yr, prev_wk - (num_wks_to_show - 1), prev_wk)) %>%
    ungroup()

  cur_yr_df <- df %>% filter(yr_num == cur_yr)
  prev_yr_df <- df %>% filter(yr_num == prev_yr)

  prev_yr_var_df <- right_join(cur_yr_df %>% select(wk_num_in_yr, !!metric, type),
                               prev_yr_df %>% select(wk_num_in_yr, !!metric),
                               by = c("wk_num_in_yr" = "wk_num_in_yr"),
                               suffix = c("_cur_yr","_prev_yr")) %>%
    mutate(
     !!metric_prev_yr_var_name :=
       round( 100 * ( (UQ(metric_cur_yr_name)) - (UQ(metric_prev_yr_name)) )
              /
                (UQ(metric_prev_yr_name)), 2 )
   )

  final <- prev_yr_var_df %>%
    gather(metric, value, -wk_num_in_yr) %>%
    spread(wk_num_in_yr, value)

  if(show_type){
    final <- final
  }
  else{
    final <- final %>% filter(metric != "type")
  }

  final %>% arrange(
    metric = ordered(metric, levels = ordering_array)
  )

}
