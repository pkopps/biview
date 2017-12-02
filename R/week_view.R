# week

week_view <- function(
  df,
  metric,
  # run_rate = FALSE,
  show_type = FALSE,
  hide_op2 = TRUE
) {

  if(missing(df)){ stop("'df' argument is mandatory") }
  if(missing(metric)){ stop("'metric' argument is mandatory") }

  # lst_wk <- week(Sys.Date() - 7)

  metric <- enquo(metric)
  metric_name <- quo_name(metric)

  # print(metric_name) FORDEBUG

  df <- df %>%
    group_by(yr_num, wk_num_in_yr) %>%
    summarise_at(vars(!!metric
    ), funs(sum)) %>%
    mutate(type = "actual") %>%
    filter(between(wk_num_in_yr, prev_wk - 3, prev_wk)) %>%
    ungroup()

  # suppressMessages({
  #   df <- full_join(df, op2 %>% select(yr_num, mth_num_in_yr, revenue_op2)) %>% arrange(yr_num %>% desc)
  # })

  # df <- df %>% mutate(revenue_op2_var = round((revenue - revenue_op2)/(revenue) * 100, 2)) %>% ungroup()

  cur_yr_df <- df %>% filter(yr_num == cur_yr)
  prev_yr_df <- df %>% filter(yr_num == prev_yr)

  suppressMessages({
    prev_yr_var_df <- right_join(cur_yr_df %>% select(wk_num_in_yr, revenue, type),
                                 prev_yr_df %>% select(wk_num_in_yr, revenue),
                                 by = c("wk_num_in_yr" = "wk_num_in_yr"),
                                 suffix = c("_cur_yr","_prev_yr"))
  })

  # for(metric in metrics){
  # prev_yr_var_df <- metric
  # print(paste0(metric, "_prev_yr_var"))
  # print(paste0(metric, "_cur_yr"))
  # print(paste0(metric, "_prev_yr"))
  #
  # prev_yr_var_df %>% mutate(print(paste0(metric, "_prev_yr_var")) =
  #                 print(paste0(metric, "_cur_yr")) -
  #                 print(paste0(metric, "_prev_yr")) )
  # }

  prev_yr_var_df <- prev_yr_var_df %>% mutate(revenue_prev_yr_var =
                                                round((revenue_cur_yr - revenue_prev_yr)/(revenue_cur_yr) * 100, 2)
                                              #   ,
                                              # trials_prev_yr_var =
                                              #   ((trials_cur_yr - trials_prev_yr)/trials_cur_yr) * 100)
  )

  # suppressMessages({
  #   final <- left_join(prev_yr_var_df,
  #                      df %>% filter(yr_num == cur_year))
  # })

  final <- prev_yr_var_df %>%
    mutate(revenue_op2 = NA,
           revenue_op2_var = NA) %>%
    gather(metric, value, -wk_num_in_yr) %>%
    spread(wk_num_in_yr, value)

  if(show_type){
    final <- final
  }
  else{
    final <- final %>% filter(metric != "type")
  }

  if(hide_op2){
    final <- final %>% filter(!(metric %in% c("revenue_op2","revenue_op2_var")))
  }
  else{
    final <- final
  }

  final

}

# week_view(sample_data, metric = revenue, show_type = FALSE, hide_op2 = FALSE) -> w
