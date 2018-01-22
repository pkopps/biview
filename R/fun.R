fun <- function(
  df,
  metric,
  grouping,
  goal_df,
  goal_metric,
  new_name = NULL
){

  if(missing(grouping)) stop("'grouping' is missing")

  metric <- enquo(metric)
  grouping <- enquo(grouping)
  goal_metric <- enquo(goal_metric)

  if(grouping == "~wk_num_in_yr"){
    df <- df %>% group_by(yr_num, !!grouping, wk_end_date) %>%
      summarise_at(vars(!!metric), funs(sum)) %>%
      filter(wk_end_date >= ceiling_date( ( Sys.Date() - (7 * 4) ) ) ) %>%
      select(-wk_end_date)
  }else{
    df <- df %>% group_by(yr_num, !!grouping) %>%
      summarise_at(vars(!!metric), funs(sum))
  }

  if(grouping != "~yr_num"){
    df_cur_yr <- df %>% filter(yr_num == max(df$yr_num)) %>% ungroup() %>% select(-yr_num) %>% rename(metric_cur_yr = !!metric)
    df_prev_yr <- df %>% filter(yr_num == max(df$yr_num) - 1) %>% ungroup() %>% select(-yr_num) %>% rename(metric_prev_yr = !!metric)
  }else{ # do not remove yr_num for join
    df_cur_yr <- df %>% filter(yr_num == max(df$yr_num)) %>% ungroup() %>% rename(metric_cur_yr = !!metric)
    df_prev_yr <- df %>% filter(yr_num == max(df$yr_num) - 1) %>% ungroup() %>% rename(metric_prev_yr = !!metric) %>% mutate(yr_num = yr_num + 1)
  }

  print(df_cur_yr)
  print(df_prev_yr)

  print(grouping)

  # if(grouping == "~yr_num"){
  #
  # }else{
    df <-
      full_join(
        df_cur_yr,
        df_prev_yr,
        suffix = c("_cur_yr", "_prev_yr")
      ) %>%
      mutate(
        prev_yr_var = ( ( metric_cur_yr - metric_prev_yr ) / metric_prev_yr ) # previous year variance
      )
  # }

  if(!missing(goal_df)){
    goal_df <- goal_df %>% select(!!grouping, !!goal_metric, -yr_num) %>% rename(goal_metric = !!goal_metric)
    df <-
      left_join(
        df,
        goal_df
      ) %>%
      mutate(
        goal_var = ( ( metric_cur_yr - goal_metric ) / goal_metric ) # goal variance
      )
  }

  ordering_array = c('metric_cur_yr', 'metric_prev_yr', 'prev_yr_var', 'goal_metric', 'goal_var')

  df <- df %>%
    gather(metric, value, -!!grouping) %>%
    spread(!!grouping, value) %>%
    arrange(
      metric = ordered(metric, levels = ordering_array)
    )

  print(df)

  if(!is.null(new_name)){
    if(!missing(goal_df)){
      df <- df %>% mutate(
        metric = c(new_name, "Prior Year", "Variance vs. Prior Year", "OP2 Plan", "Variance vs. Plan")
      )
    }else(
      df <- df %>% mutate(
        metric = c(new_name, "Prior Year", "Variance vs. Prior Year")
      )
    )
  }

  df

}
