# wmy_one

wmy_1e <- function(
  df,
  metric,
  # grouping,
  df_goal,
  metric_goal,
  df_3p9,
  metric_3p9,
  df_6p6,
  metric_6p6,
  df_9p3,
  metric_9p3,
  full_yr = TRUE,
  full_yr_rate = FALSE,
  new_name = NULL,
  accounting = TRUE,
  div_by_1000 = TRUE,
  prefix = "",
  suffix = "",
  spark = FALSE,
  pop = FALSE
){

  metric <- enquo(metric)
  metric_goal <- enquo(metric_goal)
  metric_3p9 <- enquo(metric_3p9)
  metric_6p6 <- enquo(metric_6p6)
  metric_9p3 <- enquo(metric_9p3)

  # week
  w <- # no goals or predictions
    fun(
      df = df,
      metric = !!metric,
      grouping = wk_num_in_yr,
      full_yr = FALSE,
      full_yr_rate = FALSE,
      new_name = new_name,
      accounting = accounting,
      div_by_1000 = div_by_1000,
      prefix = prefix,
      suffix = suffix,
      spark = spark,
      pop = pop
    )

  # month
  if(missing(df_goal)){
    m <-
      fun(
        df = df,
        metric = !!metric,
        grouping = mth_num_in_yr,
        # df_goal = df_goal,
        # metric_goal = !!metric_goal,
        df_3p9 = df_3p9,
        metric_3p9 = !!metric_3p9,
        df_6p6 = df_6p6,
        metric_6p6 = !!metric_6p6,
        df_9p3 = df_9p3,
        metric_9p3 = !!metric_9p3,
        full_yr = full_yr,
        full_yr_rate = FALSE,
        new_name = new_name,
        accounting = accounting,
        div_by_1000 = div_by_1000,
        prefix = prefix,
        suffix = suffix,
        spark = spark,
        pop = pop
      )
  }else{
    m <-
      fun(
        df = df,
        metric = !!metric,
        grouping = mth_num_in_yr,
        df_goal = df_goal,
        metric_goal = !!metric_goal,
        df_3p9 = df_3p9,
        metric_3p9 = !!metric_3p9,
        df_6p6 = df_6p6,
        metric_6p6 = !!metric_6p6,
        df_9p3 = df_9p3,
        metric_9p3 = !!metric_9p3,
        full_yr = full_yr,
        full_yr_rate = FALSE,
        new_name = new_name,
        accounting = accounting,
        div_by_1000 = div_by_1000,
        prefix = prefix,
        suffix = suffix,
        spark = spark,
        pop = pop
      )
  }

  # year
  y <-
    fun(
      df = df,
      metric = !!metric,
      grouping = yr_num,
      df_goal = df_goal,
      metric_goal = !!metric_goal,
      df_3p9 = df_3p9,
      metric_3p9 = !!metric_3p9,
      df_6p6 = df_6p6,
      metric_6p6 = !!metric_6p6,
      df_9p3 = df_9p3,
      metric_9p3 = !!metric_9p3,
      full_yr = FALSE,
      full_yr_rate = FALSE,
      new_name = new_name,
      accounting = accounting,
      div_by_1000 = div_by_1000,
      prefix = prefix,
      suffix = suffix,
      spark = FALSE,
      pop = FALSE
    )

  # join wmy together
  message("join")
  df <- right_join(w, m, by = c('metric' = 'metric')) %>% left_join(y, by = c('metric' = 'metric'))

  # return df
  message("return")
  df

}
