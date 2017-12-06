# year run rate

year_run_rate <- function(
  df,
  metric
){

  if(missing(df)){ stop("'df' argument is mandatory") }
  if(missing(metric)){ stop("'metric' argument is mandatory") }

  metric <- enquo(metric)
  metric_name <- quo_name(metric)

  metric_cur_yr_name <- as.name(paste(metric_name, "cur_yr", sep = "_"))
  metric_prev_yr_name <- as.name(paste(metric_name, "prev_yr", sep = "_"))

  cur_yr <- max(df$yr_num)
  prev_yr <- cur_yr - 1
  # cur_mth <- max(df$mth_num_in_yr)
  cur_mth <- df %>% filter(yr_num == cur_yr) %>% summarise(max(mth_num_in_yr)) %>% pull()
  prev_mth <- cur_mth - 1
  today <- max(df$date_value)
  today_prev_mth <- today - 30
  today_prev_yr <- today - 365

  prev_yr_actuals_df <- df %>% filter(
    yr_num == prev_yr,
    date_value > today_prev_yr
  )

  cur_yr_actuals_df <- df %>% filter(
    yr_num == cur_yr,
    date_value <= today
  )

  final <- rbind(prev_yr_actuals_df, cur_yr_actuals_df) %>%
    mutate(yr_num = max(yr_num)) %>%
    group_by(yr_num) %>%
    ungroup() %>%
    select(-yr_num) %>%
    summarise_at(vars(!!metric), funs(round_sum)) %>%
    rename(!!metric_cur_yr_name := !!metric) %>%
    mutate(type = "run_rate")

  final

}


