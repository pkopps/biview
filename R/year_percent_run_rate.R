# year percent run rate

year_percent_run_rate <- function(
  df,
  numerator,
  denominator
){

  if(missing(df)){ stop("'df' argument is mandatory") }
  if(missing(numerator)){ stop("'numerator' argument is mandatory") }
  if(missing(denominator)){ stop("'denominator' argument is mandatory") }

  numerator <- enquo(numerator)
  numerator_name <- quo_name(numerator)

  denominator <- enquo(denominator)
  denominator_name <- quo_name(denominator)

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
    summarise_at(vars(!!numerator, !!denominator), funs(round_sum)) %>%
    mutate(
      rate_cur_yr = round(100 * ( (UQ(numerator)) / (UQ(denominator)) ), 2),
      type = "run_rate"
      ) %>%
    select(rate_cur_yr, type)

  final

}


