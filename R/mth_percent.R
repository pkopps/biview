# month percent

month_percent <- function(
  df,
  numerator,
  denominator,
  df_op2,
  metric_op2,
  run_rate = FALSE,
  show_type = FALSE,
  new_name = NULL
  # ,
  # op2 = FALSE
) {

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

  ###### define order of metrics for output ######

  if(!missing(new_name)){
    ordering_array <- c("type",
                        new_name,
                        "Prior Year",
                        "Variance vs. Prior Year")
  }else{
    ordering_array <- c("type",
                        "rate_cur_yr",
                        "rate_prev_yr",
                        "rate_prev_yr_var")
  }

  ###

  cur_yr_df <- df %>%
    filter(yr_num == cur_yr) %>%
    group_by(yr_num, mth_num_in_yr) %>%
    summarise_at(vars(!!numerator, !!denominator), funs(sum)) %>%
    mutate(
      rate_cur_yr = round( 100 * ( UQ(numerator) / UQ(denominator) ), 2),
      type = "actual") %>%
    ungroup() %>%
    select(-yr_num, -!!numerator, -!!denominator)

  ###### opt in to replace actual measures for run rate here ######

  if(run_rate == TRUE){

    cur_yr_df <- month_percent_run_rate(df = df, numerator = !!numerator, denominator = !!denominator)

  }

  ###

  prev_yr_df <- df %>% filter(yr_num == prev_yr) %>%
    group_by(yr_num, mth_num_in_yr) %>%
    summarise_at(vars(!!numerator, !!denominator), funs(sum)) %>%
    mutate(
      rate_prev_yr = round( 100 * ( UQ(numerator) / UQ(denominator) ), 2),
      type = "actual") %>%
    ungroup() %>%
    select(rate_prev_yr)

  final <-
    cbind(cur_yr_df, prev_yr_df) %>%
    mutate(rate_prev_yr_var = round( (rate_cur_yr - rate_prev_yr) , 2) )

  ###### Change names of OP2 columns if OP2 arguments are provided ######

  if(!missing(new_name) & missing(metric_op2)){

    final <- final %>%
      rename(
        !!new_name := rate_cur_yr,
        `Prior Year` = rate_prev_yr,
        `Variance vs. Prior Year` = rate_prev_yr_var
      ) %>%
      mutate(
        `OP2 Plan` = NA,
        `Variance vs. Plan` = NA
      )

  }else if(!missing(new_name) & !missing(metric_op2)){

    final <- final %>%
      rename(
        !!new_name := UQ(rate_cur_yr),
        `Prior Year` = UQ(rate_prev_yr),
        `Variance vs. Prior Year` = UQ(rate_prev_yr_var),
        `OP2 Plan` = UQ(metric_op2_name), ## TOCHANGE
        `Variance vs. Plan` = UQ(metric_op2_var_name) ## TOCHANGE
      )

  }

  ###

  if(run_rate == FALSE){
    final <-
      final %>%
      gather(metric, value, -mth_num_in_yr) %>%
      spread(mth_num_in_yr, value)
    # %>%
    #   rename(cur_mth = value)

  }else{
    final <-
      final %>%
      rename(cur_mth = value) %>%
      gather(metric, value, -mth_num_in_yr) %>%
      spread(mth_num_in_yr, value)

  }

  if(show_type){
    final <- final
  }
  else{
    final <- final %>% filter(metric != "type")
  }

  final <- final %>% arrange(
    metric = ordered(metric, levels = ordering_array)
  )

  final

}
