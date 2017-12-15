# year view rate

yr_rate <- function(
  df,
  numerator,
  denominator,
  show_type = FALSE,
  run_rate = TRUE,
  new_name = NULL,
  scaler = 1,
  round = 2
  # ,
  # sparkline = FALSE
) {

  if(missing(df)){ stop("'df' argument is mandatory") }
  if(missing(numerator)){ stop("'numerator' argument is mandatory") }
  if(missing(denominator)){ stop("'denominator' argument is mandatory") }

  cur_yr <- max(df$yr_num)
  prev_yr <- cur_yr - 1

  if(!missing(new_name)) new_name <- quo_name(new_name)

  numerator <- enquo(numerator)
  numerator_name <- quo_name(numerator)

  denominator <- enquo(denominator)
  denominator_name <- quo_name(denominator)

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

  cur_yr_df <- df %>% filter(yr_num == cur_yr) %>%
    group_by(yr_num) %>%
    summarise_at(vars(!!numerator, !!denominator), funs(sum)) %>%
    mutate(
      rate_cur_yr = round(scaler * ( (UQ(numerator)) / (UQ(denominator)) ), round),
      type = "actual") %>%
    ungroup() %>%
    select(rate_cur_yr, type)

  print(cur_yr_df)

  ###### opt in to replace actual measures for run rate here ######

  if(run_rate == TRUE){

    cur_yr_df <- year_percent_run_rate(df = df, numerator = !!numerator, denominator = !!denominator)

  }

  ###

  prev_yr_df <- df %>% filter(yr_num == prev_yr) %>%
    group_by(yr_num) %>%
    summarise_at(vars(!!numerator, !!denominator), funs(sum)) %>%
    mutate(
      rate_prev_yr = round(scaler * ( (UQ(numerator)) / (UQ(denominator)) ), round),
      type = "actual") %>%
    ungroup() %>%
    select(rate_prev_yr)

  prev_yr_var_df <-
    cbind(cur_yr_df, prev_yr_df) %>%
    mutate(rate_prev_yr_var = round( (rate_cur_yr - rate_prev_yr) , 2) )

  if(!missing(new_name)){

    prev_yr_var_df <- prev_yr_var_df %>% rename(!!new_name := rate_cur_yr,
                                                `Prior Year` = rate_prev_yr,
                                                `Variance vs. Prior Year` = rate_prev_yr_var)

  }

  if(run_rate == FALSE){
    final <-
      prev_yr_var_df %>%
      gather(metric, value) %>%
      rename(YTD = value)
  }else{
    final <-
      prev_yr_var_df %>%
      gather(metric, value) %>%
      rename(`Full Year` = value)
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
