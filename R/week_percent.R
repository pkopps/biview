# week percent

week_percent_view <- function(
  df,
  numerator,
  denominator,
  show_type = FALSE,
  num_wks_to_show = 4,
  new_name = NULL
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
    group_by(yr_num, wk_num_in_yr) %>%
    summarise_at(vars(!!numerator, !!denominator), funs(sum)) %>%
    mutate(
      rate = round(100 * ( (UQ(numerator)) / (UQ(denominator)) ), 2),
      type = "actual") %>%
    filter(between(wk_num_in_yr, prev_wk - (num_wks_to_show - 1), prev_wk)) %>%
    ungroup()

  prev_yr_df <- df %>% filter(yr_num == prev_yr) %>%
    group_by(yr_num, wk_num_in_yr) %>%
    summarise_at(vars(!!numerator, !!denominator), funs(sum)) %>%
    mutate(
      rate = round(100 * ( (UQ(numerator)) / (UQ(denominator)) ), 2),
      type = "actual") %>%
    filter(between(wk_num_in_yr, prev_wk - (num_wks_to_show - 1), prev_wk)) %>%
    ungroup()

  prev_yr_var_df <-
    right_join(
      cur_yr_df %>% select(wk_num_in_yr, rate, type),
      prev_yr_df %>% select(wk_num_in_yr, rate),
      by = c("wk_num_in_yr" = "wk_num_in_yr"),
      suffix = c("_cur_yr","_prev_yr")
      ) %>%
    mutate(rate_prev_yr_var = round( (rate_cur_yr - rate_prev_yr) , 2) ) ## "3 ppts"

  if(!missing(new_name)){

    prev_yr_var_df <- prev_yr_var_df %>% rename(!!new_name := rate_cur_yr,
                                                `Prior Year` = rate_prev_yr,
                                                `Variance vs. Prior Year` = rate_prev_yr_var)

  }

  prev_yr_var_df <- prev_yr_var_df %>%
    mutate(wk_num_in_yr = paste0("w", wk_num_in_yr))

  final <- prev_yr_var_df %>%
    gather(metric, value, -wk_num_in_yr) %>%
    spread(wk_num_in_yr, value)

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
