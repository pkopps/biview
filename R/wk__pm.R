wk_rpm <- function(
  df_numerator,
  numerator,
  df_denominator,
  denominator,
  show_type = FALSE,
  num_wks_to_show = 4,
  new_name = NULL,
  prefix = "",
  suffix = "",
  scaler = 1,
  round = 2
  # ,
  # sparkline = FALSE
) {

  if(missing(df_numerator)){ stop("'df_numerator' argument is mandatory") }
  if(missing(numerator)){ stop("'numerator' argument is mandatory") }
  if(missing(df_denominator)){ stop("'df_numerator' argument is mandatory") }
  if(missing(denominator)){ stop("'denominator' argument is mandatory") }

  cur_yr <- max(df_numerator$yr_num)
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

  ###

  dfn <- df_numerator %>%
    group_by(yr_num, wk_num_in_yr) %>%
    summarise_at(vars(!!numerator), funs(sum(., na.rm=TRUE)))

  # dfd <- df_denominator %>%
  #   filter(wk_eop == 1) %>%
  #   group_by(yr_num, wk_num_in_yr) %>%
  #   summarise_at(vars(!!denominator_bop, !!denominator_eop), funs(sum(., na.rm=TRUE))) %>%
  #   mutate(avg_memberbase = )

  dfd <- df_denominator %>%
    filter(wk_bop == 1 | wk_eop == 1) %>%
    select(yr_num, wk_num_in_yr, date_value, wk_bop, wk_eop, `Members (BOP)`, `Members (EOP)`) %>%
    mutate(
      bop = case_when(
        wk_bop == 1 ~ `Members (BOP)`,
        TRUE ~ as.integer(0)
      ),
      eop = case_when(
        wk_eop == 1 ~ `Members (EOP)`,
        TRUE ~ as.integer(0)
      )
    ) %>% group_by(
      yr_num, wk_num_in_yr
    ) %>%
    summarise_at(vars(bop, eop), funs(sum)) %>%
    mutate(avg_memberbase = (bop + eop) / 2)

  dfnd <- left_join(dfn, dfd)

  ###

    cur_yr_df <- dfnd %>% filter(yr_num == cur_yr) %>%
      # group_by(yr_num, wk_num_in_yr) %>%
      # summarise_at(vars(!!numerator, !!denominator), funs(sum)) %>%
      mutate(
        rate = round(scaler * ( (UQ(numerator)) / (UQ(denominator)) ), round),
        type = "actual") %>%
      filter(between(wk_num_in_yr, prev_wk - (num_wks_to_show - 1), prev_wk)) %>%
      ungroup()

    prev_yr_df <- dfnd %>% filter(yr_num == prev_yr) %>%
      # group_by(yr_num, wk_num_in_yr) %>%
      # summarise_at(vars(!!numerator, !!denominator), funs(sum)) %>%
      mutate(
        rate = round(scaler * ( (UQ(numerator)) / (UQ(denominator)) ), round),
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

  prev_yr_var_df <- prev_yr_var_df %>%
    mutate(wk_num_in_yr = paste0("w", wk_num_in_yr),
           rate_cur_yr = paste0(prefix, rate_cur_yr, suffix),
           rate_prev_yr = paste0(prefix, rate_prev_yr, suffix)
           # ,
           # rate_prev_yr_var = paste0(prefix, rate_prev_yr_var, suffix)
           )

  if(!missing(new_name)){

    prev_yr_var_df <- prev_yr_var_df %>% rename(!!new_name := rate_cur_yr,
                                                `Prior Year` = rate_prev_yr,
                                                `Variance vs. Prior Year` = rate_prev_yr_var)

  }

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
