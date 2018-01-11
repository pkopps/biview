#my_view

my_view_2e <- function(
  df_mth,
  df_yr,
  metric,
  df_op2,
  metric_op2,
  mth_rr = FALSE,
  show_type = FALSE,
  num_wks_to_show = 4,
  new_name,
  div_by_one_thousand = FALSE
                  ){

if(missing(metric_op2)){

    metric <- enquo(metric)
    # metric_op2 <- enquo(metric_op2)

    mth_view(
      df = df_mth,
      metric = !!metric,
      # df_op2 = df_op2,
      # metric_op2 = !!metric_op2,
      mth_rr = mth_rr,
      show_type = show_type,
      new_name = new_name,
      div_by_one_thousand = div_by_one_thousand
    ) -> month_res

    yr_view(
      df = df_yr,
      metric = !!metric,
      # df_op2 = df_op2,
      # metric_op2 = !!metric_op2,
      show_type = show_type,
      new_name = new_name
    ) -> year_res_actual

    ret <-
      # right_join(week_res, month_res, by = 'metric') %>%
      month_res %>%
      left_join(year_res_actual, by = 'metric') %>%
      # left_join(year_res_rr, by = 'metric') %>%
      mutate_all(funs(replace(., is.na(.), "NA")))

  }

else if(!missing(metric_op2)){

  metric <- enquo(metric)
  metric_op2 <- enquo(metric_op2)

  mth_view(
    df = df_mth,
    metric = !!metric,
    df_op2 = df_op2,
    metric_op2 = !!metric_op2,
    mth_rr = mth_rr,
    show_type = show_type,
    new_name = new_name,
    div_by_one_thousand = div_by_one_thousand
  ) -> month_res

  yr_view(
    df = df_yr,
    metric = !!metric,
    df_op2 = df_op2,
    metric_op2 = !!metric_op2,
    show_type = show_type,
    new_name = new_name
  ) -> year_res_actual

  ret <-
    # right_join(week_res, month_res, by = 'metric') %>% # right join because week_res does not have op2 data
    month_res %>%
    left_join(year_res_actual, by = 'metric') %>%
    # left_join(year_res_rr, by = 'metric') %>%
    mutate_all(funs(replace(., is.na(.), "NA")))

}

  ret

}

