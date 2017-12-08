# perc_of_total_wmy_view

perc_of_total_wmy_view <- function(
  df,
  metric,
  df_op2,
  metric_op2,
  group_col,
  filter_val,
  # run_rate = FALSE,
  # show_type = FALSE,
  new_name
){

  if(missing(metric_op2)){

    metric <- enquo(metric)
    group_col <- enquo(group_col)

    perc_of_total_wk_view(
      df = df,
      metric = !!metric,
      group_col = !!group_col,
      filter_val = filter_val,
      new_name = new_name
    ) -> week_res

    perc_of_total_mth_view(
      df = df,
      metric = !!metric,
      group_col = !!group_col,
      filter_val = filter_val,
      new_name = new_name
    ) -> month_res

    # year_view(
    #   df = df,
    #   metric = !!metric,
    #   # df_op2 = df_op2,
    #   # metric_op2 = !!metric_op2,
    #   show_type = show_type,
    #   new_name = new_name,
    #   run_rate = FALSE
    # ) -> year_res_actual

    # year_view(
    #   df = df,
    #   metric = !!metric,
    #   # df_op2 = df_op2,
    #   # metric_op2 = !!metric_op2,
    #   show_type = show_type,
    #   new_name = new_name,
    #   run_rate = run_rate
    # ) -> year_res_rr

    ret <- right_join(week_res, month_res, by = 'metric') %>%
      # left_join(year_res_actual, by = 'metric') %>%
      # left_join(year_res_rr, by = 'metric') %>%
      mutate_all(funs(replace(., is.na(.), "")))

  }

  else if(!missing(metric_op2)){

    metric <- enquo(metric)
    metric_op2 <- enquo(metric_op2)

    perc_of_total_wk_view(
      df = df,
      metric = !!metric,
      group_col = !!group_col,
      filter_val = filter_val,
      new_name = new_name
    ) -> week_res

    perc_of_total_mth_view(
      df = df,
      metric = !!metric,
      group_col = !!group_col,
      filter_val = filter_val,
      new_name = new_name
    ) -> month_res

    # year_view(
    #   df = df,
    #   metric = !!metric,
    #   df_op2 = df_op2,
    #   metric_op2 = !!metric_op2,
    #   show_type = show_type,
    #   new_name = new_name,
    #   run_rate = FALSE
    # ) -> year_res_actual
    #
    # year_view(
    #   df = df,
    #   metric = !!metric,
    #   df_op2 = df_op2,
    #   metric_op2 = !!metric_op2,
    #   show_type = show_type,
    #   new_name = new_name,
    #   run_rate = run_rate
    # ) -> year_res_rr

    right_join(week_res, month_res, by = 'metric') %>% # right join because week_res does not have op2 data
      # left_join(year_res_actual, by = 'metric') %>%
      # left_join(year_res_rr, by = 'metric') %>%
      mutate_all(funs(replace(., is.na(.), "")))

  }

}

