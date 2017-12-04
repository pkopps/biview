# magic percent

magic_percent <- function(df,
                  numerator,
                  denominator,
                  # df_op2,
                  # metric_op2,
                  run_rate = TRUE,
                  show_type = FALSE,
                  num_wks_to_show = 4,
                  new_name
){

  numerator <- enquo(numerator)
  denominator <- enquo(denominator)

  # metric_op2 <- enquo(metric_op2)

  # numerator_op2 <- enquo(numerator_op2)
  # denominator_op2 <- enquo(denominator_op2)

  week_percent(
    df = df,
    numerator = !!numerator,
    denominator = !!denominator,
    show_type = show_type,
    num_wks_to_show = num_wks_to_show,
    new_name = new_name
  ) -> week_res

  month_percent(
    df = df,
    numerator = !!numerator,
    denominator = !!denominator,
    show_type = show_type,
    run_rate = FALSE,
    new_name = new_name
  ) -> month_res

  year_view_rate(
    df = df,
    numerator = !!numerator,
    denominator = !!denominator,
    show_type = show_type,
    run_rate = FALSE,
    new_name = new_name
  ) -> year_res_actual

  year_view_rate(
    df = df,
    numerator = !!numerator,
    denominator = !!denominator,
    show_type = show_type,
    run_rate = run_rate,
    new_name = new_name
  ) -> year_res_rr

  right_join(week_res, month_res, by = 'metric') %>%
    left_join(year_res_actual, by = 'metric') %>%
    left_join(year_res_rr, by = 'metric') %>%
    mutate_all(funs(replace(., is.na(.), "")))

  # left_join(week_res, year_res_actual, by = 'metric') %>% left_join(year_res_rr, by = 'metric')

}


