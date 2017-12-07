#wmy_view

#' Transform regular data into the BI view of weeks, next to months, next to years,
#' with previous year variance, OP2, etc.
#'
#' @param df A data frame with date dimensions
#' @param metric Column to be group and summate on
#' @param df_op2 The same data frame from df or another one if OP2 data is stored in another one
#' @param metric_op2 Corresponding OP2 column to be group and summate on
#' @param run_rate Should current month and year be run rate instead of actual?
#'
#' @return transformed data frame
#' @examples
#'
#'

wmy_view <- function(
  df,
  metric,
  df_op2,
  metric_op2,
  run_rate = TRUE,
  show_type = FALSE,
  num_wks_to_show = 4,
  new_name
                  ){

if(missing(metric_op2)){

    metric <- enquo(metric)
    # metric_op2 <- enquo(metric_op2)

    week_view(
      df = df,
      metric = !!metric,
      show_type = show_type,
      num_wks_to_show = num_wks_to_show,
      new_name = new_name
    ) -> week_res

    month_view(
      df = df,
      metric = !!metric,
      # df_op2 = df_op2,
      # metric_op2 = !!metric_op2,
      run_rate = run_rate, show_type = show_type,
      new_name = new_name
    ) -> month_res

    year_view(
      df = df,
      metric = !!metric,
      # df_op2 = df_op2,
      # metric_op2 = !!metric_op2,
      show_type = show_type,
      new_name = new_name,
      run_rate = FALSE
    ) -> year_res_actual

    year_view(
      df = df,
      metric = !!metric,
      # df_op2 = df_op2,
      # metric_op2 = !!metric_op2,
      show_type = show_type,
      new_name = new_name,
      run_rate = run_rate
    ) -> year_res_rr

    ret <- right_join(week_res, month_res, by = 'metric') %>%
      left_join(year_res_actual, by = 'metric') %>%
      left_join(year_res_rr, by = 'metric') %>%
      mutate_all(funs(replace(., is.na(.), "")))

  }

else if(!missing(metric_op2)){

  metric <- enquo(metric)
  metric_op2 <- enquo(metric_op2)

  week_view(
    df = df,
    metric = !!metric,
    show_type = show_type,
    num_wks_to_show = num_wks_to_show,
    new_name = new_name
  ) -> week_res

  month_view(
    df = df,
    metric = !!metric,
    df_op2 = df_op2,
    metric_op2 = !!metric_op2,
    run_rate = run_rate, show_type = show_type,
    new_name = new_name
  ) -> month_res

  year_view(
    df = df,
    metric = !!metric,
    df_op2 = df_op2,
    metric_op2 = !!metric_op2,
    show_type = show_type,
    new_name = new_name,
    run_rate = FALSE
  ) -> year_res_actual

  year_view(
    df = df,
    metric = !!metric,
    df_op2 = df_op2,
    metric_op2 = !!metric_op2,
    show_type = show_type,
    new_name = new_name,
    run_rate = run_rate
  ) -> year_res_rr

  right_join(week_res, month_res, by = 'metric') %>% # right join because week_res does not have op2 data
    left_join(year_res_actual, by = 'metric') %>%
    left_join(year_res_rr, by = 'metric') %>%
    mutate_all(funs(replace(., is.na(.), "")))

}

}

