#' [w]eek [m]onth [y]ear _ [3] [e]xports
#'
#' @param df_wk weekly grain data frame
#' @param df_mth monthly grain data frame
#' @param df_yr yearly grain data frame
#' @param metric column name for metric (uniform across all the grains of data frames)
#' @param df_rr data frame containing run rate
#' @param metric_rr column name from df_rr
#' @param df_goal
#' @param metric_goal
#' @param df_3p9
#' @param metric_3p9
#' @param df_6p6
#' @param metric_6p6
#' @param df_9p3
#' @param metric_9p3
#' @param week_end_dates
#' @param week_start_dates
#' @param full_yr
#' @param cbr_ytd
#' @param rate
#' @param new_name
#' @param accounting
#' @param div_by_1000
#' @param prefix
#' @param suffix
#' @param scalar
#' @param spark
#' @param pop
#' @param pop_threshold
#' @param in_mth_header_op2
#' @param in_mth_header_3p9
#' @param in_mth_header_6p6
#' @param in_mth_header_9p3
#'
#' @return
#' @export
#'
#' @examples
wmy_3e <- function(
  df_wk,
  df_mth,
  df_yr,
  metric,
  # grouping,
  df_rr,
  metric_rr,
  df_goal,
  metric_goal,
  df_3p9,
  metric_3p9,
  df_6p6,
  metric_6p6,
  df_9p3,
  metric_9p3,
  week_end_dates = FALSE,
  week_start_dates = TRUE,
  full_yr = TRUE,
  cbr_ytd = TRUE,
  rate = FALSE,
  new_name = NULL,
  accounting = TRUE,
  div_by_1000 = TRUE,
  prefix = "",
  suffix = "",
  scalar = 1,
  spark = FALSE,
  pop = FALSE,
  pop_threshold = 100,
  in_mth_header_op2 = TRUE,
  in_mth_header_3p9 = FALSE,
  in_mth_header_6p6 = FALSE,
  in_mth_header_9p3 = FALSE
  ){

  metric <- enquo(metric)
  metric_rr <- enquo(metric_rr)
  metric_goal <- enquo(metric_goal)
  metric_3p9 <- enquo(metric_3p9)
  metric_6p6 <- enquo(metric_6p6)
  metric_9p3 <- enquo(metric_9p3)

  # week
  # message("week")
  w <- # no goals or predictions
    fun(
      df = df_wk,
      metric = !!metric,
      grouping = wk_num_in_yr,
      full_yr = FALSE,
      rate = FALSE,
      week_end_dates = week_end_dates,
      week_start_dates = week_start_dates,
      new_name = new_name,
      accounting = accounting,
      div_by_1000 = div_by_1000,
      prefix = prefix,
      suffix = suffix,
      scalar = scalar,
      spark = spark,
      pop = pop,
      pop_threshold = pop_threshold
    )

  # month
  # message("month")
  if(!missing(df_goal)){
    m <-
      fun(
        df = df_mth,
        metric = !!metric,
        grouping = mth_num_in_yr,
        df_rr = df_rr,
        metric_rr = !!metric_rr,
        df_goal = df_goal,
        metric_goal = !!metric_goal,
        df_3p9 = df_3p9,
        metric_3p9 = !!metric_3p9,
        df_6p6 = df_6p6,
        metric_6p6 = !!metric_6p6,
        df_9p3 = df_9p3,
        metric_9p3 = !!metric_9p3,
        full_yr = full_yr,
        cbr_ytd = cbr_ytd,
        rate = rate,
        new_name = new_name,
        accounting = accounting,
        div_by_1000 = div_by_1000,
        prefix = prefix,
        suffix = suffix,
        scalar = scalar,
        spark = spark,
        pop = pop,
        pop_threshold = pop_threshold,
        in_mth_header_op2 = in_mth_header_op2
      )
  }else{
    m <-
      fun(
        df = df_mth,
        metric = !!metric,
        grouping = mth_num_in_yr,
        df_rr = df_rr,
        metric_rr = !!metric_rr,
        # df_goal = df_goal,
        # metric_goal = !!metric_goal,
        df_3p9 = df_3p9,
        metric_3p9 = !!metric_3p9,
        df_6p6 = df_6p6,
        metric_6p6 = !!metric_6p6,
        df_9p3 = df_9p3,
        metric_9p3 = !!metric_9p3,
        full_yr = full_yr,
        cbr_ytd = cbr_ytd,
        rate = rate,
        new_name = new_name,
        accounting = accounting,
        div_by_1000 = div_by_1000,
        prefix = prefix,
        suffix = suffix,
        scalar = scalar,
        spark = spark,
        pop = pop,
        pop_threshold = pop_threshold,
        in_mth_header_op2 = in_mth_header_op2
      )
  }

  # year
  # message("year")
  # y <-
  #   fun(
  #     df = df_yr,
  #     metric = !!metric,
  #     grouping = yr_num,
  #     df_goal = df_goal,
  #     metric_goal = !!metric_goal,
  #     df_3p9 = df_3p9,
  #     metric_3p9 = !!metric_3p9,
  #     df_6p6 = df_6p6,
  #     metric_6p6 = !!metric_6p6,
  #     df_9p3 = df_9p3,
  #     metric_9p3 = !!metric_9p3,
  #     full_yr = FALSE,
  #     rate = FALSE,
  #     new_name = new_name,
  #     accounting = accounting,
  #     div_by_1000 = div_by_1000,
  #     prefix = prefix,
  #     suffix = suffix,
  #     scalar = scalar,
  #     spark = FALSE,
  #     pop = FALSE
  #   )

  # join wmy together
  # message("join")
  df <- right_join(w, m, by = c('metric' = 'metric'))
  # %>% left_join(y, by = c('metric' = 'metric'))
  # df1 <- df %>% select(YTD, `Full Year`)
  # df2 <- df %>% select(-YTD, -`Full Year`)
  # df <- cbind(df2, df1)

  # return df
  # message("return")
  df

}
