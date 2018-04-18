my_2e <- function(
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
  full_yr = TRUE,
  rate = FALSE,
  new_name = NULL,
  commas = FALSE,
  neg_var_paren = FALSE,
  div_by_1000 = FALSE,
  prefix = "",
  suffix = "",
  spark = FALSE,
  pop = FALSE,
  digitsAfterDecimal = 2,
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

  # month
  # message("month")
  if(missing(df_rr) & missing(df_goal)){
    m <-
      fun(
        df = df_mth,
        metric = !!metric,
        grouping = mth_num_in_yr,
        # df_rr = df_rr,
        # metric_rr = !!metric_rr,
        # df_goal = df_goal,
        # metric_goal = !!metric_goal,
        df_3p9 = df_3p9,
        metric_3p9 = !!metric_3p9,
        df_6p6 = df_6p6,
        metric_6p6 = !!metric_6p6,
        df_9p3 = df_9p3,
        metric_9p3 = !!metric_9p3,
        full_yr = full_yr,
        rate = rate,
        new_name = new_name,
        commas = commas,
        neg_var_paren = neg_var_paren,
        div_by_1000 = div_by_1000,
        prefix = prefix,
        suffix = suffix,
        spark = spark,
        pop = pop,
        digitsAfterDecimal = digitsAfterDecimal,
        in_mth_header_op2 = in_mth_header_op2
      )
  }else if(missing(df_rr)){
    m <-
      fun(
        df = df_mth,
        metric = !!metric,
        grouping = mth_num_in_yr,
        # df_rr = df_rr,
        # metric_rr = !!metric_rr,
        df_goal = df_goal,
        metric_goal = !!metric_goal,
        df_3p9 = df_3p9,
        metric_3p9 = !!metric_3p9,
        df_6p6 = df_6p6,
        metric_6p6 = !!metric_6p6,
        df_9p3 = df_9p3,
        metric_9p3 = !!metric_9p3,
        full_yr = full_yr,
        rate = rate,
        new_name = new_name,
        commas = commas,
        neg_var_paren = neg_var_paren,
        div_by_1000 = div_by_1000,
        prefix = prefix,
        suffix = suffix,
        spark = spark,
        pop = pop,
        digitsAfterDecimal = digitsAfterDecimal,
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
        df_goal = df_goal,
        metric_goal = !!metric_goal,
        df_3p9 = df_3p9,
        metric_3p9 = !!metric_3p9,
        df_6p6 = df_6p6,
        metric_6p6 = !!metric_6p6,
        df_9p3 = df_9p3,
        metric_9p3 = !!metric_9p3,
        full_yr = full_yr,
        rate = rate,
        new_name = new_name,
        commas = commas,
        neg_var_paren = neg_var_paren,
        div_by_1000 = div_by_1000,
        prefix = prefix,
        suffix = suffix,
        spark = spark,
        pop = pop,
        digitsAfterDecimal = digitsAfterDecimal
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
  #     spark = FALSE,
  #     pop = FALSE
  #   )

  # join my together
  # message("join")
  # df <- left_join(m, y, by = c('metric' = 'metric'))
  df <- m
  # df1 <- df %>% select(YTD, `Full Year`)
  # df2 <- df %>% select(-YTD, -`Full Year`)
  # df <- cbind(df2, df1)

  # return df
  # message("return")
  df

}
