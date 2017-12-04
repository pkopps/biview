# month percent

month_percent <- function(
  df,
  numerator,
  denominator,
  run_rate = FALSE,
  show_type = FALSE
  # ,
  # op2 = FALSE
) {

  if(missing(df)){ stop("'df' argument is mandatory") }
  if(missing(numerator)){ stop("'numerator' argument is mandatory") }
  if(missing(denominator)){ stop("'denominator' argument is mandatory") }

  numerator <- enquo(numerator)
  denominator <- enquo(denominator)

  cur_yr_df <- df %>%
    filter(yr_num == cur_yr) %>%
    group_by(yr_num, mth_num_in_yr) %>%
    summarise_at(vars(!!numerator, !!denominator), funs(sum)) %>%
    mutate(
      rate_cur_yr = ( UQ(numerator) / UQ(members) ),
      type = "actual") %>%
    ungroup()

  ### INJECT MONTH RUN RATE HERE ###

  # if(run_rate == TRUE){
  #
  #   df <- df %>%
  #     filter(( yr_num != cur_yr | mth_num_in_yr != cur_month )) %>%
  #     rbind(month_run_rate(df = df, metric = !!metric))
  #
  # }

  ### -------------------------- ###

  suppressMessages({

    # if(op2 == TRUE){

      df <- full_join(df, op2 %>% select(yr_num, mth_num_in_yr, revenue_per_member_op2)) %>%
        arrange(yr_num %>% desc) %>%
        mutate(revenue_per_member_op2_var =
                  round(
                  (revenue_per_member - revenue_per_member_op2)
                   /
                  (revenue_per_member)
                  * 100, 2)
        ) %>% ungroup()

    # }

  })

  cur_yr_df <- df %>% filter(yr_num == cur_yr)
  prev_yr_df <- df %>% filter(yr_num == prev_yr)

  suppressMessages({
    prev_yr_var_df <- right_join(cur_yr_df %>% select(mth_num_in_yr, revenue_per_member, type),
                                 prev_yr_df %>% select(mth_num_in_yr, revenue_per_member),
                                 by = c("mth_num_in_yr" = "mth_num_in_yr"),
                                 suffix = c("_cur_yr","_prev_yr"))
  })

  final <- prev_yr_var_df %>% mutate(revenue_per_member_prev_yr_var =
                                                round((revenue_per_member_cur_yr - revenue_per_member_prev_yr)/(revenue_per_member_cur_yr) * 100, 2)
                                              #   ,
                                              # trials_prev_yr_var =
                                              #   ((trials_cur_yr - trials_prev_yr)/trials_cur_yr) * 100)
  )

  print(df)

  suppressMessages({

    # if(op2 == TRUE){

    final <- left_join(final,
                       df %>% filter(yr_num == cur_yr) %>%
                         select(mth_num_in_yr, revenue_per_member_op2, revenue_per_member_op2_var))

    # }

  })

  final <- final %>% gather(metric, value, -mth_num_in_yr) %>%
    spread(mth_num_in_yr, value)

  if(show_type){
    final
  }
  else{
    final %>% filter(metric != "type")
  }

}

# month_rate_view(df, num = revenue, denom = members, show_type = TRUE) %>% View()
