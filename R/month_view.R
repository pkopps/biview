# month

month_view <- function(
  df,
  metric,
  df_op2,
  metric_op2,
  df_3p9,
  metric_3p9,
  df_6p6,
  metric_6p6,
  df_9p3,
  metric_9p3,
  run_rate = TRUE,
  show_type = FALSE,
  new_name = NULL
) {

###### message to clarify ACTUAL vs RUN RATE ######

  if(run_rate == FALSE) message(glue("Month {cur_mth} (current month) value is ACTUAL")) else message(glue("Month {cur_mth} (current month) value is RUN RATE"))

###

###### Error messaging ######

  if(missing(df)){ stop("'df' argument is mandatory") }
  if(missing(metric)){ stop("'metric' argument is mandatory") }

###

  cur_yr <- max(df$yr_num)
  prev_yr <- cur_yr - 1
  # cur_mth <- max(df$mth_num_in_yr)
  cur_mth <- df %>% filter(yr_num == cur_yr) %>% summarise(max(mth_num_in_yr)) %>% pull()
  prev_mth <- cur_mth - 1
  today <- max(df$date_value)
  today_prev_mth <- today - 30

###### create quosures and quosure names to use non-standardly ######

  if(!missing(new_name)) new_name <- quo_name(new_name)

  metric <- enquo(metric)
  metric_name <- quo_name(metric)

  metric_cur_yr_name <- as.name(paste(metric_name, "cur_yr", sep = "_"))
  # metric_cur_yr <- enquo(metric_cur_yr_var_name)
  metric_prev_yr_name <- as.name(paste(metric_name, "prev_yr", sep = "_"))
  # metric_prev_yr <- enquo(metric_prev_yr_var_name)

  metric_prev_yr_var_name <- paste(metric_name, "prev_yr_var", sep = "_")
  metric_prev_yr_var <- enquo(metric_prev_yr_var_name)

if(!missing(metric_op2)){

  metric_op2 <- enquo(metric_op2)
  metric_op2_name <- quo_name(metric_op2)

  metric_op2_var_name <- paste(metric_name, "op2_var", sep = "_")
  metric_op2_var <- enquo(metric_op2_var_name)

}

if(!missing(metric_3p9)){

    metric_3p9 <- enquo(metric_3p9)
    metric_3p9_name <- quo_name(metric_3p9)

  }

###### define order of metrics for output ######

  if(missing(new_name) & missing(metric_op2)){
    ordering_array <- c("type",
                        metric_cur_yr_name,
                        metric_prev_yr_name,
                        metric_prev_yr_var_name
                        # ,
                        # metric_op2_name,
                        # metric_op2_var_name
                        )
  }else if(missing(new_name) & !missing(metric_op2)){
    ordering_array <- c("type",
                        metric_cur_yr_name,
                        metric_prev_yr_name,
                        metric_prev_yr_var_name,
                        metric_op2_name,
                        metric_op2_var_name
    )
  }else if(!missing(new_name) & missing(metric_op2)){
    ordering_array <- c("type",
                        new_name,
                        "Prior Year",
                        "Variance vs. Prior Year")
  }else if(!missing(new_name) & !missing(metric_op2)){
    ordering_array <- c("type",
                        new_name,
                        "Prior Year",
                        "Variance vs. Prior Year",
                        "OP2 Plan",
                        "Variance vs. Plan")
  }
  # else{
  #   ordering_array <- NULL
  # }

###

###### opt in to replace actual measures for run rate here ######

if(run_rate == TRUE){

  rr <- month_run_rate(df = df, metric = !!metric)

  df <- df %>%
    group_by(yr_num, mth_num_in_yr) %>% # group by year and month
    summarise_at(vars(!!metric), funs(round_sum)) %>% # get sum of metric per year and month
    mutate(type = "actual") %>% # create column indicating these are actuals
    ungroup()

  cur_yr_df <- df %>%
    filter(yr_num == cur_yr) %>%
    filter(mth_num_in_yr != cur_mth ) %>%
    bind_rows(rr) %>%
    select(-yr_num) %>%
    rename(!!metric_cur_yr_name := UQ(metric_name))

  prev_yr_df <- df %>%
    filter(yr_num == prev_yr) %>%
    select(-yr_num, -type) %>%
    rename(!!metric_prev_yr_name := UQ(metric_name))

}else{

  df <- df %>%
    group_by(yr_num, mth_num_in_yr) %>% # group by year and month
    summarise_at(vars(!!metric), funs(round_sum)) %>% # get sum of metric per year and month
    mutate(type = "actual") %>% # create column indicating these are actuals
    ungroup()

  cur_yr_df <- df %>%
    filter(yr_num == cur_yr) %>%
    select(-yr_num) %>%
    rename(!!metric_cur_yr_name := UQ(metric_name))

  prev_yr_df <- df %>%
    filter(yr_num == prev_yr) %>%
    select(-yr_num, -type) %>%
    rename(!!metric_prev_yr_name := UQ(metric_name))

}

###

###### opt in to add op2 view ######

if(!missing(metric_op2)){

  cur_yr_df <-
    full_join(
      cur_yr_df,
      df_op2 %>% select(mth_num_in_yr, !!metric_op2),
      by = c("mth_num_in_yr")
      ) %>%
    mutate(
      !!metric_op2_var_name := round(
      100 * ( (UQ(metric_cur_yr_name)) - (!!metric_op2)
                # (UQ(metric_op2_name))
              ) # numerator
      /
      (UQ(metric_cur_yr_name)), # denominator
      2)
    )

}

###

###### opt in to add 3 + 9 ######

if(!missing(metric_3p9)){ ## if metric_3p9 is provided

  cur_yr_df <-
    full_join(
      df,
      df_3p9 %>% select(yr_num, mth_num_in_yr, !!metric_3p9),
      by = c("yr_num", "mth_num_in_yr")
      ) %>%
    ungroup()

}

###

###### previous year variance calculation ######

final <- right_join(
  cur_yr_df %>% ungroup(),
  prev_yr_df %>% ungroup(), # isolate last year's data
  by = "mth_num_in_yr",
  suffix = c("_cur_yr", "_prev_yr")) %>%
  # mutate_at(vars(UQ(metric_cur_yr_name), UQ(metric_prev_yr_name)), funs(div_by_one_thousand(.))) %>%
  # mutate_at(vars(UQ(metric_cur_yr_name), UQ(metric_prev_yr_name)), funs(round_to_two(.))) %>%
  mutate(
    !!metric_prev_yr_var_name :=
      round( 100 * ( (UQ(metric_cur_yr_name)) - (UQ(metric_prev_yr_name)) )
      /
      (UQ(metric_prev_yr_name)), 2 )
    ) %>%
  ungroup()

###

###### Inlcude OP2 columns if OP2 arguments are provided ######

# l <- prev_yr_var_df %>% ungroup()
# r <- df %>% filter(yr_num == cur_yr)
#
# print(l)
# print(r)

# if(!missing(metric_op2)){
#
#   final <- left_join(
#     l,
#     r %>% ungroup()
#     # %>% select(mth_num_in_yr, !!metric_op2, !!metric_op2_var)
#     ,
#     by = "mth_num_in_yr"
#     )
#
# }else{
#
#   final <- left_join(
#     l,
#     r %>% ungroup()
#     # %>% select(mth_num_in_yr)
#     ,
#     by = "mth_num_in_yr")
#
# }

###

###### 3 + 9 TODO ######


###

###### Change names of OP2 columns if OP2 arguments are provided ######

if(!missing(new_name) & missing(metric_op2)){

  final <- final %>%
    rename(!!new_name := UQ(metric_cur_yr_name),
          `Prior Year` = UQ(metric_prev_yr_name),
          `Variance vs. Prior Year` = UQ(metric_prev_yr_var_name)) %>%
    mutate(`OP2 Plan` = NA,
           `Variance vs. Plan` = NA)

}else if(!missing(new_name) & !missing(metric_op2)){

  final <- final %>%
    rename(!!new_name := UQ(metric_cur_yr_name),
          `Prior Year` = UQ(metric_prev_yr_name),
          `Variance vs. Prior Year` = UQ(metric_prev_yr_var_name),
          `OP2 Plan` = UQ(metric_op2_name),
          `Variance vs. Plan` = UQ(metric_op2_var_name)
          )

}

###

  final <- final %>% gather(metric, value, -mth_num_in_yr) %>%
    spread(mth_num_in_yr, value)


###### opt in to hiding type row, predominantly for reporting ######

if(show_type){
  final <- final
  }
else{
  final <- final %>% filter(metric != "type")
  } # hide type row

###

###### order metrics for view & return #######

final %>% arrange(
  metric = ordered(metric, levels = ordering_array)
  )

###

}


