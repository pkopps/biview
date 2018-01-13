#year_view

yr_view <- function(
  df,
  metric,
  df_op2,
  metric_op2,
  show_type = FALSE,
  new_name = NULL,
  yr_rr = FALSE,
  rate = FALSE,
  div_by_one_thousand = TRUE,
  accounting = TRUE,
  suffix = ""
  # ,
  # funct = sum
  # ,
  # sparkline = FALSE
  ) {

  ###### message to clarify ACTUAL vs RUN RATE ######

  if(yr_rr == FALSE){
    message(glue("Year {cur_yr} (current year) value is ACTUAL"))
  }
  else{
    message(glue("Year {cur_yr} (current year) value is RUN RATE"))
  }

  ###

  if(missing(df)){ stop("'df' argument is mandatory") }
  if(missing(metric)){ stop("'metric' argument is mandatory") }

  if(!missing(new_name)) new_name <- quo_name(new_name)

  funct <- enquo(funct)

  metric <- enquo(metric)
  metric_name <- quo_name(metric)

  metric_cur_yr_name <- as.name(paste(metric_name, "cur_yr", sep = "_"))
  metric_prev_yr_name <- as.name(paste(metric_name, "prev_yr", sep = "_"))

  metric_prev_yr_var_name <- paste(metric_name, "prev_yr_var", sep = "_")
  metric_prev_yr_var <- enquo(metric_prev_yr_var_name)

  metric_prev_yr_var_name <- paste(metric_name, "prev_yr_var", sep = "_")

if(!missing(metric_op2)){

  metric_op2 <- enquo(metric_op2)
  metric_op2_name <- quo_name(metric_op2)

  metric_op2_var_name <- paste(metric_name, "op2_var", sep = "_")
  metric_op2_var <- enquo(metric_op2_var_name)

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

  cur_yr_df <- df %>% filter(yr_num == cur_yr) %>%
    group_by(yr_num) %>%
    summarise_at(vars(!!metric), funs(round_sum)) %>%
    mutate(type = "actual") %>%
    rename(!!metric_cur_yr_name := !!metric) %>%
    select(-yr_num)

  prev_yr_df <- df %>% filter(yr_num == prev_yr) %>%
    group_by(yr_num) %>%
    summarise_at(vars(!!metric), funs(round_sum)) %>%
    mutate(type = "actual") %>%
    rename(!!metric_prev_yr_name := !!metric) %>%
    select(-yr_num, -type)

  ###### opt in to replace actual measures for run rate here ######

  if(yr_rr == TRUE){
    rr <- yr_rr_using_days(df = df, metric = !!metric)
    cur_yr_df <- rr
  }

  ###

  ###### opt in to add op2 view ######

  if(!missing(metric_op2) & !missing(df_op2)){

    r <- df_op2 %>%
      filter(yr_num == cur_yr) %>%
      select(yr_num, !!metric_op2) %>%
      group_by(yr_num) %>%
      summarise_at(vars(!!metric_op2), funs(sum)) %>%
      select(!!metric_op2)

    cur_yr_df <- cbind(cur_yr_df, r) %>% mutate(
      !!metric_op2_var_name := round(
        100 * ( (UQ(metric_cur_yr_name)) - (!!metric_op2) )
        /
          (UQ(metric_cur_yr_name))
        , 2)
    ) %>%
      ungroup()

  }


  ###

  final <-
    cbind(cur_yr_df, prev_yr_df) %>% mutate(
       !!metric_prev_yr_var_name :=
         round( 100 * ( (UQ(metric_cur_yr_name)) - (UQ(metric_prev_yr_name)) )
                /
                  (UQ(metric_prev_yr_name)), 2 )
     )

  ###

  if(div_by_one_thousand){
    final <- final %>% mutate_at(vars(!!metric_cur_yr_name, !!metric_prev_yr_name), funs(div_by_one_thousand))
  }

  if(accounting){

    final <- final %>%
      mutate_at(
        vars(
          !!metric_cur_yr_name,
          !!metric_prev_yr_name
          # ,
          # !!metric_prev_yr_var_name ### BUG, won't wrap negative numbers in parenthesis
        ),
        funs(prettyNum(., big.mark = ","))
      )

    final <- final %>%
      mutate_at(
        vars(!!metric_prev_yr_var_name),
        funs(neg_paren)
      )
  }

  ###


  # if(sparkline == TRUE){

    # metric_cur_yr_spark <- prev_yr_var_df %>% pull(!!metric_cur_yr_name) %>% sparkline()
    # metric_prev_yr_spark <- prev_yr_var_df %>% pull(!!metric_prev_yr_name) %>% sparkline()
    # metric_prev_yr_var_spark <- prev_yr_var_df %>% pull(!!metric_prev_yr_var_name) %>% sparkline(type = 'bar')

    # spark_df <- rbind(metric_cur_yr_spark,
    #                   metric_prev_yr_spark,
    #                   metric_prev_yr_var_spark)

  # }

  ###### Change names of OP2 columns if new name `AND` OP2 arguments are provided ######

  if(!missing(new_name) & missing(metric_op2)){

    final <- final %>%
      rename(!!new_name := UQ(metric_cur_yr_name),
             `Prior Year` = UQ(metric_prev_yr_name),
             `Variance vs. Prior Year` = UQ(metric_prev_yr_var_name)
      )

  }else if(!missing(new_name) & !missing(metric_op2)){

    final <- final %>%
      rename(!!new_name := UQ(metric_cur_yr_name),
             `Prior Year` = UQ(metric_prev_yr_name),
             `Variance vs. Prior Year` = UQ(metric_prev_yr_var_name),
             `OP2 Plan` = UQ(metric_op2_name),
             `Variance vs. Plan` = UQ(metric_op2_var_name)
      )

  }

  if(yr_rr == FALSE){
    final <-
      final %>%
      gather(metric, value) %>%
      rename(YTD = value)
  }else{
    final <-
      final %>%
      gather(metric, value) %>%
      rename(`Full Year` = value)
  }

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
