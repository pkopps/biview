#year_view

year_view <- function(
  df,
  metric,
  show_type = FALSE,
  new_name = NULL,
  run_rate = TRUE
  # ,
  # funct = sum
  # ,
  # sparkline = FALSE
  ) {

  ###### message to clarify ACTUAL vs RUN RATE ######

  if(run_rate == FALSE){
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

  metric_op2 <- enquo(metric_op2)
  metric_op2_name <- quo_name(metric_op2)

  metric_op2_var_name <- paste(metric_name, "op2_var", sep = "_")
  metric_op2_var <- enquo(metric_op2_var_name)

  df_ <- df

  if(!missing(new_name)){
    ordering_array <- c("type",
                        new_name,
                        "Prior Year",
                        "Variance vs. Prior Year")
  }else{
    ordering_array <- c("type",
                        metric_cur_yr_name,
                        metric_prev_yr_name,
                        metric_prev_yr_var_name)
  }

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

  if(run_rate == TRUE){

    rr <- year_run_rate(df = df_, metric = !!metric)

    cur_yr_df <- rr

  }

  ###

  prev_yr_var_df <-
    cbind(cur_yr_df, prev_yr_df) %>% mutate(
       !!metric_prev_yr_var_name :=
         round( 100 * ( (UQ(metric_cur_yr_name)) - (UQ(metric_prev_yr_name)) )
                /
                  (UQ(metric_prev_yr_name)), 2 )
     )

  # if(sparkline == TRUE){

    # metric_cur_yr_spark <- prev_yr_var_df %>% pull(!!metric_cur_yr_name) %>% sparkline()
    # metric_prev_yr_spark <- prev_yr_var_df %>% pull(!!metric_prev_yr_name) %>% sparkline()
    # metric_prev_yr_var_spark <- prev_yr_var_df %>% pull(!!metric_prev_yr_var_name) %>% sparkline(type = 'bar')

    # spark_df <- rbind(metric_cur_yr_spark,
    #                   metric_prev_yr_spark,
    #                   metric_prev_yr_var_spark)

  # }

  if(!missing(new_name)){

    prev_yr_var_df <- prev_yr_var_df %>% rename(!!new_name := UQ(metric_cur_yr_name),
                                                `Prior Year` = UQ(metric_prev_yr_name),
                                                `Variance vs. Prior Year` = UQ(metric_prev_yr_var_name))

  }

  if(run_rate == FALSE){
    final <-
      prev_yr_var_df %>%
      gather(metric, value) %>%
      rename(YTD = value)
  }else{
    final <-
      prev_yr_var_df %>%
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
