#' Core function of wmy_1e, wmy_3e, & my_2e
#'
#' @param df data frame containing metric
#' @param metric column name from df
#' @param grouping time diminesion column name (wk_num_in_yr, mth_num_in_yr, yr_num)
#' @param df_rr data frame containing run rate
#' @param metric_rr column name from df_rr
#' @param df_goal data frame containing metric goal
#' @param metric_goal column name from df_goal
#' @param df_3p9 data frame containing metric prediction
#' @param metric_3p9 column name from df_3p9
#' @param df_6p6 data frame containing metric prediction
#' @param metric_6p6 column name from df_6p6
#' @param df_9p3 data frame containing metric prediction
#' @param metric_9p3 column name from df_9p3
#' @param full_yr logical. Inlcude full year summation column for month view
#' @param cbr_ytd logical. Inlcude YTD definition for CBR : Actual values from last completed month
#' @param rate logical. Adjust logic to handle full year for rates
#' @param new_name name for metric output. Also creates new labels for previous year, previous year variance, etc.
#' @param accounting format values as accountants do. ie: 1234000 -> 1,234,000 & -3000 -> (3000)
#' @param div_by_1000 also an accounting practice; divide all value by 1000. ie: 1234000 -> 1234.00
#' @param prefix add prefix to values. ie: 2000 -> $2000
#' @param suffix add suffix to values. ie: 20.54 -> 20.54\%
#' @param spark logical should a spark chart be added as a column in the output
#' @param pop logical stands for Period over Period. Include percent change from previous period (ie: week 4 to week 5) next to value
#'
#' @examples
#'## week view
#'fun(
#' performance,
#' revenue,
#' wk_num_in_yr,
#' new_name = "Revenue",
#' accounting = TRUE,
#' div_by_1000 = FALSE,
#' full_yr = FALSE,
#' prefix = "$"
#')
#'
#'## month view
#'fun(
#' performance,
#' revenue,
#' mth_num_in_yr,
#' new_name = "Revenue",
#' accounting = TRUE,
#' div_by_1000 = FALSE,
#' full_yr = FALSE,
#' prefix = "$"
#' )
fun <- function(
  df,
  metric,
  grouping,
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
  weeks_back = 4,
  week_end_dates = FALSE,
  week_start_dates = TRUE,
  cbr_ytd = FALSE,
  full_yr = FALSE,
  rate = FALSE,
  new_name = NULL,
  accounting = FALSE,
  div_by_1000 = FALSE,
  prefix = "",
  suffix = "",
  spark = FALSE,
  pop = FALSE,
  pop_threshold = 100,
  scalar = 1,
  digitsAfterDecimal = 2,
  op2_and_var_ph = TRUE,
  month_names = TRUE,
  in_mth_header_op2 = TRUE,
  in_mth_header_3p9 = FALSE,
  in_mth_header_6p6 = FALSE,
  in_mth_header_9p3 = FALSE
){

  #round sum helper fun
  round_sum <- function(x){
    round(sum(x, na.rm=TRUE), digitsAfterDecimal)
  }

  # 'enquo' args for !!/!!!
  metric <- enquo(metric)
  grouping <- enquo(grouping)
  metric_rr <- enquo(metric_rr)

  if(!missing(metric_goal)) metric_goal <- enquo(metric_goal)

  # for cbr ytd
  cur_yr <- max(df$yr_num)
  cur_yr_mth <- month(Sys.Date())

  # errors/messaging
  ## if missing args
  if(missing(df)) stop("'df' is missing")
  if(missing(grouping)) stop("'grouping' is missing")

  ## if illegal value TODEBUG
  # if(grouping != "~wk_num_in_yr" | grouping != "~mth_num_in_yr" | grouping != "~yr_num") stop("grouping value must be: 'wk_num_in_yr', 'mth_num_in_yr' or, 'yr_num'")

  ## 0 record data frame
  if(nrow(df) == 0) stop("empty data frame supplied")

  ## conflicting argument values errors/warnings

  if(suffix == "%" & div_by_1000 == TRUE) warning("Divide percentage by 1000? Are you sure?")
  if(grouping == "~wk_num_in_yr" & full_yr == TRUE) stop("`full_yr` = TRUE not applicable for weekly grouping")
  if(grouping == "~wk_num_in_yr" & cbr_ytd == TRUE) stop("`cbr_ytd` = TRUE not applicable for weekly grouping")
  if(grouping == "~wk_num_in_yr" & !missing(df_goal)) stop("goal not applicable for weekly grouping")
  if(grouping == "~yr_num" & full_yr == TRUE) stop("`full_yr` = TRUE not applicable for year grouping")
  if(grouping != "~mth_num_in_yr" & !missing(df_rr)) stop("run rate not applicable for groupings other than mth_num_in_yr")

  # indicator variable for op2, 3+3, etc.
  # if(!missing(metric_goal)){
  #   indicator <- "OP2"
  #     if(any(df_goal %>% select(!!metric_goal) %>% pull() %in% 0)) warning("Warning: '0' values in metric_goal argument\n")
  # }else if(!missing(metric_3p9)){
  #   indicator <- "3+9"
  # }else if(!missing(metric_6p6)){
  #   indicator <- "6+6"
  # }else if(!missing(metric_9p3)){
  #   indicator <- "9+3"
  # }else{
  #   indicator <- NULL
  # }

  # get relevant wk numbers
  if(grouping == "~wk_num_in_yr"){

    wk_nums <- df %>%
      arrange(wk_start_date) %>%
      filter(wk_start_date >= ceiling_date( ( max(wk_start_date) - (7 * weeks_back) ) ) ) %>% # controls weeks back to calculate metric
      select(wk_num_in_yr) %>%
      pull() %>% unique()

    wk_start_dates <- df %>%
      arrange(wk_start_date) %>%
      filter(wk_start_date >= ceiling_date( ( max(wk_start_date) - (7 * weeks_back) ) ) ) %>%
      select(wk_start_date) %>%
      mutate(wk_start_date = format(wk_start_date, format = "%d-%b")) %>%
      pull() %>% unique()

  }

  # get number of months for current year ONLY (previous year will always be 12) for rate = TRUE
  # if((grouping == "~mth_num_in_yr" | grouping == "~yr_num") & rate == TRUE){
  #   cur_yr_mths_w_data <-
  #     df %>%
  #       filter(yr_num == max(yr_num)) %>%
  #       group_by(mth_num_in_yr) %>%
  #       summarise_at(vars(!!metric), funs(sum)) %>%
  #       mutate(
  #         cur_yr_mths_w_data = case_when(
  #           !!metric != 0 | !is.na(!!metric) ~ 1,
  #           TRUE ~ 0
  #         )
  #       ) %>%
  #       pull() %>% sum()
  # }

  # group and summarize, if week grouping, bring wk_end_date to order by (ie: w51, w52, w1, w2)
  # *** if df is already grouped and summarised, this function will simply spit the same data frame back out,
  #     making this function able to consume both daily gain data as well as already grouped and summarised data
  if(grouping == "~wk_num_in_yr"){
    df <- df %>% group_by(yr_num, !!grouping) %>%
      summarise_at(vars(!!metric), funs(sum)) %>%
      filter(wk_num_in_yr %in% wk_nums)
  }else{
    df <- df %>% group_by(yr_num, !!grouping) %>%
      summarise_at(vars(!!metric), funs(sum))
  }

  # this template is used for new marketplaces (like CA) that do not have at least 12 months of a year's worth of data to create a table
  # with 1:12 (otherwise you might get 1,2,6,7,...12)
  if(grouping == "~mth_num_in_yr"){
    df <- mth_num_in_yr_template %>% left_join(., df)
  }

  # split data into current year and previous year
  if(grouping != "~yr_num"){

    cur_yr_df <- df %>%
      filter(yr_num == max(df$yr_num)) %>%
      ungroup() %>%
      select(-yr_num) %>%
      rename(metric_cur_yr = !!metric)

    prev_yr_df <- df %>%
      filter(yr_num == max(df$yr_num) - 1) %>%
      ungroup() %>%
      select(-yr_num) %>%
      rename(metric_prev_yr = !!metric)

  }else{ # do not remove yr_num for join

    cur_yr_df <- df %>%
      filter(yr_num == max(df$yr_num)) %>%
      ungroup() %>%
      rename(metric_cur_yr = !!metric)

    prev_yr_df <- df %>%
      filter(yr_num == max(df$yr_num) - 1) %>%
      ungroup() %>%
      rename(metric_prev_yr = !!metric) %>%
      mutate(yr_num = yr_num + 1)
  }

  # join run rate to cur_yr_df if grouping is mth_num_in_yr
  if(grouping == "~mth_num_in_yr" & !missing(df_rr)){

    # Preventative error handling for num vs int
    # metric_class <- df %>% select(!!metric) %>% pull() %>% class()
    # metric_rr_class <- df %>% select(!!metric_rr) %>% pull() %>% class()
    # if(metric_class != metric_rr_class) stop(glue("Metric class {metric_class} and Run Rate class {metric_rr_class}"))

    rr_mth <- df_rr$mth_num_in_yr # store month num of run rate for message

    df_rr <- df_rr %>% select(yr_num, mth_num_in_yr, !!metric_rr) %>%
      rename(metric_rr = !!metric_rr) %>%
      mutate(cur_yr_type = "Run Rate")

    cur_yr_df <- left_join(cur_yr_df, df_rr) %>%
      mutate(metric_cur_yr = if_else(is.na(metric_rr), metric_cur_yr, metric_rr)) %>%
      select(-metric_rr)

    message(glue("Current Year Month {rr_mth} is RUN RATE")) # for visibility, echo run rate message

  }

  # join current year and previous year together
  if(grouping == "~wk_num_in_yr"){ # ***function will join wk_num_in_yr with wk_end_date strangely if you leave `by` argument empty
    df <-
      full_join(
        cur_yr_df,
        prev_yr_df,
        suffix = c("_cur_yr", "_prev_yr"),
        by = c("wk_num_in_yr" = "wk_num_in_yr")
      )
  }else{
    df <-
      full_join(
        cur_yr_df,
        prev_yr_df,
        suffix = c("_cur_yr", "_prev_yr")
      )

      # if(in_mth_header_op2 & grouping == "~mth_num_in_yr"){
      #   df <- df %>%
      #     mutate(
      #       mth_name = case_when(
      #         mth_num_in_yr > month(Sys.Date()) ~ paste0(mth_name, " (OP2)"),
      #         TRUE ~ as.character(mth_name)
      #       ),
      #       mth_num_in_yr = case_when(
      #         mth_num_in_yr > month(Sys.Date()) ~ paste0(mth_num_in_yr, " (OP2)"),
      #         TRUE ~ as.character(mth_num_in_yr)
      #       )
      #     ) %>%
      #     mutate_at(vars(mth_num_in_yr, mth_name), funs(as.factor(.)))
      # }

  }

  # if goal(op2) is provided, join it
  if(!missing(df_goal)){
    if(grouping == "~mth_num_in_yr"){
      df_goal <- df_goal %>%
        select(!!grouping, !!metric_goal, -yr_num) %>%
        rename(metric_goal = !!metric_goal)
    }else if(grouping == "~yr_num"){ # if grouping is yr, group and summate the goal for the year
      df_goal <- df_goal %>%
        select(!!grouping, !!metric_goal, yr_num) %>%
        group_by(yr_num) %>%
        summarise_at(vars(!!metric_goal), funs(sum)) %>%
        rename(metric_goal = !!metric_goal)
      if(rate){
        df_goal <- df_goal %>%
          mutate(metric_goal = metric_goal/12)
      }
    }

    df <-
      left_join(
        df,
        df_goal,
        by = c("mth_num_in_yr" = "mth_num_in_yr")
      ) %>%
      mutate(goal_var = round ( ( ( ( metric_cur_yr - metric_goal ) / metric_goal ) * 100 ), 2 ) )
  } # END if goal(op2) is provided, join it

  # if 3+9 is provided, join it
  if(!missing(df_3p9)){
    df_3p9 <- df_3p9 %>% select(!!grouping, !!metric_3p9, -yr_num) %>% rename(metric_3p9 = !!metric_3p9)
    df <-
      left_join(
        df,
        df_3p9
      )
  }

  # if 6+6 is provided, join it
  if(!missing(df_6p6)){
    df_6p6 <- df_6p6 %>% select(!!grouping, !!metric_6p6, -yr_num) %>% rename(metric_6p6 = !!metric_6p6)
    df <-
      left_join(
        df,
        df_6p6
      )
  }

  # if 9+3 is provided, join it
  if(!missing(df_9p3)){
    df_9p3 <- df_9p3 %>% select(!!grouping, !!metric_9p3, -yr_num) %>% rename(metric_9p3 = !!metric_9p3)
    df <-
      left_join(
        df,
        df_9p3
      )
  }

  # put predictions or goal in current year row (9+3 over 6+6, 6+6 over 3+9, etc.)
  if(grouping == "~mth_num_in_yr"){ # only execute for month grouping
    if(!missing(df_9p3)){ # 9 + 3
      df <- df %>%
        mutate(metric_cur_yr = if_else(is.na(metric_cur_yr), metric_9p3, metric_cur_yr)) %>%
        select(-metric_9p3)
    }else if(!missing(df_6p6)){ # 6 + 6
      df <- df %>%
        mutate(metric_cur_yr = if_else(is.na(metric_cur_yr), metric_6p6, metric_cur_yr)) %>%
        select(-metric_6p6)
    }else if(!missing(df_3p9)){ # 3 + 9
      df <- df %>%
        mutate(metric_cur_yr = if_else(is.na(metric_cur_yr), metric_3p9, metric_cur_yr)) %>%
        select(-metric_3p9)
    }else if(!missing(df_goal)){ # OP2

      df <- df %>%
        mutate(
          cur_yr_type = if_else(!is.na(metric_cur_yr), "Actual", "OP2"),
          # mth_num_in_yr = paste0(mth_num_in_yr,"|",type_cur_yr),
          metric_cur_yr = if_else(is.na(metric_cur_yr), metric_goal, metric_cur_yr)
          ) %>%
        mutate(goal_var = round ( ( ( ( metric_cur_yr - metric_goal ) / metric_goal ) * 100 ), 2 ) ) %>%  # op2 var
        mutate(goal_var = ifelse(goal_var == 0, NA, goal_var))

    }else{ # else do nothing
      NULL
    }
  }

  # use this to determine if goal or forecasts have been provided. If one has been, do current year full year calc, else do not
  goal_or_forecasts_provided = !missing(df_goal) | !missing(df_3p9) | !missing(df_6p6) | !missing(df_9p3)

  # FULL YR
  # calculate full yr values for mth view, store in variable as dataframe to join later: included by default
  if(grouping == "~mth_num_in_yr" & full_yr){
    if(!missing(df_goal)){
      df_full_yr <-
        df %>% select(metric_cur_yr, metric_prev_yr, metric_goal) %>%
          summarise_all(funs(sum(., na.rm = TRUE))) %>%
          mutate(prev_yr_var = round ( ( ( ( metric_cur_yr - metric_prev_yr ) / metric_prev_yr ) * 100 ), 2 ) ) %>% # previous yr variance
          mutate(goal_var = round ( ( ( ( metric_cur_yr - metric_goal ) / metric_goal ) * 100 ), 2 ) ) %>%  # goal variance
          select(metric_cur_yr, metric_prev_yr, prev_yr_var, metric_goal, goal_var) # do select to enforce order
      if(rate){
        df_full_yr <- df_full_yr %>% mutate(metric_goal = round((metric_goal/12), digitsAfterDecimal))
      }
    }else{
      df_full_yr <-
        df %>% select(metric_cur_yr, metric_prev_yr) %>%
          summarise_all(funs(sum(., na.rm = TRUE))) %>%
          mutate(metric_cur_yr = NA) %>%
      # %>%
          # mutate(prev_yr_var = round ( ( ( ( metric_cur_yr - metric_prev_yr ) / metric_prev_yr ) * 100 ), 2 ) ) %>% # previous yr variance
          select(metric_cur_yr, metric_prev_yr) # do select to enforce order
    }
  }

  # CBR YTD
  # calculate cbr_ytd values for mth view, store in variable as dataframe to join later: included by default
  if(grouping == "~mth_num_in_yr" & cbr_ytd){
    if(!missing(df_goal)){

      df_cbr_ytd <-
        df %>%
          select(mth_num_in_yr, metric_cur_yr, metric_prev_yr, metric_goal) %>%
          filter(mth_num_in_yr < cur_yr_mth) %>%
          summarise_all(funs(sum(., na.rm = TRUE))) %>%
          mutate(prev_yr_var = round ( ( ( ( metric_cur_yr - metric_prev_yr ) / metric_prev_yr ) * 100 ), 2 ) ) %>% # previous yr variance
          mutate(goal_var = round ( ( ( ( metric_cur_yr - metric_goal ) / metric_goal ) * 100 ), 2 ) ) %>%  # goal variance
          select(metric_cur_yr, metric_prev_yr, prev_yr_var, metric_goal, goal_var) # do select to enforce order

    }else{
      df_cbr_ytd <-
        df %>%
          select(mth_num_in_yr, metric_cur_yr, metric_prev_yr) %>%
          filter(mth_num_in_yr < cur_yr_mth) %>%
          summarise_all(funs(sum(., na.rm = TRUE))) %>%
          select(metric_cur_yr, metric_prev_yr) # do select to enforce order
    }
  }

################

    # handle division for Full Year rates and YTD rates

    # if(grouping == "~yr_num"){ ####### used only if you were to add up 12 months of rates and divide by 12*****
    #   df <- df %>%
    #     mutate(
    #       metric_cur_yr = round((metric_cur_yr / 12), 2),
    #       metric_prev_yr = round((metric_prev_yr / 12), 2)
    #     )
    # }

###################

  # FULL YR
  # divide rate full yr values (sum of monthly rates) by 12 (number of month periods)
  if(grouping == "~mth_num_in_yr" & full_yr & rate){
    df_full_yr <- df_full_yr %>%
      mutate(
        metric_cur_yr = round((metric_cur_yr / 12), digitsAfterDecimal),
        metric_prev_yr = round((metric_prev_yr / 12), digitsAfterDecimal)
      )
  }

  if(grouping == "~mth_num_in_yr" & cbr_ytd & rate){
    df_cbr_ytd <- df_cbr_ytd %>%
      mutate(
        metric_cur_yr = round((metric_cur_yr / ( cur_yr_mth - 1) ), digitsAfterDecimal), # number of months with actual values
        metric_prev_yr = round((metric_prev_yr / ( cur_yr_mth - 1) ), digitsAfterDecimal)
      )
  }

  if(grouping == "~mth_num_in_yr" & cbr_ytd & rate & !missing(df_goal)){
    df_cbr_ytd <- df_cbr_ytd %>%
      mutate(
        metric_goal = round((metric_goal / ( cur_yr_mth - 1) ), 2)
      )
  }

  # FULL YR
  # calculate previous year variance (could be with actuals, op2, or predictions)
  if(suffix == "%"){
      df <- df %>%
        mutate(prev_yr_var = round ( ( metric_cur_yr - metric_prev_yr ), 2 ) ) %>% # previous year variance
        arrange(!!grouping)
    if(full_yr){
      df_full_yr <- df_full_yr %>% mutate(prev_yr_var = round ( ( metric_cur_yr - metric_prev_yr ), 2 ))
    }
    if(cbr_ytd){
      df_cbr_ytd <- df_cbr_ytd %>% mutate(prev_yr_var = round ( ( metric_cur_yr - metric_prev_yr ), 2 ))
    }
  }else{
      df <- df %>%
        mutate(
          prev_yr_var = round ( ( ( ( metric_cur_yr - metric_prev_yr ) / metric_prev_yr ) * 100 ), 2 )  # previous year variance
        ) %>%
        arrange(!!grouping)
    if(full_yr){
      df_full_yr <- df_full_yr %>%
        mutate(prev_yr_var = round ( ( ( ( metric_cur_yr - metric_prev_yr ) / metric_prev_yr ) * 100 ), 2 ))
    }
    if(cbr_ytd){
      df_cbr_ytd <- df_cbr_ytd %>%
        mutate(prev_yr_var = round ( ( ( ( metric_cur_yr - metric_prev_yr ) / metric_prev_yr ) * 100 ), 2 ))
    }
  }

  # divide values by 1000
  ## df
  if(div_by_1000){
    if(!missing(df_goal)){
      df <- df %>% mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(div_by_1000))
    }else{
      df <- df %>% mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(div_by_1000))
    }
  }
  ## df_full_yr
  if(div_by_1000){
    if(grouping == "~mth_num_in_yr" & full_yr & missing(df_goal)){
      df_full_yr <- df_full_yr %>% mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(div_by_1000))
    }
    if(grouping == "~mth_num_in_yr" & full_yr & !missing(df_goal)){
      df_full_yr <- df_full_yr %>% mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(div_by_1000))
    }
  }
  ## df_cbr_ytd
  if(div_by_1000){
    if(grouping == "~mth_num_in_yr" & cbr_ytd & missing(df_goal)){
      df_cbr_ytd <- df_cbr_ytd %>% mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(div_by_1000))
    }
    if(grouping == "~mth_num_in_yr" & cbr_ytd & !missing(df_goal)){
      df_cbr_ytd <- df_cbr_ytd %>% mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(div_by_1000))
    }
  }

  ###### round everything by digitsAfterDecimal
  if(!missing(df_goal)){
    df <-
      df %>%
      mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(round(., digitsAfterDecimal)))
  }else{
    df <-
      df %>%
      mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(round(., digitsAfterDecimal)))
  }
  ##df_full_yr
  if(grouping == "~mth_num_in_yr" & full_yr & missing(df_goal)){
    df_full_yr <- df_full_yr %>% mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(round(., digitsAfterDecimal)))
  }
  if(grouping == "~mth_num_in_yr" & full_yr & !missing(df_goal)){
    df_full_yr <- df_full_yr %>% mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(round(., digitsAfterDecimal)))
  }
  ## df_cbr_ytd
  if(grouping == "~mth_num_in_yr" & cbr_ytd & missing(df_goal)){
    df_cbr_ytd <- df_cbr_ytd %>% mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(round(., digitsAfterDecimal)))
  }
  if(grouping == "~mth_num_in_yr" & cbr_ytd & !missing(df_goal)){
    df_cbr_ytd <- df_cbr_ytd %>% mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(round(., digitsAfterDecimal)))
  }



  #sparkline # TODO, enforce order of wks <- NOTDONE and mnths <- DONE and DEBUG yr
  if(spark){
    spark_df <- tibble(
      metric = c('metric_cur_yr', 'metric_prev_yr', 'prev_yr_var'),
      chart = c(
        df %>% pull(metric_cur_yr) %>% spk_chr(type='line'),
        df %>% pull(metric_prev_yr) %>% spk_chr(type='line'),
        df %>% pull(prev_yr_var) %>% spk_chr(type='line')
      )
    )
  }

  #pop (Period over Period change)
  if(pop){
    df <- df %>%
      mutate(
        metric_cur_yr_pop = round ( ( ( ( metric_cur_yr - lag(metric_cur_yr) ) / lag(metric_cur_yr) ) * 100 ), 2 ),
        metric_prev_yr_pop = round ( ( ( ( metric_prev_yr - lag(metric_prev_yr) ) / lag(metric_prev_yr) ) * 100 ), 2 )
      )

    if(grouping == "~wk_num_in_yr"){

      pop_df <- df %>% select(wk_num_in_yr, metric_cur_yr_pop, metric_prev_yr_pop) %>%
        filter(!is.na(metric_cur_yr_pop)) %>%
        filter(wk_num_in_yr == max(wk_num_in_yr))

      if(abs(pop_df$metric_cur_yr_pop) > pop_threshold) message(glue("Week: pop_df$metric_cur_yr_pop (absolute value) {abs(pop_df$metric_cur_yr_pop)} > pop_threshold {pop_threshold}"))

    }

    if(grouping == "~mth_num_in_yr"){

      pop_df <- df %>% select(mth_num_in_yr, metric_cur_yr_pop) %>%
        filter(!is.na(metric_cur_yr_pop)) %>%
        filter(mth_num_in_yr == max(mth_num_in_yr))

      if(abs(pop_df$metric_cur_yr_pop) > pop_threshold) message(glue("Month: pop_df$metric_cur_yr_pop (absolute value) {abs(pop_df$metric_cur_yr_pop)} > pop_threshold {pop_threshold}"))

    }


    df <- df %>%
      mutate(
        metric_cur_yr_pop = paste0(metric_cur_yr_pop,"%"),
        metric_prev_yr_pop = paste0(metric_prev_yr_pop,"%")
      ) %>%
      mutate(
        metric_cur_yr = paste(metric_cur_yr, metric_cur_yr_pop, sep = " | "),
        metric_prev_yr = paste(metric_prev_yr, metric_prev_yr_pop, sep = " | ")
      ) %>%
      select(-metric_cur_yr_pop, -metric_prev_yr_pop)

  }

  # add prefix and suffix
  # for df
  if(!missing(df_goal)){
    df <- df %>%
      mutate(
        metric_cur_yr = ifelse(!is.na(metric_cur_yr), paste0(prefix, metric_cur_yr, suffix), NA),
        metric_prev_yr = ifelse(!is.na(metric_prev_yr), paste0(prefix, metric_prev_yr, suffix), NA),
        metric_goal = ifelse(!is.na(metric_goal), paste0(prefix, metric_goal, suffix), NA)
      )
  }else{
    df <- df %>%
      mutate(
        metric_cur_yr = ifelse(!is.na(metric_cur_yr), paste0(prefix, metric_cur_yr, suffix), NA),
        metric_prev_yr = ifelse(!is.na(metric_prev_yr), paste0(prefix, metric_prev_yr, suffix), NA)
      )
  }

  # FULL YR
  # for df_full_yr
  if(grouping == "~mth_num_in_yr"){
    if(full_yr){
      if(!missing(df_goal)){
        df_full_yr <- df_full_yr %>%
          mutate(
            metric_cur_yr = ifelse(!is.na(metric_cur_yr), paste0(prefix, metric_cur_yr, suffix), NA),
            metric_prev_yr = ifelse(!is.na(metric_prev_yr), paste0(prefix, metric_prev_yr, suffix), NA),
            metric_goal = ifelse(!is.na(metric_goal), paste0(prefix, metric_goal, suffix), NA)
          )
      }else{
        df_full_yr <- df_full_yr %>%
          mutate(
            metric_cur_yr = ifelse(!is.na(metric_cur_yr), paste0(prefix, metric_cur_yr, suffix), NA),
            metric_prev_yr = ifelse(!is.na(metric_prev_yr), paste0(prefix, metric_prev_yr, suffix), NA)
          )
      }
    }

    if(cbr_ytd){
      if(!missing(df_goal)){
        df_cbr_ytd <- df_cbr_ytd %>%
          mutate(
            metric_cur_yr = paste0(prefix, metric_cur_yr, suffix),
            metric_prev_yr = paste0(prefix, metric_prev_yr, suffix),
            metric_goal = paste0(prefix, metric_goal, suffix)
          )
      }else{
        df_cbr_ytd <- df_cbr_ytd %>%
          mutate(
            metric_cur_yr = paste0(prefix, metric_cur_yr, suffix),
            metric_prev_yr = paste0(prefix, metric_prev_yr, suffix)
          )
      }
    }

  }
  # END add prefix and suffix


  # FULL YR
  # handle % vs ppts for values and rates respectively for prev yr var and op2 var
  if(suffix == "%"){
      df <- df %>% mutate(
        prev_yr_var = ifelse(!is.na(prev_yr_var), paste0(prev_yr_var, " ppts"), NA)
        )
      if(!missing(df_goal)){
        df <- df %>% mutate(
          goal_var = ifelse(!is.na(goal_var), paste0(goal_var, " ppts"), NA)
        )
      }

      if(full_yr){
        df_full_yr <- df_full_yr %>%
          mutate(
            prev_yr_var = ifelse(!is.na(prev_yr_var), paste0(prev_yr_var, " ppts"), NA)
            )
        if(!missing(df_goal)){
          df_full_yr <- df_full_yr %>%
            mutate(
              goal_var = ifelse(!is.na(goal_var), paste0(goal_var, " ppts"), NA)
            )
        }
      }

      if(cbr_ytd){
        df_cbr_ytd <- df_cbr_ytd %>%
          mutate(
            prev_yr_var = ifelse(!is.na(prev_yr_var), paste0(prev_yr_var, " ppts"), NA)
          )
        if(!missing(df_goal)){
          df_cbr_ytd <- df_cbr_ytd %>%
            mutate(
              goal_var = ifelse(!is.na(goal_var), paste0(goal_var, " ppts"), NA)
            )
        }
      }

    }else{

      df <- df %>% mutate(
        prev_yr_var = ifelse(!is.na(prev_yr_var), paste0(prev_yr_var, "%"), NA)
        )
      if(!missing(df_goal)){
        df <- df %>% mutate(
          goal_var = ifelse(!is.na(goal_var), paste0(goal_var, "%"), NA)
        )
      }

      if(full_yr){
        df_full_yr <- df_full_yr %>%
          mutate(
            prev_yr_var = ifelse(!is.na(prev_yr_var), paste0(prev_yr_var, "%"), NA)
            )
        if(!missing(df_goal)){
          df_full_yr <- df_full_yr %>%
            mutate(
              goal_var = ifelse(!is.na(goal_var), paste0(goal_var, "%"), NA)
            )
        }
      }

      if(cbr_ytd){
        df_cbr_ytd <- df_cbr_ytd %>%
          mutate(
            prev_yr_var = ifelse(!is.na(prev_yr_var), paste0(prev_yr_var, "%"), NA)
          )
        if(!missing(df_goal)){
          df_cbr_ytd <- df_cbr_ytd %>%
            mutate(
              goal_var = ifelse(!is.na(goal_var), paste0(goal_var, "%"), NA)
            )
        }
      }
  }


  # apply accounting formatting -7437834 -> (7437834); 17000 -> 17,000
  if(accounting){
    # df
    ## add commas (17000 -> 17,000)
    if(grouping == "~wk_num_in_yr"){ # no goal for wk view
      df <- df %>%
        mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(pretty_num))
    }else{
      if(!missing(df_goal)){
        df <- df %>%
          mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(pretty_num))
      }else{
        df <- df %>%
          mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(pretty_num))
      }
    }

    ## wrap negative numbers in parenthesis (ie: -7 -> (7))
    if(grouping == "~wk_num_in_yr"){ # no goal var for wk view
      df <- df %>%
        mutate_at(
          vars(prev_yr_var), funs(neg_paren)
          ) #neg_paren() in R/helpers.R
    }else{
      if(!missing(df_goal)){
        df <- df %>%
          mutate_at(
            vars(prev_yr_var, goal_var), funs(neg_paren))
      }else{
        df <- df %>%
          mutate_at(
            vars(prev_yr_var), funs(neg_paren))
      }
    }


    # FULL YR
    #df_full_yr
    if(full_yr){
      if(!missing(df_goal)){

        df_full_yr <- df_full_yr %>%
          mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(pretty_num))

        df_full_yr <- df_full_yr %>%
          mutate_at(
            vars(prev_yr_var, goal_var), funs(neg_paren))

      }else{

        df_full_yr <- df_full_yr %>%
          mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(pretty_num))

        df_full_yr <- df_full_yr %>%
          mutate_at(
            vars(prev_yr_var), funs(neg_paren))

      }
    }

    #df_cbr_ytd
    if(cbr_ytd){
      if(!missing(df_goal)){

        df_cbr_ytd <- df_cbr_ytd %>%
          mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(pretty_num))

        df_cbr_ytd <- df_cbr_ytd %>%
          mutate_at(
            vars(prev_yr_var, goal_var), funs(neg_paren)
            )

      }else{

        df_cbr_ytd <- df_cbr_ytd %>%
          mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(pretty_num))

        df_cbr_ytd <- df_cbr_ytd %>%
          mutate_at(
            vars(prev_yr_var), funs(neg_paren))

      }
    }

  }




  # define order for metrics to display in output
  ordering_array <- c('metric_cur_yr', 'metric_prev_yr', 'prev_yr_var', 'metric_goal', 'goal_var')

  #### transform from long to wide/horizontal view ####
  if(grouping == '~wk_num_in_yr'){ # wk

    df <- df %>%
      gather(metric, value, -!!grouping) %>%
      spread(!!grouping, value) %>%
      arrange(
        metric = ordered(metric, levels = ordering_array)
      ) %>%
      select(c("metric", wk_nums)) # orders the columns so the wks are chronological (ie: 51 52 1 2), else they will display (1 2 51 52)

    # add 'w' to week column names
    names(df) <- c('metric', paste0("w",wk_nums))

  }else{ # mth & yr

    df <- df %>%
      # select(-cur_yr_type) %>%
      select(-mth_name) %>% ######### throws warning because of the factor order of mth_names if not removed
      gather(metric, value, -!!grouping) %>%
      spread(!!grouping, value) %>%
      arrange(
        metric = ordered(metric, levels = ordering_array)
      )
    # if(grouping == "~mth_num_in_yr"){ ## forces order of months
    #   df <- df %>% select(
    #     metric, starts_with("1|"), starts_with("2|"), starts_with("3|"), starts_with("4|"),
    #     starts_with("5|"), starts_with("6|"), starts_with("7|"), starts_with("8|"),
    #     starts_with("9|"), starts_with("10|"), starts_with("11|"), starts_with("12|")
    #     )
    # }
  }

  # add week start dates to week column (to be header)
  if(week_start_dates & grouping == "~wk_num_in_yr"){
    names(df)[2:5] <- paste(paste0("w",wk_nums), wk_start_dates %>% as.character(), sep = "|")
  }

  # join df_cbr_ytd to mth data
  if(cbr_ytd){
    df <- left_join(df, df_cbr_ytd %>% gather(metric, `YTD`), by = c('metric' = 'metric'))
  }

  # FULL YR
  # join df_full_yr to mth data
  if(full_yr){
    df <- left_join(df, df_full_yr %>% gather(metric, `Full Year`), by = c('metric' = 'metric'))
  }

  # join spark chart
  if(spark){
    df <- left_join(df, spark_df, by = c('metric' = 'metric'))
  }

  #if op2_and_var_ph (op2 and variance place holder) == TRUE add placeholder lines
  if(op2_and_var_ph == TRUE){
    ph <- tibble(metric = c('metric_cur_yr', 'metric_prev_yr', 'prev_yr_var', 'metric_goal', 'goal_var'))
    df <- left_join(ph, df, by = c('metric' = 'metric'))
  }

  # if new name is provided, rename metric labels with respect to if goal (op2) is provided
  # if(!is.null(new_name)){
  #     if(!missing(df_goal)){
  #       df <- df %>% mutate(
  #         metric = c(new_name, "Prior Year", "Variance vs. Prior Year", "OP2 Plan", "Variance vs. Plan")
  #       )
  #   }else(
  #     df <- df %>% mutate(
  #       metric = c(new_name, "Prior Year", "Variance vs. Prior Year")
  #     )
  #   )
  # }
  if(!is.null(new_name)){
    df <- df %>% mutate(
              metric = c(new_name, "Prior Year", "Variance vs. Prior Year", "OP2 Plan", "Variance vs. Plan")
            )
  }

  # if yr grouping, rename column name from current year number to 'YTD' (ie: `2018` -> `YTD`)
  if(grouping == "~yr_num"){
    names(df) = c("metric", "YTD")
  }

  if(grouping == "~mth_num_in_yr" & month_names){
    names(df)[2:13] <- month.abb
      if(in_mth_header_op2){
        month(Sys.Date()) + 2 -> start_pos
        names(df)[start_pos:13] <- paste0(names(df)[start_pos:13], " (OP2)")
      }
  }

  # if(grouping == "~mth_num_in_yr" & in_mth_header_op2){
  #   month(Sys.Date()) + 2
  #   names(df)[(month(Sys.Date()) + 1):13] <- paste0(names(df), " (OP2)")
  # }

  # return data frame
  df

}
