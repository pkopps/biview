#' Depended on by wmy_1e & wmy_3e. Transform regular long data into BI horizontal wk, mth, and yr, views with current yr, previous yr, previous yr variance, etc.
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
#' @param full_yr_rate TODO
#' @param new_name name for metric output. Also creates new labels for previous year, previous year variance, etc.
#' @param accounting format values as accountants do. ie: 1234000 -> 1,234,000 & -3000 -> (3000)
#' @param div_by_1000 also an accounting practice; divide all value by 1000. ie: 1234000 -> 1234.00
#' @param prefix add prefix to values. ie: 2000 -> $2000
#' @param suffix add suffix to values. ie: 20.54 -> 20.54%
#' @param spark logical. should a spark chart be added as a column in the output
#' @param pop logical. stands for Period over Period. Include percent change from previous period (ie: week 4 to week 5) next to value
#'
#' @return data frame
#'
#' @examples
#'fun(
#' performance,
#' revenue,
#' wk_num_in_yr,
#` new_name = "Revenue",
#` accounting = TRUE,
#` div_by_1000 = FALSE,
#` full_yr = FALSE,
#` prefix = "$"
#` )
#`
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
  full_yr = FALSE,
  full_yr_rate = FALSE,
  new_name = NULL,
  accounting = FALSE,
  div_by_1000 = TRUE,
  prefix = "",
  suffix = "",
  spark = FALSE,
  pop = FALSE
){

  # 'enquo' args for !!/!!!
  metric <- enquo(metric)
  grouping <- enquo(grouping)
  metric_rr <- enquo(metric_rr)
  metric_goal <- enquo(metric_goal)

  # errors/messaging
  ## if missing args
  if(missing(df)) stop("'df' is missing")
  if(missing(grouping)) stop("'grouping' is missing")

  # print(grouping)
  ## if illegal value TODEBUG
  # if(grouping != "~wk_num_in_yr" | grouping != "~mth_num_in_yr" | grouping != "~yr_num") stop("grouping value must be: 'wk_num_in_yr', 'mth_num_in_yr' or, 'yr_num'")

  ## conflicting argument values warnings
  if(suffix == "%" & div_by_1000 == TRUE) warning("Divide percentage by 1000? Are you sure?")
  if(grouping == "~wk_num_in_yr" & full_yr == TRUE) stop("`full_yr` = TRUE not applicable for weekly grouping")

  # get relevant wk numbers
  if(grouping == "~wk_num_in_yr"){
    wk_nums <- df %>%
      filter(wk_end_date >= ceiling_date( ( max(wk_end_date) - (7 * weeks_back) ) ) ) %>% # controls weeks back to calculate metric
      select(wk_num_in_yr) %>%
      pull() %>% unique()
  }

  # group and summarize, if week grouping, bring wk_end_date to order by (ie: w51, w52, w1, w2)
  # *** if df is already grouped and summarised, this function will simply spit the same data frame back out,
  #     making this function able to consume both daily gain data as well as already grouped and summarised data

  if(grouping == "~wk_num_in_yr"){
    df <- df %>% group_by(yr_num, !!grouping) %>%
      summarise_at(vars(!!metric), funs(round_sum)) %>%
      filter(wk_num_in_yr %in% wk_nums)
  }else{
    df <- df %>% group_by(yr_num, !!grouping) %>%
      summarise_at(vars(!!metric), funs(round_sum))
  }

  # split data into current year and previous year
  if(grouping != "~yr_num"){
    cur_yr_df <- df %>% filter(yr_num == max(df$yr_num)) %>% ungroup() %>% select(-yr_num) %>% rename(metric_cur_yr = !!metric)
    prev_yr_df <- df %>% filter(yr_num == max(df$yr_num) - 1) %>% ungroup() %>% select(-yr_num) %>% rename(metric_prev_yr = !!metric)
  }else{ # do not remove yr_num for join
    cur_yr_df <- df %>% filter(yr_num == max(df$yr_num)) %>% ungroup() %>% rename(metric_cur_yr = !!metric)
    prev_yr_df <- df %>% filter(yr_num == max(df$yr_num) - 1) %>% ungroup() %>% rename(metric_prev_yr = !!metric) %>% mutate(yr_num = yr_num + 1) # hack to join on current yr value
  }

  # join rr to cur_yr_df if grouping is mth_num_in_yr
  if(grouping == "~mth_num_in_yr" & !missing(df_rr)){
    rr_mth <- df_rr$mth_num_in_yr # store month num of run rate for message
    df_rr <- df_rr %>% select(yr_num, mth_num_in_yr, !!metric_rr) %>%
      rename(metric_rr = !!metric_rr)
    cur_yr_df <- left_join(cur_yr_df, df_rr) %>%
      select(-yr_num) %>%
      mutate(metric_cur_yr = if_else(is.na(metric_rr), metric_cur_yr, metric_rr)) %>%
      select(-metric_rr)
    message(glue("Month {rr_mth} is RUN RATE")) # for visibility, echo run rate message
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
    }

    df <-
      left_join(
        df,
        df_goal
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
      df <- df %>% mutate(metric_cur_yr = if_else(is.na(metric_cur_yr), metric_goal, metric_cur_yr)) %>%
        mutate(goal_var = round ( ( ( ( metric_cur_yr - metric_goal ) / metric_goal ) * 100 ), 2 ) ) # op2 var
    }else{ # else do nothing
      NULL
    }
  }

  # calculate previous year variance (could be with actuals, op2, or predictions)
  if(suffix == "%"){
    df <- df %>%
      mutate(
        prev_yr_var = round ( ( metric_cur_yr - metric_prev_yr ), 2 )  # previous year variance
      ) %>%
      arrange(!!grouping)
  }else{
    df <- df %>%
      mutate(
        prev_yr_var = round ( ( ( ( metric_cur_yr - metric_prev_yr ) / metric_prev_yr ) * 100 ), 2 )  # previous year variance
      ) %>%
      arrange(!!grouping)
  }

  # calculate full yr values for mth view, store in variable as dataframe to join later: included by default
  if(full_yr){
    if(!missing(df_goal)){
      df_full_yr <-
        df %>% select(metric_cur_yr, metric_prev_yr, metric_goal) %>%
          summarise_all(funs(sum(., na.rm = TRUE))) %>%
          mutate(prev_yr_var = round ( ( ( ( metric_cur_yr - metric_prev_yr ) / metric_prev_yr ) * 100 ), 2 ) ) %>% # previous yr variance
          mutate(goal_var = round ( ( ( ( metric_cur_yr - metric_goal ) / metric_goal ) * 100 ), 2 ) ) %>%  # goal variance
          select(metric_cur_yr, metric_prev_yr, prev_yr_var, metric_goal, goal_var) # do select to enforce order
    }else{
      df_full_yr <-
        df %>% select(metric_cur_yr, metric_prev_yr) %>%
        summarise_all(funs(sum(., na.rm = TRUE))) %>%
        mutate(prev_yr_var = round ( ( ( ( metric_cur_yr - metric_prev_yr ) / metric_prev_yr ) * 100 ), 2 ) ) %>% # previous yr variance
        select(metric_cur_yr, metric_prev_yr, prev_yr_var) # do select to enforce order
    }
  }

  # divide values by 1000
  if(div_by_1000){
    if(!missing(df_goal)){
      df <- df %>% mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(div_by_1000))
    }else{
      df <- df %>% mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(div_by_1000))
    }
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
      ) %>%
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
  df <- df %>%
    mutate(
      metric_cur_yr = paste0(prefix, metric_cur_yr, suffix),
      metric_prev_yr = paste0(prefix, metric_prev_yr, suffix)
    )

  if(grouping == "~mth_num_in_yr"){
    if(full_yr == TRUE){

      df_full_yr <- df_full_yr %>%
        mutate(
          metric_cur_yr = paste0(prefix, metric_cur_yr, suffix),
          metric_prev_yr = paste0(prefix, metric_prev_yr, suffix)
        )
    }

  }

  if(suffix == "%"){
    df <- df %>% mutate(prev_yr_var = paste0(prev_yr_var, " ppts"))
  }else{
    df <- df %>% mutate(prev_yr_var = paste0(prev_yr_var, "%"))
  }
  # END add prefix and suffix

  # apply accounting formatting -7437834 -> (7437834); 17000 -> 17,000
  if(accounting){

    # df
    ## add commas (17000 -> 17,000)
    if(grouping == "~wk_num_in_yr"){ # no goal for wk view
      df <- df %>%
        mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(prettyNum(., big.mark = ",")))
    }else{
      if(!missing(df_goal)){
        df <- df %>%
          mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(prettyNum(., big.mark = ",")))
      }else{
        df <- df %>%
          mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(prettyNum(., big.mark = ",")))
      }
    }

    ## wrap negative numbers in parenthesis (ie: -7 -> (7))
    if(grouping == "~wk_num_in_yr"){ # no goal var for wk view
      df <- df %>%
        mutate_at(
          vars(prev_yr_var), funs(neg_paren)) #neg_paren() in R/helpers.R
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

    #df_full_yr
    if(full_yr){
      if(!missing(df_goal)){

        df_full_yr <- df_full_yr %>%
          mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(prettyNum(., big.mark = ",")))

        df_full_yr <- df_full_yr %>%
          mutate_at(
            vars(prev_yr_var, goal_var), funs(neg_paren))

      }else{

        df_full_yr <- df_full_yr %>%
          mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(prettyNum(., big.mark = ",")))

        df_full_yr <- df_full_yr %>%
          mutate_at(
            vars(prev_yr_var), funs(neg_paren))

      }
    }

  }

  # define order for metrics to display in output
  ordering_array = c('metric_cur_yr', 'metric_prev_yr', 'prev_yr_var', 'metric_goal', 'goal_var')

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
      gather(metric, value, -!!grouping) %>%
      spread(!!grouping, value) %>%
      arrange(
        metric = ordered(metric, levels = ordering_array)
      )
  }

  # join df_full_yr to mth data
  if(full_yr){
    df <- left_join(df, df_full_yr %>% gather(metric, `Full Year`))
  }

  # join spark chart
  if(spark){
    df <- left_join(df, spark_df)
  }

  # if new name is provided, rename metric labels with respect to if goal (op2) is provided
    if(!is.null(new_name)){
      if(!missing(df_goal)){
        df <- df %>% mutate(
          metric = c(new_name, "Prior Year", "Variance vs. Prior Year", "OP2 Plan", "Variance vs. Plan")
        )
    }else(
      df <- df %>% mutate(
        metric = c(new_name, "Prior Year", "Variance vs. Prior Year")
      )
    )
  }

  # if yr grouping, rename column name from current year number to 'YTD' (ie: `2018` -> `YTD`)
  if(grouping == "~yr_num"){
    names(df) = c("metric", "YTD")
  }

  # return data frame
  df

}
