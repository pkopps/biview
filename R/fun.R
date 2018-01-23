fun <- function(
  df,
  metric,
  grouping,
  df_goal,
  metric_goal,
  df_3p9,
  metric_3p9,
  df_6p6,
  metric_6p6,
  df_9p3,
  metric_9p3,
  full_yr = FALSE,
  full_yr_rate = FALSE,
  new_name = NULL,
  accounting = FALSE,
  div_by_1000 = TRUE
){

  # throw error if missing args
  if(missing(df)) stop("'df' is missing")
  if(missing(grouping)) stop("'grouping' is missing")

  # 'enquo' args for !!/!!!
  metric <- enquo(metric)
  grouping <- enquo(grouping)
  metric_goal <- enquo(metric_goal)

  # get relevant wk numbers
  if(grouping == "~wk_num_in_yr"){
    wk_nums <- df %>%
      filter(wk_end_date >= ceiling_date( ( Sys.Date() - (7 * 5) ) ) ) %>%
      select(wk_num_in_yr) %>%
      pull() %>% unique()
  }

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

  # split data into current year and previous year
  if(grouping != "~yr_num"){
    cur_yr_df <- df %>% filter(yr_num == max(df$yr_num)) %>% ungroup() %>% select(-yr_num) %>% rename(metric_cur_yr = !!metric)
    prev_yr_df <- df %>% filter(yr_num == max(df$yr_num) - 1) %>% ungroup() %>% select(-yr_num) %>% rename(metric_prev_yr = !!metric)
  }else{ # do not remove yr_num for join
    cur_yr_df <- df %>% filter(yr_num == max(df$yr_num)) %>% ungroup() %>% rename(metric_cur_yr = !!metric)
    prev_yr_df <- df %>% filter(yr_num == max(df$yr_num) - 1) %>% ungroup() %>% rename(metric_prev_yr = !!metric) %>% mutate(yr_num = yr_num + 1) # hack to join on current yr value
  }

  # join current year and previous year together
  if(grouping == "~wk_num_in_yr"){ # ***function will join wk_num_in_yr with wk_end_date strangely if you leave by argument empty
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
  }

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
  df <- df %>%
    mutate(
      prev_yr_var = round ( ( ( ( metric_cur_yr - metric_prev_yr ) / metric_prev_yr ) * 100 ), 2 )  # previous year variance
    )

  # calculate full yr values for mth view, store in variable as df to join later: included by default
  if(full_yr){
    df_full_yr <-
      df %>% select(metric_cur_yr, metric_prev_yr, metric_goal) %>%
        summarise_all(funs(sum(., na.rm = TRUE))) %>%
        mutate(prev_yr_var = round ( ( ( ( metric_cur_yr - metric_prev_yr ) / metric_prev_yr ) * 100 ), 2 ) ) %>% # previous yr variance
        mutate(goal_var = round ( ( ( ( metric_cur_yr - metric_goal ) / metric_goal ) * 100 ), 2 ) ) %>%  # goal variance
        select(metric_cur_yr, metric_prev_yr, prev_yr_var, metric_goal, goal_var) # do to enforce order
  }

  # divide values by 1000
  if(div_by_1000){
    if(!missing(df_goal)){
      df <- df %>% mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(div_by_one_thousand))
    }else{
      df <- df %>% mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(div_by_one_thousand))
    }
  }

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
      df_full_yr <- df_full_yr %>%
        mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(prettyNum(., big.mark = ",")))

      df_full_yr <- df_full_yr %>%
        mutate_at(
          vars(prev_yr_var, goal_var), funs(neg_paren))
    }

  }

  # define order for metrics to display in output
  ordering_array = c('metric_cur_yr', 'metric_prev_yr', 'prev_yr_var', 'metric_goal', 'goal_var')

  # transform from long to wide/horizontal view
  if(grouping == '~wk_num_in_yr'){ # wk
    df <- df %>%
      gather(metric, value, -!!grouping) %>%
      spread(!!grouping, value) %>%
      arrange(
        metric = ordered(metric, levels = ordering_array)
      ) %>%
      select(c("metric", wk_nums)) # orders the columns so the wks are chronological (ie: 51 52 1 2)
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
