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
#' @param cbr_ytd logical. Inlcude YTD definition for CBR : Actual values from last completed month
#' @param rate logical. Adjust logic to handle full year for rates
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
  week_end_dates = FALSE,
  week_start_dates = TRUE,
  cbr_ytd = FALSE,
  full_yr = FALSE,
  rate = FALSE,
  new_name = NULL,
  accounting = FALSE,
  div_by_1000 = TRUE,
  prefix = "",
  suffix = "",
  spark = FALSE,
  pop = FALSE,
  scalar = 1,
  op2_and_var_ph = TRUE,
  month_names = TRUE
){

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
  if(!missing(metric_goal)){
    indicator <- "OP2"
  }else if(!missing(metric_3p9)){
    indicator <- "3+9"
  }else if(!missing(metric_6p6)){
    indicator <- "6+6"
  }else if(!missing(metric_9p3)){
    indicator <- "9+3"
  }else{
    indicator <- NULL
  }

  # get relevant wk numbers
  if(grouping == "~wk_num_in_yr"){
    # wk_nums <- df %>%
    #   arrange(wk_end_date) %>%
    #   filter(wk_end_date >= ceiling_date( ( max(wk_end_date) - (7 * weeks_back) ) ) ) %>% # controls weeks back to calculate metric
    #   select(wk_num_in_yr) %>%
    #   pull() %>% unique()
    wk_nums <- df %>%
      arrange(wk_start_date) %>%
      filter(wk_start_date >= ceiling_date( ( max(wk_start_date) - (7 * weeks_back) ) ) ) %>% # controls weeks back to calculate metric
      select(wk_num_in_yr) %>%
      pull() %>% unique()
    # wk_end_dates <- df %>%
    #   arrange(wk_end_date) %>%
    #   filter(wk_end_date >= ceiling_date( ( max(wk_end_date) - (7 * weeks_back) ) ) ) %>%
    #   select(wk_end_date) %>%
    #   pull() %>% unique()
    wk_start_dates <- df %>%
      arrange(wk_start_date) %>%
      filter(wk_start_date >= ceiling_date( ( max(wk_start_date) - (7 * weeks_back) ) ) ) %>%
      select(wk_start_date) %>%
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
      summarise_at(vars(!!metric), funs(round_sum)) %>%
      filter(wk_num_in_yr %in% wk_nums)
  }else{
    df <- df %>% group_by(yr_num, !!grouping) %>%
      summarise_at(vars(!!metric), funs(round_sum))
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
      rename(metric_rr = !!metric_rr)

    cur_yr_df <- left_join(cur_yr_df, df_rr) %>%
      select(-yr_num) %>%
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
      df <- df %>%
        mutate(
          type_cur_yr = if_else(!is.na(metric_cur_yr), "Actual", "OP2"),
          # mth_num_in_yr = paste0(mth_num_in_yr,"|",type_cur_yr),
          metric_cur_yr = if_else(is.na(metric_cur_yr), metric_goal, metric_cur_yr)
          ) %>%
        mutate(goal_var = round ( ( ( ( metric_cur_yr - metric_goal ) / metric_goal ) * 100 ), 2 ) ) %>%  # op2 var
        mutate(goal_var = ifelse(goal_var == 0, NA, goal_var))

    }else{ # else do nothing
      NULL
    }
  }

  # calculate goal var TODO

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
        metric_cur_yr = round((metric_cur_yr / 12), 2),
        metric_prev_yr = round((metric_prev_yr / 12), 2)
      )
  }

  if(grouping == "~mth_num_in_yr" & cbr_ytd & rate){
    df_cbr_ytd <- df_cbr_ytd %>%
      mutate(
        metric_cur_yr = round((metric_cur_yr / ( cur_yr_mth - 1) ), 2), # number of months with actual values
        metric_prev_yr = round((metric_prev_yr / ( cur_yr_mth - 1) ), 2)
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



  # FULL YR
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
  # for df
  if(!missing(df_goal)){
    df <- df %>%
      mutate(
        metric_cur_yr = paste0(prefix, metric_cur_yr, suffix),
        metric_prev_yr = paste0(prefix, metric_prev_yr, suffix),
        metric_goal = paste0(prefix, metric_goal, suffix)
      )
  }else{
    df <- df %>%
      mutate(
        metric_cur_yr = paste0(prefix, metric_cur_yr, suffix),
        metric_prev_yr = paste0(prefix, metric_prev_yr, suffix)
      )
  }

  # FULL YR
  # for df_full_yr
  if(grouping == "~mth_num_in_yr"){
    if(full_yr){
      if(!missing(df_goal)){
        df_full_yr <- df_full_yr %>%
          mutate(
            metric_cur_yr = paste0(prefix, metric_cur_yr, suffix),
            metric_prev_yr = paste0(prefix, metric_prev_yr, suffix),
            metric_goal = paste0(prefix, metric_goal, suffix)
          )
      }else{
        df_full_yr <- df_full_yr %>%
          mutate(
            metric_cur_yr = paste0(prefix, metric_cur_yr, suffix),
            metric_prev_yr = paste0(prefix, metric_prev_yr, suffix)
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
        prev_yr_var = paste0(prev_yr_var, " ppts")
        )
      if(!missing(df_goal)){
        df <- df %>% mutate(
          goal_var = paste0(goal_var, " ppts")
        )
      }

      if(full_yr){
        df_full_yr <- df_full_yr %>%
          mutate(
            prev_yr_var = paste0(prev_yr_var, " ppts")
            )
        if(!missing(df_goal)){
          df_full_yr <- df_full_yr %>%
            mutate(
              goal_var = paste0(goal_var, " ppts")
            )
        }
      }

      if(cbr_ytd){
        df_cbr_ytd <- df_cbr_ytd %>%
          mutate(
            prev_yr_var = paste0(prev_yr_var, " ppts")
          )
        if(!missing(df_goal)){
          df_cbr_ytd <- df_cbr_ytd %>%
            mutate(
              goal_var = paste0(goal_var, " ppts")
            )
        }
      }

  }else{

      df <- df %>% mutate(
        prev_yr_var = paste0(prev_yr_var, "%")
        )
      if(!missing(df_goal)){
        df <- df %>% mutate(
          goal_var = paste0(goal_var, "%")
        )
      }

      if(full_yr){
        df_full_yr <- df_full_yr %>%
          mutate(
            prev_yr_var = paste0(prev_yr_var, "%")
            )
        if(!missing(df_goal)){
          df_full_yr <- df_full_yr %>%
            mutate(
              goal_var = paste0(goal_var, "%")
            )
        }
      }

      if(cbr_ytd){
        df_cbr_ytd <- df_cbr_ytd %>%
          mutate(
            prev_yr_var = paste0(prev_yr_var, "%")
          )
        if(!missing(df_goal)){
          df_cbr_ytd <- df_cbr_ytd %>%
            mutate(
              goal_var = paste0(goal_var, "%")
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

    # FULL YR
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

    #df_cbr_ytd
    if(cbr_ytd){
      if(!missing(df_goal)){

        df_cbr_ytd <- df_cbr_ytd %>%
          mutate_at(vars(metric_cur_yr, metric_prev_yr, metric_goal), funs(prettyNum(., big.mark = ",")))

        df_cbr_ytd <- df_cbr_ytd %>%
          mutate_at(
            vars(prev_yr_var, goal_var), funs(neg_paren))

      }else{

        df_cbr_ytd <- df_cbr_ytd %>%
          mutate_at(vars(metric_cur_yr, metric_prev_yr), funs(prettyNum(., big.mark = ",")))

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

  # add week end dates to week column (to be header)
  if(week_end_dates & grouping == "~wk_num_in_yr"){
    # print(names(df))
    # print(wk_nums)
    # print(wk_end_dates %>% as.character())
    names(df)[2:5] <- paste(paste0("w",wk_nums), wk_end_dates %>% as.character(), sep = "|")
  }

  # add week start dates to week column (to be header)
  if(week_start_dates & grouping == "~wk_num_in_yr"){
    # print(names(df))
    # print(wk_nums)
    # print(wk_end_dates %>% as.character())
    names(df)[2:5] <- paste(paste0("w",wk_nums), wk_start_dates %>% as.character(), sep = "|")
  }

  # join df_cbr_ytd to mth data
  if(cbr_ytd){
    df <- left_join(df, df_cbr_ytd %>% gather(metric, `YTD`))
  }

  # FULL YR
  # join df_full_yr to mth data
  if(full_yr){
    df <- left_join(df, df_full_yr %>% gather(metric, `Full Year`))
  }

  # join spark chart
  if(spark){
    df <- left_join(df, spark_df)
  }

  #if op2_and_var_ph (op2 and variance place holder) == TRUE add placeholder lines
  if(op2_and_var_ph == TRUE){
    ph <- tibble(metric = c('metric_cur_yr', 'metric_prev_yr', 'prev_yr_var', 'metric_goal', 'goal_var'))
    df <- left_join(ph, df)
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
  }

  # return data frame
  df

}
