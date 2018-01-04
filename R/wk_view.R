#week view

#' Transform regular data into the week view for BI view
#'
#' @param df A data frame with date dimensions
#' @param metric Column to be group and summate on
#' @param show_type Add row to output to indicate actual or run rate
#' @param num_wks_to_show Defaults to 4. Change to show more than 4 weeks
#' @param new_name Clean up nomenclature for metric column
#'
#' @return transformed data frame
#' @examples
#'
#'

week_view <- function(
  df,
  metric,
  show_type = FALSE,
  num_wks_to_show = 4,
  new_name = NULL,
  suffix = "",
  div_by_one_thousand = TRUE,
  accounting = TRUE
  # ,
  # sparkline = FALSE
) {

###### Error messaging ######

  if(missing(df)){ stop("'df' argument is mandatory") }
  if(missing(metric)){ stop("'metric' argument is mandatory") }

  required_cols <- list(
    'yr_num',
    'mth_num_in_yr',
    'wk_num_in_yr'
  )

  # if(any(!(required_cols %in% names(df)))){ stop("'df' argument missing required time dimension column(s): must have 'yr_num', 'mth_num_in_yr', and 'wk_num_in_yr'") }

###

###

cur_yr <- max(df$yr_num)
prev_yr <- cur_yr - 1
# cur_mth <- max(df$mth_num_in_yr)
# cur_mth <- df %>% filter(yr_num == cur_yr) %>% summarise(max(mth_num_in_yr)) %>% pull()
# prev_mth <- cur_mth - 1
today <- max(df$date_value)
today_prev_mth <- today - 30

###

  if(!missing(new_name)) new_name <- quo_name(new_name)

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
    group_by(yr_num, wk_num_in_yr) %>%
    summarise_at(vars(!!metric), funs(round_sum)) %>%
    # summarise_at(vars(!!metric), funs(sum)) %>%
    mutate(
      type = "actual"
      # ,
      # pop = round( 100 * ( ( (!!metric) - lag(!!metric) ) / lag(!!metric) ), 2 )
      ) %>%
    filter(between(wk_num_in_yr, prev_wk - (num_wks_to_show - 1), prev_wk)) %>%
    ungroup()

  prev_yr_df <- df %>% filter(yr_num == prev_yr) %>%
    group_by(yr_num, wk_num_in_yr) %>%
    summarise_at(vars(!!metric), funs(round_sum)) %>%
    # summarise_at(vars(!!metric), funs(sum)) %>%
    mutate(type = "actual") %>%
    filter(between(wk_num_in_yr, prev_wk - (num_wks_to_show - 1), prev_wk)) %>%
    ungroup()

  prev_yr_var_df <- right_join(cur_yr_df %>% select(wk_num_in_yr, !!metric, type),
                               prev_yr_df %>% select(wk_num_in_yr, !!metric),
                               by = c("wk_num_in_yr" = "wk_num_in_yr"),
                               suffix = c("_cur_yr","_prev_yr")) %>%
    mutate(
     !!metric_prev_yr_var_name :=
       round( 100 * ( (UQ(metric_cur_yr_name)) - (UQ(metric_prev_yr_name)) )
              /
                (UQ(metric_prev_yr_name)), 2 )
   )

  if(div_by_one_thousand){
    prev_yr_var_df <- prev_yr_var_df %>% mutate_at(vars(!!metric_cur_yr_name, !!metric_prev_yr_name), funs(div_by_one_thousand))
  }

  if(accounting){

    prev_yr_var_df <- prev_yr_var_df %>%
      mutate_at(
        vars(
          !!metric_cur_yr_name,
          !!metric_prev_yr_name
          # ,
          # !!metric_prev_yr_var_name ### BUG, won't wrap negative numbers in parenthesis
        ),
        funs(prettyNum(., big.mark = ","))
      )

    prev_yr_var_df <- prev_yr_var_df %>%
      mutate_at(vars(!!metric_prev_yr_var_name),
                funs(neg_paren))
  }



  #### SPARKLINE #####

  # if(sparkline == TRUE){

    # metric_cur_yr_spark <- prev_yr_var_df %>% pull(!!metric_cur_yr_name) %>% sparkline()
    # metric_prev_yr_spark <- prev_yr_var_df %>% pull(!!metric_prev_yr_name) %>% sparkline()
    # metric_prev_yr_var_spark <- prev_yr_var_df %>% pull(!!metric_prev_yr_var_name) %>% sparkline(type = 'bar')

    # spark_df <- rbind(metric_cur_yr_spark,
    #                   metric_prev_yr_spark,
    #                   metric_prev_yr_var_spark)

  # }

  ### add 'w' for week nums & and opt in adding suffixs

  prev_yr_var_df <- prev_yr_var_df %>%
    mutate(wk_num_in_yr = paste0("w", wk_num_in_yr),
           !!metric_cur_yr_name := paste0(!!metric_cur_yr_name, suffix),
           !!metric_prev_yr_name := paste0(!!metric_prev_yr_name, suffix))

  if(!missing(new_name)){

    prev_yr_var_df <- prev_yr_var_df %>% rename(
      !!new_name := UQ(metric_cur_yr_name),
      `Prior Year` = UQ(metric_prev_yr_name),
      `Variance vs. Prior Year` = UQ(metric_prev_yr_var_name)
    )

  }

  final <- prev_yr_var_df %>%
    gather(metric, value, -wk_num_in_yr) %>%
    spread(wk_num_in_yr, value)

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
