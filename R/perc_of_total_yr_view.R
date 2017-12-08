# perc_of_total_week_view

perc_of_total_yr_view <- function(
  df,
  metric,
  group_col,
  filter_col,
  show_type = FALSE,
  num_wks_to_show = 4,
  new_name = NULL,
  prefix = "",
  suffix = ""
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

  if(any(!(required_cols %in% names(df)))){ stop("'df' argument missing required time dimension column(s):
                                                 must have 'yr_num', 'mth_num_in_yr', and 'wk_num_in_yr'") }

###

  if(!missing(new_name)) new_name <- quo_name(new_name)

  metric <- enquo(metric)
  metric_name <- quo_name(metric)

  metric_cur_yr_name <- as.name(paste(metric_name, "cur_yr", sep = "_"))
  metric_prev_yr_name <- as.name(paste(metric_name, "prev_yr", sep = "_"))

  metric_prev_yr_var_name <- paste(metric_name, "prev_yr_var", sep = "_")
  metric_prev_yr_var <- enquo(metric_prev_yr_var_name)

  metric_prev_yr_var_name <- paste(metric_name, "prev_yr_var", sep = "_")

  group_col <- enquo(group_col)
  group_col_name <- quo_name(group_col)

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

  cur_yr_df <- df %>%
    filter(yr_num == cur_yr) %>%
    group_by(yr_num, wk_num_in_yr, !!group_col) %>%
    summarise_at(vars(!!metric), funs(sum)) %>%
    mutate(perc_total = round(100 * (UQ(metric) / sum(UQ(metric))), 2)) %>%
    mutate(type = "actual") %>%
    filter(between(wk_num_in_yr, prev_wk - (num_wks_to_show - 1), prev_wk)) %>%
    filter(UQE(group_col) == filter_col) %>%
    ungroup()

  prev_yr_df <- df %>%
    filter(yr_num == prev_yr) %>%
    group_by(yr_num, wk_num_in_yr, !!group_col) %>%
    summarise_at(vars(!!metric), funs(sum)) %>%
    mutate(perc_total = round(100 * (UQ(metric) / sum(UQ(metric))), 2)) %>%
    mutate(type = "actual") %>%
    filter(between(wk_num_in_yr, prev_wk - (num_wks_to_show - 1), prev_wk)) %>%
    filter(UQE(group_col) == filter_col) %>% ### UQE not UQ #######
    ungroup()

  prev_yr_var_df <-
    right_join(
      cur_yr_df %>% select(wk_num_in_yr, perc_total, type),
      prev_yr_df %>% select(wk_num_in_yr, perc_total),
      by = c("wk_num_in_yr" = "wk_num_in_yr"),
      suffix = c("_cur_yr","_prev_yr")
    ) %>%
    mutate(perc_total_prev_yr_var = round( (perc_total_cur_yr - perc_total_prev_yr) , 2) ) ## "3 ppts"

  prev_yr_var_df <- prev_yr_var_df %>%
    mutate(wk_num_in_yr = paste0("w", wk_num_in_yr),
           perc_total_cur_yr = paste0(prefix, perc_total_cur_yr, suffix),
           perc_total_prev_yr = paste0(prefix, perc_total_prev_yr, suffix)
           # ,
           # rate_prev_yr_var = paste0(prefix, rate_prev_yr_var, suffix)
    )

  if(!missing(new_name)){

    prev_yr_var_df <- prev_yr_var_df %>% rename(!!new_name := perc_total_cur_yr,
                                                `Prior Year` = perc_total_prev_yr,
                                                `Variance vs. Prior Year` = perc_total_prev_yr_var)

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
