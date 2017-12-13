# month run rate

mth_rr <- function(
  df,
  metric
){

if(missing(df)){ stop("'df' argument is mandatory") }
if(missing(metric)){ stop("'metric' argument is mandatory") }

metric <- enquo(metric)

cur_yr <- max(df$yr_num)
prev_yr <- cur_yr - 1
# cur_mth <- max(df$mth_num_in_yr)
cur_mth <- df %>% filter(yr_num == cur_yr) %>% summarise(max(mth_num_in_yr)) %>% pull()
prev_mth <- cur_mth - 1
today <- max(df$date_value)
today_prev_mth <- today - 30

df <- df %>% filter(yr_num == cur_yr,
                    mth_num_in_yr %in% c(cur_mth, prev_mth))

prev_mth_actuals_df <- df %>% filter(
  mth_num_in_yr == prev_mth,
  date_value > today_prev_mth
  )

cur_mth_actuals_df <- df %>% filter(
  mth_num_in_yr == cur_mth,
  date_value <= today
  )

final <- rbind(prev_mth_actuals_df, cur_mth_actuals_df) %>%
  mutate(mth_num_in_yr = max(mth_num_in_yr)) %>%
  group_by(yr_num, mth_num_in_yr) %>%
  summarise_at(vars(!!metric), funs(round_sum)) %>%
  mutate(type = "run_rate")

final

}


