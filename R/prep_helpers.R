# prep with sample data and libraries

library(tidyverse)
library(rlang)
library(lubridate)
library(glue)

div_by_one_thousand <- function(x){
  x/1000
}

round_sum <- function(x){
  round(sum(x, na.rm=TRUE), 2)
}

round_to_two <- function(x){
  round(x, 2)
}

date_seq <- seq(as.Date("2016-01-01"), Sys.Date(), "day")

today <- Sys.Date()
# cur_month <- lubridate::month(Sys.Date())

cur_yr <- year(Sys.Date())
prev_yr <- year(Sys.Date() - 365)

# cur_year <- year(Sys.Date())
cur_mth <- month(Sys.Date())
prev_mth <- month(Sys.Date()) - 1

prev_wk <- week(Sys.Date() - 7)

today <- Sys.Date()
today_prev_mth <- Sys.Date() - 30 # is this the correct way to do this?

# metrics <- c("revenue",
#            "trials")

dimensions <- c("date_value",
                "yr_num",
                "mth_num_in_yr",
                "wk_num_in_yr")

# sample_data <- data.frame(
#   marketplace_short_name = "US",
#   date_value = date_seq,
#   yr_num = year(date_seq),
#   mth_num_in_yr = lubridate::month(date_seq),
#   mth_name = lubridate::month(date_seq, label = TRUE),
#   wk_num_in_yr = week(date_seq),
#   wk_start_date = floor_date(date_seq, unit = 'weeks'),
#   revenue = rnorm(n = length(date_seq), mean = 50000, sd = 3582),
#   trials = round(rnorm(n = length(date_seq), mean = 500, sd = 35), 0),
#   converts = round(rnorm(n = length(date_seq), mean = 100, sd = 35), 0),
#   members = round(rnorm(n = length(date_seq), mean = 1000000, sd = 35), 0) # TOFIX (INCREASING)
#   )

# sample_data %>% write_csv("sample_data.csv")

# op2 is at monthly grain
# sample_goals <- data.frame(yr_num = 2017,
#                   mth_num_in_yr = 1:12,
#                   revenue_op2 = 1555000,
#                   trials_op2 = 17000,
#                   converts_op2 = 12000,
#                   revenue_per_member_op2 = .05)

# sample_goals %>% write_csv("sample_budget_data.csv")

# sample_data2 <- sample_data %>% filter(date_value <= '2017-08-15')

