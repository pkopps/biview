neg_paren <- function(x){
  ind <- grepl("-", x)
  x[ind] <-  paste0("(", sub("-", "", x[ind]), "%)")
  x
}

div_by_one_thousand <- function(x){
  y <- x/1000
  round(y, 2)
}

round_sum <- function(x){
  round(sum(x, na.rm=TRUE), 2)
}

round_to_two <- function(x){
  round(x, 2)
}

date_seq <- seq(as.Date("2017-01-01"), Sys.Date(), "day")

today <- Sys.Date()
# cur_month <- lubridate::month(Sys.Date())

cur_yr <- year(Sys.Date() - 2)
prev_yr <- year(Sys.Date() - (365 + 2))

# cur_year <- year(Sys.Date())
cur_mth <- month(Sys.Date())
prev_mth <- month(Sys.Date()) - 1

cur_yr_prev_wk <- week(Sys.Date() - 7)
prev_yr_prev_wk <- week((Sys.Date() - 365) - 7)

today <- Sys.Date()
today_prev_mth <- Sys.Date() - 30 # is this the correct way to do this?

# metrics <- c("revenue",
#            "trials")

dimensions <- c("date_value",
                "yr_num",
                "mth_num_in_yr",
                "wk_num_in_yr")

# performance <- data.frame(
#   company_name = "Dunder Mifflin",
#   marketplace_short_name = "US",
#   date_value = date_seq,
#   yr_num = epiyear(date_seq),
#   mth_num_in_yr = lubridate::month(date_seq),
#   mth_name = lubridate::month(date_seq, label = TRUE),
#   wk_num_in_yr = epiweek(date_seq),
#   wk_start_date = floor_date(date_seq, unit = 'weeks'),
#   wk_end_date = ceiling_date(date_seq, unit = 'weeks'),
#   revenue = rnorm(n = length(date_seq), mean = 500000, sd = 3582),
#   trials = round(rnorm(n = length(date_seq), mean = 500, sd = 35), 0),
#   converts = round(rnorm(n = length(date_seq), mean = 100, sd = 35), 0),
#   members = round(rnorm(n = length(date_seq), mean = 1000000, sd = 35), 0) # TOFIX (INCREASING)
# )
#
# devtools::use_data(performance, performance, overwrite = TRUE)

#
# sample_data %>% write_csv("sample_data.csv")
# sample_data <- read_csv("sample_data.csv")

# op2 is at monthly grain
# sample_goals <- data.frame(yr_num = 2017,
#                   mth_num_in_yr = 1:12,
#                   revenue_op2 = 1555000,
#                   trials_op2 = 17000,
#                   converts_op2 = 12000,
#                   revenue_per_member_op2 = .05)

# sample_goals %>% write_csv("sample_budget_data.csv")

# sample_data2 <- sample_data %>% filter(date_value <= '2017-08-15')

# sample_gaap <- data.frame(
#   yr_num = 2017,
#   mth_num_in_yr = 1:12,
#   revenue_gaap = 1553000
#   )
#
# sample_gaap %>% write_csv("sample_gaap.csv")

