# Overview

This R package contains a series of functions used to achieve a horizontal-oriented data reporting view

# Installation

```r
devtools::install_github("pkopps/biview")
```

# Usage

```r
fun(
  performance,
  metric = revenue,
  grouping = wk_num_in_yr
)
# # A tibble: 5 x 5
#   metric         `w23|03-Jun` `w24|10-Jun` `w25|17-Jun` `w26|24-Jun`
#   <chr>          <chr>        <chr>        <chr>        <chr>       
# 1 metric_cur_yr  3495997.57   3522205.21   3503224.4    2990343.11  
# 2 metric_prev_yr 3503042.46   3497006.25   3501900.98   3500468.76  
# 3 prev_yr_var    -0.2%        0.72%        0.04%        -14.57%     
# 4 metric_goal    NA           NA           NA           NA          
# 5 goal_var       NA           NA           NA           NA
```
