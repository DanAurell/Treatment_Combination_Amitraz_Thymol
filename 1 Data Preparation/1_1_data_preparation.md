---
title: "1.1 Data Preparation"
author: "Dan Aurell"
date: "2023-04-05"
output:   
  html_document: 
    keep_md: yes
---


```r
library(MASS)
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.1     ✔ purrr   1.0.1
## ✔ tibble  3.1.8     ✔ dplyr   1.1.0
## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
## ✔ readr   2.1.4     ✔ forcats 1.0.0
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ✖ dplyr::select() masks MASS::select()
```

# Load data, make simpler dataset --------

Rename

```r
datum_orig <- read.csv("./data/data9 flat v2.csv")

datum_orig <- datum_orig %>% rename(
  bees = bees_num,
  pattern = brd_pattern,
  phor = perc_phoretic_mites_by_weight,
  infest_200 = infested_in_first_200_cells_num,
  trt = treatment,
  wk_cells = wrk_capbrd_cells_num,
  fob_at_all = fob_covered_at_all,
  fob_top_bip = top_of_box_bip_style_fob
  
)
```


Here I change the units of a few columns and select the columns of interest

```r
datum <- datum_orig %>% 
  mutate(
    days = ifelse(time_point == 1, 0, 
                     ifelse(time_point == 10, 10, 
                            ifelse(time_point == 17, 21, 
                                   ifelse(time_point == 22, 42, 
                                          ifelse(time_point == 34, 138, NA))))),
    est_bee_num_by_wt_floor = floor(est_bee_num_by_wt),
    bees_without_mites = est_bee_num_by_wt_floor-mites_recov_3_washes,
    bees_frames = bees_tot_sides/2, # now expressed in frames
    wk_frames = wk_capbrd_tot_sides/2, # now expressed in frames
    honey_frames = revised_honey_tot_sides/2, # now expressed in frames
    pollen_frames = revised_pollen_tot_sides/2, # now expressed in frames
    phor2 = 100*mites_recov_3_washes/est_bee_num_by_wt, # make unrounded variable for phor
    cap_brd_indiv = dr_capbrd_cells_num + wk_cells, 
    indiv_total = cap_brd_indiv + bees, # Make a composite variable of both brood and bees
    indiv_capbrd_pct = 100*cap_brd_indiv/indiv_total,
    indiv_adult_pct = 100*bees/indiv_total,
    phoretic_mites_num_estimate = floor(phoretic_mites_num_estimate),
    wk_cells_infested_estimate = floor(wk_cells_infested_estimate),
    dr_cells_infested_estimate = floor(dr_cells_infested_estimate),
    mites_total_estimate = floor(mites_total_estimate),
    ) %>% 
  select(
    col_no,
    trt,
    time_point,
    date,
    days,
    q_status,
    q_event,
    pattern,
    notes,
    est_bee_num_by_wt,
    est_bee_num_by_wt_floor,
    mites_recov_3_washes,
    bees_without_mites,
    phor2, # Use unrounded column
    infest_200,
    dwv_num_sides_obs,
    dwv_tf,
    cdb_num_sides_obs,
    bees,
    wk_cells,
    dr_capbrd_cells_num,
    phoretic_mites_num_estimate,
    wk_cells_infested_estimate,
    dr_cells_infested_estimate,
    mites_total_estimate,
    cap_brd_indiv,
    indiv_total,
    indiv_adult_pct,
    indiv_capbrd_pct,
    bees_frames,
    wk_frames,
    honey_frames,
    pollen_frames,
    fob_at_all,
    fob_top_bip,
    col_hist
  ) %>%
  # Filter to include only the time points where we have data
  filter(days %in% c(0, 10, 21, 42, 138),
         (col_no != "C15" | days != "42") # Remove row for a deadout on Day 42
         )
datum[sapply(datum, is.infinite)] <- NA # To remove Inf as value in cells
datum[sapply(datum, is.nan)] <- NA
```


Vector of queen events

```r
# From "collated qlessness" sheet

# Any colonies that became queenless between day 0 and 42
qless <- c("D8", "RC1", "D21", "D24", "C6", "BC15")
```


How many colonies in each treatment group came from the repeated-OA vapor trial?

```r
datum %>% 
  filter(col_hist == "Christian OA")
```

```
##   col_no          trt time_point   date days q_status q_event pattern notes
## 1      8 Apiguard_Api          1 19-Sep    0       QR             3.0      
## 2     S1 Apiguard_Api          1 19-Sep    0       QS             3.5      
## 3     17       Apivar          1 19-Sep    0       QR             2.0      
## 4     C8 Apiguard_Api          1 19-Sep    0       QR             2.5      
## 5     C2          AEC          1 19-Sep    0    QS-MP             2.5      
## 6    N51          AEC          1 19-Sep    0       QR             3.5      
## 7    C15       Apivar          1 19-Sep    0       QS             2.5      
##   est_bee_num_by_wt est_bee_num_by_wt_floor mites_recov_3_washes
## 1             307.5                     307                   27
## 2             296.1                     296                   21
## 3             281.3                     281                   19
## 4             300.6                     300                   29
## 5             319.7                     319                   13
## 6             256.0                     256                   14
## 7             316.6                     316                   51
##   bees_without_mites     phor2 infest_200 dwv_num_sides_obs dwv_tf
## 1                280  8.780488          8                 0      0
## 2                275  7.092199         17                 1      1
## 3                262  6.754355         20                 0      0
## 4                271  9.647372         50                 1      1
## 5                306  4.066312         13                 0      0
## 6                242  5.468750         32                 1      1
## 7                265 16.108654         12                 0      0
##   cdb_num_sides_obs     bees wk_cells dr_capbrd_cells_num
## 1                 0 11937.38  23993.2                 0.0
## 2                 0  8383.50  16218.4                 0.0
## 3                 0  7654.50  14295.6                 0.0
## 4                 0  9234.00  10282.8                 0.0
## 5                 0 19561.50  17806.8                 0.0
## 6                 0 12089.25  19646.0               485.1
## 7                 0  5315.63  12122.0                 0.0
##   phoretic_mites_num_estimate wk_cells_infested_estimate
## 1                        1048                        959
## 2                         594                       1378
## 3                         516                       1429
## 4                         890                       2570
## 5                         795                       1157
## 6                         661                       3143
## 7                         856                        727
##   dr_cells_infested_estimate mites_total_estimate cap_brd_indiv indiv_total
## 1                         NA                 2008       23993.2    35930.58
## 2                         NA                 1973       16218.4    24601.90
## 3                         NA                 1946       14295.6    21950.10
## 4                         NA                 3461       10282.8    19516.80
## 5                         NA                 1952       17806.8    37368.30
## 6                         NA                 3804       20131.1    32220.35
## 7                         NA                 1583       12122.0    17437.63
##   indiv_adult_pct indiv_capbrd_pct bees_frames wk_frames honey_frames
## 1        33.22345         66.77655      4.9125    3.5875        5.050
## 2        34.07664         65.92336      3.4500    2.4250        4.100
## 3        34.87228         65.12772      3.1500    2.1375        3.225
## 4        47.31308         52.68692      3.8000    1.5375        6.525
## 5        52.34785         47.65215      8.0500    2.6625        3.125
## 6        37.52054         62.47946      4.9750    2.9375        6.375
## 7        30.48367         69.51633      2.1875    1.8125        3.175
##   pollen_frames fob_at_all fob_top_bip     col_hist
## 1            NA         NA          NA Christian OA
## 2            NA         NA          NA Christian OA
## 3            NA         NA          NA Christian OA
## 4            NA         NA          NA Christian OA
## 5            NA         NA          NA Christian OA
## 6            NA         NA          NA Christian OA
## 7            NA         NA          NA Christian OA
```

```r
datum %>% 
  filter(!(col_no %in% qless),
         col_hist == "Christian OA" 
  )
```

```
##   col_no          trt time_point   date days q_status q_event pattern notes
## 1      8 Apiguard_Api          1 19-Sep    0       QR             3.0      
## 2     S1 Apiguard_Api          1 19-Sep    0       QS             3.5      
## 3     17       Apivar          1 19-Sep    0       QR             2.0      
## 4     C8 Apiguard_Api          1 19-Sep    0       QR             2.5      
## 5     C2          AEC          1 19-Sep    0    QS-MP             2.5      
## 6    N51          AEC          1 19-Sep    0       QR             3.5      
## 7    C15       Apivar          1 19-Sep    0       QS             2.5      
##   est_bee_num_by_wt est_bee_num_by_wt_floor mites_recov_3_washes
## 1             307.5                     307                   27
## 2             296.1                     296                   21
## 3             281.3                     281                   19
## 4             300.6                     300                   29
## 5             319.7                     319                   13
## 6             256.0                     256                   14
## 7             316.6                     316                   51
##   bees_without_mites     phor2 infest_200 dwv_num_sides_obs dwv_tf
## 1                280  8.780488          8                 0      0
## 2                275  7.092199         17                 1      1
## 3                262  6.754355         20                 0      0
## 4                271  9.647372         50                 1      1
## 5                306  4.066312         13                 0      0
## 6                242  5.468750         32                 1      1
## 7                265 16.108654         12                 0      0
##   cdb_num_sides_obs     bees wk_cells dr_capbrd_cells_num
## 1                 0 11937.38  23993.2                 0.0
## 2                 0  8383.50  16218.4                 0.0
## 3                 0  7654.50  14295.6                 0.0
## 4                 0  9234.00  10282.8                 0.0
## 5                 0 19561.50  17806.8                 0.0
## 6                 0 12089.25  19646.0               485.1
## 7                 0  5315.63  12122.0                 0.0
##   phoretic_mites_num_estimate wk_cells_infested_estimate
## 1                        1048                        959
## 2                         594                       1378
## 3                         516                       1429
## 4                         890                       2570
## 5                         795                       1157
## 6                         661                       3143
## 7                         856                        727
##   dr_cells_infested_estimate mites_total_estimate cap_brd_indiv indiv_total
## 1                         NA                 2008       23993.2    35930.58
## 2                         NA                 1973       16218.4    24601.90
## 3                         NA                 1946       14295.6    21950.10
## 4                         NA                 3461       10282.8    19516.80
## 5                         NA                 1952       17806.8    37368.30
## 6                         NA                 3804       20131.1    32220.35
## 7                         NA                 1583       12122.0    17437.63
##   indiv_adult_pct indiv_capbrd_pct bees_frames wk_frames honey_frames
## 1        33.22345         66.77655      4.9125    3.5875        5.050
## 2        34.07664         65.92336      3.4500    2.4250        4.100
## 3        34.87228         65.12772      3.1500    2.1375        3.225
## 4        47.31308         52.68692      3.8000    1.5375        6.525
## 5        52.34785         47.65215      8.0500    2.6625        3.125
## 6        37.52054         62.47946      4.9750    2.9375        6.375
## 7        30.48367         69.51633      2.1875    1.8125        3.175
##   pollen_frames fob_at_all fob_top_bip     col_hist
## 1            NA         NA          NA Christian OA
## 2            NA         NA          NA Christian OA
## 3            NA         NA          NA Christian OA
## 4            NA         NA          NA Christian OA
## 5            NA         NA          NA Christian OA
## 6            NA         NA          NA Christian OA
## 7            NA         NA          NA Christian OA
```

Curiosity: bee-brood ratio

```r
datum %>% 
  filter(!col_no %in% qless, days == 0) %>%
  summarise(
    mean_bees = mean(bees),
    mean_wk_capped = mean(wk_cells)
  )
```

```
##   mean_bees mean_wk_capped
## 1  10309.28       17345.98
```

```r
# bees: 10309
# capped worker: 17345
# all worker: 17345*21/12 = 30353
# suitable: 30353/21 = 1445
# Assume 100 mg per bee, so 10,000 bees/ kg
# kg bees = 10309/10000 = 1.03
# cells suitable per kg bees = 1445/1.03 = 1402
  # Way extrapolates data from Boot. This would be a super short phoretic period with say 0.5 or more probability of invasion each day
```






# CALCULATE DAY-0 VARIABLES --------------

### Make new pre-treatment columns

Make a column of pre-treatment values for each variable of interest.
In order to do ANCOVA-POST if I wish to - and to include as predictors in mixed modeling:


```r
# Extract row based on colony number of row i and day = 0
# Write the value of the column of interest (say bees to start with) into a pre_bees column

#    pattern
for(i in 1:nrow(datum)) {
  datum$pre_pattern[i] <- datum[which(datum$days == 0 & datum$col_no == datum$col_no[i]), ]$pattern
}

#    phor2
for(i in 1:nrow(datum)) {
  datum$pre_phor2[i] <- datum[which(datum$days == 0 & datum$col_no == datum$col_no[i]), ]$phor2
}

#    infest_200
for(i in 1:nrow(datum)) {
  datum$pre_infest_200[i] <- datum[which(datum$days == 0 & datum$col_no == datum$col_no[i]), ]$infest_200
}

#    mites_tot
for(i in 1:nrow(datum)) {
  datum$pre_mites_tot[i] <- datum[which(datum$days == 0 & datum$col_no == datum$col_no[i]), ]$mites_tot
}

#    bees
for(i in 1:nrow(datum)) {
  datum$pre_bees[i] <- datum[which(datum$days == 0 & datum$col_no == datum$col_no[i]), ]$bees
}

#    bees_frames
for(i in 1:nrow(datum)) {
  datum$pre_bees_frames[i] <- datum[which(datum$days == 0 & datum$col_no == datum$col_no[i]), ]$bees_frames
}

#    wk_cells
for(i in 1:nrow(datum)) {
  datum$pre_wk_cells[i] <- datum[which(datum$days == 0 & datum$col_no == datum$col_no[i]), ]$wk_cells
}

#    wk_frames
for(i in 1:nrow(datum)) {
  datum$pre_wk_frames[i] <- datum[which(datum$days == 0 & datum$col_no == datum$col_no[i]), ]$wk_frames
}

#    honey_frames
for(i in 1:nrow(datum)) {
  datum$pre_honey_frames[i] <- datum[which(datum$days == 0 & datum$col_no == datum$col_no[i]), ]$honey_frames
}

#    pollen_frames
for(i in 1:nrow(datum)) {
  datum$pre_pollen_frames[i] <- datum[which(datum$days == 0 & datum$col_no == datum$col_no[i]), ]$pollen_frames
}

# Other variables that could be valuable predictors:
# dwv_num_sides_obs
# dwv_tf
# cdb_num_sides_obs
```

Save datum as a .csv

```r
write.csv(datum, "../2 Analysis/data_prepped/datum_prepped.csv")
```

