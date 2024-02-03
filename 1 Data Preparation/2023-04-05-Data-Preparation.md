---
title: "Data Preparation"
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
    bees_without_mites = est_bee_num_by_wt_floor-mites_recov_total,
    bees_frames = bees_tot_sides/2, # expressed in frames
    wk_frames = wk_capbrd_tot_sides/2, # expressed in frames
    honey_frames = revised_honey_tot_sides/2, # expressed in frames
    pollen_frames = revised_pollen_tot_sides/2, # expressed in frames
    phor2 = 100*mites_recov_total/est_bee_num_by_wt, # make unrounded variable for phor
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
    mites_recov_total,
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
    fob_top_bip
  ) %>%
  # Filter to include only the time points where we have data
  filter(days %in% c(0, 10, 21, 42, 138),
         (col_no != "C15" | days != "42") # Remove row for a deadout on Day 42
         )
datum[sapply(datum, is.infinite)] <- NA
datum[sapply(datum, is.nan)] <- NA

# That got rid of the infinite value cells
```


Vector of queen events

```r
# From "collated qlessness" sheet

# Any colonies that became queenless between day 0 and 42
qless <- c("D8", "RC1", "D21", "D24", "C6", "BC15")
```


Make a separate table of colony survival








# CALCULATE DAY-0 VARIABLES --------------

### Make new pre-treatment columns

Make a column of pre-treatment values for each variable of interest.
In order to do ANCOVA-POST and to include as predictors in mixed modeling:


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



prep datum for graphing - necessary?

```r
# datum_cont <- datum
# datum_cont$days <- as.numeric(as.character(datum_cont$days))
```

Save datum as a .csv

```r
write.csv(datum, "../2 Analysis/data_prepped/datum_prepped.csv")
```

