---
title: "2.1 Varroa Infestation"
author: "Dan Aurell"
date: "2023-05-22"
output:   
  html_document: 
    keep_md: yes
---

# Setup


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

```r
library(lme4)
```

```
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
```

```r
library(boot)
library(emmeans)
library(broom)
library(DHARMa)
```

```
## This is DHARMa 0.4.6. For overview type '?DHARMa'. For recent changes, type news(package = 'DHARMa')
```

Vector of queen events

```r
# From "collated qlessness" sheet

# Any colonies that became queenless between day 0 and 42
qless <- c("D8", "RC1", "D21", "D24", "C6", "BC15")
```


```r
datum <- read.csv("../2 Analysis/data_prepped/datum_prepped.csv")
```

Check treatment group of the colonies that were removed

```r
datum %>% 
  filter(days == 0, col_no %in% qless)
```

```
##    X col_no          trt time_point   date days q_status q_event pattern notes
## 1  9     D8       Apivar          1 18-Sep    0     QS-B             3.5      
## 2 10    RC1       Apivar          1 19-Sep    0       QR             2.0      
## 3 22    D21       Apivar          1 18-Sep    0       QR             3.5      
## 4 26    D24 Apiguard_Api          1 18-Sep    0     QS-B             2.5      
## 5 33     C6 Apiguard_Api          1 19-Sep    0       QR             2.0      
## 6 34   BC15       Apivar          1 19-Sep    0       QR             3.0      
##   est_bee_num_by_wt est_bee_num_by_wt_floor mites_recov_3_washes
## 1             322.2                     322                   35
## 2             428.1                     428                   97
## 3             343.6                     343                    9
## 4             324.1                     324                   56
## 5             347.3                     347                   82
## 6             303.9                     303                   50
##   bees_without_mites     phor2 infest_200 dwv_num_sides_obs dwv_tf
## 1                287 10.862818         22                 1      1
## 2                331 22.658257         58                 1      1
## 3                334  2.619325         11                 0      1
## 4                268 17.278618         34                 0      1
## 5                265 23.610711         42                 3      1
## 6                253 16.452781         68                 1      1
##   cdb_num_sides_obs     bees wk_cells dr_capbrd_cells_num
## 1                 2 14198.49 20174.35              791.25
## 2                 1  7543.94 14964.40                0.00
## 3                 0 13365.00 22822.80              161.70
## 4                 0  8383.50 18141.20              269.50
## 5                 0  8687.25 16803.60              107.80
## 6                 2 13000.50 18559.20              323.40
##   phoretic_mites_num_estimate wk_cells_infested_estimate
## 1                        1542                       2219
## 2                        1709                       4339
## 3                         350                       1255
## 4                        1448                       3084
## 5                        2051                       3528
## 6                        2138                       6310
##   dr_cells_infested_estimate mites_total_estimate cap_brd_indiv indiv_total
## 1                        553                 4315       20965.6    35164.09
## 2                         NA                 6048       14964.4    22508.34
## 3                         NA                 1605       22984.5    36349.50
## 4                         NA                 4532       18410.7    26794.20
## 5                         NA                 5580       16911.4    25598.65
## 6                         NA                 8448       18882.6    31883.10
##   indiv_adult_pct indiv_capbrd_pct bees_frames wk_frames honey_frames
## 1        40.37781         59.62219      5.8430    3.0165        2.925
## 2        33.51620         66.48380      3.1045    2.2375        3.915
## 3        36.76804         63.23196      5.5000    3.4125        3.675
## 4        31.28849         68.71151      3.4500    2.7125        3.150
## 5        33.93636         66.06364      3.5750    2.5125        4.025
## 6        40.77552         59.22448      5.3500    2.7750        5.775
##   pollen_frames fob_at_all fob_top_bip          col_hist pre_pattern pre_phor2
## 1         0.065         NA          NA      Richland pkg         3.5 10.862818
## 2         1.090         NA          NA Christian Control         2.0 22.658257
## 3         0.215         NA          NA      Richland pkg         3.5  2.619325
## 4         0.300         NA          NA      Richland pkg         2.5 17.278618
## 5         0.340         NA          NA Christian Control         2.0 23.610711
## 6         1.325         NA          NA Christian Control         3.0 16.452781
##   pre_infest_200 pre_mites_tot pre_bees pre_bees_frames pre_wk_cells
## 1             22          4315 14198.49          5.8430     20174.35
## 2             58          6048  7543.94          3.1045     14964.40
## 3             11          1605 13365.00          5.5000     22822.80
## 4             34          4532  8383.50          3.4500     18141.20
## 5             42          5580  8687.25          3.5750     16803.60
## 6             68          8448 13000.50          5.3500     18559.20
##   pre_wk_frames pre_honey_frames pre_pollen_frames
## 1        3.0165            2.925             0.065
## 2        2.2375            3.915             1.090
## 3        3.4125            3.675             0.215
## 4        2.7125            3.150             0.300
## 5        2.5125            4.025             0.340
## 6        2.7750            5.775             1.325
```

```r
# 4 were in Apivar
# 2 were in Combination
```




Data prep for phor, infest, bees modeling

```r
tempdatum <- datum %>% 
  filter(days %in% c(0,21,42),
         !(col_no %in% qless)
         )

# Simple sample size calculation to respond to reviewer
# These were manually entered into table generated in the Plotting folder

# Relevant variables with same n: trt days mites_recov_3_washes bees

tempdatum %>% 
  group_by(days, trt) %>% 
  summarize(n= n())
```

```
## `summarise()` has grouped output by 'days'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 9 × 3
## # Groups:   days [3]
##   days  trt              n
##   <fct> <fct>        <int>
## 1 0     Apivar          12
## 2 0     Apiguard_Api    14
## 3 0     AEC             15
## 4 21    Apivar          12
## 5 21    Apiguard_Api    14
## 6 21    AEC             15
## 7 42    Apivar          10
## 8 42    Apiguard_Api    12
## 9 42    AEC             15
```

```r
# Different n: infest_200
tempdatum %>% 
  filter(!is.na(infest_200)) %>% 
  group_by(days, trt) %>% 
  summarize(n= n())
```

```
## `summarise()` has grouped output by 'days'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 9 × 3
## # Groups:   days [3]
##   days  trt              n
##   <fct> <fct>        <int>
## 1 0     Apivar          12
## 2 0     Apiguard_Api    14
## 3 0     AEC             15
## 4 21    Apivar          11
## 5 21    Apiguard_Api    10
## 6 21    AEC             11
## 7 42    Apivar          10
## 8 42    Apiguard_Api    12
## 9 42    AEC             15
```

Quick descriptive stats

```r
tempdatum %>%
 summarise(
 mean = mean(est_bee_num_by_wt),
 mini = min(est_bee_num_by_wt),
 maxi = max(est_bee_num_by_wt),
 variance = var(est_bee_num_by_wt),
 sd = sqrt(variance)
 )
```

```
##       mean  mini  maxi variance       sd
## 1 321.7311 251.4 412.5 1529.709 39.11149
```




# Treatment Effects on Varroa Infestation
## Varroa on adult bees

### Poisson vs. Negative Binomial

Checking for overdispersion - 

```r
dispersionstats <- datum %>%
    filter(days %in% c(0,21,42),
         !(col_no %in% qless)) %>% 
 summarise(
 means = mean(mites_recov_3_washes),
 variances = var(mites_recov_3_washes),
 ratio = variances/means)

# Variance-to-mean ratio of 22.5; use negative binomial distribution instead of Poisson
```


Fit Poisson and negative binomial models

```r
m.phor.pois1 <- glmer(mites_recov_3_washes ~ 
                     trt + 
                     days + 
                     trt:days + 
                     (1 | col_no), 
                   data = tempdatum, family = poisson)


m.phor.nb1 <- glmer.nb(mites_recov_3_washes ~ 
                     trt + 
                     days + 
                     trt:days + 
                     (1 | col_no), 
                   data = tempdatum)

summary(m.phor.pois1)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: poisson  ( log )
## Formula: mites_recov_3_washes ~ trt + days + trt:days + (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##   1018.0   1045.8   -499.0    998.0      109 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8439 -1.2000 -0.4187  0.6870  6.2286 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  col_no (Intercept) 0.3278   0.5725  
## Number of obs: 119, groups:  col_no, 41
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             3.34160    0.17492  19.104  < 2e-16 ***
## trtApiguard_Api        -0.04477    0.23809  -0.188  0.85083    
## trtAEC                  0.10204    0.23358   0.437  0.66223    
## days21                 -1.71342    0.12660 -13.534  < 2e-16 ***
## days42                 -1.10824    0.11412  -9.711  < 2e-16 ***
## trtApiguard_Api:days21 -0.57310    0.20201  -2.837  0.00455 ** 
## trtAEC:days21          -1.78877    0.28262  -6.329 2.46e-10 ***
## trtApiguard_Api:days42 -0.30487    0.16658  -1.830  0.06722 .  
## trtAEC:days42           0.24513    0.13899   1.764  0.07779 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days21 days42 tA_A:2 tAEC:2 tA_A:4
## trtApgrd_Ap -0.733                                                 
## trtAEC      -0.748  0.549                                          
## days21      -0.111  0.081  0.083                                   
## days42      -0.134  0.098  0.100  0.169                            
## trtApg_A:21  0.069 -0.098 -0.052 -0.627 -0.106                     
## trtAEC:dy21  0.050 -0.036 -0.065 -0.448 -0.076  0.281              
## trtApg_A:42  0.092 -0.129 -0.068 -0.116 -0.685  0.141  0.052       
## trtAEC:dy42  0.110 -0.081 -0.140 -0.139 -0.821  0.087  0.110  0.562
```

```r
summary(m.phor.nb1)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(2.7646)  ( log )
## Formula: mites_recov_3_washes ~ trt + days + trt:days + (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##    799.6    830.1   -388.8    777.6      108 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.4665 -0.7074 -0.2575  0.3417  2.8541 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  col_no (Intercept) 0.1638   0.4047  
## Number of obs: 119, groups:  col_no, 41
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             3.41169    0.22189  15.376  < 2e-16 ***
## trtApiguard_Api        -0.05086    0.29904  -0.170    0.865    
## trtAEC                  0.11490    0.29608   0.388    0.698    
## days21                 -1.74295    0.28286  -6.162 7.19e-10 ***
## days42                 -1.16471    0.29655  -3.928 8.58e-05 ***
## trtApiguard_Api:days21 -0.58309    0.40131  -1.453    0.146    
## trtAEC:days21          -1.81177    0.44515  -4.070 4.70e-05 ***
## trtApiguard_Api:days42 -0.37893    0.40531  -0.935    0.350    
## trtAEC:days42           0.20660    0.38718   0.534    0.594    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days21 days42 tA_A:2 tAEC:2 tA_A:4
## trtApgrd_Ap -0.735                                                 
## trtAEC      -0.746  0.550                                          
## days21      -0.542  0.403  0.407                                   
## days42      -0.552  0.406  0.412  0.413                            
## trtApg_A:21  0.388 -0.532 -0.290 -0.704 -0.294                     
## trtAEC:dy21  0.352 -0.259 -0.478 -0.634 -0.266  0.449              
## trtApg_A:42  0.410 -0.541 -0.305 -0.301 -0.734  0.399  0.196       
## trtAEC:dy42  0.431 -0.314 -0.571 -0.315 -0.770  0.227  0.381  0.568
```


Comparing nb to poisson with deviance F-test

```r
# Test whether nb is a stat sig better fit than poisson:
anova(m.phor.nb1, m.phor.pois1)
```

```
## Data: tempdatum
## Models:
## m.phor.pois1: mites_recov_3_washes ~ trt + days + trt:days + (1 | col_no)
## m.phor.nb1: mites_recov_3_washes ~ trt + days + trt:days + (1 | col_no)
##              npar     AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)    
## m.phor.pois1   10 1018.01 1045.80 -499.01   998.01                         
## m.phor.nb1     11  799.57  830.14 -388.79   777.57 220.44  1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Yes, highly significantly better (p<2.2e-16)

summary(m.phor.pois1)$coefficients
```

```
##                           Estimate Std. Error     z value     Pr(>|z|)
## (Intercept)             3.34160059  0.1749152  19.1041209 2.333239e-81
## trtApiguard_Api        -0.04477365  0.2380867  -0.1880561 8.508327e-01
## trtAEC                  0.10203626  0.2335818   0.4368330 6.622325e-01
## days21                 -1.71341904  0.1266002 -13.5340903 9.839293e-42
## days42                 -1.10823811  0.1141182  -9.7113166 2.698290e-22
## trtApiguard_Api:days21 -0.57310014  0.2020060  -2.8370457 4.553309e-03
## trtAEC:days21          -1.78876829  0.2826167  -6.3293084 2.462625e-10
## trtApiguard_Api:days42 -0.30486559  0.1665766  -1.8301829 6.722259e-02
## trtAEC:days42           0.24512837  0.1389866   1.7636837 7.778524e-02
```

```r
summary(m.phor.nb1)$coefficients
```

```
##                           Estimate Std. Error    z value     Pr(>|z|)
## (Intercept)             3.41169097  0.2218874 15.3757780 2.379676e-53
## trtApiguard_Api        -0.05086181  0.2990444 -0.1700811 8.649463e-01
## trtAEC                  0.11490078  0.2960839  0.3880683 6.979655e-01
## days21                 -1.74295276  0.2828625 -6.1618374 7.190566e-10
## days42                 -1.16470523  0.2965474 -3.9275516 8.581500e-05
## trtApiguard_Api:days21 -0.58309494  0.4013066 -1.4529913 1.462262e-01
## trtAEC:days21          -1.81177476  0.4451547 -4.0699895 4.701526e-05
## trtApiguard_Api:days42 -0.37892991  0.4053108 -0.9349119 3.498337e-01
## trtAEC:days42           0.20659848  0.3871784  0.5336002 5.936181e-01
```

```r
# The p values for my predictors went up, but the fit is way better
  # This makes sense... Poisson is not conservative enough with overdispersed data
# The parameter estimates stay pretty much the same. Good.
```

### Fit model that uses number of bees in sample as the offset


```r
# A priori model
m.phor.nb1 <- glmer.nb(mites_recov_3_washes ~ 
                     trt + 
                     days + 
                     trt:days + 
                     (1 | col_no), 
                   data = tempdatum)
summary(m.phor.nb1)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(2.7646)  ( log )
## Formula: mites_recov_3_washes ~ trt + days + trt:days + (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##    799.6    830.1   -388.8    777.6      108 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.4665 -0.7074 -0.2575  0.3417  2.8541 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  col_no (Intercept) 0.1638   0.4047  
## Number of obs: 119, groups:  col_no, 41
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             3.41169    0.22189  15.376  < 2e-16 ***
## trtApiguard_Api        -0.05086    0.29904  -0.170    0.865    
## trtAEC                  0.11490    0.29608   0.388    0.698    
## days21                 -1.74295    0.28286  -6.162 7.19e-10 ***
## days42                 -1.16471    0.29655  -3.928 8.58e-05 ***
## trtApiguard_Api:days21 -0.58309    0.40131  -1.453    0.146    
## trtAEC:days21          -1.81177    0.44515  -4.070 4.70e-05 ***
## trtApiguard_Api:days42 -0.37893    0.40531  -0.935    0.350    
## trtAEC:days42           0.20660    0.38718   0.534    0.594    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days21 days42 tA_A:2 tAEC:2 tA_A:4
## trtApgrd_Ap -0.735                                                 
## trtAEC      -0.746  0.550                                          
## days21      -0.542  0.403  0.407                                   
## days42      -0.552  0.406  0.412  0.413                            
## trtApg_A:21  0.388 -0.532 -0.290 -0.704 -0.294                     
## trtAEC:dy21  0.352 -0.259 -0.478 -0.634 -0.266  0.449              
## trtApg_A:42  0.410 -0.541 -0.305 -0.301 -0.734  0.399  0.196       
## trtAEC:dy42  0.431 -0.314 -0.571 -0.315 -0.770  0.227  0.381  0.568
```

```r
# Take into account the number of bees per sample

m.phor <- glmer.nb(mites_recov_3_washes ~ 
                     trt + 
                     days + 
                     trt:days + 
                     offset(log(est_bee_num_by_wt_floor)) +
                     (1 | col_no),
                   data = tempdatum)
summary(m.phor) 
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(2.9619)  ( log )
## Formula: 
## mites_recov_3_washes ~ trt + days + trt:days + offset(log(est_bee_num_by_wt_floor)) +  
##     (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##    797.5    828.1   -387.8    775.5      108 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.5292 -0.7174 -0.2435  0.3572  2.7668 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  col_no (Intercept) 0.1987   0.4457  
## Number of obs: 119, groups:  col_no, 41
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             -2.4096     0.2233 -10.790  < 2e-16 ***
## trtApiguard_Api          0.0136     0.3018   0.045   0.9641    
## trtAEC                   0.1598     0.2985   0.535   0.5925    
## days21                  -1.6345     0.2763  -5.915 3.31e-09 ***
## days42                  -1.1651     0.2894  -4.025 5.69e-05 ***
## trtApiguard_Api:days21  -0.6738     0.3929  -1.715   0.0863 .  
## trtAEC:days21           -1.9050     0.4380  -4.349 1.37e-05 ***
## trtApiguard_Api:days42  -0.4265     0.3954  -1.079   0.2807    
## trtAEC:days42            0.1442     0.3770   0.382   0.7021    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days21 days42 tA_A:2 tAEC:2 tA_A:4
## trtApgrd_Ap -0.734                                                 
## trtAEC      -0.746  0.548                                          
## days21      -0.520  0.386  0.389                                   
## days42      -0.529  0.388  0.395  0.410                            
## trtApg_A:21  0.369 -0.507 -0.276 -0.703 -0.290                     
## trtAEC:dy21  0.333 -0.245 -0.454 -0.629 -0.261  0.444              
## trtApg_A:42  0.389 -0.517 -0.290 -0.300 -0.733  0.394  0.192       
## trtAEC:dy42  0.412 -0.299 -0.548 -0.314 -0.770  0.223  0.376  0.566
```

```r
# I need to provide a value of the offset when making predictions from the model


# Test significance of trt:days interaction

## Fit model that omits trt:days interaction

m.phor.0 <- glmer.nb(mites_recov_3_washes ~ 
                     trt + 
                     days + 
                     # trt:days + 
                     offset(log(est_bee_num_by_wt_floor)) +
                     (1 | col_no),
                   data = tempdatum)
summary(m.phor.0)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(2.08)  ( log )
## Formula: 
## mites_recov_3_washes ~ trt + days + offset(log(est_bee_num_by_wt_floor)) +  
##     (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##    816.2    835.7   -401.1    802.2      112 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.3430 -0.6425 -0.2723  0.3576  3.8385 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  col_no (Intercept) 0.1488   0.3858  
## Number of obs: 119, groups:  col_no, 41
## 
## Fixed effects:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      -2.1127     0.2058 -10.264  < 2e-16 ***
## trtApiguard_Api  -0.3495     0.2430  -1.438    0.150    
## trtAEC           -0.2296     0.2382  -0.964    0.335    
## days21           -2.3782     0.1876 -12.680  < 2e-16 ***
## days42           -1.2008     0.1754  -6.848  7.5e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days21
## trtApgrd_Ap -0.672                     
## trtAEC      -0.687  0.555              
## days21      -0.434  0.091  0.141       
## days42      -0.362  0.046 -0.043  0.405
```

```r
anova(m.phor, m.phor.0) 
```

```
## Data: tempdatum
## Models:
## m.phor.0: mites_recov_3_washes ~ trt + days + offset(log(est_bee_num_by_wt_floor)) + (1 | col_no)
## m.phor: mites_recov_3_washes ~ trt + days + trt:days + offset(log(est_bee_num_by_wt_floor)) + (1 | col_no)
##          npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
## m.phor.0    7 816.25 835.70 -401.12   802.25                         
## m.phor     11 797.51 828.08 -387.76   775.51 26.733  4  2.251e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# There was a significant interaction between day and treatment (p=2.251e-05) when tested by ANOVA. Therefore we analyzed each date separately.

# Final model is m.phor ".f" means "final"
m.phor.f <- m.phor
```

### Joint tests of phor within day (Like ANOVA of phor within day)
Added step based on reviewer comment


```r
#joint_tests(m.phor, by = "days") 
# Duplicate row names not allowed error

#joint_tests(m.phor, offset = log(100))
# Error in .find.by.rows(args, by) : 'by' variables are not all in the grid

#joint_tests(m.phor, offset = log(100), by = "days")
# Error in .find.by.rows(args, by) : 'by' variables are not all in the grid

# Used the joint_tests documentation to recreate the individual steps of what joint_tests does, adding in the offset at the appropriate step: https://cran.r-hub.io/web/packages/emmeans/emmeans.pdf

emm.phor.f <- emmeans(m.phor.f, pairwise ~ trt | days)
con.phor.f <- contrast(emm.phor.f, offset = log(100))
test(con.phor.f, joint = TRUE, by = "days")
```

```
##  days df1 df2 F.ratio p.value note
##  0      2 Inf   0.187  0.8291  d  
##  21     2 Inf   9.388  0.0001  d  
##  42     2 Inf   2.542  0.0787  d  
## 
## d: df1 reduced due to linear dependence
```

```r
# On day 0, no significant differences between treatments (F ratio 2,Inf = 0.187, P=0.8291)
# On day 21, significant differences between treatments (F ratio 2,Inf = 9.388, P=0.0001)
# On day 42, no significant differences between treatments (F ratio 2,Inf = 2.542, P=0.0787)
```





## Infestation of Brood

Checking for overdispersion

```r
dispersionstats2 <- datum %>%
    filter(days %in% c(0,21,42),
         !(col_no %in% qless)) %>% 
 summarise(
 means = mean(infest_200, na.rm = T),
 variances = var(infest_200, na.rm = T),
 ratio = variances/means)

# Variance-to-mean ratio of 14.2; use negative binomial distribution instead of Poisson
```


```r
# a priori model
m.infest.nb1 <- glmer.nb(infest_200 ~ 
                     trt + 
                     days + 
                     trt:days + 
                     (1 | col_no),
                   data = tempdatum)
summary(m.infest.nb1)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(2.927)  ( log )
## Formula: infest_200 ~ trt + days + trt:days + (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##    889.9    919.6   -434.0    867.9       99 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.4549 -0.6620 -0.2522  0.5550  3.3728 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  col_no (Intercept) 0.08164  0.2857  
## Number of obs: 110, groups:  col_no, 41
## 
## Fixed effects:
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             3.396525   0.199688  17.009  < 2e-16 ***
## trtApiguard_Api        -0.017077   0.270644  -0.063  0.94969    
## trtAEC                  0.006119   0.265756   0.023  0.98163    
## days21                  0.164440   0.259312   0.634  0.52599    
## days42                 -0.877069   0.278991  -3.144  0.00167 ** 
## trtApiguard_Api:days21 -0.761186   0.373188  -2.040  0.04138 *  
## trtAEC:days21          -0.928417   0.366209  -2.535  0.01124 *  
## trtApiguard_Api:days42  0.256060   0.377665   0.678  0.49777    
## trtAEC:days42           0.009770   0.364129   0.027  0.97859    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days21 days42 tA_A:2 tAEC:2 tA_A:4
## trtApgrd_Ap -0.727                                                 
## trtAEC      -0.746  0.548                                          
## days21      -0.627  0.465  0.472                                   
## days42      -0.580  0.435  0.439  0.454                            
## trtApg_A:21  0.443 -0.599 -0.330 -0.693 -0.311                     
## trtAEC:dy21  0.447 -0.328 -0.603 -0.707 -0.319  0.493              
## trtApg_A:42  0.427 -0.601 -0.324 -0.336 -0.739  0.437  0.236       
## trtAEC:dy42  0.444 -0.334 -0.604 -0.348 -0.767  0.238  0.442  0.567
```

```r
m.infest.nb0 <- glmer.nb(infest_200 ~ 
                     trt + 
                     days + 
                     # trt:days + 
                     (1 | col_no),
                   data = tempdatum)

anova(m.infest.nb1, m.infest.nb0)
```

```
## Data: tempdatum
## Models:
## m.infest.nb0: infest_200 ~ trt + days + (1 | col_no)
## m.infest.nb1: infest_200 ~ trt + days + trt:days + (1 | col_no)
##              npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## m.infest.nb0    7 891.66 910.57 -438.83   877.66                       
## m.infest.nb1   11 889.92 919.62 -433.96   867.92 9.7447  4    0.04496 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# We found a significant interaction of treatment × experiment day on infestation rate of capped
# worker brood (ChiSq 4=9.74, P=0.045)


# Final model
m.infest.f <- m.infest.nb1
```


### Joint tests of infest within day (Like ANOVA of infest within day)
Added step based on reviewer comment


```r
joint_tests(m.infest.f) # Does not give a duplicate row names error
```

```
##  model term df1 df2 F.ratio p.value
##  trt          2 Inf   1.287  0.2761
##  days         2 Inf  14.257  <.0001
##  trt:days     4 Inf   2.648  0.0316
```

```r
joint_tests(m.infest.f, by = "days") 
```

```
## days = 0:
##  model term df1 df2 F.ratio p.value
##  trt          2 Inf   0.004  0.9957
## 
## days = 21:
##  model term df1 df2 F.ratio p.value
##  trt          2 Inf   5.678  0.0034
## 
## days = 42:
##  model term df1 df2 F.ratio p.value
##  trt          2 Inf   0.427  0.6525
```

```r
# Testing that this method (used for phor) gives same result
emm.infest.f <- emmeans(m.infest.f, pairwise ~ trt | days)
con.infest.f <- contrast(emm.infest.f, offset = log(100))
test(con.infest.f, joint = TRUE, by = "days")
```

```
##  days df1 df2 F.ratio p.value note
##  0      2 Inf   0.004  0.9957  d  
##  21     2 Inf   5.678  0.0034  d  
##  42     2 Inf   0.427  0.6525  d  
## 
## d: df1 reduced due to linear dependence
```

```r
# Yes, exactly the same output. This confirms the workflow used above for phor

# On day 0, no significant differences (F ratio 2,Inf=0.004 , P=0.9957)
# On day 21, yes significant differences (F ratio 2,Inf= 5.678, P=0.0034)
# On day 42, no significant differences (F ratio 2,Inf= 0.427, P=0.6525)
```



# Summarize models for text, tables, and plotting


```r
# Estimated marginal means (and CIs) for text, tables, and plotting
# For the models which included an offset, report the emm's and CIs for a hypothetical sample of 100 bees
# This method is based on the emmeans documentation at: https://cran.r-project.org/web/packages/emmeans/vignettes/sophisticated.html#offsets

emplot_phor <- emmip(m.phor.f, trt~days, type = "response", CIs = TRUE, plotit = FALSE, offset = log(100))
emplot_phor$days <- as.numeric(as.character(emplot_phor$days))
emplot_phor$trt <- factor(emplot_phor$trt, levels = c("Apivar", "AEC", "Apiguard_Api"))
levels(emplot_phor$trt) # check order of factors
```

```
## [1] "Apivar"       "AEC"          "Apiguard_Api"
```

```r
emplot_infest <- emmip(m.infest.f, trt~days, type = "response", CIs = TRUE, plotit = FALSE)
emplot_infest$days <- as.numeric(as.character(emplot_infest$days))
emplot_infest$trt <- factor(emplot_infest$trt, levels = c("Apivar", "AEC", "Apiguard_Api"))
levels(emplot_infest$trt) # check order of factors
```

```
## [1] "Apivar"       "AEC"          "Apiguard_Api"
```

```r
# This is simply the number of infested cells per 200 worker cells
# Now scale this down to infested per 100 worker cells
emplot_infest$yvar <- emplot_infest$yvar/2
emplot_infest$LCL <- emplot_infest$LCL/2
emplot_infest$UCL <- emplot_infest$UCL/2
```


# Save these summarized tables of model estimates to their own CSVs


```r
write.csv(emplot_phor, "../3 Plotting/data_analyzed/emplot_phor.csv")
write.csv(emplot_infest, "../3 Plotting/data_analyzed/emplot_infest.csv")
```

# Post-hoc comparisons


```r
# Phor within day
emmeans(m.phor.f, pairwise ~ trt | days, type = "response")$contrasts
```

```
## days = 0:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 0.986 0.298 Inf    1  -0.045  0.9989
##  Apivar / AEC          0.852 0.254 Inf    1  -0.535  0.8540
##  Apiguard_Api / AEC    0.864 0.246 Inf    1  -0.512  0.8653
## 
## days = 21:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 1.935 0.685 Inf    1   1.865  0.1489
##  Apivar / AEC          5.727 2.307 Inf    1   4.333  <.0001
##  Apiguard_Api / AEC    2.960 1.203 Inf    1   2.668  0.0208
## 
## days = 42:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 1.511 0.532 Inf    1   1.172  0.4697
##  Apivar / AEC          0.738 0.242 Inf    1  -0.925  0.6244
##  Apiguard_Api / AEC    0.488 0.155 Inf    1  -2.255  0.0624
## 
## P value adjustment: tukey method for comparing a family of 3 estimates 
## Tests are performed on the log scale
```

```r
confint(emmeans(m.phor.f, pairwise ~ trt | days, type = "response"))$contrasts
```

```
## days = 0:
##  contrast              ratio    SE  df asymp.LCL asymp.UCL
##  Apivar / Apiguard_Api 0.986 0.298 Inf     0.486      2.00
##  Apivar / AEC          0.852 0.254 Inf     0.423      1.72
##  Apiguard_Api / AEC    0.864 0.246 Inf     0.443      1.69
## 
## days = 21:
##  contrast              ratio    SE  df asymp.LCL asymp.UCL
##  Apivar / Apiguard_Api 1.935 0.685 Inf     0.844      4.44
##  Apivar / AEC          5.727 2.307 Inf     2.228     14.72
##  Apiguard_Api / AEC    2.960 1.203 Inf     1.141      7.68
## 
## days = 42:
##  contrast              ratio    SE  df asymp.LCL asymp.UCL
##  Apivar / Apiguard_Api 1.511 0.532 Inf     0.662      3.45
##  Apivar / AEC          0.738 0.242 Inf     0.342      1.59
##  Apiguard_Api / AEC    0.488 0.155 Inf     0.232      1.03
## 
## Confidence level used: 0.95 
## Conf-level adjustment: tukey method for comparing a family of 3 estimates 
## Intervals are back-transformed from the log scale
```

```r
# Confirm that emmeans is using asymptotic degrees of freedom
emmeans(m.phor.f, pairwise ~ trt | days, type = "response")$contrasts
```

```
## days = 0:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 0.986 0.298 Inf    1  -0.045  0.9989
##  Apivar / AEC          0.852 0.254 Inf    1  -0.535  0.8540
##  Apiguard_Api / AEC    0.864 0.246 Inf    1  -0.512  0.8653
## 
## days = 21:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 1.935 0.685 Inf    1   1.865  0.1489
##  Apivar / AEC          5.727 2.307 Inf    1   4.333  <.0001
##  Apiguard_Api / AEC    2.960 1.203 Inf    1   2.668  0.0208
## 
## days = 42:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 1.511 0.532 Inf    1   1.172  0.4697
##  Apivar / AEC          0.738 0.242 Inf    1  -0.925  0.6244
##  Apiguard_Api / AEC    0.488 0.155 Inf    1  -2.255  0.0624
## 
## P value adjustment: tukey method for comparing a family of 3 estimates 
## Tests are performed on the log scale
```

```r
emmeans(m.phor.f, pairwise ~ trt | days, type = "response", lmer.df = "asymptotic")$contrasts
```

```
## days = 0:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 0.986 0.298 Inf    1  -0.045  0.9989
##  Apivar / AEC          0.852 0.254 Inf    1  -0.535  0.8540
##  Apiguard_Api / AEC    0.864 0.246 Inf    1  -0.512  0.8653
## 
## days = 21:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 1.935 0.685 Inf    1   1.865  0.1489
##  Apivar / AEC          5.727 2.307 Inf    1   4.333  <.0001
##  Apiguard_Api / AEC    2.960 1.203 Inf    1   2.668  0.0208
## 
## days = 42:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 1.511 0.532 Inf    1   1.172  0.4697
##  Apivar / AEC          0.738 0.242 Inf    1  -0.925  0.6244
##  Apiguard_Api / AEC    0.488 0.155 Inf    1  -2.255  0.0624
## 
## P value adjustment: tukey method for comparing a family of 3 estimates 
## Tests are performed on the log scale
```

```r
# Yes, same results

# Does Satterthwaite method work? No, defaults to asymptotic
emmeans(m.phor.f, pairwise ~ trt | days, type = "response")$contrasts
```

```
## days = 0:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 0.986 0.298 Inf    1  -0.045  0.9989
##  Apivar / AEC          0.852 0.254 Inf    1  -0.535  0.8540
##  Apiguard_Api / AEC    0.864 0.246 Inf    1  -0.512  0.8653
## 
## days = 21:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 1.935 0.685 Inf    1   1.865  0.1489
##  Apivar / AEC          5.727 2.307 Inf    1   4.333  <.0001
##  Apiguard_Api / AEC    2.960 1.203 Inf    1   2.668  0.0208
## 
## days = 42:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 1.511 0.532 Inf    1   1.172  0.4697
##  Apivar / AEC          0.738 0.242 Inf    1  -0.925  0.6244
##  Apiguard_Api / AEC    0.488 0.155 Inf    1  -2.255  0.0624
## 
## P value adjustment: tukey method for comparing a family of 3 estimates 
## Tests are performed on the log scale
```

```r
emmeans(m.phor.f, pairwise ~ trt | days, type = "response", lmer.df = "satterthwaite")$contrasts
```

```
## days = 0:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 0.986 0.298 Inf    1  -0.045  0.9989
##  Apivar / AEC          0.852 0.254 Inf    1  -0.535  0.8540
##  Apiguard_Api / AEC    0.864 0.246 Inf    1  -0.512  0.8653
## 
## days = 21:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 1.935 0.685 Inf    1   1.865  0.1489
##  Apivar / AEC          5.727 2.307 Inf    1   4.333  <.0001
##  Apiguard_Api / AEC    2.960 1.203 Inf    1   2.668  0.0208
## 
## days = 42:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 1.511 0.532 Inf    1   1.172  0.4697
##  Apivar / AEC          0.738 0.242 Inf    1  -0.925  0.6244
##  Apiguard_Api / AEC    0.488 0.155 Inf    1  -2.255  0.0624
## 
## P value adjustment: tukey method for comparing a family of 3 estimates 
## Tests are performed on the log scale
```

```r
# Infest within day
emmeans(m.infest.f, pairwise ~ trt | days, type = "response")$contrasts
```

```
## days = 0:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 1.017 0.275 Inf    1   0.063  0.9978
##  Apivar / AEC          0.994 0.264 Inf    1  -0.023  0.9997
##  Apiguard_Api / AEC    0.977 0.249 Inf    1  -0.091  0.9955
## 
## days = 21:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 2.178 0.659 Inf    1   2.572  0.0273
##  Apivar / AEC          2.515 0.743 Inf    1   3.122  0.0051
##  Apiguard_Api / AEC    1.155 0.358 Inf    1   0.465  0.8876
## 
## days = 42:
##  contrast              ratio    SE  df null z.ratio p.value
##  Apivar / Apiguard_Api 0.787 0.240 Inf    1  -0.783  0.7133
##  Apivar / AEC          0.984 0.289 Inf    1  -0.054  0.9984
##  Apiguard_Api / AEC    1.250 0.344 Inf    1   0.812  0.6958
## 
## P value adjustment: tukey method for comparing a family of 3 estimates 
## Tests are performed on the log scale
```

```r
confint(emmeans(m.infest.f, pairwise ~ trt | days, type = "response"))$contrasts
```

```
## days = 0:
##  contrast              ratio    SE  df asymp.LCL asymp.UCL
##  Apivar / Apiguard_Api 1.017 0.275 Inf     0.539      1.92
##  Apivar / AEC          0.994 0.264 Inf     0.533      1.85
##  Apiguard_Api / AEC    0.977 0.249 Inf     0.537      1.78
## 
## days = 21:
##  contrast              ratio    SE  df asymp.LCL asymp.UCL
##  Apivar / Apiguard_Api 2.178 0.659 Inf     1.072      4.43
##  Apivar / AEC          2.515 0.743 Inf     1.258      5.03
##  Apiguard_Api / AEC    1.155 0.358 Inf     0.559      2.39
## 
## days = 42:
##  contrast              ratio    SE  df asymp.LCL asymp.UCL
##  Apivar / Apiguard_Api 0.787 0.240 Inf     0.385      1.61
##  Apivar / AEC          0.984 0.289 Inf     0.494      1.96
##  Apiguard_Api / AEC    1.250 0.344 Inf     0.656      2.38
## 
## Confidence level used: 0.95 
## Conf-level adjustment: tukey method for comparing a family of 3 estimates 
## Intervals are back-transformed from the log scale
```


