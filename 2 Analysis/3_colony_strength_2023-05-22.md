---
title: "3_colony_strength_2023-05-22"
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



Data prep for phor, infest, bees modeling

```r
tempdatum <- datum %>% 
  filter(days %in% c(0,21,42),
         !(col_no %in% qless)
         ) %>%
  mutate(tot = wk_cells_infested_estimate + phoretic_mites_num_estimate,
           perc_of_mitepop_wkbrd = 100*wk_cells_infested_estimate/tot
           )
```


# Treatment Effects on Colony Strength
## Adult Bee Population


```r
tempdatum <- datum %>% 
  filter(days %in% c(0,21,42),
         !(col_no %in% qless)
         ) %>%
  mutate(tot = wk_cells_infested_estimate + phoretic_mites_num_estimate,
           perc_of_mitepop_wkbrd = 100*wk_cells_infested_estimate/tot
           )

# A priori model
m.bee1 <- lmer(bees_frames ~ 
                 trt + 
                 days + 
                 trt:days + 
                 pre_phor2 + 
                 pre_phor2:days +
                 (1 | col_no), data = tempdatum)
summary(m.bee1)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: bees_frames ~ trt + days + trt:days + pre_phor2 + pre_phor2:days +  
##     (1 | col_no)
##    Data: tempdatum
## 
## REML criterion at convergence: 390.7
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -1.56434 -0.52555 -0.08592  0.36719  3.15820 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  col_no   (Intercept) 0.9537   0.9766  
##  Residual             0.9127   0.9554  
## Number of obs: 119, groups:  col_no, 41
## 
## Fixed effects:
##                        Estimate Std. Error t value
## (Intercept)             5.04377    0.51252   9.841
## trtApiguard_Api         0.42894    0.53748   0.798
## trtAEC                  0.29198    0.52992   0.551
## days21                  0.06438    0.50686   0.127
## days42                 -0.30274    0.52726  -0.574
## pre_phor2              -0.09976    0.03212  -3.106
## trtApiguard_Api:days21 -0.93016    0.53154  -1.750
## trtAEC:days21           0.79235    0.52407   1.512
## trtApiguard_Api:days42 -0.39018    0.56342  -0.693
## trtAEC:days42           1.08864    0.54540   1.996
## days21:pre_phor2       -0.07746    0.03176  -2.439
## days42:pre_phor2       -0.07663    0.03421  -2.240
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days21 days42 pr_ph2 tA_A:2 tAEC:2 tA_A:4
## trtApgrd_Ap -0.559                                                        
## trtAEC      -0.538  0.547                                                 
## days21      -0.494  0.277  0.266                                          
## days42      -0.475  0.266  0.256  0.481                                   
## pre_phor2   -0.639 -0.008 -0.055  0.316  0.304                            
## trtApg_A:21  0.277 -0.494 -0.270 -0.559 -0.269  0.004                     
## trtAEC:dy21  0.266 -0.270 -0.494 -0.538 -0.258  0.027  0.547              
## trtApg_A:42  0.261 -0.466 -0.255 -0.264 -0.583  0.004  0.472  0.258       
## trtAEC:dy42  0.255 -0.260 -0.475 -0.258 -0.538  0.026  0.263  0.480  0.561
## dys21:pr_p2  0.316  0.004  0.027 -0.639 -0.307 -0.494 -0.008 -0.055 -0.004
## dys42:pr_p2  0.293  0.004  0.025 -0.296 -0.615 -0.459 -0.004 -0.025  0.005
##             tAEC:4 d21:_2
## trtApgrd_Ap              
## trtAEC                   
## days21                   
## days42                   
## pre_phor2                
## trtApg_A:21              
## trtAEC:dy21              
## trtApg_A:42              
## trtAEC:dy42              
## dys21:pr_p2 -0.026       
## dys42:pr_p2 -0.101  0.464
```

```r
m.bee1.ML <- lmer(bees_frames ~ 
                 trt + 
                 days + 
                 trt:days + 
                 pre_phor2 + 
                 pre_phor2:days +
                 (1 | col_no), data = tempdatum, REML = F)

# Omitting the phor:days interaction
m.bee0.ML <- lmer(bees_frames ~ 
                 trt + 
                 days + 
                 trt:days + 
                 pre_phor2 + 
                 # pre_phor2:days +
                 (1 | col_no), data = tempdatum, REML = F)
anova(m.bee0.ML, m.bee1.ML)
```

```
## Data: tempdatum
## Models:
## m.bee0.ML: bees_frames ~ trt + days + trt:days + pre_phor2 + (1 | col_no)
## m.bee1.ML: bees_frames ~ trt + days + trt:days + pre_phor2 + pre_phor2:days + (1 | col_no)
##           npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## m.bee0.ML   12 403.16 436.51 -189.58   379.16                       
## m.bee1.ML   14 399.22 438.13 -185.61   371.22 7.9362  2    0.01891 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Significant effect of mite level... high-mite colonies shrink during successive dates of trial

# Omitting the phor:days interaction
m.bee00.ML <- lmer(bees_frames ~ 
                 trt + 
                 days + 
                 trt:days + 
                 # pre_phor2 + 
                 # pre_phor2:days +
                 (1 | col_no), data = tempdatum, REML = F)
anova(m.bee00.ML, m.bee0.ML)
```

```
## Data: tempdatum
## Models:
## m.bee00.ML: bees_frames ~ trt + days + trt:days + (1 | col_no)
## m.bee0.ML: bees_frames ~ trt + days + trt:days + pre_phor2 + (1 | col_no)
##            npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
## m.bee00.ML   11 426.39 456.96 -202.20   404.39                         
## m.bee0.ML    12 403.16 436.51 -189.58   379.16 25.231  1  5.085e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Second reduced model omitting the trt:days interaction
m.bee000.ML <- lmer(bees_frames ~ 
                 trt + 
                 days + 
                 # trt:days + 
                 pre_phor2 + 
                 pre_phor2:days +
                 (1 | col_no), data = tempdatum, REML = F)
anova(m.bee000.ML, m.bee1.ML)
```

```
## Data: tempdatum
## Models:
## m.bee000.ML: bees_frames ~ trt + days + pre_phor2 + pre_phor2:days + (1 | col_no)
## m.bee1.ML: bees_frames ~ trt + days + trt:days + pre_phor2 + pre_phor2:days + (1 | col_no)
##             npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
## m.bee000.ML   10 405.82 433.61 -192.91   385.82                        
## m.bee1.ML     14 399.22 438.13 -185.61   371.22 14.594  4   0.005622 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Significant trt:days interaction

# True a priori model
m.bee.apriori.ML <- lmer(bees_frames ~ 
                 trt + 
                 days + 
                 trt:days +
                 (1 | col_no), data = tempdatum, REML = F)

m.bee.apriori0.ML <- lmer(bees_frames ~ 
                 trt + 
                 days + 
                 # trt:days +
                 (1 | col_no), data = tempdatum, REML = F)

anova(m.bee.apriori.ML, m.bee.apriori0.ML)
```

```
## Data: tempdatum
## Models:
## m.bee.apriori0.ML: bees_frames ~ trt + days + (1 | col_no)
## m.bee.apriori.ML: bees_frames ~ trt + days + trt:days + (1 | col_no)
##                   npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## m.bee.apriori0.ML    7 430.49 449.94 -208.24   416.49                       
## m.bee.apriori.ML    11 426.39 456.96 -202.20   404.39 12.097  4    0.01664 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Save REML version
m.bee.apriori <- lmer(bees_frames ~ 
                 trt + 
                 days + 
                 trt:days +
                 (1 | col_no), data = tempdatum, REML = F)
```


## Population of capped worker brood


```r
tempdatum <- datum %>% 
  filter(days %in% c(0,10,21,42),
         !(col_no %in% qless)
         )
```



```r
m.brood1 <- lmer(wk_frames ~ 
                   trt + 
                   days + 
                   trt:days + 
                   pre_phor2 +
                   pre_phor2:days +
                  (1 | col_no), 
                 data = tempdatum)
summary(m.brood1)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: wk_frames ~ trt + days + trt:days + pre_phor2 + pre_phor2:days +  
##     (1 | col_no)
##    Data: tempdatum
## 
## REML criterion at convergence: 243.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.2006 -0.4918 -0.0492  0.5207  2.6671 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  col_no   (Intercept) 0.0619   0.2488  
##  Residual             0.1649   0.4061  
## Number of obs: 160, groups:  col_no, 41
## 
## Fixed effects:
##                        Estimate Std. Error t value
## (Intercept)             2.69654    0.17867  15.093
## trtApiguard_Api         0.26472    0.18737   1.413
## trtAEC                  0.26208    0.18474   1.419
## days10                 -1.14977    0.21545  -5.337
## days21                 -1.06697    0.21545  -4.952
## days42                 -1.92770    0.22310  -8.641
## pre_phor2              -0.02736    0.01120  -2.444
## trtApiguard_Api:days10 -0.66068    0.22595  -2.924
## trtAEC:days10          -0.16539    0.22277  -0.742
## trtApiguard_Api:days21 -0.16514    0.22595  -0.731
## trtAEC:days21          -0.09063    0.22277  -0.407
## trtApiguard_Api:days42 -0.32512    0.23790  -1.367
## trtAEC:days42          -0.12991    0.23076  -0.563
## days10:pre_phor2       -0.01290    0.01350  -0.956
## days21:pre_phor2       -0.01080    0.01350  -0.800
## days42:pre_phor2        0.02021    0.01442   1.401
```

```
## 
## Correlation matrix not shown by default, as p = 16 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
m.brood1.ML <- lmer(wk_frames ~ 
                   trt + 
                   days + 
                   trt:days + 
                   pre_phor2 +
                   pre_phor2:days +
                  (1 | col_no), 
                 data = tempdatum, REML = F)
summary(m.brood1.ML)
```

```
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: wk_frames ~ trt + days + trt:days + pre_phor2 + pre_phor2:days +  
##     (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##    221.8    277.1    -92.9    185.8      142 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3738 -0.5179 -0.0525  0.5495  2.8113 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  col_no   (Intercept) 0.05588  0.2364  
##  Residual             0.14835  0.3852  
## Number of obs: 160, groups:  col_no, 41
## 
## Fixed effects:
##                        Estimate Std. Error t value
## (Intercept)             2.69654    0.16953  15.906
## trtApiguard_Api         0.26472    0.17779   1.489
## trtAEC                  0.26208    0.17529   1.495
## days10                 -1.14977    0.20434  -5.627
## days21                 -1.06697    0.20434  -5.221
## days42                 -1.92777    0.21159  -9.111
## pre_phor2              -0.02736    0.01062  -2.575
## trtApiguard_Api:days10 -0.66068    0.21429  -3.083
## trtAEC:days10          -0.16539    0.21128  -0.783
## trtApiguard_Api:days21 -0.16514    0.21429  -0.771
## trtAEC:days21          -0.09063    0.21128  -0.429
## trtApiguard_Api:days42 -0.32511    0.22564  -1.441
## trtAEC:days42          -0.12984    0.21886  -0.593
## days10:pre_phor2       -0.01290    0.01281  -1.008
## days21:pre_phor2       -0.01080    0.01281  -0.844
## days42:pre_phor2        0.02021    0.01368   1.478
```

```
## 
## Correlation matrix not shown by default, as p = 16 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
# Omit pre_phor2:days effect
m.brood01.ML <- lmer(wk_frames ~ 
                   trt + 
                   days + 
                   trt:days + 
                   pre_phor2 +
                   # pre_phor2:days +
                  (1 | col_no), 
                 data = tempdatum, REML = F)
summary(m.brood01.ML)
```

```
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: wk_frames ~ trt + days + trt:days + pre_phor2 + (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##    222.7    268.8    -96.4    192.7      145 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3042 -0.5974  0.0052  0.5653  2.7159 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  col_no   (Intercept) 0.05371  0.2318  
##  Residual             0.15719  0.3965  
## Number of obs: 160, groups:  col_no, 41
## 
## Fixed effects:
##                         Estimate Std. Error t value
## (Intercept)             2.717876   0.151805  17.904
## trtApiguard_Api         0.265014   0.180670   1.467
## trtAEC                  0.263981   0.177987   1.483
## days10                 -1.281250   0.161861  -7.916
## days21                 -1.177083   0.161861  -7.272
## days42                 -1.744905   0.171095 -10.198
## pre_phor2              -0.029454   0.007257  -4.059
## trtApiguard_Api:days10 -0.662500   0.220579  -3.003
## trtAEC:days10          -0.177083   0.217159  -0.815
## trtApiguard_Api:days21 -0.166667   0.220579  -0.756
## trtAEC:days21          -0.100417   0.217159  -0.462
## trtApiguard_Api:days42 -0.330591   0.232176  -1.424
## trtAEC:days42          -0.088428   0.224127  -0.395
```

```
## 
## Correlation matrix not shown by default, as p = 13 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
anova(m.brood01.ML, m.brood1.ML) # Simpler model preferred, but just barely (p=0.07395)
```

```
## Data: tempdatum
## Models:
## m.brood01.ML: wk_frames ~ trt + days + trt:days + pre_phor2 + (1 | col_no)
## m.brood1.ML: wk_frames ~ trt + days + trt:days + pre_phor2 + pre_phor2:days + (1 | col_no)
##              npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## m.brood01.ML   15 222.72 268.84 -96.358   192.72                       
## m.brood1.ML    18 221.78 277.13 -92.890   185.78 6.9364  3    0.07395 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Full model minus the trt:days interaction
m.brood02.ML <- lmer(wk_frames ~ 
                   trt + 
                   days + 
                   # trt:days + 
                   pre_phor2 +
                   pre_phor2:days +
                  (1 | col_no), 
                 data = tempdatum, REML = F)
summary(m.brood02.ML)
```

```
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: wk_frames ~ trt + days + pre_phor2 + pre_phor2:days + (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##    221.2    258.1    -98.6    197.2      148 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.87487 -0.57730  0.02543  0.50380  3.09546 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  col_no   (Intercept) 0.05174  0.2275  
##  Residual             0.16340  0.4042  
## Number of obs: 160, groups:  col_no, 41
## 
## Fixed effects:
##                  Estimate Std. Error t value
## (Intercept)       2.83051    0.15381  18.403
## trtApiguard_Api  -0.02144    0.12093  -0.177
## trtAEC            0.16617    0.11886   1.398
## days10           -1.44112    0.16756  -8.601
## days21           -1.15556    0.16756  -6.896
## days42           -2.08492    0.17142 -12.163
## pre_phor2        -0.02747    0.01089  -2.522
## days10:pre_phor2 -0.01241    0.01341  -0.925
## days21:pre_phor2 -0.01090    0.01341  -0.812
## days42:pre_phor2  0.02016    0.01423   1.416
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days10 days21 days42 pr_ph2 d10:_2 d21:_2
## trtApgrd_Ap -0.419                                                        
## trtAEC      -0.403  0.549                                                 
## days10      -0.545  0.000  0.000                                          
## days21      -0.545  0.000  0.000  0.500                                   
## days42      -0.532 -0.006  0.005  0.489  0.489                            
## pre_phor2   -0.737 -0.006 -0.036  0.521  0.521  0.509                     
## dys10:pr_p2  0.461  0.000  0.000 -0.846 -0.423 -0.414 -0.616              
## dys21:pr_p2  0.461  0.000  0.000 -0.423 -0.846 -0.414 -0.616  0.500       
## dys42:pr_p2  0.438  0.005 -0.020 -0.399 -0.399 -0.843 -0.579  0.471  0.471
```

```r
anova(m.brood02.ML, m.brood1.ML)
```

```
## Data: tempdatum
## Models:
## m.brood02.ML: wk_frames ~ trt + days + pre_phor2 + pre_phor2:days + (1 | col_no)
## m.brood1.ML: wk_frames ~ trt + days + trt:days + pre_phor2 + pre_phor2:days + (1 | col_no)
##              npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
## m.brood02.ML   12 221.16 258.06 -98.579   197.16                       
## m.brood1.ML    18 221.78 277.13 -92.890   185.78 11.378  6    0.07737 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Weak evidence for a brood:days interaction (p=0.07737) in the presence of the pre_phor stuff

# Omit trt:days in the context of pre_phor 2 main effect
m.brood04.ML <- lmer(wk_frames ~ 
                   trt + 
                   days + 
                   # trt:days + 
                   pre_phor2 +
                   # pre_phor2:days +
                  (1 | col_no), 
                 data = tempdatum, REML = F)
anova(m.brood04.ML, m.brood01.ML)
```

```
## Data: tempdatum
## Models:
## m.brood04.ML: wk_frames ~ trt + days + pre_phor2 + (1 | col_no)
## m.brood01.ML: wk_frames ~ trt + days + trt:days + pre_phor2 + (1 | col_no)
##              npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)  
## m.brood04.ML    9 221.47 249.15 -101.736   203.47                       
## m.brood01.ML   15 222.72 268.84  -96.358   192.72 10.757  6     0.0962 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Again, trt:days effect not quite significant (p=0.0962)


# Omit pre_phor2
m.brood05.ML <- lmer(wk_frames ~ 
                   trt + 
                   days + 
                   trt:days + 
                   # pre_phor2 +
                   # pre_phor2:days +
                  (1 | col_no), 
                 data = tempdatum, REML = F)
anova(m.brood05.ML, m.brood01.ML)
```

```
## Data: tempdatum
## Models:
## m.brood05.ML: wk_frames ~ trt + days + trt:days + (1 | col_no)
## m.brood01.ML: wk_frames ~ trt + days + trt:days + pre_phor2 + (1 | col_no)
##              npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)    
## m.brood05.ML   14 234.55 277.60 -103.273   206.55                         
## m.brood01.ML   15 222.72 268.84  -96.358   192.72 13.831  1      2e-04 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Omit trt:days in the context of pre_phor 2 main effect
m.brood06.ML <- lmer(wk_frames ~ 
                   trt + 
                   # days + 
                   # trt:days + 
                   pre_phor2 +
                   # pre_phor2:days +
                  (1 | col_no), 
                 data = tempdatum, REML = F)
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
anova(m.brood06.ML, m.brood04.ML)
```

```
## Data: tempdatum
## Models:
## m.brood06.ML: wk_frames ~ trt + pre_phor2 + (1 | col_no)
## m.brood04.ML: wk_frames ~ trt + days + pre_phor2 + (1 | col_no)
##              npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
## m.brood06.ML    6 415.06 433.51 -201.53   403.06                         
## m.brood04.ML    9 221.47 249.15 -101.74   203.47 199.58  3  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Again, days effect significant (p<0.0001)

# Preferred model: m.brood01.ML
# Make REML version
m.brood01 <- lmer(wk_frames ~ 
                   trt + 
                   days + 
                   trt:days + 
                   pre_phor2 +
                   # pre_phor2:days +
                  (1 | col_no), 
                 data = tempdatum)

summary(m.brood01)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: wk_frames ~ trt + days + trt:days + pre_phor2 + (1 | col_no)
##    Data: tempdatum
## 
## REML criterion at convergence: 228.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.1676 -0.5653  0.0034  0.5358  2.6019 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  col_no   (Intercept) 0.06082  0.2466  
##  Residual             0.16994  0.4122  
## Number of obs: 160, groups:  col_no, 41
## 
## Fixed effects:
##                         Estimate Std. Error t value
## (Intercept)             2.717914   0.159063  17.087
## trtApiguard_Api         0.265014   0.188982   1.402
## trtAEC                  0.263985   0.186177   1.418
## days10                 -1.281250   0.168296  -7.613
## days21                 -1.177083   0.168296  -6.994
## days42                 -1.745861   0.177927  -9.812
## pre_phor2              -0.029457   0.007645  -3.853
## trtApiguard_Api:days10 -0.662500   0.229349  -2.889
## trtAEC:days10          -0.177083   0.225793  -0.784
## trtApiguard_Api:days21 -0.166667   0.229349  -0.727
## trtAEC:days21          -0.100417   0.225793  -0.445
## trtApiguard_Api:days42 -0.330381   0.241445  -1.368
## trtAEC:days42          -0.087472   0.233060  -0.375
```

```
## 
## Correlation matrix not shown by default, as p = 13 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
confint(m.brood01)
```

```
## Computing profile confidence intervals ...
```

```
##                              2.5 %      97.5 %
## .sig01                  0.13746579  0.33467855
## .sigma                  0.35064552  0.45330423
## (Intercept)             2.41753275  3.01834805
## trtApiguard_Api        -0.09168296  0.62171182
## trtAEC                 -0.08741977  0.61538939
## days10                 -1.60111491 -0.96138509
## days21                 -1.49694825 -0.85721842
## days42                 -2.08227708 -1.40511331
## pre_phor2              -0.04403989 -0.01489445
## trtApiguard_Api:days10 -1.09840240 -0.22659760
## trtAEC:days10          -0.60622715  0.25206048
## trtApiguard_Api:days21 -0.60256907  0.26923573
## trtAEC:days21          -0.52956048  0.32872715
## trtApiguard_Api:days42 -0.78959833  0.12788761
## trtAEC:days42          -0.53293242  0.35360762
```

# Tidy the output of models
To be able to report predicted response


```r
# Bees

emres_bees<- emmeans(m.bee1, pairwise ~ trt | days, type = "response")$emmeans %>% 
  tidy()

emmeans(m.bee1, pairwise ~ trt | days, type = "response")$contrasts
```

```
## days = 0:
##  contrast              estimate    SE   df t.ratio p.value
##  Apivar - Apiguard_Api  -0.4289 0.537 71.4  -0.798  0.7054
##  Apivar - AEC           -0.2920 0.530 71.4  -0.551  0.8463
##  Apiguard_Api - AEC      0.1370 0.508 71.4   0.269  0.9608
## 
## days = 21:
##  contrast              estimate    SE   df t.ratio p.value
##  Apivar - Apiguard_Api   0.5012 0.537 71.4   0.933  0.6216
##  Apivar - AEC           -1.0843 0.530 71.4  -2.046  0.1087
##  Apiguard_Api - AEC     -1.5856 0.508 71.4  -3.119  0.0073
## 
## days = 42:
##  contrast              estimate    SE   df t.ratio p.value
##  Apivar - Apiguard_Api  -0.0388 0.570 80.3  -0.068  0.9974
##  Apivar - AEC           -1.3806 0.551 77.6  -2.504  0.0378
##  Apiguard_Api - AEC     -1.3419 0.526 76.7  -2.553  0.0336
## 
## Degrees-of-freedom method: kenward-roger 
## P value adjustment: tukey method for comparing a family of 3 estimates
```

```r
confint(emmeans(m.bee1, pairwise ~ trt | days, type = "response"))$contrasts
```

```
## days = 0:
##  contrast              estimate    SE   df lower.CL upper.CL
##  Apivar - Apiguard_Api  -0.4289 0.537 71.4   -1.715   0.8575
##  Apivar - AEC           -0.2920 0.530 71.4   -1.560   0.9764
##  Apiguard_Api - AEC      0.1370 0.508 71.4   -1.080   1.3536
## 
## days = 21:
##  contrast              estimate    SE   df lower.CL upper.CL
##  Apivar - Apiguard_Api   0.5012 0.537 71.4   -0.785   1.7877
##  Apivar - AEC           -1.0843 0.530 71.4   -2.353   0.1841
##  Apiguard_Api - AEC     -1.5856 0.508 71.4   -2.802  -0.3689
## 
## days = 42:
##  contrast              estimate    SE   df lower.CL upper.CL
##  Apivar - Apiguard_Api  -0.0388 0.570 80.3   -1.399   1.3212
##  Apivar - AEC           -1.3806 0.551 77.6   -2.698  -0.0631
##  Apiguard_Api - AEC     -1.3419 0.526 76.7   -2.598  -0.0854
## 
## Degrees-of-freedom method: kenward-roger 
## Confidence level used: 0.95 
## Conf-level adjustment: tukey method for comparing a family of 3 estimates
```

```r
## Day 42 frames of bees estimates
### AEC 4.292991
### Apivar 2.912365
### Combination 2.951125

## What percentage of bees were lacking in Apivar?
100*(4.292991-2.912365)/4.292991 #32%
```

```
## [1] 32.16
```

```r
# 1.38 missing frames of bees. Convert to number of bees
# Beebook reference
2430*1.38 # 3353
```

```
## [1] 3353.4
```

```r
## What percentage of bees were lacking in Combination?
100*(4.292991-2.951125)/4.292991 # 31%
```

```
## [1] 31.25714
```

```r
## Combination missing 1.34 frames of bees. Convert to number of bees
## Beebook conversion
2430*1.34 # 3256 bees missing
```

```
## [1] 3256.2
```

```r
## Combination missing how many frames of brood on Day 10?
1.2-0.73 # 0.47 frames of brood
```

```
## [1] 0.47
```

```r
880*3.8*2 # 6688 cells of worker brood per frame
```

```
## [1] 6688
```

```r
## Combination missing 0.47 frames of brood on Day 10. Convert to number of bees
6688*0.47 #3143 brood missing
```

```
## [1] 3143.36
```

```r
## Over time
emmeans(m.bee1, pairwise ~ days | trt, type = "response")$contrasts
```

```
## trt = Apivar:
##  contrast        estimate    SE   df t.ratio p.value
##  days0 - days21   0.73862 0.390 70.0   1.894  0.1481
##  days0 - days42   1.09718 0.417 71.9   2.630  0.0278
##  days21 - days42  0.35856 0.417 71.9   0.860  0.6673
## 
## trt = Apiguard_Api:
##  contrast        estimate    SE   df t.ratio p.value
##  days0 - days21   1.66878 0.361 70.0   4.621  <.0001
##  days0 - days42   1.48736 0.383 71.7   3.884  0.0007
##  days21 - days42 -0.18142 0.383 71.7  -0.474  0.8839
## 
## trt = AEC:
##  contrast        estimate    SE   df t.ratio p.value
##  days0 - days21  -0.05374 0.350 70.0  -0.154  0.9871
##  days0 - days42   0.00853 0.350 70.0   0.024  0.9997
##  days21 - days42  0.06227 0.350 70.0   0.178  0.9827
## 
## Degrees-of-freedom method: kenward-roger 
## P value adjustment: tukey method for comparing a family of 3 estimates
```

```r
# Bees a priori

emres_bees_apriori<- emmeans(m.bee.apriori, pairwise ~ trt | days, type = "response")$emmeans %>% 
  tidy()

emmeans(m.bee.apriori, pairwise ~ trt | days, type = "response")$contrasts
```

```
## days = 0:
##  contrast              estimate    SE   df t.ratio p.value
##  Apivar - Apiguard_Api  -0.4149 0.672 69.7  -0.617  0.8113
##  Apivar - AEC           -0.2016 0.662 69.7  -0.305  0.9502
##  Apiguard_Api - AEC      0.2133 0.635 69.7   0.336  0.9398
## 
## days = 21:
##  contrast              estimate    SE   df t.ratio p.value
##  Apivar - Apiguard_Api   0.5262 0.672 69.7   0.783  0.7149
##  Apivar - AEC           -0.9237 0.662 69.7  -1.396  0.3488
##  Apiguard_Api - AEC     -1.4499 0.635 69.7  -2.283  0.0649
## 
## days = 42:
##  contrast              estimate    SE   df t.ratio p.value
##  Apivar - Apiguard_Api  -0.0533 0.701 78.5  -0.076  0.9968
##  Apivar - AEC           -1.1852 0.679 75.1  -1.745  0.1955
##  Apiguard_Api - AEC     -1.1318 0.648 73.9  -1.747  0.1950
## 
## Degrees-of-freedom method: kenward-roger 
## P value adjustment: tukey method for comparing a family of 3 estimates
```

```r
confint(emmeans(m.bee.apriori, pairwise ~ trt | days, type = "response"))$contrasts
```

```
## days = 0:
##  contrast              estimate    SE   df lower.CL upper.CL
##  Apivar - Apiguard_Api  -0.4149 0.672 69.7    -2.02    1.195
##  Apivar - AEC           -0.2016 0.662 69.7    -1.79    1.384
##  Apiguard_Api - AEC      0.2133 0.635 69.7    -1.31    1.734
## 
## days = 21:
##  contrast              estimate    SE   df lower.CL upper.CL
##  Apivar - Apiguard_Api   0.5262 0.672 69.7    -1.08    2.136
##  Apivar - AEC           -0.9237 0.662 69.7    -2.51    0.661
##  Apiguard_Api - AEC     -1.4499 0.635 69.7    -2.97    0.071
## 
## days = 42:
##  contrast              estimate    SE   df lower.CL upper.CL
##  Apivar - Apiguard_Api  -0.0533 0.701 78.5    -1.73    1.622
##  Apivar - AEC           -1.1852 0.679 75.1    -2.81    0.439
##  Apiguard_Api - AEC     -1.1318 0.648 73.9    -2.68    0.418
## 
## Degrees-of-freedom method: kenward-roger 
## Confidence level used: 0.95 
## Conf-level adjustment: tukey method for comparing a family of 3 estimates
```

```r
# Brood



emres_brood<- emmeans(m.brood01, pairwise ~ trt | days, type = "response")$emmeans %>% 
  tidy()

emmeans(m.brood01, pairwise ~ trt | days, type = "response")$contrasts
```

```
## days = 0:
##  contrast              estimate    SE  df t.ratio p.value
##  Apivar - Apiguard_Api -0.26501 0.189 121  -1.402  0.3429
##  Apivar - AEC          -0.26398 0.186 121  -1.418  0.3349
##  Apiguard_Api - AEC     0.00103 0.179 121   0.006  1.0000
## 
## days = 10:
##  contrast              estimate    SE  df t.ratio p.value
##  Apivar - Apiguard_Api  0.39749 0.189 121   2.103  0.0934
##  Apivar - AEC          -0.08690 0.186 121  -0.467  0.8870
##  Apiguard_Api - AEC    -0.48439 0.179 121  -2.712  0.0208
## 
## days = 21:
##  contrast              estimate    SE  df t.ratio p.value
##  Apivar - Apiguard_Api -0.09835 0.189 121  -0.520  0.8615
##  Apivar - AEC          -0.16357 0.186 121  -0.879  0.6548
##  Apiguard_Api - AEC    -0.06522 0.179 121  -0.365  0.9292
## 
## days = 42:
##  contrast              estimate    SE  df t.ratio p.value
##  Apivar - Apiguard_Api  0.06537 0.204 130   0.321  0.9448
##  Apivar - AEC          -0.17651 0.195 127  -0.904  0.6386
##  Apiguard_Api - AEC    -0.24188 0.185 126  -1.304  0.3954
## 
## Degrees-of-freedom method: kenward-roger 
## P value adjustment: tukey method for comparing a family of 3 estimates
```

```r
confint(emmeans(m.brood01, pairwise ~ trt | days, type = "response"))$contrasts
```

```
## days = 0:
##  contrast              estimate    SE  df lower.CL upper.CL
##  Apivar - Apiguard_Api -0.26501 0.189 121   -0.713   0.1834
##  Apivar - AEC          -0.26398 0.186 121   -0.706   0.1778
##  Apiguard_Api - AEC     0.00103 0.179 121   -0.423   0.4249
## 
## days = 10:
##  contrast              estimate    SE  df lower.CL upper.CL
##  Apivar - Apiguard_Api  0.39749 0.189 121   -0.051   0.8459
##  Apivar - AEC          -0.08690 0.186 121   -0.529   0.3549
##  Apiguard_Api - AEC    -0.48439 0.179 121   -0.908  -0.0606
## 
## days = 21:
##  contrast              estimate    SE  df lower.CL upper.CL
##  Apivar - Apiguard_Api -0.09835 0.189 121   -0.547   0.3501
##  Apivar - AEC          -0.16357 0.186 121   -0.605   0.2782
##  Apiguard_Api - AEC    -0.06522 0.179 121   -0.489   0.3586
## 
## days = 42:
##  contrast              estimate    SE  df lower.CL upper.CL
##  Apivar - Apiguard_Api  0.06537 0.204 130   -0.418   0.5483
##  Apivar - AEC          -0.17651 0.195 127   -0.639   0.2864
##  Apiguard_Api - AEC    -0.24188 0.185 126   -0.682   0.1979
## 
## Degrees-of-freedom method: kenward-roger 
## Confidence level used: 0.95 
## Conf-level adjustment: tukey method for comparing a family of 3 estimates
```

```r
# Note that we have a significant comparison here even though the inclusion of day:trt effect in the overall model was not significant.
```

# Summarize models for plotting


```r
emplot_bees <- emmip(m.bee1, trt~days, type = "response", CIs = TRUE, plotit = FALSE)
emplot_bees$days <- as.numeric(as.character(emplot_bees$days))
emplot_bees$trt <- factor(emplot_bees$trt, levels = c("Apivar", "AEC", "Apiguard_Api"))
levels(emplot_bees$trt) # check order of factors
```

```
## [1] "Apivar"       "AEC"          "Apiguard_Api"
```

```r
emplot_bees_apriori <- emmip(m.bee.apriori, trt~days, type = "response", CIs = TRUE, plotit = FALSE)
emplot_bees_apriori$days <- as.numeric(as.character(emplot_bees_apriori$days))
emplot_bees_apriori$trt <- factor(emplot_bees_apriori$trt, levels = c("Apivar", "AEC", "Apiguard_Api"))
levels(emplot_bees_apriori$trt) # check order of factors
```

```
## [1] "Apivar"       "AEC"          "Apiguard_Api"
```

```r
emplot_brood <- emmip(m.brood1, trt~days, type = "response", CIs = TRUE, plotit = FALSE)
emplot_brood$days <- as.numeric(as.character(emplot_brood$days))
emplot_brood$trt <- factor(emplot_brood$trt, levels = c("Apivar", "AEC", "Apiguard_Api"))
levels(emplot_brood$trt) # check order of factors
```

```
## [1] "Apivar"       "AEC"          "Apiguard_Api"
```


# Save these summarized tables of model estimates to their own CSVs


```r
write.csv(emplot_bees, "../3 Plotting/data_analyzed/emplot_bees.csv")
write.csv(emplot_bees_apriori, "../3 Plotting/data_analyzed/emplot_bees_apriori.csv")
write.csv(emplot_brood, "../3 Plotting/data_analyzed/emplot_brood.csv")
```

