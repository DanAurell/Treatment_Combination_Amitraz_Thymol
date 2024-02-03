---
title: "1_Varroa_infestation_2023-05-22"
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
         )
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
 means = mean(mites_recov_total),
 variances = var(mites_recov_total),
 ratio = variances/means)

# Variance-to-mean ratio of 22; use negative binomial distribution instead of Poisson
```


Fit Poisson and negative binomial models

```r
m.phor.pois1 <- glmer(mites_recov_total ~ 
                     trt + 
                     days + 
                     trt:days + 
                     (1 | col_no), 
                   data = tempdatum, family = poisson)


m.phor.nb1 <- glmer.nb(mites_recov_total ~ 
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
## Formula: mites_recov_total ~ trt + days + trt:days + (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##   1023.2   1051.0   -501.6   1003.2      109 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8777 -1.2001 -0.3968  0.6802  6.1902 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  col_no (Intercept) 0.328    0.5727  
## Number of obs: 119, groups:  col_no, 41
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             3.35203    0.17490  19.165  < 2e-16 ***
## trtApiguard_Api        -0.03797    0.23802  -0.160  0.87325    
## trtAEC                  0.09704    0.23359   0.415  0.67783    
## days21                 -1.69866    0.12505 -13.584  < 2e-16 ***
## days42                 -1.11985    0.11396  -9.827  < 2e-16 ***
## trtApiguard_Api:days21 -0.60392    0.20095  -3.005  0.00265 ** 
## trtAEC:days21          -1.74846    0.27536  -6.350 2.16e-10 ***
## trtApiguard_Api:days42 -0.31044    0.16633  -1.866  0.06199 .  
## trtAEC:days42           0.26440    0.13861   1.907  0.05646 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days21 days42 tA_A:2 tAEC:2 tA_A:4
## trtApgrd_Ap -0.733                                                 
## trtAEC      -0.748  0.549                                          
## days21      -0.111  0.081  0.083                                   
## days42      -0.133  0.097  0.099  0.170                            
## trtApg_A:21  0.069 -0.098 -0.052 -0.622 -0.106                     
## trtAEC:dy21  0.050 -0.037 -0.066 -0.454 -0.077  0.283              
## trtApg_A:42  0.091 -0.127 -0.068 -0.116 -0.685  0.140  0.053       
## trtAEC:dy42  0.109 -0.080 -0.139 -0.140 -0.822  0.087  0.112  0.563
```

```r
summary(m.phor.nb1)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(2.7679)  ( log )
## Formula: mites_recov_total ~ trt + days + trt:days + (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##    802.3    832.9   -390.2    780.3      108 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.4711 -0.6873 -0.2068  0.3607  2.8386 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  col_no (Intercept) 0.1625   0.4031  
## Number of obs: 119, groups:  col_no, 41
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             3.42138    0.22152  15.445  < 2e-16 ***
## trtApiguard_Api        -0.04362    0.29844  -0.146    0.884    
## trtAEC                  0.10805    0.29547   0.366    0.715    
## days21                 -1.72307    0.28152  -6.121 9.32e-10 ***
## days42                 -1.17364    0.29624  -3.962 7.44e-05 ***
## trtApiguard_Api:days21 -0.61904    0.40028  -1.547    0.122    
## trtAEC:days21          -1.77054    0.43967  -4.027 5.65e-05 ***
## trtApiguard_Api:days42 -0.38664    0.40498  -0.955    0.340    
## trtAEC:days42           0.22907    0.38677   0.592    0.554    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days21 days42 tA_A:2 tAEC:2 tA_A:4
## trtApgrd_Ap -0.736                                                 
## trtAEC      -0.746  0.550                                          
## days21      -0.543  0.404  0.408                                   
## days42      -0.552  0.407  0.412  0.413                            
## trtApg_A:21  0.388 -0.532 -0.290 -0.702 -0.294                     
## trtAEC:dy21  0.355 -0.262 -0.483 -0.639 -0.268  0.451              
## trtApg_A:42  0.410 -0.541 -0.305 -0.301 -0.735  0.399  0.198       
## trtAEC:dy42  0.431 -0.315 -0.572 -0.315 -0.770  0.227  0.384  0.568
```


Plot residuals and see if it's zero-inflated.



Comparing nb to poisson with deviance F-test

```r
# Test whether nb is a stat sig better fit than poisson:
anova(m.phor.nb1, m.phor.pois1)
```

```
## Data: tempdatum
## Models:
## m.phor.pois1: mites_recov_total ~ trt + days + trt:days + (1 | col_no)
## m.phor.nb1: mites_recov_total ~ trt + days + trt:days + (1 | col_no)
##              npar     AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)    
## m.phor.pois1   10 1023.19 1050.98 -501.60  1003.19                         
## m.phor.nb1     11  802.31  832.88 -390.15   780.31 222.88  1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Yes, highly significantly better (p<2.2e-16)

summary(m.phor.pois1)$coefficients
```

```
##                           Estimate Std. Error     z value     Pr(>|z|)
## (Intercept)             3.35203052  0.1749037  19.1650089 7.254428e-82
## trtApiguard_Api        -0.03797070  0.2380180  -0.1595287 8.732524e-01
## trtAEC                  0.09703682  0.2335859   0.4154224 6.778327e-01
## days21                 -1.69866465  0.1250489 -13.5840046 4.982595e-42
## days42                 -1.11985378  0.1139586  -9.8268474 8.627737e-23
## trtApiguard_Api:days21 -0.60392497  0.2009547  -3.0052785 2.653378e-03
## trtAEC:days21          -1.74846271  0.2753634  -6.3496564 2.157964e-10
## trtApiguard_Api:days42 -0.31044468  0.1663334  -1.8664001 6.198541e-02
## trtAEC:days42           0.26439513  0.1386085   1.9074954 5.645646e-02
```

```r
summary(m.phor.nb1)$coefficients
```

```
##                           Estimate Std. Error    z value     Pr(>|z|)
## (Intercept)             3.42137875  0.2215181 15.4451399 8.135095e-54
## trtApiguard_Api        -0.04361581  0.2984436 -0.1461442 8.838075e-01
## trtAEC                  0.10804762  0.2954734  0.3656763 7.146066e-01
## days21                 -1.72306833  0.2815210 -6.1205673 9.324280e-10
## days42                 -1.17363886  0.2962445 -3.9617238 7.441059e-05
## trtApiguard_Api:days21 -0.61904411  0.4002808 -1.5465247 1.219779e-01
## trtAEC:days21          -1.77054492  0.4396662 -4.0270205 5.648810e-05
## trtApiguard_Api:days42 -0.38664119  0.4049820 -0.9547120 3.397234e-01
## trtAEC:days42           0.22907415  0.3867677  0.5922784 5.536642e-01
```

```r
# The p values for my predictors went up, but the fit is way better
  # This makes sense... Poisson is not conservative enough with overdispersed data
# The parameter estimates stay pretty much the same. Good.
```

### Fit model that uses number of bees in sample as the offset


```r
# A priori model
m.phor.nb1 <- glmer.nb(mites_recov_total ~ 
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
##  Family: Negative Binomial(2.7679)  ( log )
## Formula: mites_recov_total ~ trt + days + trt:days + (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##    802.3    832.9   -390.2    780.3      108 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.4711 -0.6873 -0.2068  0.3607  2.8386 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  col_no (Intercept) 0.1625   0.4031  
## Number of obs: 119, groups:  col_no, 41
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             3.42138    0.22152  15.445  < 2e-16 ***
## trtApiguard_Api        -0.04362    0.29844  -0.146    0.884    
## trtAEC                  0.10805    0.29547   0.366    0.715    
## days21                 -1.72307    0.28152  -6.121 9.32e-10 ***
## days42                 -1.17364    0.29624  -3.962 7.44e-05 ***
## trtApiguard_Api:days21 -0.61904    0.40028  -1.547    0.122    
## trtAEC:days21          -1.77054    0.43967  -4.027 5.65e-05 ***
## trtApiguard_Api:days42 -0.38664    0.40498  -0.955    0.340    
## trtAEC:days42           0.22907    0.38677   0.592    0.554    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days21 days42 tA_A:2 tAEC:2 tA_A:4
## trtApgrd_Ap -0.736                                                 
## trtAEC      -0.746  0.550                                          
## days21      -0.543  0.404  0.408                                   
## days42      -0.552  0.407  0.412  0.413                            
## trtApg_A:21  0.388 -0.532 -0.290 -0.702 -0.294                     
## trtAEC:dy21  0.355 -0.262 -0.483 -0.639 -0.268  0.451              
## trtApg_A:42  0.410 -0.541 -0.305 -0.301 -0.735  0.399  0.198       
## trtAEC:dy42  0.431 -0.315 -0.572 -0.315 -0.770  0.227  0.384  0.568
```

```r
# Take into account the number of bees per sample

m.phor <- glmer.nb(mites_recov_total ~ 
                     trt + 
                     days + 
                     trt:days + 
                     offset(log(est_bee_num_by_wt_floor)) +
                     (1 | col_no),
                   data = tempdatum)
summary(m.phor) # Very different coefficients... because varroa per bee rather than per sample...
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(2.9337)  ( log )
## Formula: 
## mites_recov_total ~ trt + days + trt:days + offset(log(est_bee_num_by_wt_floor)) +  
##     (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##    800.2    830.8   -389.1    778.2      108 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.5276 -0.7166 -0.2146  0.3452  2.7595 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  col_no (Intercept) 0.195    0.4416  
## Number of obs: 119, groups:  col_no, 41
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -2.39917    0.22319 -10.750  < 2e-16 ***
## trtApiguard_Api         0.02053    0.30153   0.068   0.9457    
## trtAEC                  0.15308    0.29827   0.513   0.6078    
## days21                 -1.61254    0.27603  -5.842 5.16e-09 ***
## days42                 -1.17444    0.29028  -4.046 5.21e-05 ***
## trtApiguard_Api:days21 -0.71164    0.39322  -1.810   0.0703 .  
## trtAEC:days21          -1.86716    0.43372  -4.305 1.67e-05 ***
## trtApiguard_Api:days42 -0.43378    0.39658  -1.094   0.2740    
## trtAEC:days42           0.16686    0.37822   0.441   0.6591    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days21 days42 tA_A:2 tAEC:2 tA_A:4
## trtApgrd_Ap -0.734                                                 
## trtAEC      -0.746  0.548                                          
## days21      -0.523  0.388  0.392                                   
## days42      -0.531  0.390  0.396  0.411                            
## trtApg_A:21  0.370 -0.510 -0.277 -0.701 -0.290                     
## trtAEC:dy21  0.338 -0.248 -0.461 -0.635 -0.264  0.447              
## trtApg_A:42  0.391 -0.519 -0.291 -0.300 -0.733  0.394  0.194       
## trtAEC:dy42  0.413 -0.301 -0.550 -0.314 -0.770  0.223  0.379  0.566
```

```r
  # Check on this interpretation


# Test significance of trt:days interaction

## Fit model that omits trt:days interaction

m.phor.0 <- glmer.nb(mites_recov_total ~ 
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
##  Family: Negative Binomial(2.0893)  ( log )
## Formula: 
## mites_recov_total ~ trt + days + offset(log(est_bee_num_by_wt_floor)) +  
##     (1 | col_no)
##    Data: tempdatum
## 
##      AIC      BIC   logLik deviance df.resid 
##    818.9    838.3   -402.4    804.9      112 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.3492 -0.6437 -0.2860  0.3991  3.8306 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  col_no (Intercept) 0.1478   0.3845  
## Number of obs: 119, groups:  col_no, 41
## 
## Fixed effects:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      -2.0998     0.2055 -10.219  < 2e-16 ***
## trtApiguard_Api  -0.3578     0.2422  -1.477    0.140    
## trtAEC           -0.2291     0.2375  -0.965    0.335    
## days21           -2.3643     0.1865 -12.679  < 2e-16 ***
## days42           -1.2034     0.1750  -6.876 6.15e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) trtA_A trtAEC days21
## trtApgrd_Ap -0.674                     
## trtAEC      -0.686  0.556              
## days21      -0.436  0.095  0.141       
## days42      -0.362  0.047 -0.045  0.405
```

```r
anova(m.phor, m.phor.0) 
```

```
## Data: tempdatum
## Models:
## m.phor.0: mites_recov_total ~ trt + days + offset(log(est_bee_num_by_wt_floor)) + (1 | col_no)
## m.phor: mites_recov_total ~ trt + days + trt:days + offset(log(est_bee_num_by_wt_floor)) + (1 | col_no)
##          npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
## m.phor.0    7 818.85 838.31 -402.43   804.85                         
## m.phor     11 800.24 830.81 -389.12   778.24 26.618  4  2.375e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# There was a significant interaction between day and treatment (p=2.375e-05) when tested by ANOVA. Therefore we analyzed each date separately.
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

# Variance-to-mean ratio of 14; use negative binomial distribution instead of Poisson
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





# Summarize models for text, tables, and plotting


```r
# Estimated marginal means (and CIs) for text, tables, and plotting
# For the models which included an offset, report the emm's and CIs for a hypothetical sample of 100 bees
# This method is based on the emmeans documentation at: https://cran.r-project.org/web/packages/emmeans/vignettes/sophisticated.html#offsets

emplot_phor <- emmip(m.phor, trt~days, type = "response", CIs = TRUE, plotit = FALSE, offset = log(100))
emplot_phor$days <- as.numeric(as.character(emplot_phor$days))
emplot_phor$trt <- factor(emplot_phor$trt, levels = c("Apivar", "AEC", "Apiguard_Api"))
levels(emplot_phor$trt) # check order of factors
```

```
## [1] "Apivar"       "AEC"          "Apiguard_Api"
```

```r
emplot_infest <- emmip(m.infest.nb1, trt~days, type = "response", CIs = TRUE, plotit = FALSE)
emplot_infest$days <- as.numeric(as.character(emplot_infest$days))
emplot_infest$trt <- factor(emplot_infest$trt, levels = c("Apivar", "AEC", "Apiguard_Api"))
levels(emplot_infest$trt) # check order of factors
```

```
## [1] "Apivar"       "AEC"          "Apiguard_Api"
```


# Save these summarized tables of model estimates to their own CSVs


```r
write.csv(emplot_phor, "../3 Plotting/data_analyzed/emplot_phor.csv")
write.csv(emplot_infest, "../3 Plotting/data_analyzed/emplot_infest.csv")
```

