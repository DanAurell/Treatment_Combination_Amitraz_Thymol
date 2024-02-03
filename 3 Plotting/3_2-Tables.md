---
title: "3.2 Tables"
author: "Dan Aurell"
date: "2023-05-26"
output:   
  html_document: 
    keep_md: yes
---


# Setup


```r
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
```

```r
library(gt)
```

Required to read in


```r
emplot_phor <- read.csv("./data_analyzed/emplot_phor.csv")
emplot_infest <- read.csv("./data_analyzed/emplot_infest.csv")
emplot_bees <- read.csv("./data_analyzed/emplot_bees.csv")
emplot_brood <- read.csv("./data_analyzed/emplot_brood.csv")
```


# Add variables


```r
emplot_phor$variable <- "varroa_adults"
emplot_infest$variable <- "varroa_brood"
emplot_bees$variable <- "frames_bees"
emplot_brood$variable <- "frames_brood"

"varroa_adults"
```

```
## [1] "varroa_adults"
```

```r
"varroa_brood"
```

```
## [1] "varroa_brood"
```

```r
"frames_bees"
```

```
## [1] "frames_bees"
```

```r
"frames_brood"
```

```
## [1] "frames_brood"
```



```r
df <- rbind(emplot_phor, emplot_infest, emplot_bees, emplot_brood)

df2 <- df %>% 
  select(variable, days, trt, yvar, LCL, UCL)
```





```r
# Round the estimate and confidence limits to 2 decimal places
df2$yvar <- sprintf("%.2f", df2$yvar)
df2$LCL <- sprintf("%.2f", df2$LCL)
df2$UCL <- sprintf("%.2f", df2$UCL)

# Make pretty treatment names
df2 <- df2 %>% 
  mutate(trt = case_when(trt == "Apivar" ~ "Apivar",
                          trt == "AEC" ~ "Amitraz E.C.",
                          trt == "Apiguard_Api" ~ "Combination"
                        ))

# Make pretty variable names
df2 <- df2 %>% 
  mutate(variable = case_when(variable == "varroa_adults" ~ "% Varroa infestation (adult bees)",
                              variable == "varroa_brood" ~ "% Varroa infestation (worker brood)",
                              variable == "frames_bees" ~ "Frames of bees",
                              variable == "frames_brood" ~ "Frames of worker brood"
                        ))




df2 <- df2 %>% 
  mutate(
    Estimate = paste0(yvar, " [", LCL, "-", UCL, "]")
  ) %>% 
  select(-c(yvar, LCL, UCL))




df2$trt <- factor(df2$trt , levels=c("Apivar", "Amitraz E.C.", "Combination"))

df3<- df2 %>% 
  group_by(variable, days) %>% 
  arrange(trt, .by_group = TRUE) %>% 
  ungroup()
```



```r
df_tbl <- gt(df3)

df_rtf <- df_tbl %>%
  as_rtf()

my_conn <- file("./outputs/tables/Table2_2024-02-03.RTF", "w")
writeLines(df_rtf, my_conn)
close(my_conn)
```


Sample size added after the fact to Table 2; See Script 2.1 Varroa Infestation for details.
