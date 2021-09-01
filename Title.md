Expectation-augmented Phillips curve
================
Kyle Deng
4/15/2021

#### Obtain the inflation surprises and unemployment gap according to Coiban and Gorodnichenko(2015).

##### Backward-looking inflation expecatation formula:

$$E\_t\\pi\_{t+1}=\\frac{1}{4}(\\pi\_{t-1}+\\pi\_{t-2}+\\pi\_{t-3}+\\pi\_{t-4})$$

##### Inflation surprises:

*π*<sub>*t*</sub> − *E*<sub>*t*</sub>*π*<sub>*t* + 1</sub>

##### Unemployment gap:

*U**E*<sub>*t*</sub><sup>*g**a**p*</sup> = *U**E*<sub>*t*</sub> − *U**E*<sub>*t*</sub><sup>*n**a**t**u**r**a**l*</sup>

``` r
data$cpiaucsl_a = (data$CPIAUCSL/lag(data$CPIAUCSL, 4) - 1) * 100 
data$cpiaucsl_q = (data$CPIAUCSL/lag(data$CPIAUCSL, 1) - 1) * 400
data$back_cpiaucsl_qa = data$cpiaucsl_q - lag(data$cpiaucsl_a, 1)
data$cpilfesl_a = (data$CPILFESL/lag(data$CPILFESL, 4) - 1) * 100 
data$cpilfesl_q = (data$CPILFESL/lag(data$CPILFESL, 1) - 1) * 400
data$back_cpilfesl_qa = data$cpilfesl_q - lag(data$cpilfesl_a, 1)
data$un_gap <- data$UNRATE - data$NROUST

# Delete observation before 1985Q1  
# create a split point base on the cut-off point chosen in Figure 5 of Coiban and Gorodnichenko (2015) for my initial analysis
data <- data[-c(1:which(data$date == "1984-10-01")),] %>%
  mutate(label1 = case_when(
    date < "2007-10-01"  ~ "1985Q1-2007Q3",
    date >= "2007-10-01" ~ "2007Q4-2021Q2"
  ))
# Create a time series stamp
data$t <- seq.int(nrow(data))
```

<!-- ### Summary statistics and plot on the variables used  -->

### Time series plots on the variables used

``` r
date <- seq(data$date[1], data$date[146], by = '+3 month')
plot(xts(data$back_cpiaucsl_qa, order.by = date), main = "CPI inflation surprise")
```

![](Expectation-augmented-Phillips-curve_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
plot(xts(data$back_cpilfesl_qa, order.by = date), main = "Core CPI inflation surprise")
```

![](Expectation-augmented-Phillips-curve_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
plot(xts(data$un_gap, order.by = date), main = "Unemployment gap")
```

![](Expectation-augmented-Phillips-curve_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

### Scatter plot to show the Phillips curve relationship

#### Phillips curve with regular CPI inflation

``` r
ggplot() +
  geom_point(data = data, mapping = aes(x = un_gap, y = back_cpiaucsl_qa, color = label1, shape = label1)) + 
  geom_smooth(data = data, mapping = aes(x = un_gap, y = back_cpiaucsl_qa), method = "lm", color = "black", se = F, size = 0.75) +
  coord_cartesian(ylim = c(-5.5, 5), xlim = c(-1.5, 5)) +
  xlab("Unemployment gap") +
  ylab("CPI Inflation surprises") 
```

![](Expectation-augmented-Phillips-curve_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

2008Q4 and 2020Q2 are out of bound so are not reported in the regular
CPI inflation graph

#### Phillips curve with core CPI inflation

``` r
ggplot() +
  geom_point(data = data, mapping = aes(x = un_gap, y = back_cpilfesl_qa, color = label1, shape = label1)) + 
  geom_smooth(data = data, mapping = aes(x = un_gap, y = back_cpilfesl_qa), method = "lm", color = "black", se = F, size = 0.75) +
  coord_cartesian(ylim = c(-4, 3), xlim = c(-1.5, 9)) +
  xlab("Unemployment gap") +
  ylab("Core CPI Inflation surprises") 
```

![](Expectation-augmented-Phillips-curve_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### OLS and IV estimate of the Phillips curve relationship for the full sample

##### OLS formula:

*π*<sub>*t*</sub> − *E*<sub>*t*</sub>*π*<sub>*t* + 1</sub> = *c* + *κ**U**E*<sub>*t*</sub><sup>*g**a**p*</sup> + *v*<sub>*t*</sub>

#### Instrumental variable, two-stage least squares formula:

##### First stage regression:

$$\\widehat{UE\_t^{gap}}=\\alpha+\\beta UE\_{t-1}^{gap}+\\epsilon\_t$$

##### Outcome regression:

$$\\pi\_t-E\_t\\pi\_{t+1}=c+\\kappa \\widehat{UE\_{t-1}^{gap}}+u\_t$$

``` r
## OLS Phillips curve model for regular CPI 
full_ols <- lm(back_cpiaucsl_qa ~ un_gap, data = data)
## OLS Phillips curve model for core CPI 
full_ols_core <- lm(back_cpilfesl_qa ~ un_gap, data = data)
## IV Phillips curve model for regular CPI 
full_iv <- ivreg(back_cpiaucsl_qa ~ un_gap | lag(un_gap, 1), data = data)
## IV Phillips curve model for core CPI 
full_iv_core <- ivreg(back_cpilfesl_qa ~ un_gap | lag(un_gap, 1), data = data)

# Newey-West standard errors
robust3 <- sqrt(diag(NeweyWest(full_ols, prewhite = F, adjust = T)))
robust4 <- sqrt(diag(NeweyWest(full_iv, prewhite = F, adjust = T)))
robust3c <- sqrt(diag(NeweyWest(full_ols_core, prewhite = F, adjust = T)))
robust4c <- sqrt(diag(NeweyWest(full_iv_core, prewhite = F, adjust = T)))

# Estimates from the models with Newey-West standard errors
coeftest(full_ols, vcov.=NeweyWest(full_ols, prewhite = F, adjust = T))
```

    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  0.045396   0.105265  0.4313   0.6669
    ## un_gap      -0.067056   0.073274 -0.9151   0.3616

``` r
coeftest(full_iv, vcov.=NeweyWest(full_iv, prewhite = F, adjust = T))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -0.17357    0.15497 -1.1200   0.2646
    ## un_gap       0.25953    0.19362  1.3404   0.1823

``` r
coeftest(full_ols_core, vcov.=NeweyWest(full_ols_core, prewhite = F, adjust = T))
```

    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  0.019087   0.064359  0.2966  0.76722  
    ## un_gap      -0.081379   0.043547 -1.8688  0.06369 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
coeftest(full_iv_core, vcov.=NeweyWest(full_iv_core, prewhite = F, adjust = T))
```

    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -0.088445   0.083607 -1.0579   0.2919
    ## un_gap       0.081913   0.118317  0.6923   0.4899

### Exhaustive search for the split point

##### Chow-test statistics:

$$C=\\frac{RSS-RSS\_1-RSS\_2}{RSS\_1+RSS\_2}\\times\\frac{T-2k}{k}$$
*k* = number of parameters in the model

#### Analysis on the regular CPI inflation data

``` r
RSS = sum(resid(full_iv)^2)

t <- ceiling(0.15 * nrow(data))
tt <- floor(0.85 * nrow(data))
chow <- 0
index <- 0
for(i in t:tt){
  data1 <- data %>% filter(t <= i)
  data2 <- data %>% filter(t > i)
  sample1 <- ivreg(back_cpiaucsl_qa ~ un_gap | lag(un_gap, 1) , data = data1)
  sample2 <- ivreg(back_cpiaucsl_qa ~ un_gap | lag(un_gap, 1) , data = data2)
  RSS1 = sum(resid(sample1)^2)
  RSS2 = sum(resid(sample2)^2)
  C = ((RSS - RSS1 - RSS2) / (RSS1 + RSS2)) * ((146 - 2*2) / 2)
  if(C > chow){
    chow = C
    index = i
  }
}
qalpha = qf(0.95,2,146-4)
ifelse(chow>qalpha, 
       "Decision: Reject Null of Stability with signif. level 5%",
       "Decision: Do NOT reject Null of Stability with signif. level 5%")
```

    ## [1] "Decision: Reject Null of Stability with signif. level 5%"

``` r
## Split point from the search
data$date[index]
```

    ## [1] "2008-07-01"

``` r
## Create a dummy base on the split 
data$dummy = ifelse(data$date >= data$date[index], 1, 0)
## Create another label base on the split
data <- data %>% 
  mutate(label2 = case_when(
    date >= "1985-01-01" & date < "2008-07-01"   ~ "1985Q1-2008Q2",
    date >= "2008-07-01" & date < "2020-01-01" ~ "2008Q3-2019Q4",
    date >= "2020-01-01" ~ "2020Q1-2021Q2"
  ))
```

#### Analysis on the core CPI inflation data

``` r
RSS_core = sum(resid(full_iv_core)^2)
chowc <- 0
indexc <- 0
for(i in t:tt){
  data1 <- data %>% filter(t <= i)
  data2 <- data %>% filter(t > i)
  sample1 <- ivreg(back_cpilfesl_qa ~ un_gap | lag(un_gap, 1) , data = data1)
  sample2 <- ivreg(back_cpilfesl_qa ~ un_gap | lag(un_gap, 1) , data = data2)
  RSS1c = sum(resid(sample1)^2)
  RSS2c = sum(resid(sample2)^2)
  Cc = ((RSS_core - RSS1c - RSS2c) / (RSS1c + RSS2c)) * ((146 - 2*2) / 2)
  if(Cc > chowc){
    chowc = Cc
    indexc = i
  }
}
ifelse(chowc>qalpha, 
       "Decision: Reject Null of Stability with signif. level 5%",
       "Decision: Do NOT reject Null of Stability with signif. level 5%")
```

    ## [1] "Decision: Do NOT reject Null of Stability with signif. level 5%"

``` r
## Split point from the search
data$date[indexc]
```

    ## [1] "1991-01-01"

``` r
## Create a dummy base on the split 
data$dummyc = ifelse(data$date >= data$date[indexc], 1, 0)
## Create another label base on the split
data <- data %>% 
  mutate(label3 = case_when(
    date >= "1985-01-01" & date < "1991-01-01"   ~ "1985Q1-1990Q4",
    date >= "1991-01-01" & date < "2020-01-01" ~ "1991Q1-2019Q4",
    date >= "2020-01-01" ~ "2020Q1-2021Q2"
  ))
```

### Phillips curve on the regular CPI inflation data with the new split

``` r
data_pre = subset(data, label2 == "1985Q1-2008Q2")
data_post = subset(data, (label2 == "2008Q3-2019Q4" | label2 == "2020Q1-2021Q2"))

ggplot() +
  geom_point(data = data, mapping = aes(x = un_gap, y = back_cpiaucsl_qa, color = label2, shape = label2)) +
  geom_text(data = data[141:146, ], aes(x = un_gap, y = back_cpiaucsl_qa, label = date), position=position_dodge(width=1.5), hjust = 1.5, size = 2.5) +
  geom_text_repel(data = data[141:146, ], aes(x = un_gap, y = back_cpiaucsl_qa, label = date), size = 2.5) +
  geom_smooth(data = data_pre, mapping = aes(x = un_gap, y = back_cpiaucsl_qa), method = "lm", se = F, color = "red", size = 0.75) +
  geom_smooth(data = data_post, mapping = aes(x = un_gap, y = back_cpiaucsl_qa), method = 'lm', se = F, color = "darkgreen", size = 0.75) +
  ggtitle("Time variation in the slope of the Phillips curve") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(-14, 5.5), xlim = c(-2, 8.5)) +
  xlab("Unemployment gap") +
  ylab("CPI inflation surprises") 
```

![](Expectation-augmented-Phillips-curve_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Phillips curve on the core CPI inflation data with the new split

``` r
data_prec = subset(data, label3 == "1985Q1-1990Q4")
data_postc = subset(data, (label3 == "1991Q1-2019Q4" | label3 == "2020Q1-2021Q2"))

ggplot() +
  geom_point(data = data, mapping = aes(x = un_gap, y = back_cpilfesl_qa, color = label3, shape = label3)) +
  geom_text(data = data[141:146, ], aes(x = un_gap, y = back_cpilfesl_qa, label = date), position=position_dodge(width=1.5), hjust = 1.5, size = 2.5) +
  geom_text_repel(data = data[141:146, ], aes(x = un_gap, y = back_cpilfesl_qa, label = date), size = 2.5) +
  geom_smooth(data = data_prec, mapping = aes(x = un_gap, y = back_cpilfesl_qa), method = "lm", se = F, color = "red", size = 0.75) +
  geom_smooth(data = data_postc, mapping = aes(x = un_gap, y = back_cpilfesl_qa), method = 'lm', se = F, color = "darkgreen", size = 0.75) +
  coord_cartesian(ylim = c(-4, 6.25), xlim = c(-1.75, 8.5)) +
  ggtitle("Time variation in the slope of the Phillips curve(Core cpi inflation)") +
   theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Unemployment gap") +
  ylab("Core CPI inflation surprises") 
```

![](Expectation-augmented-Phillips-curve_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Seperate IV regression on the 2 sub-periods to find out how much the slope has changed

``` r
# Regular CPI inflation
split1 <- ivreg(back_cpiaucsl_qa ~ un_gap | un_gap + lag(un_gap, 1), data = data[1:index-1,])
# summary(split1)
robust1 <- sqrt(diag(NeweyWest(split1, prewhite = F, adjust = T)))
split2 <- ivreg(back_cpiaucsl_qa ~ un_gap | un_gap + lag(un_gap, 1), data = data[index:nrow(data),])
robust2 <- sqrt(diag(NeweyWest(split2, prewhite = F, adjust = T)))
# summary(split2)
coeftest(split1, vcov.= NeweyWest(split1, prewhite = F, adjust = T))
```

    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  0.029977   0.155373  0.1929   0.8474
    ## un_gap      -0.215351   0.168356 -1.2791   0.2041

``` r
coeftest(split2, vcov.= NeweyWest(split2, prewhite = F, adjust = T))
```

    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  0.055787   0.458390  0.1217   0.9036
    ## un_gap      -0.042667   0.165770 -0.2574   0.7980

``` r
# Core CPI inflation
split1c <- ivreg(back_cpilfesl_qa ~ un_gap | lag(un_gap, 1), data = data[1:indexc-1,])
split2c <- ivreg(back_cpilfesl_qa ~ un_gap | lag(un_gap, 1), data = data[indexc:nrow(data),])
coeftest(split1c, vcov.= NeweyWest(split1c, prewhite = F, adjust = T))
```

    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  0.065759   0.091314  0.7201  0.47938  
    ## un_gap      -0.290017   0.137534 -2.1087  0.04715 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
coeftest(split2c, vcov.= NeweyWest(split2c, prewhite = F, adjust = T))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -0.12344    0.10197 -1.2105   0.2285
    ## un_gap       0.10219    0.12435  0.8218   0.4128

``` r
robust1c <- sqrt(diag(NeweyWest(split1c, prewhite = F, adjust = T)))
robust2c <- sqrt(diag(NeweyWest(split2c, prewhite = F, adjust = T)))
```

<!-- ### Output the table of the regression results -->
<!-- ##### Table for regular CPI inflation -->
<!-- ##### Table for core CPI inflation -->