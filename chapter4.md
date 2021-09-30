Chapter 4. Modeling the Time Course of Continuous Outcomes
================

This publishment modified a sample R code that are now being publicly
probided on the book website (<http://www.intensivelongitudinal.com/>).
I added interpretations for myself (and maybe, for future researchers),
and also changed the code for plots (from R basic code to ggplot2)

``` r
#load the nlme package
library(nlme)

#Read a csv file containing the data
time <- read.csv('time.csv')
summary(time)
```

    ##        id            time           time01        intimacy       treatment  
    ##  Min.   : 1.0   Min.   : 0.00   Min.   :0.00   Min.   :0.000   Min.   :0.0  
    ##  1st Qu.:13.0   1st Qu.: 3.75   1st Qu.:0.25   1st Qu.:2.368   1st Qu.:0.0  
    ##  Median :25.5   Median : 7.50   Median :0.50   Median :3.330   Median :0.5  
    ##  Mean   :25.5   Mean   : 7.50   Mean   :0.50   Mean   :3.469   Mean   :0.5  
    ##  3rd Qu.:38.0   3rd Qu.:11.25   3rd Qu.:0.75   3rd Qu.:4.540   3rd Qu.:1.0  
    ##  Max.   :50.0   Max.   :15.00   Max.   :1.00   Max.   :9.410   Max.   :1.0

``` r
table(time$treatment)
```

    ## 
    ##   0   1 
    ## 400 400

-   time01(0\~1 range)
-   time(0\~15 range)
-   treatment: binary (0/1)
-   16 timepoints for each person
-   25 person for treatment, 25 person for control group.

``` r
#Panel plots for Control Group & treatment group
library(ggplot2)
library(dplyr)
```

``` r
#control group
time_ct <- time %>% filter(treatment==0)

ggplot(time_ct, aes(time, intimacy)) +
  geom_line() +
  geom_point() +
  facet_wrap(~id) +
  ggtitle("Panel plot of control group")
```

![](chapter4_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#treatment group
time_tr <- time %>% filter(treatment==1)

ggplot(time_tr, aes(time, intimacy)) +
  geom_line() +
  geom_point() +
  facet_wrap(~id) +
  ggtitle("Panel plot of treatment group")
```

![](chapter4_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

Two graphs are showing the panel plots of control & treatment group.
After exploring the panel plots, I directly analyzed using `lme`
function. I allowed the autocorrelation error, since this data is
time-series. (`correlation = corAR1`) Also, this model includes the
random effect of intercept and slope. (\`random=\~time \| id\`\`)

``` r
# time 01(0~1 range) / time(0~15 range), no difference on analysis result

lgmodel <- lme(fixed=intimacy ~ time01*treatment, data=time, random=~time01 | id, correlation = corAR1())
print(summary(lgmodel))
```

    ## Linear mixed-effects model fit by REML
    ##   Data: time 
    ##        AIC      BIC    logLik
    ##   2852.392 2894.508 -1417.196
    ## 
    ## Random effects:
    ##  Formula: ~time01 | id
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev    Corr  
    ## (Intercept) 0.8281209 (Intr)
    ## time01      1.3761434 -0.453
    ## Residual    1.3009532       
    ## 
    ## Correlation Structure: AR(1)
    ##  Formula: ~1 | id 
    ##  Parameter estimate(s):
    ##           Phi 
    ## -3.601108e-05 
    ## Fixed effects:  intimacy ~ time01 * treatment 
    ##                       Value Std.Error  DF   t-value p-value
    ## (Intercept)       2.8989745 0.2070310 748 14.002607  0.0000
    ## time01            0.7352012 0.3472019 748  2.117504  0.0345
    ## treatment        -0.0564426 0.2927861  48 -0.192777  0.8479
    ## time01:treatment  0.9214365 0.4910176 748  1.876586  0.0610
    ##  Correlation: 
    ##                  (Intr) time01 trtmnt
    ## time01           -0.599              
    ## treatment        -0.707  0.423       
    ## time01:treatment  0.423 -0.707 -0.599
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -2.62425713 -0.67979280 -0.01899203  0.64336207  2.44625355 
    ## 
    ## Number of Observations: 800
    ## Number of Groups: 50

``` r
lgmodel2 <- lme(fixed=intimacy ~ time*treatment, data=time, random=~time | id, correlation = corAR1())
print(summary(lgmodel2))
```

    ## Linear mixed-effects model fit by REML
    ##   Data: time 
    ##        AIC     BIC    logLik
    ##   2863.224 2905.34 -1422.612
    ## 
    ## Random effects:
    ##  Formula: ~time | id
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev     Corr  
    ## (Intercept) 0.82811162 (Intr)
    ## time        0.09174247 -0.453
    ## Residual    1.30095335       
    ## 
    ## Correlation Structure: AR(1)
    ##  Formula: ~1 | id 
    ##  Parameter estimate(s):
    ##           Phi 
    ## -3.572045e-05 
    ## Fixed effects:  intimacy ~ time * treatment 
    ##                     Value  Std.Error  DF   t-value p-value
    ## (Intercept)     2.8989745 0.20702959 748 14.002706  0.0000
    ## time            0.0490134 0.02314673 748  2.117510  0.0345
    ## treatment      -0.0564425 0.29278406  48 -0.192779  0.8479
    ## time:treatment  0.0614291 0.03273441 748  1.876591  0.0610
    ##  Correlation: 
    ##                (Intr) time   trtmnt
    ## time           -0.599              
    ## treatment      -0.707  0.423       
    ## time:treatment  0.423 -0.707 -0.599
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -2.62425566 -0.67979965 -0.01899356  0.64336329  2.44625287 
    ## 
    ## Number of Observations: 800
    ## Number of Groups: 50

We can check the fixed & random effect estimates through above results.
Using the estimated coefficients and standard deviations, we can also
compute the confidence interval of each parameter.

``` r
#Panel plots for control group with fitted line

time_fit <- cbind(time, fitted(lgmodel))  
colnames(time_fit)[6] <- 'fit'


time_ct_fit <- time_fit %>% filter(treatment==0)
ggplot(time_ct_fit, aes(time, intimacy)) +
  geom_line() +
  geom_point() +
  geom_smooth(aes(time, fit), color='red') +
  facet_wrap(~id) +
  ggtitle('Panel plots for Control Group with fitted line')
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](chapter4_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#Panel plots for treatment group with fitted line
time_tr_fit <- time_fit %>% filter(treatment==1)
ggplot(time_tr_fit, aes(time, intimacy)) +
  geom_line() +
  geom_point() +
  geom_smooth(aes(time, fit), color='red') +
  facet_wrap(~id) +
  ggtitle('Panel plots for treatment group with fitted line')
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](chapter4_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

panel plots for treatment & control group with the fitted lines.
