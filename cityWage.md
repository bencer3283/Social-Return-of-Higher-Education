Workplace Wage by City
================

``` r
library(knitr)
library(readxl) #for reading Excel file
library(stargazer) #for Latex table generation
library(lmtest) #for heteroskedaticity test
library(data.table) #for transpose() function
library(ivreg) #for instrumental variable 2SLS regression
library(dplyr)
```

# Import Data

``` r
rawGraduate <- read_excel("109cityu.xls")
rawWage <- read_excel("workplaceWageCity.xlsx")
rawWorkforce <- read_excel("cityWorkforce.xlsx")
rawCityAddition <- read_excel("additionalCityData.xlsx")
```

``` r
CITY <- cbind(rawWage["...1"], rawWage["109"], rawWage["108"], rawWage["107"], rawGraduate$...10[5:24])
colnames(CITY) <- c("city", "wage2020", "wage2019", "wage2018", "graduate2020")
CITY$graduate2020 <- as.numeric(CITY$graduate2020)
direct <- seq(0, 0, length.out = 20)
direct[1:6] <- 1
CITY <- cbind(CITY, direct)
rm(direct)
```

## Data manipulation

``` r
CITY <- workforceTranspose("workforceCollege", 3, 14)
CITY <- workforceTranspose("workforceYoung", 16, 27)
CITY <- workforceTranspose("workforceMiddle", 29, 40)
CITY <- workforceTranspose("workforceOld", 42, 53)
CITY <- workforceTranspose("workforceRetired", 55, 66)
```

``` r
for(i in 2:ncol(rawCityAddition)) {
    if(!is.na(rawCityAddition[2, i])){
        CITY <- cbind(CITY, rawCityAddition[1:20, i])
    }   
}
```

``` r
colnames(CITY)[67:69] <- c("hired2018", "hired2019", "hired2020")
colnames(CITY)[71:73] <- c("workforcePopulation2018", "workforcePopulation2019", "workforcePopulation2020")
colnames(CITY)[75:77] <- c("gender2018", "gender2019", "gender2020")
colnames(CITY)[79:81] <- c("manufecture2018", "manufecture2019", "manufecture2020")
colnames(CITY)[83:85] <- c("service2018", "service2019", "service2020")
colnames(CITY)[87:89] <- c("eduExpense2018", "eduExpense2019", "eduExpense2020")
colnames(CITY)[91:93] <- c("married2018", "married2019", "married2020")
colnames(CITY)[95:97] <- c("eduLevel2018", "eduLevel2019", "eduLevel2020")
colnames(CITY)[99:101] <- c("expensePerCapita2018", "expensePerCapita2019", "expensePerCapita2020")
colnames(CITY)[103:105] <- c("unemployment2018", "unemployment2019", "unemployment2020")
```

# SLR with college graduates number

``` r
slr <- lm(CITY$wage2020 ~ CITY$graduate2020, )
summary(slr)
#stargazer(slr)
```

    ## 
    ## Call:
    ## lm(formula = CITY$wage2020 ~ CITY$graduate2020)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.626  -5.494  -4.060   1.423  34.223 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       6.003e+01  3.482e+00  17.241 1.23e-12 ***
    ## CITY$graduate2020 2.481e-04  1.615e-04   1.536    0.142    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.73 on 18 degrees of freedom
    ## Multiple R-squared:  0.1159, Adjusted R-squared:  0.06674 
    ## F-statistic: 2.359 on 1 and 18 DF,  p-value: 0.142

``` r
plot(CITY$graduate2020, CITY$wage2020, main="2020 City Data", xlab="No. of Graduate", ylab="Average yearly wage ($10,000)")
text(CITY$graduate2020[c(8,19)], CITY$wage2020[c(8,19)], labels = c("Hsinchu County", "Hsinchu City"), cex = 0.6, pos = 4)
abline(slr)
```

![](cityWage_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# SLR with college worker share

``` r
slr2 <- lm(CITY$wage2020 ~ CITY$workforceCollege_2020, )
summary(slr2)
#stargazer(slr2)
```

    ## 
    ## Call:
    ## lm(formula = CITY$wage2020 ~ CITY$workforceCollege_2020)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.5443  -4.5628  -0.7453   2.5238  22.5534 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 28.3545     8.8623   3.199 0.004969 ** 
    ## CITY$workforceCollege_2020   0.7252     0.1779   4.077 0.000707 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.991 on 18 degrees of freedom
    ## Multiple R-squared:  0.4801, Adjusted R-squared:  0.4512 
    ## F-statistic: 16.62 on 1 and 18 DF,  p-value: 0.0007073

``` r
plot(CITY$workforceCollege_2020, CITY$wage2020, main="2020 City Data", xlab="Share of college worker (%)", ylab="Average yearly wage ($10,000)")
text(CITY$workforceCollege_2020[c(8,19)], CITY$wage2020[c(8,19)], labels = c("Hsinchu County", "Hsinchu City"), cex = 0.6, pos = 4)
abline(slr2)
```

![](cityWage_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Heteroskedaticity

### Test

We use the White Test for heteroskedaticity by specifying a formula with
interaction term and sqaure term to the BP Test.

``` r
bptest(slr2, ~ CITY$workforceCollege_2020 + I(CITY$workforceCollege_2020^2))
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  slr2
    ## BP = 4.351, df = 2, p-value = 0.1135

Heteroskedaticity doesn’t seem present.

### Robust

# MLR

``` r
mlr <- lm(CITY$wage2020 ~ CITY$workforceCollege_2020 + CITY$direct + CITY$wage2018, )
summary(mlr)
#stargazer(mlr)
```

    ## 
    ## Call:
    ## lm(formula = CITY$wage2020 ~ CITY$workforceCollege_2020 + CITY$direct + 
    ##     CITY$wage2018)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.2489 -0.3238  0.1111  0.3235  1.5463 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -2.31769    0.86964  -2.665   0.0169 *  
    ## CITY$workforceCollege_2020  0.05211    0.02062   2.528   0.0224 *  
    ## CITY$direct                -0.13042    0.38319  -0.340   0.7380    
    ## CITY$wage2018               1.02562    0.01879  54.582   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6781 on 16 degrees of freedom
    ## Multiple R-squared:  0.9974, Adjusted R-squared:  0.9969 
    ## F-statistic:  2024 on 3 and 16 DF,  p-value: < 2.2e-16

``` r
mlr2 <- lm(wage2020 ~ workforceCollege_2020 + direct + hired2020 + manufecture2020 + service2020 + gender2020 + eduExpense2020 + eduLevel2020 + married2020 + expensePerCapita2020 + unemployment2020 ,data = CITY)
summary(mlr2)
bptest(mlr2)
```

    ## 
    ## Call:
    ## lm(formula = wage2020 ~ workforceCollege_2020 + direct + hired2020 + 
    ##     manufecture2020 + service2020 + gender2020 + eduExpense2020 + 
    ##     eduLevel2020 + married2020 + expensePerCapita2020 + unemployment2020, 
    ##     data = CITY)
    ## 
    ## Residuals:
    ## ALL 20 residuals are 0: no residual degrees of freedom!
    ## 
    ## Coefficients: (46 not defined because of singularities)
    ##                                   Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                        154.293        NaN     NaN      NaN
    ## workforceCollege_2020                6.289        NaN     NaN      NaN
    ## direct                               9.503        NaN     NaN      NaN
    ## hired2020                           -4.860        NaN     NaN      NaN
    ## manufecture202018.63              -174.539        NaN     NaN      NaN
    ## manufecture202021.46               -11.235        NaN     NaN      NaN
    ## manufecture202023.74               -26.847        NaN     NaN      NaN
    ## manufecture202026.33              -122.260        NaN     NaN      NaN
    ## manufecture202026.97               -33.057        NaN     NaN      NaN
    ## manufecture202028.28                -6.022        NaN     NaN      NaN
    ## manufecture202032.1                 22.511        NaN     NaN      NaN
    ## manufecture202032.200000000000003  -29.020        NaN     NaN      NaN
    ## manufecture202032.51                16.034        NaN     NaN      NaN
    ## manufecture202032.58                10.015        NaN     NaN      NaN
    ## manufecture202034.909999999999997   -6.511        NaN     NaN      NaN
    ## manufecture202036.979999999999997  -36.204        NaN     NaN      NaN
    ## manufecture202040.630000000000003  -69.217        NaN     NaN      NaN
    ## manufecture202041.88               -51.022        NaN     NaN      NaN
    ## manufecture202043.1                -19.487        NaN     NaN      NaN
    ## manufecture202043.22                    NA         NA      NA       NA
    ## manufecture202046.22                10.989        NaN     NaN      NaN
    ## manufecture202047                       NA         NA      NA       NA
    ## manufecture202047.41                    NA         NA      NA       NA
    ## service202045.37                        NA         NA      NA       NA
    ## service202045.84                        NA         NA      NA       NA
    ## service202046.91                        NA         NA      NA       NA
    ## service202049.9                         NA         NA      NA       NA
    ## service202050.15                        NA         NA      NA       NA
    ## service202051.53                        NA         NA      NA       NA
    ## service202053.97                        NA         NA      NA       NA
    ## service202055.68                        NA         NA      NA       NA
    ## service202056.82                        NA         NA      NA       NA
    ## service202057.64                        NA         NA      NA       NA
    ## service202059.52                        NA         NA      NA       NA
    ## service202061.6                         NA         NA      NA       NA
    ## service202062.36                        NA         NA      NA       NA
    ## service202067.19                        NA         NA      NA       NA
    ## service202069.34                        NA         NA      NA       NA
    ## service202071.34                        NA         NA      NA       NA
    ## service202071.42                        NA         NA      NA       NA
    ## service202071.459999999999994           NA         NA      NA       NA
    ## service202081.27                        NA         NA      NA       NA
    ## gender2020102.3                         NA         NA      NA       NA
    ## gender2020103.37                        NA         NA      NA       NA
    ## gender2020103.85                        NA         NA      NA       NA
    ## gender2020104.29                        NA         NA      NA       NA
    ## gender2020104.51                        NA         NA      NA       NA
    ## gender2020105.7                         NA         NA      NA       NA
    ## gender2020105.99                        NA         NA      NA       NA
    ## gender2020106.35                        NA         NA      NA       NA
    ## gender2020107.16                        NA         NA      NA       NA
    ## gender2020107.82                        NA         NA      NA       NA
    ## gender202090.82                         NA         NA      NA       NA
    ## gender202093.69                         NA         NA      NA       NA
    ## gender202095.43                         NA         NA      NA       NA
    ## gender202096.52                         NA         NA      NA       NA
    ## gender202097.33                         NA         NA      NA       NA
    ## gender202097.39                         NA         NA      NA       NA
    ## gender202098.23                         NA         NA      NA       NA
    ## gender202099.24                         NA         NA      NA       NA
    ## gender202099.47                         NA         NA      NA       NA
    ## eduExpense2020                          NA         NA      NA       NA
    ## eduLevel2020                            NA         NA      NA       NA
    ## married2020                             NA         NA      NA       NA
    ## expensePerCapita2020                    NA         NA      NA       NA
    ## unemployment2020                        NA         NA      NA       NA
    ## 
    ## Residual standard error: NaN on 0 degrees of freedom
    ## Multiple R-squared:      1,  Adjusted R-squared:    NaN 
    ## F-statistic:   NaN on 19 and 0 DF,  p-value: NA
    ## 
    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  mlr2
    ## BP = NaN, df = 19, p-value = NA

## F-test for educational variable

``` r
edu <- lm(wage2020 ~ workforceCollege_2020 + eduExpense2020 + eduLevel2020, data = CITY)
summary(edu)
```

    ## 
    ## Call:
    ## lm(formula = wage2020 ~ workforceCollege_2020 + eduExpense2020 + 
    ##     eduLevel2020, data = CITY)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.647  -4.011  -1.883   2.755  19.845 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)             9.4242    18.9259   0.498    0.625
    ## workforceCollege_2020   0.9457     1.2312   0.768    0.454
    ## eduExpense2020          0.6498     0.5304   1.225    0.238
    ## eduLevel2020           -0.3332     1.2592  -0.265    0.795
    ## 
    ## Residual standard error: 9.118 on 16 degrees of freedom
    ## Multiple R-squared:  0.5247, Adjusted R-squared:  0.4356 
    ## F-statistic: 5.889 on 3 and 16 DF,  p-value: 0.006598

## Use educational expense as main explanatory

``` r
eduexpense <- lm(wage2020 ~ direct + manufecture2020 + eduExpense2020 + wage2018, data = CITY)
summary(eduexpense)
```

    ## 
    ## Call:
    ## lm(formula = wage2020 ~ direct + manufecture2020 + eduExpense2020 + 
    ##     wage2018, data = CITY)
    ## 
    ## Residuals:
    ## ALL 20 residuals are 0: no residual degrees of freedom!
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                                   Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                           55.2        NaN     NaN      NaN
    ## direct                                12.5        NaN     NaN      NaN
    ## manufecture202018.63                  18.6        NaN     NaN      NaN
    ## manufecture202021.46                  -0.8        NaN     NaN      NaN
    ## manufecture202023.74                   1.9        NaN     NaN      NaN
    ## manufecture202026.33                   2.8        NaN     NaN      NaN
    ## manufecture202026.97                   1.1        NaN     NaN      NaN
    ## manufecture202028.28                   6.9        NaN     NaN      NaN
    ## manufecture202032.1                   -1.7        NaN     NaN      NaN
    ## manufecture202032.200000000000003     -4.0        NaN     NaN      NaN
    ## manufecture202032.51                   0.8        NaN     NaN      NaN
    ## manufecture202032.58                   3.2        NaN     NaN      NaN
    ## manufecture202034.909999999999997      5.9        NaN     NaN      NaN
    ## manufecture202036.979999999999997     -5.8        NaN     NaN      NaN
    ## manufecture202040.630000000000003     -8.6        NaN     NaN      NaN
    ## manufecture202041.88                  42.0        NaN     NaN      NaN
    ## manufecture202043.1                   -6.4        NaN     NaN      NaN
    ## manufecture202043.22                    NA         NA      NA       NA
    ## manufecture202046.22                   8.8        NaN     NaN      NaN
    ## manufecture202047                     -3.4        NaN     NaN      NaN
    ## manufecture202047.41                  30.6        NaN     NaN      NaN
    ## eduExpense2020                          NA         NA      NA       NA
    ## wage2018                                NA         NA      NA       NA
    ## 
    ## Residual standard error: NaN on 0 degrees of freedom
    ## Multiple R-squared:      1,  Adjusted R-squared:    NaN 
    ## F-statistic:   NaN on 19 and 0 DF,  p-value: NA

``` r
bptest(eduexpense)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  eduexpense
    ## BP = NaN, df = 19, p-value = NA

## Heteroskedaticity

### Test

We use the White Test for heteroskedaticity by specifying a formula with
interaction term and sqaure term to the BP Test.

``` r
bptest(mlr, ~ CITY$workforceCollege_2020 + CITY$workforceCollege_2020 + CITY$direct + CITY$wage2018 + I(CITY$workforceCollege_2020^2) + I(CITY$wage2018^2) + I(CITY$workforceCollege_2020*CITY$wage2018))
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  mlr
    ## BP = 9.7492, df = 6, p-value = 0.1356

Heteroskedaticity doesn’t seem present.

### Robust

## Instrumental variable: workforce age structure

``` r
IVmlr <- ivreg(CITY$wage2020 ~ CITY$workforceCollege_2020 + CITY$direct + CITY$wage2018 | CITY$workforceMiddle_2010 + CITY$workforceOld_2010)
```

    ## Warning in ivreg.fit(X, Y, Z, weights, offset, method, ...): more regressors
    ## than instruments

``` r
summary(IVmlr)
```

    ## 
    ## Call:
    ## ivreg(formula = CITY$wage2020 ~ CITY$workforceCollege_2020 + 
    ##     CITY$direct + CITY$wage2018 | CITY$workforceMiddle_2010 + 
    ##     CITY$workforceOld_2010)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -338.29 -134.86  -35.30   94.21  398.76 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                 -386.09    6269.77  -0.062    0.952
    ## CITY$workforceCollege_2020    12.56     177.78   0.071    0.945
    ## CITY$direct                 -532.10    7861.76  -0.068    0.947
    ## CITY$wage2018                    NA         NA      NA       NA
    ## 
    ## Diagnostic tests:
    ##                                               df1 df2 statistic  p-value    
    ## Weak instruments (CITY$workforceCollege_2020)   2  17    10.877 0.000908 ***
    ## Weak instruments (CITY$direct)                  2  17     1.798 0.195695    
    ## Weak instruments (CITY$wage2018)                2  17     2.583 0.104817    
    ## Wu-Hausman                                      2  14     0.771 0.481229    
    ## Sargan                                         -1  NA        NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 233.6 on 17 degrees of freedom
    ## Multiple R-Squared: -330.4,  Adjusted R-squared: -369.3 
    ## Wald test: 0.006003 on 2 and 17 DF,  p-value: 0.994

# First difference panel data

## Construct difference term

``` r
CITY <- mutate(CITY, wageDiff1 = wage2020-wage2019, 
wageDiff2 = wage2019-wage2018, 
workforceCollegeDiff1 = workforceCollege_2020 - workforceCollege_2019, 
workforceCollegeDiff2 = workforceCollege_2019 - workforceCollege_2018)
```

``` r
wageDiff <- c(CITY$wageDiff1, CITY$wageDiff2)
workforceCollegeDiff <- c(CITY$workforceCollegeDiff1, CITY$workforceCollegeDiff2) 
direct2 <- c(CITY$direct, CITY$direct)
```

``` r
plm <- lm(wageDiff ~ workforceCollegeDiff + direct2)
summary(plm)
```

    ## 
    ## Call:
    ## lm(formula = wageDiff ~ workforceCollegeDiff + direct2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8534 -0.6148 -0.1205  0.4865  1.5287 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           0.73944    0.15385   4.806 2.56e-05 ***
    ## workforceCollegeDiff  0.06563    0.09312   0.705    0.485    
    ## direct2               0.27535    0.24464   1.126    0.268    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7085 on 37 degrees of freedom
    ## Multiple R-squared:  0.0471, Adjusted R-squared:  -0.004406 
    ## F-statistic: 0.9145 on 2 and 37 DF,  p-value: 0.4096
