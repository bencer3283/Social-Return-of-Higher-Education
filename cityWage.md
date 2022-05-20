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
library(robustbase)
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
        CITY <- cbind(CITY, as.numeric(unlist(rawCityAddition[1:20, i])))
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

``` r
nacol <- vector()
for(j in 1:ncol(CITY)) {
    if(is.na(CITY[2, j])) {
        nacol <- c(nacol, j)
    }
}
CITY <- CITY[-nacol]
```

``` r
CITY <- mutate(CITY, lwage2020 = log(wage2020), lwage2019 = log(wage2019), lwage2018 = log(wage2018), directEdu = direct * workforceCollege_2020, graduateShare2020 = graduate2020/workforcePopulation2020)
```

# SLR with college graduates number

``` r
slr <- lm(CITY$wage2020 ~ CITY$graduate2020, )
summary(slr)
#stargazer(slr)
slr1 <- lm(CITY$wage2020 ~ CITY$graduateShare2020)
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

![](cityWage_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

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

![](cityWage_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Heteroskedaticity

### Test

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
mlr2 <- lm(wage2020 ~ workforceCollege_2020 + direct + hired2020 + manufecture2020 + service2020 + gender2020 + eduExpense2020 + eduLevel2020 + married2020 + expensePerCapita2020 + unemployment2020 + directEdu ,data = CITY)
summary(mlr2)
bptest(mlr2)
```

    ## 
    ## Call:
    ## lm(formula = wage2020 ~ workforceCollege_2020 + direct + hired2020 + 
    ##     manufecture2020 + service2020 + gender2020 + eduExpense2020 + 
    ##     eduLevel2020 + married2020 + expensePerCapita2020 + unemployment2020 + 
    ##     directEdu, data = CITY)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.1361 -1.6886  0.2735  1.4482  4.1449 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)           -2.686e+02  1.713e+02  -1.568  0.16079   
    ## workforceCollege_2020 -8.180e-01  1.814e+00  -0.451  0.66571   
    ## direct                 8.130e+01  2.344e+01   3.468  0.01043 * 
    ## hired2020              1.486e+00  6.083e-01   2.442  0.04463 * 
    ## manufecture2020       -2.045e+00  8.503e-01  -2.405  0.04712 * 
    ## service2020           -1.049e+00  9.145e-01  -1.147  0.28896   
    ## gender2020             2.499e+00  9.936e-01   2.515  0.04010 * 
    ## eduExpense2020         1.162e+00  5.716e-01   2.034  0.08148 . 
    ## eduLevel2020           1.866e+00  1.746e+00   1.069  0.32058   
    ## married2020            1.504e+00  1.141e+00   1.318  0.22909   
    ## expensePerCapita2020   2.899e-03  6.514e-04   4.450  0.00297 **
    ## unemployment2020      -3.147e+01  2.121e+01  -1.483  0.18155   
    ## directEdu             -1.531e+00  4.155e-01  -3.686  0.00780 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.055 on 7 degrees of freedom
    ## Multiple R-squared:  0.9589, Adjusted R-squared:  0.8884 
    ## F-statistic:  13.6 on 12 and 7 DF,  p-value: 0.001024
    ## 
    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  mlr2
    ## BP = 10.651, df = 12, p-value = 0.559

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

## Use college worker Share as main explanatory

``` r
mlr <- lm(CITY$wage2020 ~ CITY$workforceCollege_2020 + CITY$direct + CITY$wage2018 + CITY$manufecture2020 + CITY$hired2020, )
summary(mlr)
#stargazer(mlr)
```

    ## 
    ## Call:
    ## lm(formula = CITY$wage2020 ~ CITY$workforceCollege_2020 + CITY$direct + 
    ##     CITY$wage2018 + CITY$manufecture2020 + CITY$hired2020)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.99226 -0.36933 -0.01989  0.35062  1.45209 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -1.25935    2.00540  -0.628   0.5401    
    ## CITY$workforceCollege_2020  0.07512    0.02882   2.606   0.0207 *  
    ## CITY$direct                -0.19412    0.41273  -0.470   0.6454    
    ## CITY$wage2018               1.01685    0.02115  48.086   <2e-16 ***
    ## CITY$manufecture2020        0.02534    0.02161   1.173   0.2605    
    ## CITY$hired2020             -0.03220    0.03756  -0.857   0.4057    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6892 on 14 degrees of freedom
    ## Multiple R-squared:  0.9976, Adjusted R-squared:  0.9968 
    ## F-statistic:  1176 on 5 and 14 DF,  p-value: < 2.2e-16

### Heteroskedaticity

#### Test

``` r
bptest(mlr)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  mlr
    ## BP = 10.854, df = 5, p-value = 0.05435

#### Robust

``` r
mlrrob <- lmrob(CITY$wage2020 ~ CITY$workforceCollege_2020 + CITY$direct + CITY$wage2018 + CITY$manufecture2020 + CITY$hired2020, )
summary(mlrrob)
#stargazer(mlr)
```

    ## 
    ## Call:
    ## lmrob(formula = CITY$wage2020 ~ CITY$workforceCollege_2020 + CITY$direct + 
    ##     CITY$wage2018 + CITY$manufecture2020 + CITY$hired2020)
    ##  \--> method = "MM"
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.11185 -0.22967  0.01904  0.32530  2.21284 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 0.290339   1.612505   0.180    0.860    
    ## CITY$workforceCollege_2020  0.079674   0.013457   5.920 3.73e-05 ***
    ## CITY$direct                 0.020426   0.239663   0.085    0.933    
    ## CITY$wage2018               0.997127   0.010547  94.539  < 2e-16 ***
    ## CITY$manufecture2020        0.009307   0.022067   0.422    0.680    
    ## CITY$hired2020             -0.034386   0.042429  -0.810    0.431    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Robust residual standard error: 0.4739 
    ## Multiple R-squared:  0.9985, Adjusted R-squared:  0.998 
    ## Convergence in 16 IRWLS iterations
    ## 
    ## Robustness weights: 
    ##  observation 8 is an outlier with |weight| <= 4.3e-05 ( < 0.005); 
    ##  2 weights are ~= 1. The remaining 17 ones are summarized as
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.5613  0.9289  0.9621  0.9215  0.9790  0.9951 
    ## Algorithmic parameters: 
    ##        tuning.chi                bb        tuning.psi        refine.tol 
    ##         1.548e+00         5.000e-01         4.685e+00         1.000e-07 
    ##           rel.tol         scale.tol         solve.tol       eps.outlier 
    ##         1.000e-07         1.000e-10         1.000e-07         5.000e-03 
    ##             eps.x warn.limit.reject warn.limit.meanrw 
    ##         1.723e-10         5.000e-01         5.000e-01 
    ##      nResample         max.it       best.r.s       k.fast.s          k.max 
    ##            500             50              2              1            200 
    ##    maxit.scale      trace.lev            mts     compute.rd fast.s.large.n 
    ##            200              0           1000              0           2000 
    ##                   psi           subsampling                   cov 
    ##            "bisquare"         "nonsingular"         ".vcov.avar1" 
    ## compute.outlier.stats 
    ##                  "SM" 
    ## seed : int(0)

## Use educational level as main explanatory

``` r
edulevel <- lm(wage2020 ~ direct + manufecture2020 + eduLevel2020 + hired2020 + wage2018, data = CITY)
summary(edulevel)
bptest(edulevel)
edulevelrob <- lmrob(wage2020 ~ direct + manufecture2020 + eduLevel2020 + hired2020 + wage2018, data = CITY)
summary(edulevelrob)
```

    ## 
    ## Call:
    ## lm(formula = wage2020 ~ direct + manufecture2020 + eduLevel2020 + 
    ##     hired2020 + wage2018, data = CITY)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.00812 -0.34616 -0.02858  0.28334  1.55342 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -1.96671    1.87630  -1.048   0.3123    
    ## direct          -0.32164    0.42148  -0.763   0.4581    
    ## manufecture2020  0.02228    0.02038   1.093   0.2927    
    ## eduLevel2020     0.07080    0.02573   2.752   0.0156 *  
    ## hired2020       -0.01174    0.03253  -0.361   0.7235    
    ## wage2018         1.01497    0.02091  48.551   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6766 on 14 degrees of freedom
    ## Multiple R-squared:  0.9977, Adjusted R-squared:  0.9969 
    ## F-statistic:  1220 on 5 and 14 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  edulevel
    ## BP = 11.97, df = 5, p-value = 0.0352
    ## 
    ## 
    ## Call:
    ## lmrob(formula = wage2020 ~ direct + manufecture2020 + eduLevel2020 + hired2020 + 
    ##     wage2018, data = CITY)
    ##  \--> method = "MM"
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.84669 -0.26122  0.05761  0.30488  2.38793 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -0.247532   1.673361  -0.148 0.884511    
    ## direct          -0.106571   0.212691  -0.501 0.624115    
    ## manufecture2020  0.008061   0.020700   0.389 0.702827    
    ## eduLevel2020     0.079934   0.015476   5.165 0.000143 ***
    ## hired2020       -0.017022   0.040925  -0.416 0.683767    
    ## wage2018         0.992269   0.009169 108.223  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Robust residual standard error: 0.3719 
    ## Multiple R-squared:  0.9988, Adjusted R-squared:  0.9984 
    ## Convergence in 19 IRWLS iterations
    ## 
    ## Robustness weights: 
    ##  observation 8 is an outlier with |weight| = 0 ( < 0.005); 
    ##  The remaining 19 ones are summarized as
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.5835  0.9200  0.9542  0.9114  0.9736  0.9979 
    ## Algorithmic parameters: 
    ##        tuning.chi                bb        tuning.psi        refine.tol 
    ##         1.548e+00         5.000e-01         4.685e+00         1.000e-07 
    ##           rel.tol         scale.tol         solve.tol       eps.outlier 
    ##         1.000e-07         1.000e-10         1.000e-07         5.000e-03 
    ##             eps.x warn.limit.reject warn.limit.meanrw 
    ##         1.723e-10         5.000e-01         5.000e-01 
    ##      nResample         max.it       best.r.s       k.fast.s          k.max 
    ##            500             50              2              1            200 
    ##    maxit.scale      trace.lev            mts     compute.rd fast.s.large.n 
    ##            200              0           1000              0           2000 
    ##                   psi           subsampling                   cov 
    ##            "bisquare"         "nonsingular"         ".vcov.avar1" 
    ## compute.outlier.stats 
    ##                  "SM" 
    ## seed : int(0)

## Instrumental variable: workforce age structure

``` r
IVmlr <- ivreg(CITY$wage2020 ~ CITY$direct + CITY$wage2018 + CITY$manufecture2020 + CITY$hired2020 | CITY$workforceCollege_2020 | CITY$workforceYoung_2010)
summary(IVmlr)
```

    ## 
    ## Call:
    ## ivreg(formula = CITY$wage2020 ~ CITY$direct + CITY$wage2018 + 
    ##     CITY$manufecture2020 + CITY$hired2020 | CITY$workforceCollege_2020 | 
    ##     CITY$workforceYoung_2010)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.9873 -0.3615 -0.1494  0.3855  1.4956 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -0.64294    3.36822  -0.191    0.851    
    ## CITY$workforceCollege_2020  0.09783    0.10280   0.952    0.357    
    ## CITY$direct                -0.30822    0.65018  -0.474    0.643    
    ## CITY$wage2018               1.00663    0.04929  20.425 8.09e-12 ***
    ## CITY$manufecture2020        0.03505    0.04752   0.737    0.473    
    ## CITY$hired2020             -0.05021    0.08701  -0.577    0.573    
    ## 
    ## Diagnostic tests:
    ##                  df1 df2 statistic p-value
    ## Weak instruments   1  14     1.252   0.282
    ## Wu-Hausman         1  13     0.052   0.824
    ## Sargan             0  NA        NA      NA
    ## 
    ## Residual standard error: 0.7043 on 14 degrees of freedom
    ## Multiple R-Squared: 0.9975,  Adjusted R-squared: 0.9966 
    ## Wald test:  1125 on 5 and 14 DF,  p-value: < 2.2e-16

``` r
IVmlredulevel <- ivreg(CITY$wage2020 ~ CITY$direct + CITY$wage2018 + CITY$manufecture2020 + CITY$hired2020 | CITY$eduLevel2020 | CITY$workforceYoung_2010)
summary(IVmlredulevel)
```

    ## 
    ## Call:
    ## ivreg(formula = CITY$wage2020 ~ CITY$direct + CITY$wage2018 + 
    ##     CITY$manufecture2020 + CITY$hired2020 | CITY$eduLevel2020 | 
    ##     CITY$workforceYoung_2010)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.0080 -0.3487 -0.1733  0.3510  1.6426 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -1.48285    2.66812  -0.556    0.587    
    ## CITY$eduLevel2020     0.09654    0.10087   0.957    0.355    
    ## CITY$direct          -0.50513    0.81957  -0.616    0.548    
    ## CITY$wage2018         1.00201    0.05357  18.704 2.66e-11 ***
    ## CITY$manufecture2020  0.03283    0.04513   0.728    0.479    
    ## CITY$hired2020       -0.02595    0.06341  -0.409    0.689    
    ## 
    ## Diagnostic tests:
    ##                  df1 df2 statistic p-value
    ## Weak instruments   1  14     1.049   0.323
    ## Wu-Hausman         1  13     0.070   0.796
    ## Sargan             0  NA        NA      NA
    ## 
    ## Residual standard error: 0.7003 on 14 degrees of freedom
    ## Multiple R-Squared: 0.9975,  Adjusted R-squared: 0.9967 
    ## Wald test:  1138 on 5 and 14 DF,  p-value: < 2.2e-16

# First difference panel data

## Construct difference term

``` r
CITY <- mutate(CITY, wageDiff1 = wage2020-wage2019, 
wageDiff2 = wage2019-wage2018, 
workforceCollegeDiff1 = workforceCollege_2020 - workforceCollege_2019, 
workforceCollegeDiff2 = workforceCollege_2019 - workforceCollege_2018,
serviceDiff1 = service2020 - service2019,
serviceDiff2 = service2019 - service2018,
manufecDiff1 = manufecture2020 - manufecture2019,
manufecDiff2 = manufecture2019 - manufecture2018,
hiredDiff1 = hired2020 - hired2019,
hiredDiff2 = hired2019 - hired2018,
lwageDiff1 = lwage2020 - lwage2019,
lwageDiff2 = lwage2019 - lwage2018,
workforceYoungDiff1 = workforceYoung_2015 - workforceYoung_2014,
workforceYoungDiff2 = workforceYoung_2014 - workforceYoung_2013,
eduLevelDiff1 = eduLevel2020 - eduLevel2019,
eduLevelDiff2 = eduLevel2019 - eduLevel2018,
)
```

``` r
wageDiff <- c(CITY$wageDiff1, CITY$wageDiff2)
workforceCollegeDiff <- c(CITY$workforceCollegeDiff1, CITY$workforceCollegeDiff2) 
direct2 <- c(CITY$direct, CITY$direct)
serviceDiff <- c(CITY$serviceDiff1, CITY$workforceCollegeDiff2)
manufectDiff <- c(CITY$manufecDiff1, CITY$manufecDiff2)
hiredDiff <- c(CITY$hiredDiff1, CITY$hiredDiff2)
lwageDiff <- c(CITY$lwageDiff1, CITY$lwageDiff2)
workforceYoungDiff <- c(CITY$workforceYoungDiff1, CITY$workforceYoungDiff2)
one <- seq(1, 1, length.out = nrow(CITY))
zero <- seq(0, 0, length.out = nrow(CITY))
dummy1 <- c(one, zero)
dummy2 <- c(zero, one)
eduLevelDiff <- c(CITY$eduLevelDiff1, CITY$eduLevelDiff2)

plot(workforceCollegeDiff, wageDiff)
```

![](cityWage_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

## College worker share

``` r
plm <- lm(wageDiff ~ workforceCollegeDiff + direct2 + serviceDiff + manufectDiff + hiredDiff)
summary(plm)
plmrob <- lmrob(wageDiff ~ workforceCollegeDiff + direct2 + serviceDiff + manufectDiff + hiredDiff)
summary(plmrob)
```

    ## 
    ## Call:
    ## lm(formula = wageDiff ~ workforceCollegeDiff + direct2 + serviceDiff + 
    ##     manufectDiff + hiredDiff)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.2197 -0.4581  0.0338  0.3110  1.4876 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           0.71614    0.14301   5.007 1.68e-05 ***
    ## workforceCollegeDiff -0.13578    0.11272  -1.205   0.2367    
    ## direct2               0.39699    0.23049   1.722   0.0941 .  
    ## serviceDiff           0.31348    0.13169   2.380   0.0230 *  
    ## manufectDiff         -0.06977    0.12225  -0.571   0.5719    
    ## hiredDiff            -0.07263    0.12443  -0.584   0.5632    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6564 on 34 degrees of freedom
    ## Multiple R-squared:  0.2483, Adjusted R-squared:  0.1377 
    ## F-statistic: 2.246 on 5 and 34 DF,  p-value: 0.07205
    ## 
    ## 
    ## Call:
    ## lmrob(formula = wageDiff ~ workforceCollegeDiff + direct2 + serviceDiff + 
    ##     manufectDiff + hiredDiff)
    ##  \--> method = "MM"
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.17649 -0.39646  0.05387  0.33063  1.71991 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           0.67102    0.14124   4.751  3.6e-05 ***
    ## workforceCollegeDiff -0.10854    0.08611  -1.260  0.21608    
    ## direct2               0.34046    0.28321   1.202  0.23762    
    ## serviceDiff           0.35821    0.10309   3.475  0.00142 ** 
    ## manufectDiff         -0.02771    0.13276  -0.209  0.83589    
    ## hiredDiff            -0.09186    0.12006  -0.765  0.44951    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Robust residual standard error: 0.6003 
    ## Multiple R-squared:  0.2798, Adjusted R-squared:  0.1739 
    ## Convergence in 17 IRWLS iterations
    ## 
    ## Robustness weights: 
    ##  3 weights are ~= 1. The remaining 37 ones are summarized as
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.3918  0.9128  0.9615  0.9065  0.9772  0.9988 
    ## Algorithmic parameters: 
    ##        tuning.chi                bb        tuning.psi        refine.tol 
    ##         1.548e+00         5.000e-01         4.685e+00         1.000e-07 
    ##           rel.tol         scale.tol         solve.tol       eps.outlier 
    ##         1.000e-07         1.000e-10         1.000e-07         2.500e-03 
    ##             eps.x warn.limit.reject warn.limit.meanrw 
    ##         5.930e-12         5.000e-01         5.000e-01 
    ##      nResample         max.it       best.r.s       k.fast.s          k.max 
    ##            500             50              2              1            200 
    ##    maxit.scale      trace.lev            mts     compute.rd fast.s.large.n 
    ##            200              0           1000              0           2000 
    ##                   psi           subsampling                   cov 
    ##            "bisquare"         "nonsingular"         ".vcov.avar1" 
    ## compute.outlier.stats 
    ##                  "SM" 
    ## seed : int(0)

``` r
plmiv <- ivreg(wageDiff ~ direct2 + serviceDiff + manufectDiff + hiredDiff | workforceCollegeDiff | workforceYoungDiff)
summary(plmiv)
```

    ## 
    ## Call:
    ## ivreg(formula = wageDiff ~ direct2 + serviceDiff + manufectDiff + 
    ##     hiredDiff | workforceCollegeDiff | workforceYoungDiff)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.1672 -0.4544 -0.1166  0.4605  1.9601 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)           0.55807    0.55155   1.012    0.319
    ## workforceCollegeDiff  0.22318    1.20342   0.185    0.854
    ## direct2               0.29388    0.43255   0.679    0.501
    ## serviceDiff           0.07300    0.81555   0.090    0.929
    ## manufectDiff         -0.07336    0.13980  -0.525    0.603
    ## hiredDiff            -0.06653    0.14323  -0.464    0.645
    ## 
    ## Diagnostic tests:
    ##                  df1 df2 statistic p-value
    ## Weak instruments   1  34     0.392   0.536
    ## Wu-Hausman         1  33     0.114   0.738
    ## Sargan             0  NA        NA      NA
    ## 
    ## Residual standard error: 0.748 on 34 degrees of freedom
    ## Multiple R-Squared: 0.02407, Adjusted R-squared: -0.1195 
    ## Wald test: 1.513 on 5 and 34 DF,  p-value: 0.2116

## City educational level

``` r
plmedulevel <- lm(wageDiff ~ eduLevelDiff + direct2 + serviceDiff + hiredDiff)
summary(plmedulevel)
plmedulevelrob <- lmrob(wageDiff ~ eduLevelDiff + direct2 + serviceDiff + hiredDiff)
summary(plmedulevelrob)
```

    ## 
    ## Call:
    ## lm(formula = wageDiff ~ eduLevelDiff + direct2 + serviceDiff + 
    ##     hiredDiff)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.14653 -0.41960  0.00757  0.36959  1.67707 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.67430    0.14021   4.809 2.85e-05 ***
    ## eduLevelDiff -0.03552    0.10490  -0.339   0.7369    
    ## direct2       0.36912    0.23637   1.562   0.1274    
    ## serviceDiff   0.28316    0.12344   2.294   0.0279 *  
    ## hiredDiff    -0.11772    0.10836  -1.086   0.2847    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6627 on 35 degrees of freedom
    ## Multiple R-squared:  0.2113, Adjusted R-squared:  0.1212 
    ## F-statistic: 2.344 on 4 and 35 DF,  p-value: 0.07376
    ## 
    ## 
    ## Call:
    ## lmrob(formula = wageDiff ~ eduLevelDiff + direct2 + serviceDiff + hiredDiff)
    ##  \--> method = "MM"
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.1220 -0.3636  0.0472  0.3489  1.9118 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.62161    0.13059   4.760  3.3e-05 ***
    ## eduLevelDiff -0.01371    0.10768  -0.127  0.89940    
    ## direct2       0.29956    0.28474   1.052  0.29999    
    ## serviceDiff   0.31719    0.11212   2.829  0.00768 ** 
    ## hiredDiff    -0.10105    0.12124  -0.833  0.41025    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Robust residual standard error: 0.6151 
    ## Multiple R-squared:  0.2644, Adjusted R-squared:  0.1803 
    ## Convergence in 16 IRWLS iterations
    ## 
    ## Robustness weights: 
    ##  one weight is ~= 1. The remaining 39 ones are summarized as
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.3135  0.9225  0.9681  0.9121  0.9868  0.9983 
    ## Algorithmic parameters: 
    ##        tuning.chi                bb        tuning.psi        refine.tol 
    ##         1.548e+00         5.000e-01         4.685e+00         1.000e-07 
    ##           rel.tol         scale.tol         solve.tol       eps.outlier 
    ##         1.000e-07         1.000e-10         1.000e-07         2.500e-03 
    ##             eps.x warn.limit.reject warn.limit.meanrw 
    ##         6.694e-12         5.000e-01         5.000e-01 
    ##      nResample         max.it       best.r.s       k.fast.s          k.max 
    ##            500             50              2              1            200 
    ##    maxit.scale      trace.lev            mts     compute.rd fast.s.large.n 
    ##            200              0           1000              0           2000 
    ##                   psi           subsampling                   cov 
    ##            "bisquare"         "nonsingular"         ".vcov.avar1" 
    ## compute.outlier.stats 
    ##                  "SM" 
    ## seed : int(0)

``` r
plmivedulevel <- ivreg(wageDiff ~ direct2 + serviceDiff + hiredDiff | eduLevelDiff | workforceYoungDiff)
summary(plmivedulevel)
```

    ## 
    ## Call:
    ## ivreg(formula = wageDiff ~ direct2 + serviceDiff + hiredDiff | 
    ##     eduLevelDiff | workforceYoungDiff)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.52820 -0.43047 -0.01089  0.37966  2.14443 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)   0.55665    0.51228   1.087    0.285
    ## eduLevelDiff  0.29341    1.36692   0.215    0.831
    ## direct2       0.19888    0.75386   0.264    0.793
    ## serviceDiff   0.02738    1.06810   0.026    0.980
    ## hiredDiff    -0.04172    0.33769  -0.124    0.902
    ## 
    ## Diagnostic tests:
    ##                  df1 df2 statistic p-value
    ## Weak instruments   1  35     0.266   0.609
    ## Wu-Hausman         1  34     0.073   0.789
    ## Sargan             0  NA        NA      NA
    ## 
    ## Residual standard error: 0.75 on 35 degrees of freedom
    ## Multiple R-Squared: -0.01025,    Adjusted R-squared: -0.1257 
    ## Wald test: 1.819 on 4 and 35 DF,  p-value: 0.1471
