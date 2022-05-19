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

    ## Warning in data.frame(..., check.names = FALSE): NAs introduced by coercion
    
    ## Warning in data.frame(..., check.names = FALSE): NAs introduced by coercion
    
    ## Warning in data.frame(..., check.names = FALSE): NAs introduced by coercion
    
    ## Warning in data.frame(..., check.names = FALSE): NAs introduced by coercion
    
    ## Warning in data.frame(..., check.names = FALSE): NAs introduced by coercion
    
    ## Warning in data.frame(..., check.names = FALSE): NAs introduced by coercion
    
    ## Warning in data.frame(..., check.names = FALSE): NAs introduced by coercion
    
    ## Warning in data.frame(..., check.names = FALSE): NAs introduced by coercion
    
    ## Warning in data.frame(..., check.names = FALSE): NAs introduced by coercion

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
CITY <- mutate(CITY, lwage2020 = log(wage2020), lwage2019 = log(wage2019), lwage2018 = log(wage2018), directEdu = direct * workforceCollege_2020)
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
mlr <- lm(CITY$wage2020 ~ CITY$workforceCollege_2020 + CITY$direct + CITY$wage2018 + CITY$service2020 + CITY$hired2020, )
summary(mlr)
#stargazer(mlr)
```

    ## 
    ## Call:
    ## lm(formula = CITY$wage2020 ~ CITY$workforceCollege_2020 + CITY$direct + 
    ##     CITY$wage2018 + CITY$service2020 + CITY$hired2020)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.95032 -0.37654 -0.03503  0.32385  1.46467 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -0.53646    2.17863  -0.246   0.8091    
    ## CITY$workforceCollege_2020  0.08334    0.03240   2.573   0.0221 *  
    ## CITY$direct                -0.23074    0.41672  -0.554   0.5885    
    ## CITY$wage2018               1.01220    0.02260  44.795   <2e-16 ***
    ## CITY$service2020           -0.02740    0.02215  -1.237   0.2366    
    ## CITY$hired2020             -0.01084    0.03285  -0.330   0.7462    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6857 on 14 degrees of freedom
    ## Multiple R-squared:  0.9976, Adjusted R-squared:  0.9968 
    ## F-statistic:  1188 on 5 and 14 DF,  p-value: < 2.2e-16

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

## Use educational level as main explanatory

``` r
edulevel <- lm(wage2020 ~ direct + service2020 + eduLevel2020 + hired2020 + wage2018, data = CITY)
summary(edulevel)
bptest(edulevel)
edulevelrob <- lmrob(wage2020 ~ direct + service2020 + eduLevel2020 + hired2020 + wage2018, data = CITY)
summary(edulevelrob)
```

    ## 
    ## Call:
    ## lm(formula = wage2020 ~ direct + service2020 + eduLevel2020 + 
    ##     hired2020 + wage2018, data = CITY)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.97335 -0.35073 -0.01329  0.30765  1.57278 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -1.441606   1.973906  -0.730   0.4772    
    ## direct       -0.357050   0.427986  -0.834   0.4181    
    ## service2020  -0.023037   0.020429  -1.128   0.2784    
    ## eduLevel2020  0.076332   0.028279   2.699   0.0173 *  
    ## hired2020     0.008742   0.030650   0.285   0.7797    
    ## wage2018      1.011406   0.022118  45.727   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6749 on 14 degrees of freedom
    ## Multiple R-squared:  0.9977, Adjusted R-squared:  0.9969 
    ## F-statistic:  1226 on 5 and 14 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  edulevel
    ## BP = 11.471, df = 5, p-value = 0.0428
    ## 
    ## 
    ## Call:
    ## lmrob(formula = wage2020 ~ direct + service2020 + eduLevel2020 + hired2020 + 
    ##     wage2018, data = CITY)
    ##  \--> method = "MM"
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.76800 -0.25666  0.06608  0.30935  2.38043 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.148985   1.668662   0.089 0.930121    
    ## direct       -0.127239   0.161105  -0.790 0.442821    
    ## service2020  -0.012551   0.016709  -0.751 0.465019    
    ## eduLevel2020  0.085596   0.016746   5.111 0.000158 ***
    ## hired2020    -0.010205   0.021381  -0.477 0.640515    
    ## wage2018      0.989776   0.007188 137.693  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Robust residual standard error: 0.4012 
    ## Multiple R-squared:  0.9988, Adjusted R-squared:  0.9984 
    ## Convergence in 17 IRWLS iterations
    ## 
    ## Robustness weights: 
    ##  observation 8 is an outlier with |weight| = 0 ( < 0.005); 
    ##  The remaining 19 ones are summarized as
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.6939  0.9187  0.9612  0.9255  0.9823  0.9986 
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

## Heteroskedaticity

### Test

We use the White Test for heteroskedaticity by specifying a formula with
interaction term and sqaure term to the BP Test.

``` r
bptest(mlr)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  mlr
    ## BP = 10.357, df = 5, p-value = 0.06573

Heteroskedaticity doesn’t seem present.

### Robust

``` r
mlrrob <- lmrob(CITY$wage2020 ~ CITY$workforceCollege_2020 + CITY$direct + CITY$wage2018 + CITY$service2020 + CITY$hired2020, )
summary(mlrrob)
#stargazer(mlr)
```

    ## 
    ## Call:
    ## lmrob(formula = CITY$wage2020 ~ CITY$workforceCollege_2020 + CITY$direct + 
    ##     CITY$wage2018 + CITY$service2020 + CITY$hired2020)
    ##  \--> method = "MM"
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.04982 -0.22670  0.04797  0.35895  2.17301 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 0.721935   1.293503   0.558    0.586    
    ## CITY$workforceCollege_2020  0.086164   0.015425   5.586 6.71e-05 ***
    ## CITY$direct                -0.004249   0.103995  -0.041    0.968    
    ## CITY$wage2018               0.994973   0.014650  67.914  < 2e-16 ***
    ## CITY$service2020           -0.014448   0.023269  -0.621    0.545    
    ## CITY$hired2020             -0.027160   0.019075  -1.424    0.176    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Robust residual standard error: 0.5018 
    ## Multiple R-squared:  0.9984, Adjusted R-squared:  0.9979 
    ## Convergence in 14 IRWLS iterations
    ## 
    ## Robustness weights: 
    ##  one weight is ~= 1. The remaining 19 ones are summarized as
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.02122 0.90050 0.95820 0.88710 0.98510 0.99740 
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
IVmlr <- ivreg(CITY$wage2020 ~ CITY$direct + CITY$wage2018 + CITY$service2020 + CITY$hired2020 | CITY$workforceCollege_2020 | CITY$workforceYoung_2010)
summary(IVmlr)
```

    ## 
    ## Call:
    ## ivreg(formula = CITY$wage2020 ~ CITY$direct + CITY$wage2018 + 
    ##     CITY$service2020 + CITY$hired2020 | CITY$workforceCollege_2020 | 
    ##     CITY$workforceYoung_2010)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.9330 -0.3077 -0.1831  0.4268  1.4951 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 0.37486    5.96879   0.063    0.951    
    ## CITY$workforceCollege_2020  0.10904    0.15964   0.683    0.506    
    ## CITY$direct                -0.35810    0.88342  -0.405    0.691    
    ## CITY$wage2018               1.00010    0.07708  12.976 3.41e-09 ***
    ## CITY$service2020           -0.03945    0.07669  -0.514    0.615    
    ## CITY$hired2020             -0.01953    0.06254  -0.312    0.759    
    ## 
    ## Diagnostic tests:
    ##                  df1 df2 statistic p-value
    ## Weak instruments   1  14     0.630   0.441
    ## Wu-Hausman         1  13     0.026   0.874
    ## Sargan             0  NA        NA      NA
    ## 
    ## Residual standard error: 0.701 on 14 degrees of freedom
    ## Multiple R-Squared: 0.9975,  Adjusted R-squared: 0.9967 
    ## Wald test:  1135 on 5 and 14 DF,  p-value: < 2.2e-16

# First difference panel data

## Construct difference term

``` r
CITY <- mutate(CITY, wageDiff1 = wage2020-wage2019, 
wageDiff2 = wage2019-wage2018, 
workforceCollegeDiff1 = workforceCollege_2020 - workforceCollege_2019, 
workforceCollegeDiff2 = workforceCollege_2019 - workforceCollege_2018,
serviceDiff1 = service2020 - service2019,
serviceDiff2 = service2019 - service2018,
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

![](cityWage_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

## College worker share

``` r
plm <- lm(wageDiff ~ workforceCollegeDiff + direct2 + serviceDiff + hiredDiff)
summary(plm)
plmrob <- lmrob(wageDiff ~ workforceCollegeDiff + direct2 + serviceDiff + hiredDiff)
summary(plmrob)
```

    ## 
    ## Call:
    ## lm(formula = wageDiff ~ workforceCollegeDiff + direct2 + serviceDiff + 
    ##     hiredDiff)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.15682 -0.42429  0.02814  0.32972  1.54687 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            0.7215     0.1413   5.106 1.17e-05 ***
    ## workforceCollegeDiff  -0.1364     0.1116  -1.222  0.22997    
    ## direct2                0.3900     0.2279   1.711  0.09590 .  
    ## serviceDiff            0.3463     0.1174   2.951  0.00563 ** 
    ## hiredDiff             -0.1111     0.1036  -1.072  0.29101    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6501 on 35 degrees of freedom
    ## Multiple R-squared:  0.2411, Adjusted R-squared:  0.1544 
    ## F-statistic:  2.78 on 4 and 35 DF,  p-value: 0.04178
    ## 
    ## 
    ## Call:
    ## lmrob(formula = wageDiff ~ workforceCollegeDiff + direct2 + serviceDiff + 
    ##     hiredDiff)
    ##  \--> method = "MM"
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.14015 -0.40624  0.05274  0.33145  1.76221 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           0.66958    0.13661   4.901 2.16e-05 ***
    ## workforceCollegeDiff -0.10564    0.08445  -1.251 0.219262    
    ## direct2               0.32696    0.27090   1.207 0.235537    
    ## serviceDiff           0.37005    0.10286   3.598 0.000982 ***
    ## hiredDiff            -0.10607    0.10568  -1.004 0.322405    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Robust residual standard error: 0.5917 
    ## Multiple R-squared:  0.2813, Adjusted R-squared:  0.1992 
    ## Convergence in 17 IRWLS iterations
    ## 
    ## Robustness weights: 
    ##  2 weights are ~= 1. The remaining 38 ones are summarized as
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.3551  0.9188  0.9623  0.9061  0.9781  0.9989 
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
plmiv <- ivreg(wageDiff ~ direct2 + serviceDiff + hiredDiff | workforceCollegeDiff | workforceYoungDiff)
summary(plmiv)
```

    ## 
    ## Call:
    ## ivreg(formula = wageDiff ~ direct2 + serviceDiff + hiredDiff | 
    ##     workforceCollegeDiff | workforceYoungDiff)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.26771 -0.48369 -0.09904  0.41326  2.07309 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)           0.54693    0.56200   0.973    0.337
    ## workforceCollegeDiff  0.26081    1.22897   0.212    0.833
    ## direct2               0.27556    0.44141   0.624    0.537
    ## serviceDiff           0.08202    0.82449   0.099    0.921
    ## hiredDiff            -0.10651    0.12172  -0.875    0.388
    ## 
    ## Diagnostic tests:
    ##                  df1 df2 statistic p-value
    ## Weak instruments   1  35     0.398   0.532
    ## Wu-Hausman         1  34     0.140   0.710
    ## Sargan             0  NA        NA      NA
    ## 
    ## Residual standard error: 0.7586 on 35 degrees of freedom
    ## Multiple R-Squared: -0.03347,    Adjusted R-squared: -0.1516 
    ## Wald test: 1.778 on 4 and 35 DF,  p-value: 0.1552

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
