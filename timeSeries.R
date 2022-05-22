library(knitr)
library(readxl) #for reading Excel file
library(stargazer) #for Latex table generation
library(lmtest) #for heteroskedaticity test
library(data.table) #for transpose() function
library(ivreg) #for instrumental variable 2SLS regression
library(dplyr)
#library(HAC)
library(sandwich)
library(robustbase)

unem = c(2.14,2.71,2.45,2.91,2.66,1.97,1.69,1.57,1.67,1.51,1.51,1.45,1.56,1.79,
         2.6,2.72,2.69,2.92,2.99,4.57,5.17,4.99,4.44,4.13,3.91,3.91,4.14,5.85,
         5.21,4.39,4.24,4.18,3.96,3.78,3.92,3.76,3.71,3.73,3.85);
labpart = c(57.93,59.26,59.72,59.49,60.37,60.93,60.21,60.12,59.24,59.11,59.34,
            58.82,58.96,58.71,58.44,58.33,58.04,57.93,57.68,57.23,57.34,57.34,
            57.66,57.78,57.92,58.25,58.28,57.9,58.07,58.17,58.35,58.43,58.54,
            58.65,58.75,58.83,58.99,59.17,59.14); 
indpd = c(3023733,3315458,3802347,3875949,4419496,4850035,5131735,5419052,
          5595225,6163572,6539264,7042461,7651311,8652814,8873886,9536464,
          9867924,10140741,11195577,10156812,11165844,12292836,14612139,
          15371986,17117367,18860773,19109088,16575567,21363327,22537884,
          22311144,22388970,23466370,21821117,21199591,22175262,22941642,
          22262439,22387133);
servpd = c(1447531,1587349,1775719,1903631,2129457,2456773,2805572,3331372,
           3844859,4358388,4982654,5570457,6208621,6843821,7528144,8309629,
           9017766,9462129,10104388,10113944,10368372,10567019,11195334,
           11712232,12180923,12811415,12975823,12612946,13479062,13855909,
           14181611,14637302,15326331,15645797,15957517,16394883,17068957,
           17547219,17700129);
avgGDP = c(105586,116305,127747,132093,152768,170512,182244,201065,220637,
           244739,270928,296672,321502,347526,374569,402380,428946,445447,
           465574,452951,473260,484164,512047,529556,550863,583133,570279,
           559807,607596,614922,630749,654142,694680,726895,746526,763445,
           779260,801348,839558);
CPI = c(58.75,59.55,59.54,59.44,59.85,60.16,60.94,63.63,66.25,68.65,71.72,
        73.83,76.86,79.67,82.12,82.87,84.26,84.41,85.47,85.46,85.29,85.05,
        86.42,88.42,88.95,90.55,93.74,92.92,93.82,95.15,96.99,97.76,98.93,
        98.63,100,100.62,101.98,102.55,102.31);
edufund = c(94673666,110942492,111121049,123915028,137899432,148047536,
            168382593,200549624,245279765,300965051,351140259,401130100,
            428109963,449691445,505683604,547227576,567147236,600599956,
            548764226,590444164,614797386,632686033,658024640,683855358,
            702184753,710784665,730759895,778262118,765283147,784518065,
            817856782,832633478,843545864,856766171,873281648,886970355,
            907010190,911899756,928402785);
edurat = c(5.32,5.52,5.71,5.68,5.9,6.2,6.59,6.86,7.22,7.24,7.51,8.1,8.26,8.91,
           9.51,10.22,10.82,11.32,11.79,12.29,13.26,14.35,15.71,17.45,19.39,
           21.13,23.12,25.02,26.34,27.66,28.94,30.18,31.7,32.86,33.78,34.61,
           35.31,36.21,37.69);
income = c(58070,62821,67146,69824,75437,83104,95907,109410,124140,141164,
           156023,177531,191481,205923,210811,224851,231611,244918,246256, 
           242640,239978,249763,254643,261571,267769,273336,272742,265750, 
           273647,275984,285939,293523,303762,311256,323490,331903,339772, 
           350904,369742);
t = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27
      ,28,29,30,31,32,33,34,35,36,37,38,39);

cincome = income[2:39]-income[1:38];
gincome = log(income[2:39])-log(income[1:38]);
cavgGDP = avgGDP[2:39] - avgGDP[1:38];
gavgGDP = log(avgGDP[2:39]) - log(avgGDP[1:38]);
cCPI = CPI[2:39]-CPI[1:38];
cedufund = edufund[2:39] - edufund[1:38];
gedurat = log(edurat[2:39]) - log(edurat[1:38]);
cunem = unem[2:39] - unem[1:38];
cindpd = indpd[2:39] - indpd[1:38];
cservpd = servpd[2:39] - servpd[1:38];
clabpart = labpart[2:39] - labpart[1:38];

model1 <- lm(formula = cincome ~ cedufund + cCPI + cunem + cindpd);
model2 <- lmrob(formula = cincome ~ cedufund + cCPI + cunem + cindpd + cavgGDP);
model3 <- lm(formula = cincome[2:38] ~ cincome[1:37] + cedufund[2:38]
              + cCPI[2:38] + cunem[2:38] + cindpd[2:38] + cavgGDP[2:38]);
summary(model3)
model4 <- lm(formula = cincome[3:38] ~ cincome[2:37] + cincome[1:36] + cedufund[3:38]
             + cCPI[3:38] + cunem[3:38] + cindpd[3:38] + cavgGDP[3:38]);
summary(model4)
model5 <- lm(formula = cincome[4:38] ~ cincome[2:36] + cedufund[4:38] +
             cunem[4:38] + cindpd[4:38] + cavgGDP[4:38]);
summary(model5)

model6 <- lm(formula = income ~ edufund +
               unem + indpd + avgGDP + t);
summary(model6)

income_dt <- lm(formula = income ~ t)$residuals
cincome_dt <- income_dt[2:39] - income_dt[1:38];
edufund_dt <- lm(formula = edufund ~ t)$residuals
cedufund_dt <- edufund_dt[2:39] - edufund_dt[1:38];
unem_dt <- lm(formula = unem ~ t)$residuals
cunem_dt <- unem_dt[2:39] - unem_dt[1:38];
indpd_dt <- lm(formula = indpd ~ t)$residuals
cindpd_dt <- indpd_dt[2:39] - indpd_dt[1:38];
avgGDP_dt <- lm(formula = avgGDP ~ t)$residuals
cavgGDP_dt <- avgGDP_dt[2:39] - avgGDP_dt[1:38];

model10 <- lm(formula = cincome_dt[4:38] ~ cincome_dt[2:36] + cedufund_dt[4:38]
              + cunem_dt[4:38] + cindpd_dt[4:38] + cavgGDP_dt[4:38]);
uut = model10$residuals[2:35];
uut_1 = model10$residuals[1:34];
test <- lm(formula = uut ~ uut_1);
summary(test)
bptest(model10)
summary(model10)

model9 <- lm(formula = cincome_dt ~ cedufund_dt +
               cunem_dt + cindpd_dt + cavgGDP_dt);
summary(model9)
uut = model9$residuals[2:38];
uut_1 = model9$residuals[1:37];
test <- lm(formula = uut ~ uut_1);
summary(test)
bptest(model9)
summary(model9)

model7 <- lm(formula = cincome[4:38] ~ cincome[2:36] + cedufund[4:38] +
               cunem[4:38] + cindpd[4:38] + cavgGDP[4:38]);
summary(model7)

plot(t[1:38],cincome)
model8 <- lm(formula = cindpd ~ t[1:38]);
summary(model8)

ut = model7$residuals[2:38];
ut_1 = model7$residuals[1:37];
test <- lm(formula = ut ~ ut_1);
stargazer(test)
bptest(model1)
summary(model1)
