# Homework 5
# Andrew Pomykalski
# STAT 6021
# Numbers  6.12, 6.13, 6.14, 6.15, 8.5, 8.6

setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Appendices")

library(readr)
library(plyr)
library(MASS)

wine <- read.csv('data-table-B11.csv')

# Problem 6.12

wine.lm <- lm(Quality ~ .-Region, data = wine)
summary(wine.lm)


## Cook's Distance
cooks.distance(wine.lm)

## DFBETAS
df = 2/sqrt(38)
df

## DFFITS
dff = 2*sqrt(5/38)
dff

## A summary of potential leverage and/or influential points
summary(influence.measures(wine.lm))

#Potentially influential observations of
#lm(formula = Quality ~ . - Region, data = wine) :
#  
#  dfb.1_ dfb.Clrt dfb.Arom dfb.Body dfb.Flvr dfb.Okns dffit   cov.r   cook.d hat  
#12  0.83  -0.97    -0.40     0.23    -0.01    -0.04     1.38_*  1.16    0.30   0.39
#14 -0.03   0.11    -0.16    -0.05     0.05     0.08    -0.28    1.84_*  0.01   0.36
#20 -0.04  -0.38     0.46    -1.06_*   0.65     0.60    -1.54_*  0.30_*  0.31   0.20
#32 -0.04   0.04    -0.03     0.02     0.04    -0.03    -0.07    1.57_*  0.00   0.23
#37 -0.06  -0.15     0.04     0.28    -0.37     0.39     0.61    1.72_*  0.06   0.38

## Based on the table, we see that points 12, 14, 20, 32, 37 are all flaggged as possible influential or leverage points
## Taking a closer look, we see only points 12 and 20 are flagged by dffits and dfbetas as influential points

# Problem 6.13

temp <- read.csv('data-table-B12.csv')

temp.lm <- lm(temp~., data = temp)
summary(temp.lm)

summary(influence.measures(temp.lm))

#Potentially influential observations of
#lm(formula = temp ~ ., data = temp) :
#  
#  dfb.1_ dfb.sktm dfb.skpc dfb.dfft dfb.dffp dfb.ptch dffit   cov.r   cook.d hat    
#22  0.13   0.05    -0.20     0.31     0.25    -0.13     0.39    1.84_*  0.03   0.36  
#28  0.09  -1.34_*  -0.08    -0.92    -0.45     1.35_*   1.82_*  0.30_*  0.42   0.28  
#29 -0.07   1.71_*   0.12     1.22_*   0.41    -1.74_*  -2.17_*  0.32    0.61   0.35  
#31  0.04  -0.01    -0.05    -0.01     0.01     0.02     0.11    1.95_*  0.00   0.35  
#32  0.02  -0.07    -0.03     0.01     0.00     0.03    -0.16    5.33_*  0.00   0.76_*
  
## As we can see by the table that points 22, 28, 29, 31, 32 are all flagged as possible influential or leverage points
## Taking a closer look, we see that only 28 and 29 are marked as influential points based on DFFITS and DFBETAS

# Problem 6.14

jet <- read.csv('data-table-B13.csv')

jet.lm <- lm(y~., data = jet)
summary(jet.lm)

summary(influence.measures(jet.lm))

#Potentially influential observations of
#lm(formula = y ~ ., data = jet) :
#  
#  dfb.1_  dfb.x1  dfb.x2  dfb.x3  dfb.x4  dfb.x5  dfb.x6  dffit   cov.r   cook.d  hat    
#6   0.02   -0.07    0.08   -0.04    0.07    0.01    0.09   -0.20    1.74_*  0.01    0.30  
#9   0.01   -0.07    0.00   -0.01    0.07    0.06   -0.05    0.19    1.65_*  0.01    0.27  
#10 -0.06   -0.01    0.05    0.05   -0.01   -0.11    0.10   -0.32    1.72_*  0.02    0.31  
#11  0.86    1.45_*  0.21   -0.75   -1.15_*  0.08    0.48    1.65_*  1.39    0.37    0.50  
#20 -1.51_* -0.88   -4.41_*  1.76_*  1.63_*  1.41_* -1.18_* -5.13_*  0.82    3.01_*  0.74_*

## As we can see by the table that points 6, 9, 10, 11, 20 are all flagged as possible influential or leverage points
## Taking a closer look, we see that only 11 and 20 are marked as influential points based on DFFITS and DFBETAS

# Problem 6.15

elec <- read.csv('data-table-B14.csv')

elec.lm <- lm(y~.-x5, data = elec)
summary(elec.lm)

summary(influence.measures(elec.lm))

#Potentially influential observations of
#lm(formula = y ~ . - x5, data = elec) :
#  
#  dfb.1_  dfb.x1 dfb.x2  dfb.x3  dfb.x4  dffit   cov.r   cook.d  hat  
#2   1.67_*  0.21  -3.77_*  0.05   -1.07_* -4.67_*  0.04_*  1.98_*  0.47
#4  -0.59    0.20  -0.57   -0.94    2.34_*  2.50_*  0.91    1.04_*  0.56
#8   0.41   -0.60   1.46_* -0.54   -0.29    1.57_*  0.62    0.41    0.34
#9  -0.13   -0.09  -0.11    1.05_* -0.51    1.15    1.38    0.26    0.41
#10  0.19   -0.28   0.07   -0.19    0.02   -0.33    1.98_*  0.02    0.38

## As we can see by the table that points 2, 4, 8, 9, 10 are all flagged as possible influential or leverage points
## Taking a closer look, we see that only 2, 4, 8, and 9 are marked as influential points based on DFFITS and DFBETAS

# Problem 8.5
# Part a

gas <- read.csv('data-table-B3.csv')

gas.lm <- lm(y~x10+x11, data = gas)
summary(gas.lm)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 39.1919052  2.5570509  15.327 1.92e-15 ***
#  x10         -0.0047484  0.0009544  -4.975 2.72e-05 ***
#  x11         -2.6958431  1.9805597  -1.361    0.184    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 3.2 on 29 degrees of freedom
#Multiple R-squared:   0.76,	Adjusted R-squared:  0.7434 
#F-statistic: 45.91 on 2 and 29 DF,  p-value: 1.032e-09

# y = 39.19 - 0.0047x10 - 2.696x11

# With a p-value of 0.184 and a t-value = -1.361, this is not significant

# Part b 

gas2.lm <- lm(y~x10+x11+x10*x11, data = gas)
summary(gas2.lm)

# y = 58.11 - 0.012x10 - 26.72x11 + 0.009x10x11

# I added an interaction to the model and got the above model. 
# When x11 = 1, the model is as follows y = (58.1 - 26.2) + (-0.0125 + 0.009)x10 = 31.9 - 0.0035x10
# When x11 = 0, the model is as follows y = 58.1- 0.0125x10
# when x11 = 1, we see that on average for every increase of one in displacement, miles per gallon decreases 0.0035
# when x11 = 0, we see that on average for every increase of one in displacement, miles per gallon decreases by 0.0125.

# Problem 8.6

football <- read.csv('data-table-B1.csv')

fball.lm <- lm(y~x8+x7+x5, data = football)
summary(fball.lm)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept) 22.208783  10.160071   2.186  0.03881 * 
#  x8          -0.006318   0.001733  -3.646  0.00128 **
#  x7          -0.032886   0.130885  -0.251  0.80375   
#x5           0.077483   0.055815   1.388  0.17783   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 2.388 on 24 degrees of freedom
#Multiple R-squared:  0.5813,	Adjusted R-squared:  0.5289 
#F-statistic: 11.11 on 3 and 24 DF,  p-value: 9.164e-05

dummy <- as.numeric(football$x5 > 0)
dummy2 <- as.numeric(football$x5 < 0)

fball2.lm <- lm(y~x8+x7+dummy+dummy2, data = football)
summary(fball2.lm)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept) 19.353055   9.525127   2.032  0.05388 . 
#x8          -0.006337   0.001719  -3.685  0.00122 **
#  x7          -0.006825   0.118841  -0.057  0.95470   
#dummy        2.333031   2.483813   0.939  0.35734   
#dummy2       0.460504   2.466185   0.187  0.85351   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 2.337 on 23 degrees of freedom
#Multiple R-squared:  0.6158,	Adjusted R-squared:  0.549 
#F-statistic: 9.216 on 4 and 23 DF,  p-value: 0.0001349

# y = 19.35 - 0.0063x8 - 0.0068x7 + 2.33x51 + 0.46x52
# we see that none of these p-values are significant but my interpretation of the model is as follows:
# both of the variables are positive which is interesting. However, we see that having more than zero turnovers
# has a more positive impact on winning games but having less turnovers also has a positive impact 
# on the amount of games won. For every positive turnover it increases the wins by 2 but every number 
# under zero for turnovers has a half game increase to wins. 
