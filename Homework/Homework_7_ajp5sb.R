# Homework 7
# Andrew Pomykalski
# STAT 6021
# Numbers  11.1, 11.2, 11.3, 11.11, 11.12, 13.1, 13.2, 13.5, 13.10, 13.25

setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Appendices")

library(readr)
library(plyr)
library(MASS)
library(car)
library("DAAG")

# Problem 11.1

# From problem 3.1:
football <- read.csv("data-table-B1.csv")

lm.football <- lm(y~x2 + x7 + x8, data = football)
summary(lm.football)

# y = -1.808372 + 0.003598x2 + 0.193960x7 - 0.004816

# Part a 

press <- resid(lm.football)/(1 - lm.influence(lm.football)$hat)
press.stat <- sum(press^2) # 87.46

sst <- sum(anova(lm.football)[,2]) # 326.96

R_sq <- 1-(press.stat/sst) # 73.25%

# Based on that R_sq value we can say that this does a pretty good job of predicting

# Part b

half.ball <- football[sample(nrow(football), 14), ]

lm.half.football <- lm(y~x2 + x7 + x8, data = half.ball)
summary(lm.half.football)

# y = 10.5676 + 0.00119x2 + 0.1079x7 - 0.00617

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept) 10.567635  22.287511   0.474   0.6456  
#x2           0.001192   0.002532   0.471   0.6478  
#x7           0.107892   0.215210   0.501   0.6270  
#x8          -0.006170   0.003291  -1.875   0.0903 .

won.games.half <- as.data.frame(predict(lm.half.football, newdata = half.ball, type="response"))

press <- resid(lm.half.football)/(1 - lm.influence(lm.half.football)$hat)
press.stat <- sum(press^2) # 76.88

sst <- sum(anova(lm.half.football)[,2]) # 160.857

R_sq <- 1-(press.stat/sst) # 52.20%

# Based on that R_sq value we can say that this does not do a good job of predicting

# Part c

# obs number 8, 7, 9, 10, 17, 26

top.ball <- football[-c(8,7,9,10,17,26), ]

lm.top.football <- lm(y~x2 + x7 + x8, data = top.ball)
summary(lm.top.football)

# y = -3.84 + 0.00382x2 + 0.226x7 - 0.0049

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -3.8405792  9.6624685  -0.397 0.695692    
#x2           0.0038214  0.0007795   4.902 0.000115 ***
#x7           0.2256411  0.1099837   2.052 0.055051 .  
#x8          -0.0049043  0.0015723  -3.119 0.005925 ** 

won.games.top <- as.data.frame(predict(lm.top.football, newdata = top.ball, type="response"))

press <- resid(lm.top.football)/(1 - lm.influence(lm.top.football)$hat)
press.stat <- sum(press^2) # 73.22

sst <- sum(anova(lm.top.football)[,2]) # 262.95

R_sq <- 1-(press.stat/sst) # 72.16%

# Based on that R_sq value we can say that this does a pretty good job of predicting

# Problem 11.2

train.ball <- football[sample(nrow(football), 14), ]
test.ball <- football[-c(5,1,15,16,4,11,6,2,13,14,22,23,7,24), ]

train.null <- lm(y~1, data=train.ball)
train.full <- lm(y~., data=train.ball)

## Forward selection
step(train.null, scope=list(lower=train.null, upper=train.full), direction="forward")

# y ~ x2 + x7

#Step:  AIC=12.72
#y ~ x2 + x7
#
#Df Sum of Sq    RSS    AIC
#<none>              22.623 12.719
#+ x6    1   2.64960 19.974 12.975
#+ x4    1   2.15666 20.467 13.316
#+ x9    1   1.60447 21.019 13.689
#+ x3    1   1.11948 21.504 14.009
#+ x8    1   0.55618 22.067 14.371
#+ x1    1   0.48882 22.135 14.413
#+ x5    1   0.10297 22.520 14.655

## Backward selection
step(train.full, scope=list(lower=train.null, upper=train.full), direction="backward")

# y ~ x2 + x7

#Step:  AIC=12.72
#y ~ x2 + x7
#
#Df Sum of Sq    RSS    AIC
#<none>              22.623 12.719
#- x7    1    23.490 46.114 20.689
#- x2    1    69.861 92.484 30.432

## Stepwise selection
step(train.null, scope=list(lower=train.null, upper=train.full), direction="both")

# y ~ x2 + x7

#Step:  AIC=12.72
#y ~ x2 + x7
#
#Df Sum of Sq    RSS    AIC
#<none>              22.623 12.719
#+ x6    1     2.650 19.974 12.975
#+ x4    1     2.157 20.467 13.316
#+ x9    1     1.604 21.019 13.689
#+ x3    1     1.119 21.504 14.009
#+ x8    1     0.556 22.067 14.371
#+ x1    1     0.489 22.135 14.413
#+ x5    1     0.103 22.521 14.655
#- x7    1    23.490 46.114 20.689
#- x2    1    69.861 92.484 30.432

# I will use x2 and x7 for my model since all model picked these variables.

lm.train <- lm(y~x2+x7, data = train.ball)
summary(lm.train)

# y = -19.17 + 0.00429x2 + 0.3054x7

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.917e+01  5.704e+00  -3.362 0.006346 ** 
#  x2           4.293e-03  7.365e-04   5.828 0.000114 ***
#  x7           3.054e-01  9.038e-02   3.380 0.006148 ** 
 
ball.predict <- predict(lm.train, test.ball)
mse <- sum((ball.predict - test.ball$y)^2)/(nrow(test.ball)-4) # 8.699

# With the MSE being so low, I would say this does a pretty good job at predicting the testing set
# We have learned that if we minimize training set MSE, it should minimize testing set MSE unless we overfit the data.

# Problem 11.3

press <- resid(lm.train)/(1 - lm.influence(lm.train)$hat)
press.stat <- sum(press^2) # 34.67

sst <- sum(anova(lm.train)[,2]) # 113.43

R_sq <- 1-(press.stat/sst) # 69.44%

# My model does predict pretty well based on my R_squared value of 69.44%

# My PRESS statistic measures the accuracy of my training for prediction 
# My MSE tells me that my model is good when the model is ran on my testing set.
# Thus I feel that my MSE score being so low tells me my model predicts the outcome of my testing set very well. 

# Problem 11.11

# From Problem 3.1:

lm.football <- lm(y~x2 + x7 + x8, data = football)
summary(lm.football)

# y = -1.808372 + 0.003598x2 + 0.193960x7 - 0.004816

anova(lm.football)

#Response: y
#Df  Sum Sq Mean Sq F value    Pr(>F)    
#x2         1  76.193  76.193  26.172 3.100e-05 ***
#x7         1 139.501 139.501  47.918 3.698e-07 ***
#x8         1  41.400  41.400  14.221 0.0009378 ***
#Residuals 24  69.870   2.911  

anova(lm.train)

#Response: y
#Df Sum Sq Mean Sq F value   Pr(>F)    
#x2         1 67.315  67.315  32.730 0.000134 ***
#x7         1 23.490  23.490  11.421 0.006148 ** 
#Residuals 11 22.623   2.057 

# So we can see that the residuals are lower for my model developed off of the training set. 
# The residuals are also lower so by these things I would say that my model formed on the training set has lower standard error

# Problem 11.12

test.ball <- football[-c(5,1,15,16,4,11,6,2,13,14,22,23,7,24), ]

test.null <- lm(y~1, data=test.ball)
test.full <- lm(y~., data=test.ball)

## Forward selection
step(test.null, scope=list(lower=test.null, upper=test.full), direction="forward")

# y ~ x2 + x5 + x8

#Step:  AIC=18.7
#y ~ x8 + x2 + x5
#
#Df Sum of Sq    RSS    AIC
#<none>              30.057 18.697
#+ x7    1   2.83475 27.223 19.310
#+ x9    1   2.66165 27.396 19.399
#+ x1    1   2.18172 27.876 19.642
#+ x3    1   1.59028 28.467 19.936
#+ x6    1   0.03224 30.025 20.682
#+ x4    1   0.01501 30.042 20.690

## Backward selection
step(test.full, scope=list(lower=test.null, upper=test.full), direction="backward")

# y ~ x1 + x2 + x5 + x8 + x9

#Step:  AIC=19.25
#y ~ x1 + x2 + x5 + x8 + x9
#
#Df Sum of Sq    RSS    AIC
#<none>              23.505 19.254
#- x1    1    3.8912 27.396 19.399
#- x9    1    4.3711 27.876 19.642
#- x5    1    6.4173 29.922 20.633
#- x8    1   15.3948 38.899 24.307
#- x2    1   22.6446 46.149 26.700

## Stepwise selection
step(test.null, scope=list(lower=test.null, upper=test.full), direction="both")

# y ~ x2 + x5 + x8

#Step:  AIC=18.7
#y ~ x8 + x2 + x5
#
#Df Sum of Sq     RSS    AIC
#<none>               30.057 18.697
#+ x7    1     2.835  27.223 19.310
#+ x9    1     2.662  27.396 19.399
#+ x1    1     2.182  27.876 19.642
#+ x3    1     1.590  28.467 19.936
#+ x6    1     0.032  30.025 20.682
#+ x4    1     0.015  30.042 20.690
#- x5    1    12.888  42.945 21.692
#- x2    1    16.773  46.830 22.905
#- x8    1    73.995 104.053 34.082

# I will use x2, x5, and x8 for my model since all model picked these variables.

lm.test <- lm(y~x2+x5+x8, data = test.ball)
summary(lm.test)

# y = 15.973 + 0.0025x2 + 0.1114x5 - 0.0069x8

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 15.973143   3.426137   4.662 0.000891 ***
#x2           0.002485   0.001052   2.362 0.039801 *  
#x5           0.111444   0.053820   2.071 0.065209 .  
#x8          -0.006850   0.001381  -4.962 0.000569 ***
  
ball.test.predict <- predict(lm.test, train.ball)
mse <- sum((ball.test.predict - train.ball$y)^2)/(nrow(train.ball)-4) # 6.599

# Part a

# So my MSE for my model created off the testing set is lower than my MSE from the training set. 

# Part b

# With such a low MSE, even lower than the training set MSE, this model does a very good job at predicting the training set.

# Problem 13.1

setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Chapter 13/Problems")
target <- read.csv("data-prob-13-1.csv")

# Part a

log.target <- glm(y~., data = target, family = "binomial")
summary(log.target)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0620  -0.4868   0.3915   0.5476   2.1682  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  6.070884   2.108996   2.879  0.00399 **
#  x           -0.017705   0.006076  -2.914  0.00357 **
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 34.617  on 24  degrees of freedom
#Residual deviance: 20.364  on 23  degrees of freedom
#AIC: 24.364
#
#Number of Fisher Scoring iterations: 4

# Part b

# I would say this regression is not adequate
# The Null Deviance for this model is 34.617 on 24 degrees of freedom which is pretty large
# The p-value for B_1 is significant but I feel this is not adequate. 

# Part c 

B_1 <- -.017705 # From table above
O_1 <- exp(B_1) # 0.9824

# For every additional knot in speed the odds of hitting the target decrease by 1.77%.

# Part d

log.quad.target <- glm(y~.+x^2, data = target, family = "binomial")
summary(log.quad.target)

# since the deviance did not change one bit, I would say that the quadratic term is not required. 

# Problem 13.2

income <- read.csv("data-prob-13-2.csv")

# Part a

log.income <- glm(y~., data = income, family = "binomial")
summary(log.income)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0232  -0.8766   0.5072   0.7980   1.6046  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept) -8.7395139  4.4394326  -1.969   0.0490 *
#  x            0.0002009  0.0001006   1.998   0.0458 *
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 27.526  on 19  degrees of freedom
#Residual deviance: 22.435  on 18  degrees of freedom
#AIC: 26.435
#
#Number of Fisher Scoring iterations: 4

# Part b

# The beta_1 coefficient is significant at 0.04 which is less than 0.05.
# The Null Deviance is 27.526 on 19 degrees of freedom which is also pretty large
# I would say this model is adequate. 

# Part c 

B_1 <- 0.0002009 # see above table
O_1 <- exp(B_1) # 1.00002

# This indicates the odds are pretty even. For every additional dollar of income the odds of ownership status increase by 0.02%.

# Part d

log.quad.income <- glm(y~.+x^2, data = income, family= "binomial")
summary(log.quad.income)

# since the deviance did not change one bit, I would say that the quadratic term is not required. 

# Problem 13.5

age <- read.csv("data-prob-13-5.csv")

# Part a

log.age <- glm(y~., data = age, family = "binomial")
summary(log.age)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.5635  -0.8045  -0.1397   0.9535   1.7915  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept) -7.047e+00  4.674e+00  -1.508    0.132  
#x1           7.382e-05  6.371e-05   1.159    0.247  
#x2           9.879e-01  5.274e-01   1.873    0.061 .
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 27.726  on 19  degrees of freedom
#Residual deviance: 21.082  on 17  degrees of freedom
#AIC: 27.082
#
#Number of Fisher Scoring iterations: 5

# Part b

# The Null Deviance is 27.726 on 19 degrees of freedom which is pretty large
# But, I would say the model is adequate.

# Part c 

B_1 <- 0.00007382
O_1 <- exp(B_1) # 1.0000738

# This indicates the odds are pretty even for B_1. For every addition in income the odds of purchasing a new vehicle increase by 0.007382%.

B_2 <- 0.9879
O_1 <- exp(B_2) # 2.685

# For every addition in age the odds of purchasing a new vehicle increase by 98.79%.

# Part d

fam.2 <- data.frame(x1=45000, x2=5)
fam.pred <- predict(log.age, fam.2)
# 1.21
fam.per <- (exp(fam.pred)/(1+exp(fam.pred)))
# The estimated probaboloty is 77.1%

# Part e

log.age.int <- glm(y~.+x1*x2, data = age, family = "binomial")
summary(log.age.int)

# the residual deviance reduced significantly from 21.082 to 16.551. 
# I would say that it is required for this model

# Part f

anova(log.age, test = "Chisq")

#Analysis of Deviance Table
#
#Model: binomial, link: logit
#
#Response: y
#
#Terms added sequentially (first to last)
#
#
#Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
#NULL                    19     27.726           
#x1    1   0.7349        18     26.991  0.39129  
#x2    1   5.9094        17     21.081  0.01506 *
  
# Part g

confint(log.age, level = 0.95)

#                   2.5 %       97.5 %
#(Intercept) -1.805544e+01 1.0275430082
#x1          -4.361540e-05 0.0002184223
#x2           1.544228e-01 2.2872127855

# Problem 13.10

qqnorm(resid(log.age))
qqline(resid(log.age))

# Heavy tail on this qq plot

plot(log.age$fitted.values, log.age$residuals)

# There is no pattern with these residuals and no outliers so they are good.

# Problem 13.25

space <- read.csv("data-prob-13-25.csv")

log.space <- glm(At.Least.One.O.ring.Failure~., data = space, family = "binomial")
summary(log.space)

plot(log.space$fitted.values, log.space$residuals)

# Part b

odds <- exp(log.space)/(1+exp(log.space))

# CHECK AND CHANGE

# Part c

space.5 <- data.frame(Temperature.at.Launch=50)
space.pred <- predict(log.space, space.5)
# 2.31
space.per <- (exp(space.pred)/(1+exp(space.pred)))
# The failure probability is 91%

# Part d

space.7 <- data.frame(Temperature.at.Launch=75)
space.pred.7 <- predict(log.space, space.7)
# -1.97
space.per.7 <- (exp(space.pred.7)/(1+exp(space.pred.7)))
# The failure probability is 12.2%

# Part e

space.3 <- data.frame(Temperature.at.Launch=31)
space.pred.3 <- predict(log.space, space.3)
# 5.56
space.per.3 <- (exp(space.pred.3)/(1+exp(space.pred.3)))
# The failure probability is 99.6%

# Part f

anova(log.space, test = "Chisq")

#Analysis of Deviance Table
#
#Model: binomial, link: logit
#
#Response: At.Least.One.O.ring.Failure
#
#Terms added sequentially (first to last)
#
#
#                       Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
#NULL                                     23     28.975           
#Temperature.at.Launch  1   5.9441        22     23.030  0.01477 *

# The deviance residual is less than 0.05 and thus is significant

# Part g

log.space.quad <- glm(At.Least.One.O.ring.Failure~.+(Temperature.at.Launch)^2, data = space, family = "binomial")
summary(log.space.quad)

# The deviance is unchanged and thus it does not improve the model. 