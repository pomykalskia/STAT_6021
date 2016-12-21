# Homework 3
# Andrew Pomykalski
# STAT 6021
# Numbers 3.1, 3.3, 3.4, 3.5, 3.6, 3.8, 3.11, 3.16

setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Appendices")

library(readr)
library(plyr)

football <- read.csv("data-table-B1.csv")
gas <- read.csv("data-table-B3.csv")
chem <- read.csv("data-table-B5.csv")
engin <- read.csv("data-table-B7.csv")
life <- read.csv("data-table-B-16.csv")

# Question 3.1

# Part a

lm.football <- lm(y~x2 + x7 + x8, data = football)
summary(lm.football)

# y = -1.808372 + 0.003598x2 + 0.193960x7 - 0.004816

# Part b

anova(lm.football)

# x2 has a p-value of 3.1e^-5 which is less than 0.05 so x2 is significant
# x7 has a p-value of 3.689e-7 which is also less than 0.05 and is thus significant
# and lastly, x8 has a p-value of .0009378 which is also less than 0.05 and also significant
# So all the regressors are significant as shown in the anova table

# Part c

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.808372   7.900859  -0.229 0.820899    
# x2           0.003598   0.000695   5.177 2.66e-05 ***
# x7           0.193960   0.088233   2.198 0.037815 *  
# x8          -0.004816   0.001277  -3.771 0.000938 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 1.706 on 24 degrees of freedom
# Multiple R-squared:  0.7863,	Adjusted R-squared:  0.7596 
# F-statistic: 29.44 on 3 and 24 DF,  p-value: 3.273e-08

# So looking at the summary from part a of this problem, we see the p-values of each regressor.
# Since all the p-values are less than 0.05, then we can say that we reject the null hypothesis,
# and that all the variables are significant. Thus all the variables play a significant role in the model.

# Part d

lm.football <- lm(y~x2 + x7 + x8, data = football)
summary(lm.football)

# y = -1.808372 + 0.003598x2 + 0.193960x7 - 0.004816
# Just as in part a, I ran a linear model to fit the data
# The R squared term is 0.7863 and the adjusted R squared term is 0.7596
# The R squared term does not take into the addition of more than one regressor so we must look at the adjusted R squared
# The adjusted R squared is 0.7596 which is a pretty good correlation

# Part e

lm.full <- lm(y~x2+x7+x8, data = football)
lm.reduced <- lm(y~x2 + x8, data = football)

anova(lm.full, lm.reduced)

# Since the table gives us an F - 4.8324 and a p-value of 0.03782, we can reject the null hypothesis that B1 = B2 = 0.
# Thus we can say that x7 does contribute significant information when both x2 and x7 are considered. 
# The p-values should be almost identical thus the test should tell the exact same about the significance of the variables

# Question 3.3 

# Part a

confint(lm.football, level = 0.95)

# 2.5 %       97.5 %
#   (Intercept) -18.114944410 14.498200293
# x2            0.002163664  0.005032477
# x7            0.011855322  0.376065098
# x8           -0.007451027 -0.002179961

# As shown in the table above, with these three varibales taken into consideration, the confidence interval
# for B7 is (0.011855322, 0.376065098)

# Part b

newcol <- data.frame(x2 = 2300, x7 = 56.0, x8 = 2100)
predict(lm.football,newcol,interval = "confidence", level = 0.95)

#   fit      lwr      upr
# 1 7.216424 6.436203 7.996645

# So by the above table, the 95% confidence interval on the mean number of games won is (6.436203, 7.996645)

# Question 3.4

# Part a

lm.football4 <- lm(y~x7 + x8, data = football)
summary(lm.football4)

# y = 17.944 + 0.048x7 - 0.0065x8
# By looking at the summary of this multiple linear regression model, only the x8 regressor is significant (p-value = 0.00102 < 0.05)
# whereas the x7 is not significant (p-value = 0.68839 > 0.05)when x2 is taken out of the model for problem 3.1.

# Part b

# Looking at the same summary as Part a, the Multiple R-squared = 0.5477 and the Adjusted R-squared = 0.5115.
# Both of these values are signifcantly less than the R squared and adjusted R squared values from Question 3.1 when
# the R squared term was 0.7863 and the adjusted R squared term was 0.7596

# Part c

confint(lm.football4, level = 0.95)

#                   2.5 %       97.5 %
# (Intercept) -2.36784828 38.256485319
# x7          -0.19716429  0.293906022
# x8          -0.01015637 -0.002916818

# the confidence interval for x7 is ( -0.19716429, 0.293906022)

newcol <- data.frame(x7 = 56.0, x8 = 2100)
predict(lm.football4,newcol,interval = "confidence", level = 0.95)

#   fit      lwr      upr
# 1 6.926243 5.828643 8.023842

# The confidence interval for this condition is (5.828643, 8.023842).
# This is a significantly wider confidence interval than in Question 3.3 when the interval was (6.436203, 7.996645).

# Part d

# The conclusion I drew from this problem is that one must know which regressors play a signifcant role in the model
# and which ones do not. By omitting a regressor that is significant, it can severely weaken your models statistical
# significance as can including a regressor that is not significant.

# Question 3.5

# Part a

lm.gas <- lm(y~x1 + x6, data = gas)
summary(lm.gas)

# y = 32.88 - 0.053x1 + 0.959x6

# Part b

anova(lm.gas)

# We can see by the anova table that the x1 regressor is significant with a p-value of 3.666e-11 which is less than 0.05
# whereas the x6 regressor is not significant since its p-value is 0.1631 > 0.05.

# Part c

lm.gas <- lm(y~x1 + x6, data = gas)
summary(lm.gas)

# Multiple R-squared:  0.7873,	Adjusted R-squared:  0.7726 
# The Multiple R-squared = 0.7873 and the Adjusted R-squared = 0.7726.
# In Problem 2.4 the R squared term was 0.7723
# Thus we can see that the R squared term in this problem is greater and therefore better.

# Part d

confint(lm.gas, level = 0.95)

#               2.5 %      97.5 %
#   (Intercept) 29.74428901 36.02481266
#   x1          -0.06569892 -0.04059641
#   x6          -0.41164739  2.33009349

# Here we can see the 95% confidence interval for B1 is (-0.06569892, -0.04059641)

# Part e

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 32.884551   1.535408  21.417  < 2e-16 ***
#  x1          -0.053148   0.006137  -8.660 1.55e-09 ***
#  x6           0.959223   0.670277   1.431    0.163    
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 3.013 on 29 degrees of freedom
# Multiple R-squared:  0.7873,	Adjusted R-squared:  0.7726 
# F-statistic: 53.67 on 2 and 29 DF,  p-value: 1.79e-10

# So again we would look at the p-values in the linear model to see if we reject or accept the null hypothesis
# we see that the p-value for x1 is pretty much zero therefore it is significant to the model
# However, x6 have a p-value of 0.163 so this is not less than 0.05 and thus not significant. 

# Part f

newcol <- data.frame(x1 = 275, x6 = 2)
predict(lm.gas,newcol,interval = "confidence", level = 0.95)

#    fit      lwr      upr
# 1  20.18739 18.87221 21.50257

# we can see the 95% confidence interval with these conditions met is (18.87221, 21.50257).

# Part g

newcol <- data.frame(x1 = 257, x6 = 2)
predict(lm.gas,newcol,interval = "confidence", level = 0.95)

#    fit      lwr     upr
# 1 21.14405 19.88099 22.4071

# We can see that the 95% confidence interval with a slight change in the requirements is (19.88099, 22.4071)

# Question 3.6

predict(lm.gas,newcol,interval="predict", level=0.95)

#   fit      lwr      upr
# 1 21.14405 14.85403 27.43406

# In Problem 2.4, we got a 95% confidence interval of (14.34147, 27.05611)
# So as we can see, adding the regressor of x6 has little impact on the confidence interval
# It does shrink the interval a little bit but not significantly.

# Question 3.8

# Part a

lm.chem <- lm(y~x6 + x7, data = chem)
summary(lm.chem)

# y = 2.526 + 0.019x6 + 2.186x7

# Part b

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2.526460   3.610055   0.700   0.4908    
# x6          0.018522   0.002747   6.742 5.66e-07 ***
# x7          2.185753   0.972696   2.247   0.0341 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 9.924 on 24 degrees of freedom
# Multiple R-squared:  0.6996,	Adjusted R-squared:  0.6746 
# F-statistic: 27.95 on 2 and 24 DF,  p-value: 5.391e-07

# From the linear model in part a, we see that out R squared term is 0.6996 and our adjusted R squared term is 0.6746
# Also, we see that all of our p-values are less than 0.05 and thus significant since F = 27.95. 

# Part c

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2.526460   3.610055   0.700   0.4908    
# x6          0.018522   0.002747   6.742 5.66e-07 ***
# x7          2.185753   0.972696   2.247   0.0341 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 9.924 on 24 degrees of freedom
# Multiple R-squared:  0.6996,	Adjusted R-squared:  0.6746 
# F-statistic: 27.95 on 2 and 24 DF,  p-value: 5.391e-07

# so looking again at the p-values in this linear model, we see that both p-values are less than 0.05
# and thus both variables are significant since we reject the null hypothesis and accept the alternative hypothesis

# Part d

confint(lm.chem, level = 0.95)

#                    2.5 %     97.5 %
#  (Intercept) -4.92432697 9.97724714
#  x6           0.01285196 0.02419204
#  x7           0.17820756 4.19329833

# we see the 95% confidence interval for x6 is (0.01285196, 0.02419204) and for x7 is (0.17820756, 4.19329833)

# Part e

lm.chem6 <- lm(y~x6, data = chem)
summary(lm.chem6)

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 6.144181   3.483064   1.764   0.0899 .  
# x6          0.019395   0.002932   6.616 6.24e-07 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 10.7 on 25 degrees of freedom
# Multiple R-squared:  0.6365,	Adjusted R-squared:  0.6219 
# F-statistic: 43.77 on 1 and 25 DF,  p-value: 6.238e-07

# I would say that I am happy with the significance of the model however, 
# the R squared value is lower with just x6 than it was with x6 and x7.
# I would be satisified because a R squared value of 0.6365 is not terrible. 

# Part f

confint(lm.chem6, level = 0.95)

#                   2.5 %      97.5 %
# (Intercept) -1.02932458 13.31768586
# x6           0.01335688  0.02543261

# So the confidence interval on x6 is (0.01335688, 0.02543261)
# In part d, the confidence interval on x6 was (0.01285196, 0.02419204)
# which is only slightly narrower of an interval. Therefore, I would say
# that x7 has a positive contribution to the model and thus is significant
# and important to add to the model.

# Part g

anova(lm.chem)

# Response: y
# Df Sum Sq Mean Sq F value    Pr(>F)    
# x6         1 5008.9  5008.9 50.8557 2.267e-07 ***
# x7         1  497.3   497.3  5.0495    0.0341 *  
#  Residuals 24 2363.8    98.5    

anova(lm.chem6)

# Response: y
# Df Sum Sq Mean Sq F value    Pr(>F)    
# x6         1 5008.9  5008.9  43.766 6.238e-07 ***
#  Residuals 25 2861.2   114.4 

# So the MS_Res for the model with both regressors is 98.5 while the MS_Res for the model with
# one regressor is 114.4. So the MS_Res went up when I removed x7 from the model.
# This tells us that when adding another regressor, the error goes down but that is expected 
# since the model is adding more data to try and represent. 

# Question 3.11

# Part a 

lm.engin <- lm(y~., data = engin)
summary(lm.engin)

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.208e+01  1.889e+01   2.757 0.020218 *  
#  x1           5.556e-02  2.987e-02   1.860 0.092544 .  
#  x2           2.821e-01  5.761e-02   4.897 0.000625 ***
#  x3           1.250e-01  4.033e-01   0.310 0.762949    
#  x4           1.776e-16  2.016e-01   0.000 1.000000    
#  x5          -1.606e+01  1.456e+00 -11.035  6.4e-07 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 8.065 on 10 degrees of freedom
# Multiple R-squared:  0.9372,	Adjusted R-squared:  0.9058 
# F-statistic: 29.86 on 5 and 10 DF,  p-value: 1.055e-05

# y = 52.08 +0.05556x1 + 0.2821x2 + 0.125x3 + (1.776e-16)x4 - 16.06x5

# Part b

# So I will look at the p-values for all these regressors
# x1: p-value = 0.092 > 0.05 thus not significant
# x2: p-value = 0.000625 < 0.05 thus significant
# x3: p-value = 0.763 > 0.05 thus not significant
# x4: p-value = 1.000 > 0.05 thus not significant
# x5: p-value = 6.4e-07 > 0.05 thus significant
# Therefore the only two regressors that are significant are x2 and x5

# Part c

# So as stated above, I would reject the null hypothesis for x2 and x5 and accept the null hypothesis
# for all the others. This tells us that only x2 and x5 have a signifcant impact on the model when all
# the regressors are present.

# Part d

# All regressors: Multiple R-squared:  0.9372,	Adjusted R-squared:  0.9058 

lm.engin_sample <- lm(y~x2+x5, data = engin)
summary(lm.engin_sample)

# Multiple R-squared:  0.9149,	Adjusted R-squared:  0.9018 

# So weirdly enough, the adjusted R squared for all the variables is higher than with just the significant ones
# Thus having all the variables in the model actually better explains more of the data than just pulling out the significant variables.

# Part e

confint(lm.engin, level =0.95)

#                    2.5 %      97.5 %
# (Intercept)   9.99688896  94.1612109
# x1           -0.01100273   0.1221138
# x2            0.15378045   0.4105053
# x3           -0.77353688   1.0235369
# x4           -0.44926844   0.4492684
# x5          -19.30879739 -12.8211665

confint(lm.engin_sample, level = 0.95)

#                   2.5 %      97.5 %
# (Intercept)  67.8389462  92.4302647
# x2            0.1550559   0.4092298
# x5          -19.2765650 -12.8533989

# So we can see that with more regressors, the confidence interval for x2 is wider and for x5 the
# confidence interval is narrower. This is interesting that one regressor's confidence interval shrinks
# while another widens. 

# Question 3.16

# Part a

lf <- lm(LifeExp ~ People.per.TV + People.per.Dr, data = life)
summary(lf)

# y = 70.236 - 0.0226x1 - 0.0004x2

lfm <- lm(LifeExpMale ~ People.per.TV + People.per.Dr, data = life)
summary(lfm)

# y = 73.092 - 0.0257x1 - 0.00048x2

lff <- lm(LifeExpFemale ~ People.per.TV + People.per.Dr, data = life)
summary(lff)

# y = 67.4298 - 0.0199x1 - 0.00041x2

# Part b

# I looked at the summary of each model presented in part a. I did not comment out the entire model
# for each model I ran in part a.

# For the lf model, we see that both x1 and x2 are significant since their p-values are x1 = 0.0243 < 0.05 and 
# x2 = 0.0332 < 0.05 hence both are significant

# For the lfm model, we see that both x1 and x2 are significant since their p-values are x1 = 0.0253 < 0.05 and 
# x2 = 0.0455 < 0.05 hence both are significant

# For the lf model, we see that both x1 and x2 are significant since their p-values are x1 = 0.0239 < 0.05 and 
# x2 = 0.0267 < 0.05 hence both are significant

# Part c

# Much like in part b, I looked at the p-value for the t test. We see that since the p-values for x1 = 0.0243 < 0.05 and 
# x2 = 0.0332 < 0.05, we reject the null hypothesis for both and thus accept the alternative hypothesis 
# for both that B_1 and B_2 not equal zero.

# We see that since the p-values for x1 = 0.0253 < 0.05 and x2 = 0.0455 < 0.05, we reject the null hypothesis for both 
# and thus accept the alternative hypothesis for both that B_1 and B_2 not equal zero.

# We see that since the p-values for x1 = 0.0239 < 0.05 and x2 = 0.0267 < 0.05, we reject the null hypothesis for both 
# and thus accept the alternative hypothesis for both that B_1 and B_2 not equal zero.

# Part d

# for the lf model Multiple R-squared:  0.4457,	Adjusted R-squared:  0.414
# for the lfm model Multiple R-squared:  0.4173,	Adjusted R-squared:  0.384 
# for the lff model Multiple R-squared:  0.4457,	Adjusted R-squared:  0.414 

# Part e

confint(lf, level = 0.95)

# People.per.Dr (-0.0008563196, -3.777668e-05)

confint(lfm, level =.95)

# People.per.Dr (-0.0009470177, -1.008023e-05)

confint(lff, level = 0.95)

# People.per.Dr (-0.0007670492, -5.007977e-05)




