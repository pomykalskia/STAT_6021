# Homework 2
# Andrew Pomykalski
# STAT 6021
# Numbers 2.20, 2.21, 2.22, 2.30

setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Appendices")

library(readr)
library(plyr)

t.20 <- read_csv("data_table_B18.csv")
t.21 <- read_csv("data_table_B19.csv")
t.22 <- read_csv("data_table_B20.csv")

# Problem 2.20
plot(y~x_5, data = t.20)

p20.lm <- lm(y~x_5, data = t.20)
summary(p20.lm)

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 410.72319   18.92479  21.703 3.54e-12 ***
#  x_5          -0.26376    0.09622  -2.741   0.0159 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 7.838 on 14 degrees of freedom
# Multiple R-squared:  0.3493,	Adjusted R-squared:  0.3028 
# F-statistic: 7.514 on 1 and 14 DF,  p-value: 0.01592

# Equation derived from the linear model y = 410.72 - 0.264x
# P value = 0.0159, F statistic = 7.514, R squared = 34.93%
# By looking at the graph, there seems to be a slight negative trend to the data
# We see a p value of 0.0159 which is less than 0.05 which classifies this as significant
# Our F statistic is 7.514 which means the error on the residuals is very small leading to the fraction MS_R/MS_Res to be a big number
# The only concerning thing is that fact the R squared is not high at only 34.93% which signifies that there is a good bit of variation that is unexplained by the model
# I would agree with the engineer due to the low p value and the F statistic that tells us to accept the null hypothesis. 

# Problem 2.21
plot(y~x_3, data= t.21)

p21.lm <- lm(y~x_3, data = t.21)
summary(p21.lm)

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 16.564030   0.620906  26.677   <2e-16 ***
#  x_3         -0.012762   0.005744  -2.222    0.034 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 1.668 on 30 degrees of freedom
# Multiple R-squared:  0.1413,	Adjusted R-squared:  0.1127 
# F-statistic: 4.936 on 1 and 30 DF,  p-value: 0.03399

# Equation dervied from the linear model y = 16.564 - 0.013x
# P value = 0.034, F statistic = 4.936, R squared = 14.13%
# By looking at the graph of the linear model, we already see the negative correlation the wine maker predicted
# We also see a negative slope in the equation which also signifies a negative fit line
# The p value for this correlation is 0.034 which of course is less than 0.05 and thus significant
# Again, our F statistic is large enough, 4.936, to accept the null hypothesis
# However, yet again we have a very low, 14.13%, R squared term which is concerning
# I would agree with the wine maker that there is a negative impact just not as strong a case as I would like statistically. 

# Problem 2.22
plot(y~x_5, data = t.22)

p22.lm <- lm(y~x_5, data = t.22)
summary(p22.lm)

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
# (Intercept)    21.25      20.99   1.013    0.326
# x_5             7.80      16.78   0.465    0.648
#
# Residual standard error: 35.76 on 16 degrees of freedom
# Multiple R-squared:  0.01333,	Adjusted R-squared:  -0.04834 
# F-statistic: 0.2161 on 1 and 16 DF,  p-value: 0.6483

# Equation from linear model y = 21.25 + 7.8x
# P value = 0.648, F statistic = 0.2161, R squared = 1.33%
# Rightaway from the graph there seems to be very little correlation and that can only be back up by the statistics
# The p value is 0.648 which is very much greater than 0.05 which signifies this is nowhere near significant
# The F statistic is less than the t value and p value which means the MS_Res, or the error of the residuals, is very large and the model is not a good fit
# And the R squared is 1.33% which means that nearly none of the data is described by this model.
# Thus I would disagree witht eh chemist and say that there is no relationship between this ratio and the conversion process

# Problem 2.30
setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Chapter 2/Problems")
t.30 <- read_csv("data_prob_2_12.csv")

plot(temp~usage, data = t.30)
# Looks like a very straight line with positive slope

p30.lm <- lm(temp~usage, data = t.30)
summary(p30.lm)

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.6938172  0.1789605   3.877  0.00307 ** 
#  usage       0.1085811  0.0003988 272.255  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 0.2113 on 10 degrees of freedom
# Multiple R-squared:  0.9999,	Adjusted R-squared:  0.9999 
# F-statistic: 7.412e+04 on 1 and 10 DF,  p-value: < 2.2e-16

# Well, this data is almost too perfect.
# The p value is pretty much zero, the R squared term is almost 1, and the F statistic is over 70,000.

# http://www.personality-project.org/r/html/corr.test.html

# Part b
cor.test(t.30$usage, t.30$temp)

# Pearson's product-moment correlation
#
# data:  t.30$usage and t.30$temp
# t = 272.25, df = 10, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.9997509 0.9999817
# sample estimates:
# cor 
# 0.9999326 

# we can see from the chart above that we should reject the null hypothesis and thus p != 0. 
# I used cor.test() that I have used for other cases in statistical analysis.
# Above is also the link to the cor.test() function
# Also, since the p value = 2.2e-16 which is less than 0.05, we reject the null hypothesis (H_0).

# Part c
cor.test(t.30$usage, t.30$temp, conf.level = 0.95)

# Pearson's product-moment correlation
#
# data:  t.30$usage and t.30$temp
# t = 272.25, df = 10, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.9997509 0.9999817
# sample estimates:
# cor 
# 0.9999326 

# I just did the same thing in this part using the cor.test() to find whether or not 
# p=0.5 falls within the confidence interval defined above. Since it does not, we 
# reject the null hypothesis agains and thus p != 0.5. 
# Not sure how else to do this problem.


# Part d
cor.test(t.30$usage, t.30$temp, conf.level = 0.99)

# Pearson's product-moment correlation
#
# data:  t.30$usage and t.30$temp
# t = 272.25, df = 10, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 99 percent confidence interval:
# 0.9996244 0.9999879
# sample estimates:
# cor 
# 0.9999326 

# So for this part, I just made the confidence interval to 99%.
# Thus the CI for this part is (0.9996244, 0.9999879)

