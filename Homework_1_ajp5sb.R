# Homework 1
# Andrew Pomykalski
# STAT 6021
# Numbers 2.1, 2.2, 2.4, 2.5, 2.12

setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Appendices")

library(readr)
library(plyr)
library(MPV)


data(table.b1)
data(table.b3)

# Problem 2.1
# Part a
plot(y~x8, data = table.b1)

#Simple linear model with the table of values
y.lm <- lm(y~x8, data = table.b1)
y.lm
summary(y.lm)

# Part b
# Used an ANOVA to test for significance 
anova(y.lm)
summary(y.lm)
# Since my p-value is less than 0.05 this is considered significant

# Part c
confint(y.lm, level=0.95)
# (-0.009614347, -0.004435854)

# Part d

# This is determined by the R squared value which for this model is 0.5447 or 54.47%
# This is found in the ANOVA table in part b

# Part e
newcol <- data.frame(x8 =2000)
predict(y.lm,newcol,interval = "confidence", level = 0.95)
# The average number of games a team would win if they hold their opponents under 2000 yards has
# a confidence interval of (6.765753, 8.710348)

# Problem 2.2

x8.lm <- lm(y~x8, data = table.b1)
newrush <- data.frame(x8 = 1800)
predict(x8.lm,newrush,interval = "prediction",level=0.90)
# The team would win about 9 games

# Problem 2.4
# Part a
plot(y~x1, data = table.b3)
p2.lm <- lm(y~x1, data = table.b3)
p2.lm
summary(p2.lm)
# Linear model

# Part b
anova(p2.lm)
summary(p2.lm)
# Since my p-value is less than 0.05 this is considered significant

# Part c
summary(p2.lm)
# So the R squared term is 0.7723 or 77.23%. Therefore, 77.23% of the total variability in 
# gasoline mileage is accounted for.

# Part d
newdisplace <- data.frame(x1 = 275)
predict(p2.lm,newdisplace,interval="confidence",level = 0.90)
# The confidence interval for this is (19.77571, 21.62188)

# Part e
predict(p2.lm,newdisplace,interval="predict", level=0.95)
# (14.34147, 27.05611)

# Part f
# The interval is wider in part d than in part e
# The standard error estimate has a 1 in it leading a wider interval as discussed in class. 

# Problem 2.5
# Part a
plot(y~x10, data=table.b3)
p3.lm <- lm(y~x10, data = table.b3)
p3.lm
summary(p3.lm)

# Part b
anova(p3.lm)
summary(p3.lm)
# Since my p-value is less than 0.05 this is considered significant

# Part c
summary(p3.lm)
# So the R squared term is 0.7446
# Looking soley at the R squared value, we see that x1 is a better choice because it is 
# a large R squared value. Also the higher the F statisitc for x1 means more of the sums of 
# squares are explained by the residuals. 


# Problem 2.12
# Part a
setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Chapter 2/Problems")
twelve <- read.csv("data_prob_2_12.csv")

plot(temp~usage, data = twelve)
p12.lm <- lm(usage ~ temp, data = twelve)
p12.lm
summary(p12.lm)

# Part b
summary(p12.lm)
# Since my p-value is less than 0.05 this is considered significant

# Part c
confint(p12.lm, level=0.95)
# (9.133106, 9.283830) I created a confidence interval so I am 95% that my interval includes the population mean
# Therefore, I do not believe that the data supports the statement

# Part d
newtemp <- data.frame(temp = 58)
predict(p12.lm,newtemp,interval = "prediction", level=0.99)
# (521.2237, 534.2944)


# Problem 2.29
# We would want to take our observations from areas around x = -1,1. The endpoints have the 
# most influence on the slope of the line. By collecting observations from these two boundary
# points, we can get a true value for y at -1 and 1. If we were to take our observations from 
# a point closer to the zero, the slope could go in any direction off of that point. Hence 
# getting out observations from the endpoints should give us a true reading as to the slope 
# and minimize error. But if you have an odd number of data points, we should pick one point 
# at zero to evenly weight the ends. 
# I really don't see any practical aspects to this data collection plan. Since in the 
# practical sense, we usually don't know what our two bounds are for the data.




