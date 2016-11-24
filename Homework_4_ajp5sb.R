# Homework 4
# Andrew Pomykalski
# STAT 6021
# Numbers 4.2, 4.4, 4.8, 4.13, 4.25, 4.29, 5.2, 5.5, 5.7, 5.9, 5.10

setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Appendices")

library(readr)
library(plyr)
library(MASS)

football <- read.csv("data-table-B1.csv")
gas <- read.csv("data-table-B3.csv")

# Problem 4.2

# Part a

lm.football <- lm(y~x2 + x7 + x8, data = football)
summary(lm.football)

# y = -1.808372 + 0.003598x2 + 0.193960x7 - 0.004816

qqnorm(resid(lm.football))
qqline(resid(lm.football))

# There is a slight normality problem. Looks like heavy tails

# Part b
plot(lm.football$fitted.values, lm.football$residuals)
title(main = "Problem 4.2")

# This plot looks good, normally distributed around the line x=0.

# Part C

plot(football$x2, residuals(lm.football))
title(main = "Problem 4.2c")

plot(football$x7, residuals(lm.football))
title(main = "Problem 4.2c")

plot(football$x8, residuals(lm.football))
title(main = "Problem 4.2c")

# The plot for x8 looks good with nice distribution
# Both the plots for x2 and x7 show nonconstant variance due to a slight fanning
# But x2 shows only a little bit of nonconstant variance

# Part d

lm.fball <- lm(y~x7 + x8, data = football)
lm.fball.x2 <- lm(x2~x7 + x8, data = football)
plot(residuals(lm.fball), residuals(lm.fball.x2))
title(main = "Problem 4.2d")

lm.fball.x3 <- lm(x7~x2 + x8, data = football)
plot(residuals(lm.fball), residuals(lm.fball.x3))
title(main = "Problem 4.2d")

lm.fball.x4 <- lm(x8~x2 + x7, data = football)
plot(residuals(lm.fball), residuals(lm.fball.x4))
title(main = "Problem 4.2d")

# I would say there is a linear relationship between response and the variable x7.
# These plots in this part show the relationship between the regressor variables and the response. 
# These plots are supposed to determine if that relationship is correct.

# Part e

rstandard(lm.football)

rstudent(lm.football)

# These are to be used to determine outliers and points that might skew the graph
# In this problem, the first observation is an outlier since it is over 2 and the rest are between 2 and -1

# Problem 4.4

# Part a

lm.gas <- lm(y~x1 + x6, data = gas)
summary(lm.gas)

qqnorm(resid(lm.gas))
qqline(resid(lm.gas))

# This plot isn't quite normal
# Looks to be more heavy tails

# Part b

plot(lm.gas$fitted.values, lm.gas$residuals)
title(main = "Problem 4.4b")

# This plot looks pretty good but I would say there is a slight nonlinear pattern

# Part c

lm.all <- lm(y~x1+x6, data = gas)
lm.1 <- lm(x1~x6, data = gas)
lm.6 <- lm(x6~x1, data = gas)
plot(residuals(lm.all), residuals(lm.6))
title(main = "Problem 4.4c")
plot(residuals(lm.all), residuals(lm.1))
title(main = "Problem 4.4c")

# So the plot with x1 shows a linear pattern
# The x6 shows no pattern so it might not be necessary to inlcude x6 in the model

# Part d

rstandard(lm.gas)

rstudent(lm.gas)

# The residuals show that there is two possible outliers
# 22, 12, and 15 have outliers outside the others which fall between -1.5 and 1.5

# Problem 4.8

setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Chapter 2/Problems")
twelve <- read.csv("data_prob_2_12.csv")

p12.lm <- lm(usage ~ temp, data = twelve)
summary(p12.lm)

qqnorm(resid(p12.lm))
qqline(resid(p12.lm))

# This plot is pretty close to normal. If anything, light tails which are not bad either

# Part b

plot(p12.lm$fitted.values, p12.lm$residuals)
title(main = "Problem 4.8b")

# There is a slight fanning out from the origin to the end as x increases

# Part c

x <- seq(1, 12, by=1)
plot(x,residuals(p12.lm))
title(main = "Problem 4.8c")
# Looks a lot like the sine function graph

# Problem 4.13
# All code for this problem to calculate PRESS can be found on the following webiste:
# https://www.r-bloggers.com/can-we-do-better-than-r-squared/

setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Appendices")

chem <- read.csv("data-table-B5.csv")

lm.chem <- lm(y~x6 + x7, data = chem)
summary(lm.chem)

qqnorm(residuals(lm.chem))
qqline(residuals(lm.chem))

# Seems to be slightly not normal, with some tails

plot(lm.chem$fitted.values, lm.chem$residuals)
title(main = "Problem 4.13")

# There seems to be a fanning out of points which shows nonconstant variance

# Using the code from the above reference, this is the PRESS Residuals

pr <- residuals(lm.chem)/(1 - lm.influence(lm.chem)$hat)
pr

# This is the PRESS Statistic

PRESS <- sum(pr^2)

# Now for the second model

lm.chem6 <- lm(y~x6, data = chem)
summary(lm.chem6)

qqnorm(residuals(lm.chem6))
qqline(residuals(lm.chem6))

# Seems to be slightly not normal, with some tails

plot(lm.chem6$fitted.values, lm.chem6$residuals)
title(main = "Problem 4.13")

# There seems to be a fanning out of points which shows nonconstant variance

pr6 <- residuals(lm.chem6)/(1 - lm.influence(lm.chem6)$hat)
pr6

PRESS_6 <- sum(pr6^2)

# anova to calculate residual sum of squares
chem.anova <- anova(lm.chem)
tss <- sum(chem.anova$"Sum Sq")
# predictive R^2
pred.r.squared <- 1 - PRESS/(tss)
pred.r.squared

# This shows the R squared for the PRESS is 0.5694 which is less than the adjusted R squared for the model which was 0.6746

# anova to calculate residual sum of squares
chem6.anova <- anova(lm.chem6)
tss_6 <- sum(chem6.anova$"Sum Sq")
# predictive R^2
pred.r.squared_6 <- 1 - PRESS/(tss_6)
pred.r.squared_6

# Again we see that the PRESS R squared is 0.5694 which is the same as the other PRESS but also less than the adjusted 
# R squared term which is 0.6219. Therefore, no conclusion can be made about the best choice for this model.

# Problem 4.25
setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Appendices")

life <- read.csv("data-table-B-16.csv")

# Part a

lf <- lm(LifeExp ~ People.per.TV + People.per.Dr, data = life)
summary(lf)

qqnorm(residuals(lf))
qqline(residuals(lf))

# This shows a slight deviation from normality with tails

lfm <- lm(LifeExpMale ~ People.per.TV + People.per.Dr, data = life)
summary(lfm)

qqnorm(residuals(lfm))
qqline(residuals(lfm))

# This also shows a slight deviation which means not normal due to tails deviating from line

lff <- lm(LifeExpFemale ~ People.per.TV + People.per.Dr, data = life)
summary(lff)

qqnorm(residuals(lff))
qqline(residuals(lff))

# Shows maybe a slight problem with normality but has light tails which are not bad.
# We can work with light tails.

# Part b

plot(lf$fitted.values, lf$residuals)
title(main = "Problem 4.29b")

plot(lfm$fitted.values, lfm$residuals)
title(main = "Problem 4.29b")

plot(lff$fitted.values, lff$residuals)
title(main = "Problem 4.29b")

# Well all three plots have their points clustered to the far right which means there does not exist a linear pattern

# Problem 4.29

air <- read.csv("data_table_B20.csv")

lm.dt20 <- lm(y~., data = air)
summary(lm.dt20)

qqnorm(residuals(lm.dt20))
qqline(residuals(lm.dt20))

# Due to the weaveing above and below the normal fit line, there is no normality in this model.

plot(lm.dt20$fitted.values, lm.dt20$residuals)
title(main = "Problem 4.29")

# But after viewing the deleted residuals, we can see a nonlinear pattern
# The book says this may be that the model does not fit the data well. 
# So there may be a problem with which model we chose. 

# Problem 5.2

setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Chapter 5/Problems")

dt <- read.csv("data-prob-5-2.csv")

plot(dt$temp, dt$vapor)
title(main = "Problem 5.2")

# This does not seem linear at all!! more like half of a hyperbolic graph

# Part b

lm.5.2 <- lm(temp~vapor, data = dt)
summary(lm.5.2)

# y = 298.747 + 0.11937x1

qqnorm(residuals(lm.5.2))
qqline(residuals(lm.5.2))

# There is a pretty good normality going on.

plot(lm.5.2$fitted.values, lm.5.2$residuals)
title(main = "Problem 5.2b")

# Not so linear relationship which is expected. 

# Part c
dt$vapor <-log(dt$vapor) 
dt$temp <- -1/(dt$temp)

lm.tran <- lm((vapor)~temp, data = dt)
summary(lm.tran)

qqnorm(residuals(lm.tran))
qqline(residuals(lm.tran))

# There is an improvement in the normality of the model

plot(lm.tran$fitted.values, lm.tran$residuals)
title(main = "Problem 5.2c")

# The graph of the residuals looks like a constant inverse hyperbolic graph and the Q-Q plot looks normal.

# Problem 5.5
# Part a

tb <- read.csv("data-prob-5-5.csv")

lm.tb <- lm(defects~weeks, data=tb)
summary(lm.tb)

qqnorm(residuals(lm.tb))
qqline(residuals(lm.tb))

plot(lm.tb$fitted.values, lm.tb$residuals)
title(main = "Problem 5.5a")

# So these plots do not look like they would fit a linear model at all. They look parabolic

# Part b

boxcox(lm.tb)
title(main = "Problem 5.5b")

# Since the Box-Cox model shows the confidence interval for lambda to be around 0, we will look at the log transform of this model

lm.log.tb <- lm(log(defects)~log(weeks), data = tb)
summary(lm.log.tb)

qqnorm(residuals(lm.log.tb))
qqline(residuals(lm.log.tb))

plot(lm.log.tb$fitted.values, lm.log.tb$residuals)
title(main = "Problem 5.5b")

# This transform visually doesn't help that much but we can see the clusters more around the line x=0 and not as a parabolic pattern.

# Problem 5.7

setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Appendices")

air <- read.csv("data_table_B20.csv")

lm.dt20 <- lm(y~., data = air)
summary(lm.dt20)

qqnorm(residuals(lm.dt20))
qqline(residuals(lm.dt20))

plot(lm.dt20$fitted.values, lm.dt20$residuals)
title(main = "Problem 5.7")

# I think a transformation would be needed based on the residual plot. There is no normality and consistency in the data around x=0. 

boxcox(lm.dt20)
title(main = "Problem 5.7")

# Since the Box-Cox model is slightly pushed up towards lambda = 1/2, I am going to use a squareroot transform.

lm.log.dt20 <- lm(log(y)~sqrt(x_1)+sqrt(x_2)+sqrt(x_3)+sqrt(x_4)+sqrt(x_5), data = air)
summary(lm.log.dt20)

qqnorm(residuals(lm.log.dt20))
qqline(residuals(lm.log.dt20))

plot(lm.log.dt20$fitted.values, lm.log.dt20$residuals)
title(main = "Problem 5.7")


# Besides the one outlier, the plot looks very closely clustered around x=0 and thus the transform worked.

# Problem 5.9

p_5.9 <- read.csv("data-table-B8.csv")

lm.p_5.9 <- lm(y~., data = p_5.9)
summary(lm.p_5.9)

qqnorm(residuals(lm.p_5.9))
qqline(residuals(lm.p_5.9))

plot(lm.p_5.9$fitted.values, lm.p_5.9$residuals)
title(main = "Problem 5.9")

# This plot seems like it has a curve in the nature of the residual plots and not very tightly clustered around x=0.
# We will look at the Box-Cox Model to see which transformation to use.

# Part b

boxcox(lm.p_5.9)
title(main = "Problem 5.9")

# The Box-Cox model includes lambda = 1 in the confidence interval.
# As stated in class, when 1 is included in the confidence interval in the Box-Cox model, don't transform.
# So I am not transforming this model.

# Problem 5.10

# Part a

p_5.10 <- read.csv("data-table-B9.csv")

lm.p_5.10 <- lm(y~., data = p_5.10)
summary(lm.p_5.10)

qqnorm(residuals(lm.p_5.10))
qqline(residuals(lm.p_5.10))

plot(lm.p_5.10$fitted.values, lm.p_5.10$residuals)
title(main = "Problem 5.10")

# There is no sort of order or pattern to the residuals plot. 

# Part b

boxcox(lm.p_5.10)
title(main = "Problem 5.10b")

# Again, the Box-Cox model has 1 included in its confidence interval so I will not transform this. 

