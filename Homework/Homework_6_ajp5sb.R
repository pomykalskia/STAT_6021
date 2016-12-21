# Homework 6
# Andrew Pomykalski
# STAT 6021
# Numbers  9.7, 9.13, 9.14, 9.15, 9.19, 9.20, 10.1, 10.2, 10.14, 10.15, 10.16

setwd("C:/Users/Andrew Pomykalski/Desktop/STAT 6021 - Linear Models/linear_regression_5e_data_sets/linear_regression_5e_data_sets/Appendices")

library(readr)
library(plyr)
library(MASS)
library(car)

# Problem 9.7 

gas <- read.csv("data-table-B3.csv")

# Part a

lm.gas <- lm(y~., data = gas)
summary(lm.gas)
cor(gas[,2:12])

#           x1         x2 x3          x4         x5          x6         x7         x8         x9        x10        x11
#x1   1.0000000  0.9452080 NA -0.33015370 -0.6315968  0.65906008 -0.7814778  0.8551981  0.8013975  0.9456621  0.8354239
#x2   0.9452080  1.0000000 NA -0.29205832 -0.5170425  0.77190992 -0.6431558  0.7973892  0.7176056  0.8834004  0.7266835
#x3          NA         NA  1          NA         NA          NA         NA         NA         NA         NA         NA
#x4  -0.3301537 -0.2920583 NA  1.00000000  0.3737462 -0.04933889  0.4938104 -0.2581079 -0.3187643 -0.2772185 -0.3683612
#x5  -0.6315968 -0.5170425 NA  0.37374620  1.0000000 -0.20535194  0.8428620 -0.5481227 -0.4343576 -0.5424247 -0.7032485
#x6   0.6590601  0.7719099 NA -0.04933889 -0.2053519  1.00000000 -0.3005751  0.4251881  0.3156727  0.5206424  0.4173378
#x7  -0.7814778 -0.6431558 NA  0.49381043  0.8428620 -0.30057509  1.0000000 -0.6630802 -0.6682373 -0.7178265 -0.8549981
#x8   0.8551981  0.7973892 NA -0.25810785 -0.5481227  0.42518809 -0.6630802  1.0000000  0.8849771  0.9475859  0.6863079
#x9   0.8013975  0.7176056 NA -0.31876434 -0.4343576  0.31567268 -0.6682373  0.8849771  1.0000000  0.9015431  0.6507213
#x10  0.9456621  0.8834004 NA -0.27721850 -0.5424247  0.52064243 -0.7178265  0.9475859  0.9015431  1.0000000  0.7722283
#x11  0.8354239  0.7266835 NA -0.36836123 -0.7032485  0.41733783 -0.8549981  0.6863079  0.6507213  0.7722283  1.0000000

# Yes there are signs of multicollinearity. 

# Part b

lm.gas <- lm(y~., data = gas)

vif(lm.gas)

# x1         x2         x3         x4         x5         x6         x7         x8         x9        x10        x11 
#119.487804  42.800811 149.234409   2.060036   7.729187   5.324730  11.761341  20.917632   9.397108  85.744344   5.145052 

# There are some huge VIF's that are much much greater than 5. 
# Since several VIF's are greater than 5, I removed the one that was the greatest. 
# And then did that for every step after that. 

lm.gas1 <- lm(y~.-x3, data = gas)

vif(lm.gas1)

#x1        x2        x4        x5        x6        x7        x8        x9       x10       x11 
#63.397390 17.145579  1.919138  6.566138  4.736544 11.543634 20.848576  9.038530 85.304744  5.144754 

lm.gas2 <- lm(y~.-x3-x10, data = gas)

vif(lm.gas2)

#x1        x2        x4        x5        x6        x7        x8        x9       x11 
#26.031194 17.082672  1.786564  6.279378  4.531227 11.001978  7.621923  7.657264  5.137781 

lm.gas3 <- lm(y~.-x3-x10-x1, data = gas)
vif(lm.gas3)

#x2        x4        x5        x6        x7        x8        x9       x11 
#10.244664  1.739429  6.233270  4.436907 10.539237  7.378676  7.140246  4.505503 

lm.gas4 <- lm(y~.-x3-x10-x1-x7, data = gas)
vif(lm.gas4)

#x2       x4       x5       x6       x8       x9      x11 
#9.757124 1.487271 2.707042 4.204821 7.122145 5.525053 3.566002 

lm.gas5 <- lm(y~.-x3-x10-x1-x7-x2, data = gas)
vif(lm.gas5)

#x4       x5       x6       x8       x9      x11 
#1.418840 2.619416 1.407971 6.180991 5.309895 3.499426 

lm.gas6 <- lm(y~.-x3-x10-x1-x7-x2-x8, data = gas)
summary(lm.gas6)
vif(lm.gas6)

#x4       x5       x6       x9      x11 
#1.385608 2.399965 1.296448 1.729709 3.490515 

# Now that we see that all VIF's are below 5, the final model is listed below.
# y = 32.8321 + 2.4375x4 + 3.-961x5 - 1.4251x6 - 0.5336x9 - 1.0335x11

# Problem 9.13

fuel <- read.csv("data-table-B18.csv")

lm.fuel <- lm(y~., data = fuel)
summary(lm.fuel)

#             Estimate Std. Error t value Pr(>|t|)
#(Intercept) -312.8951  1696.8901  -0.184    0.859
#x_1           -4.2500     4.8320  -0.880    0.408
#x_2            0.1587     0.9630   0.165    0.874
#x_3            1.0272     2.9022   0.354    0.734
#x_4           -8.6450    45.2899  -0.191    0.854
#x_5           -0.4320     0.9250  -0.467    0.655
#x_6           -0.1368     1.1858  -0.115    0.911
#x_7           -0.3184     3.2783  -0.097    0.925
#x_8           -0.5242     2.2450  -0.233    0.822

vif(lm.fuel)

#      x_1        x_2        x_3        x_4        x_5        x_6        x_7        x_8 
# 1.000000   1.900541 168.467420  43.103776  60.791320 275.472571 185.707184  44.363364 

# Several VIF's are way over 5. Thus we will begin by taking out the largest first, VIF(x_6) = 275.5

lm.fuel2 <- lm(y~.-x_6, data = fuel)

vif(lm.fuel2)

#x_1       x_2       x_3       x_4       x_5       x_7       x_8 
#1.000000  1.633947 37.967038 20.013043 13.288706 10.705340 16.504395 
# Still several over 5. Next we will remove x_3 since VIF(x_3) = 37.97

lm.fuel3 <- lm(y~.-x_6-x_3, data = fuel)

vif(lm.fuel3)

#     x_1       x_2       x_4       x_5       x_7       x_8 
# 1.000000  1.582158  1.926920  9.377199 10.411144  2.011522 
# Still some VIF's over 5. Remove x_7 whose VIF = 10.41

lm.fuel4 <- lm(y~.-x_6-x_3-x_7, data = fuel)
summary(lm.fuel4)
vif(lm.fuel4)

# x_1      x_2      x_4      x_5      x_8 
# 1.000000 1.478233 1.257411 1.798838 1.998343

# All the VIF's are less than 5 thus this is the final model

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 386.4095    55.7205   6.935 4.02e-05 ***
#  x_1          -4.2500     4.1275  -1.030    0.327    
#x_2           0.1099     0.7254   0.151    0.883    
#x_4           2.9769     6.6076   0.451    0.662    
#x_5          -0.2215     0.1359  -1.630    0.134    
#x_8           0.3622     0.4070   0.890    0.394  

# y = 386.409 - 4.25x_1 + 0.110x_2 + 2.977x_4 - 0.222x_5 + 0.362x_8

# Problem 9.14

wine <- read.csv("data-table-B19.csv")

lm.wine <- lm(y~., data = wine)
summary(lm.wine)

# Coefficients: (2 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)  
#(Intercept) -12.20843   14.61153  -0.836   0.4120  
#x_1          -0.84577    0.58596  -1.443   0.1624  
#x_2           7.41839    3.51235   2.112   0.0457 *
#x_3           0.01046    0.00857   1.220   0.2347  
#x_4          -1.94732    2.22110  -0.877   0.3897  
#x_5           4.89518    3.21850   1.521   0.1419  
#x_6          -1.43382    1.81263  -0.791   0.4370  
#x_7                NA         NA      NA       NA  
#x_8         -11.42517    7.88120  -1.450   0.1606  
#x_9          -0.10802    0.22040  -0.490   0.6287  
#x_10               NA         NA      NA       NA  

# For some reason, I am not getting coefficients for x_7 and x_10 so I will remove them from the model. 
# Found this function at http://stackoverflow.com/questions/28885160/vifs-returning-aliased-coefficients-in-r
# Shows which variables are dependent on each other

ld.vars <- attributes(alias(lm.wine)$Complete)$dimnames[[1]]

# Thus I will remove the two variables and continue.

lm.wine2 <- lm(y~.-x_10-x_7, data = wine)
summary(lm.wine2)

vif(lm.wine2)

#      x_1        x_2        x_3        x_4        x_5        x_6        x_8        x_9 
# 1.970605   4.092086   4.513202 603.518791 511.870261  33.319560   7.930630  36.170717 

# Several VIF's are over 5. Thus we will remove the highest, VIF(x_4)=603.52

lm.wine3 <- lm(y~.-x_7-x_10-x_4, data = wine)
summary(lm.wine3)

vif(lm.wine3)

#     x_1       x_2       x_3       x_5       x_6       x_8       x_9 
# 1.774999  3.519898  4.131455 62.068056 17.840078  7.876878 35.369418 

# Still several over 5. Removing VIF(x_5)=62.07

lm.wine4 <- lm(y~.-x_7-x_10-x_4-x_5, data = wine)
summary(lm.wine4)

vif(lm.wine4)

#     x_1      x_2      x_3      x_6      x_8      x_9 
# 1.701595 3.239235 4.075646 4.463109 1.825523 7.379177 

# We still see that VIF(x_9) = 7.38 >5. thus removing it

lm.wine5 <- lm(y~.-x_7-x_10-x_4-x_5-x_9, data = wine)
summary(lm.wine5)

vif(lm.wine5)

#     x_1      x_2      x_3      x_6      x_8 
# 1.033593 1.887984 2.393264 1.151705 1.505924 

# All VIF's are less than 5. Thus the final model is:

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  7.004406  10.848420   0.646 0.524157    
#x_1         -0.172390   0.509609  -0.338 0.737869    
#x_2          1.828979   2.864986   0.638 0.528803    
#x_3         -0.001735   0.007494  -0.232 0.818700    
#x_6          1.568037   0.404694   3.875 0.000648 ***
#x_8         -3.099345   4.124176  -0.752 0.459097 

# y = 7.004 - 0.172x_1+1.83x_2 - 0.002x_3 + 1.568x_6 - 3.099x_8

# Problem 9.15

oxy <- read.csv("data-table-B20.csv")

lm.oxy <- lm(y~., data = oxy)
summary(lm.oxy)

vif(lm.oxy)

#x_1       x_2       x_3       x_4       x_5 
#1.519064 26.283999 26.447032  2.202201  1.922689 

# Some VIF's are greater than 5. So since VIF(x_3) = 26.45, we will remove it from the model.

lm.oxy2 <- lm(y~.-x_3, data = oxy)
summary(lm.oxy2)

vif(lm.oxy2)

#     x_1      x_2      x_4      x_5 
# 1.439308 1.173061 2.047208 1.617418 

# All VIF's are less than 5 thus all remaining variables will stay in model

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -639.0875   110.9988  -5.758 6.63e-05 ***
#  x_1           28.7137    10.1527   2.828   0.0142 *  
#  x_2            1.2809     0.2087   6.137 3.56e-05 ***
#  x_4            4.8927     5.4963   0.890   0.3895    
#  x_5           10.2142    10.1001   1.011   0.3303 

# y = -639.087 + 28.714x_1 + 1.281x_2 + 4.893x_4 + 10.214x_5

# Problem 9.19

# Part a
# From the professor's code:
sdata.m <- as.matrix(gas)
sdata.m <- na.omit(sdata.m)
library(glmnet)
s.ridge <- glmnet(sdata.m[,2:12], sdata.m[,1], alpha=0)

## Create the ridge trace plot
plot(s.ridge,xvar="lambda",label=TRUE)

# Plot of lambda values

gas.m <- as.data.frame(sdata.m)
model.9.19 <- lm(y~., data=gas.m)
summary(model.9.19)
anova(model.9.19) # RSS = 187.40

# Part b
# looking for the RSS 

model.9.19$coefficients <- as.vector(coef(glmnet(sdata.m[,2:12], sdata.m[,1], alpha=0, lambda=5.01)))
sum((gas.m$y -as.vector(predict(model.9.19, gas.m)))^2) #RSS 275.9966

# Comparing the RSS from the orignial to the ridge regressed model, there an increase of almost 100 from 187.4 to 275.99.

# Part c

rss <- sum((gas.m$y-as.vector(predict(model.9.19, gas.m)))^2)
tss<- sum((gas.m$y-mean(gas.m$y))^2)
r_2 <- (tss-rss)/tss
r_2 # 0.7577075

# This is the R squared of the ridge regressed model

gas.lm <- lm(y~., data = gas)
summary(gas.lm)$r.squared # R squared of the original model 0.8354843

# So we can see that my ridge regression significantly reduces my R squared it does not improve it

# Problem 9.20

beta <- as.matrix(summary(lm.gas)$coefficients[,1])
p <- ncol(gas) # p = k+1
var <- summary(lm.gas)$sigma
lambda <- (p*var)/(t(beta)%*%beta) # 0.111
gas.ridge <- glmnet(sdata.m[,2:12], sdata.m[,1], alpha = 0, lambda = lambda)
coef(gas.ridge)

# Yes, this model differs tremendously. The lambda here is 0.111 and not the lambda I got in 9.19.

# Problem 10.1

football <- read.csv("data-table-B1.csv")

s.null <- lm(y~1, data=football)
s.full <- lm(y~., data=football)

# Part a

step(s.null, scope=list(lower=s.null, upper=s.full), direction="forward")

# Shown below is the final step in ttheh forward process. 
# The final model has x2, x7, x8, x9 in it. 

#Step:  AIC=33.58
#y ~ x8 + x2 + x7 + x9
#
#Df Sum of Sq    RSS    AIC
#<none>              65.004 33.583
#+ x1    1   1.86452 63.140 34.768
#+ x4    1   1.74260 63.262 34.822
#+ x3    1   0.70148 64.303 35.279
#+ x6    1   0.45071 64.554 35.388
#+ x5    1   0.32667 64.678 35.442
#
#Call:
#  lm(formula = y ~ x8 + x2 + x7 + x9, data = football)
#
#Coefficients:
#  (Intercept)           x8           x2           x7           x9  
#-1.821703    -0.004015     0.003819     0.216894    -0.001635  


football.f <- lm(y~x2+x7+x8+x9, data = football)
summary(football.f)

# The final model is y = -1.82 + 0.004x2 + 0.22x7 - 0.004x8 - 0.002x9

# Part b

step(s.full, scope=list(lower=s.null, upper=s.full), direction="backward")

# The backward step pulled out the same final model. 

#Step:  AIC=33.58
#y ~ x2 + x7 + x8 + x9
#
#Df Sum of Sq     RSS    AIC
#<none>               65.004 33.583
#- x9    1     4.866  69.870 33.604
#- x7    1    16.908  81.913 38.057
#- x8    1    23.299  88.303 40.160
#- x2    1    82.892 147.897 54.601
#
#Call:
#  lm(formula = y ~ x8 + x2 + x7 + x9, data = football)
#
#Coefficients:
#  (Intercept)           x8           x2           x7           x9  
#-1.821703    -0.004015     0.003819     0.216894    -0.001635  

football.f <- lm(y~x2+x7+x8+x9, data = football)
summary(football.f)

# The final model is y = -1.82 + 0.004x2 + 0.22x7 - 0.004x8 - 0.002x9

# Part c
step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")

# And we have the same model as the first two parts!

# Step:  AIC=33.58
#y ~ x8 + x2 + x7 + x9
#
#Df Sum of Sq     RSS    AIC
#<none>               65.004 33.583
#- x9    1     4.866  69.870 33.604
#+ x1    1     1.865  63.140 34.768
#+ x4    1     1.743  63.262 34.822
#+ x3    1     0.701  64.303 35.279
#+ x6    1     0.451  64.554 35.388
#+ x5    1     0.327  64.678 35.442
#- x7    1    16.908  81.913 38.057
#- x8    1    23.299  88.303 40.160
#- x2    1    82.892 147.897 54.601
#
#Call:
#  lm(formula = y ~ x8 + x2 + x7 + x9, data = football)
#
#Coefficients:
#  (Intercept)           x8           x2           x7           x9  
#-1.821703    -0.004015     0.003819     0.216894    -0.001635  

football.f <- lm(y~x2+x7+x8+x9, data = football)
summary(football.f)

# The final model is y = -1.82 + 0.004x2 + 0.22x7 - 0.004x8 - 0.002x9

# Problem 10.2

# I will use Comparative model selection
library(leaps)
bestmod <- regsubsets(y~x1+x2+x4+x7+x8+x9, data=football)

summary(bestmod)

summary(bestmod)$rss

summary(bestmod)$adjr2

summary(bestmod)$cp

# I would say the fourth model with x2, x7, x8, x9 is the best.
# It has the second lowest C_p at 4.038492
# It has the highest adjusted R squared at .766123
# It has the third lowest RSS which is lesser when we add variables anyways

# Problem 10.14

# Part a

pn_wine <- read.csv("data-table-B11.csv")
pn_wine$Region <- as.factor(pn_wine$Region)
bestmod_wine <- regsubsets(Quality~., data=pn_wine)

summary(bestmod_wine)

summary(bestmod_wine)$cp

min(summary(bestmod_wine)$cp)

# The model with the lowest C_p has a C_p of 2.240659 which is the model with Flavor, Oakiness, and Region

# Part b

# The model with the next lowest C_p has a C_p of 2.473672 which is the model with Flavor, and Region3

best <- lm(Quality~Flavor+Oakiness+Region, data = pn_wine)
small <- lm(Quality~Flavor+Region,data = pn_wine)

qqnorm(resid(best))
qqline(resid(best))

plot(best$fitted.values, best$residuals)

qqnorm(resid(small))
qqline(resid(small))

plot(small$fitted.values, small$residuals)

# So it seems as if the plots are very similar and no striking differences. 
# So I would not say there is a practical basis for selecting between these two models. 

# Part c

pr <- resid(best)/(1 - lm.influence(best)$hat)
sum(pr^2) # 33.08173

pr <- resid(small)/(1 - lm.influence(small)$hat)
sum(pr^2) # 33.99346

# There is a very small difference between the two PRESS statistics. 

# Problem 10.15

wine.null <- lm(Quality~1, data=pn_wine)
wine.full <- lm(Quality~., data=pn_wine)

step(wine.null, scope=list(lower=wine.null, upper=wine.full), direction="both")

# The best model with stepwise is the model with Flavor, Region, and Oakiness
# This is the same as the model we picked in part a. 

# Problem 10.16

# Part a

bestmod_wine_reg <- regsubsets(Quality~.-Region, data=pn_wine)

summary(bestmod_wine_reg)

summary(bestmod_wine_reg)$cp

# Since model 3 has the lowest C_p = 3.927790 the model I recommend involves Aroma, Flavor, and Oakiness
# The C_p for this model is substantially worse at 3.922790 compared to 2.240659 from 14 part a

# Part b

part_16 <- lm(Quality~Aroma+Flavor+Oakiness, data = pn_wine)
summary(part_16)

# y = 6.4672 + 0.5801x2 + 1.1997x4 - 0.6023x5
# Where x2 = Aroma, x4 = Flavor, x5 = Oakiness

final <- predict(part_16, interval = "confidence")
final

part_14 <- lm(Quality~Flavor+Oakiness+Region, data = pn_wine)
summary(part_14)

# y = 8.1208 + 1.1920x4 - 0.3183x5 - 1.5155x61 + 1.0935x62
# Where x4 = Flavor, x5 = Oakiness, x61 = Region2, and x62 = Region3

final_16 <- predict(part_14, interval = "confidence")
final_16


mean(final_16[,3]-final_16[,2]) # 1.275132

mean(final[,3]-final[,2]) # 1.486928

# Since the average width of the confidence interval for problem 16 is smaller, I would prefer that model.