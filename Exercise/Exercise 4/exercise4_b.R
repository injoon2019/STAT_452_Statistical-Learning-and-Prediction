# Title: Exercise 5
# Author: Injun Son
# Date: October 9, 2020

#import dataset
library(dplyr)

#B. Categorical Explanatories

ins = read.csv("Insurance.csv", header=TRUE)
ins$zone = as.factor(ins$zone)
ins$make = as.factor(ins$make)

ins = ins[ins$claims >0, ]

# (a) Create a summary of the lm object.
model = lm(per~ km + zone+ bonus+ make + insured+ claims, data= ins)
summary(model)

#i. Although you fit a model with 6 variables, how many parameters are
#estimated?
#-> km, zone (1~7), bonus, make(1~9), insured, claims = 1+7+1+9+1+1 = 20

#ii. What is the intercept of the regression when make and zone are both
#at their first level, 1?
#-> 11.86

#iii. What is the intercept of the regression when make and zone are both
#at their last levels, 9 and 7, respectively?
# -> 11.86 + (-2.862) + (1.459) = 10.457