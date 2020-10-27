# Title: STAT 452 Exercise 7 L11Q11
# Author: Injun Son
# Date: October 25, 2020

library(dplyr)
library(MASS)   # For ridge regression
library(glmnet) # For LASSO
library(mgcv)
source("Helper Functions.R")
data = na.omit(airquality[, 1:4])
data$TWcp = data$Temp*data$Wind
data$TWrat = data$Temp/data$Wind

### The gam() function has similar syntax to lm(). Specify a model formula
### and a data frame, but for each predictor, you can optionally put it
### inside a function called s(). See below for a demonstration.
fit.gam = gam(Ozone ~ s(Solar.R) + s(Wind) + s(Temp) + s(TWcp) + s(TWrat),
              data = data)

### Get information about the GAM fit using the summary() function.
summary(fit.gam)


### We can get plots of the smoothers fit to each predictor using the plot()
### function. 
### It would be nice to see all three smoothers simultaneously. You can get
### multiple plots in a grid using the par() function. Inside par(), set
### mfrow equal to a vector of two numbers: the number of rows in your grid,
### then the number of columns in your grid.
par(mfrow = c(3,2))
plot(fit.gam)


### Make sure to reset your plot grid to 1x1 when you're done
par(mfrow = c(1,1))
