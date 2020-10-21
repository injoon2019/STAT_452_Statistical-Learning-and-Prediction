library(dplyr)
library(MASS)   # For ridge regression
library(glmnet) # For LASSO
source("Helper Functions.R")
data = na.omit(airquality[, 1:4])
data$TWcp = data$Temp*data$Wind
data$TWrat = data$Temp/data$Wind

#1. Find and report the median value for wind speed and temperature
wind.med = median(data$Wind)
temp.med = median(data$Temp)

#2. Use this median value to create high and low regions on both variables. Show values
#for Temp, Wind, and the two high-low region factors for these variables.
wind.split.med = with(data, Wind >= wind.med)
temp.split.med = with(data, Temp >= temp.med)

data.split.med = data.frame(Ozone = data$Ozone, Wind = data$Wind, Temp = data$Temp,  wind.split = wind.split.med, 
                            temp.split = temp.split.med)

tmp = data.split.med
tmp

#3. Fit a linear regression with the two region variables.
fit.step.2 = lm(Ozone ~wind.split, temp.split, data = data.split.med)

#(a) Report the results from summary().
summary(fit.step.2)


#(b) Do the two variables have statistically significant influence on the mean ozone level
#at the 5% Type 1 error rate? Report their p-values and your conclusion.
#(No hypotheses needed.)
#-> Both p-values < 0.05, so both are statistically significant 

#(c) Make a 3-D plot of the surface. Report a screenshot from some angle that
#shows the whole surface and describe how the surface changes with
#each variable (use one short sentence each).
with(data, plot3d(Ozone ~ Wind + Temp))

#Wind range: 2.3~ 20.7
#Temp range: 47~ 97


### Construct a grid of values for the two explanatories
vals.wind = seq(from = 2.2, to = 20.8, by = 0.02)
vals.temp = seq(from = 46, to = 98, by = 0.01)
vals.grid = expand.grid(wind = vals.wind, temp = vals.temp)

### Convert this grid of values to a grid of indicators
### The transmute function lets us define new variables, and drops all
### the variables we started with.
split.grid = transmute(vals.grid, wind.split = wind >= wind.med,
                       temp.split = temp >= temp.med)

### Get predicted values on the grid of indicators
pred.step.2 = predict(fit.step.2, split.grid)

### Plot predicted values and points
persp3d(x = vals.wind, y = vals.temp, z = pred.step.2,
        xlab = "wind", ylab = "temp", zlab = "Ozone", col = "orange")
with(data, points3d(Ozone ~ Wind + Temp, col = "blue"))




####################################################
# 4. Add the interaction of the two region variables to the model
# (a) Report the results from summary().
fit.step.3 = lm(Ozone ~wind.split, temp.split, wind.split*temp.split, data = data.split.med)
summary(fit.step.3)
# (b) Does the interaction have statistically significant influence on the mean ozone level
# at the 5% Type 1 error rate? Report the p-values and your conclusion. (No
#                                                                        hypotheses needed.)
# (c) Make a 3-D plot of the surface. Report a screenshot from some angle that
# shows the whole surface and describe how the interaction affects the
# surface (use one sentence).
