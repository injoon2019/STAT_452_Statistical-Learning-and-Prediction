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
wind.hilo = (data$Wind < median(data$Wind))
temp.hilo = (data$Temp < median(data$Temp))


head(data.frame(data$Wind, wind.hilo, data$Temp, temp.hilo))
tail(data.frame(data$Wind, wind.hilo, data$Temp, temp.hilo))

#3. Fit a linear regression with the two region variables.
mod.2step = lm(data$Ozone ~ wind.hilo + temp.hilo)

#(a) Report the results from summary().
summary(mod.2step)


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


open3d()
plot3d(data$Ozone ~ data$Wind + data$Temp, col="blue")

x1 <- seq(from=2.2, to=21, by=.05)
x2 = seq(from=46, to=98, by=.5)
xy1 <- data.frame(expand.grid(Wind=x1, Temp=x2))

xy1c = data.frame(wind.hilo = (xy1$Wind < median(data$Wind)),
                  temp.hilo = (xy1$Temp < median(data$Temp)))

pred2 <- predict(mod.2step ,newdata=xy1c)
surface2 = matrix(pred2, nrow=length(x1))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Ozone")
points3d(data$Ozone ~ data$Wind + data$, col="blue")




####################################################
# 4. Add the interaction of the two region variables to the model
# (a) Report the results from summary().
mod.2step2 = lm(data$Ozone ~ wind.hilo + temp.hilo + wind.hilo * temp.hilo)
summary(mod.2step2)
# (b) Does the interaction have statistically significant influence on the mean ozone level
# at the 5% Type 1 error rate? Report the p-values and your conclusion. (No hypotheses needed.)

# (c) Make a 3-D plot of the surface. Report a screenshot from some angle that
# shows the whole surface and describe how the interaction affects the
# surface (use one sentence).

x1 <- seq(from=2.2, to=21, by=.05)
x2 = seq(from=46, to=98, by=.5)
xy1 <- data.frame(expand.grid(Wind=x1, Temp=x2))

xy1c = data.frame(wind.hilo = (xy1$Wind < median(data$Wind)),
                  temp.hilo = (xy1$Temp < median(data$Temp)))

pred2 <- predict(mod.2step2 ,newdata=xy1c)
surface2 = matrix(pred2, nrow=length(x1))

library(rgl)  
open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Ozone")
points3d(data$Ozone ~ data$Wind + data$Temp, col="blue")


