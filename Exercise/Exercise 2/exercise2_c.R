# Exercise 2c
# Injun Son
# September 21, 2020

#import dataset
df = data.frame(airquality)
colnames(df)
colnames(df)[2]= "Solar"

#1. We will treat Ozone as the response variable and
#use Temp, Wind, and Solar.R as explanatory. We won¡¯t use Month or Day
AQ = df[, 1:4]

#Create a scatterplot matrix of these four variables
library(dyplyr)
pairs(AQ)
# (a) Relationships of each X with Y
#Ozone and Solar.R looks like has a positive relationship
#Ozone and Wind has a negative relationship
#Ozone and Temp has a more clear positive relationship

# (b) Relationships among the three explanatories.
#Solar and Wind looks like doesn't have a specific relationship
#Solar and Temp might have a positive relationship but not that strong
#Wind and temp has a negative relationship

#2. Run separate simple linear regressions of Ozone against each explanatory variable.
#(a) Report the three slopes and t-values in a table.
fit.solar = lm(Ozone ~ Solar, data = AQ)
fit.wind = lm(Ozone ~ Wind, data = AQ)
fit.temp = lm(Ozone ~ Temp , data = AQ)

summary(fit.solar)
#Solar slope: 0.12717 t-values: 0.000179
summary(fit.wind)
#Wind slope: -5.5508 t-values: 9.27e-13
summary(fit.temp)
#temp slope: 2.4287 t-values: < 2e-16

#(b) Make three separate scatterplots and add the respective regression lines to each
#plot. Present the plots and comment on how well the lines seem to fit each variable.
with(AQ, plot(Solar, Ozone))
abline(fit.solar)

with(AQ, plot(Wind, Ozone))
abline(fit.wind)

with(AQ, plot(Temp, Ozone))
abline(fit.temp)

#3. Make a 3D plot of Ozone against temperature and wind speed. Rotate it around and
#notice to yourself what relationship the ozone might have jointly with temperature
#and wind. Take a screenshot from any angle you think helps you to see most of this
#relationship. No comments are needed.

library(rgl)
open3d()
plot3d(AQ$Ozone ~ AQ$Temp + AQ$Wind, col="blue")


#4. Fit the multiple linear regression that corresponds to this 3D plot.
#(a) Report the slopes and t-values. Are they much different from when they were computed in simple linear regressions?
fit2 = lm(Ozone ~ Temp+Wind, data=AQ)
summary(fit2)
#slope: Temp  1.8402 t-value: 3.15e-11, Wind -3.0555, t-values 1.08e-05
#Yes, there are two t-values and slopes for each variable

#(b) Add the plane surface to the 3D plot. Rotate it around and comment on the quality of the fit. 
#Show a screenshot from some angle that helps to support your comment

summary(AQ) #get a range of values for the predictors
vals.wind = seq(from=1.65, to=20.8, by=0.01)
vals.temp = seq(from=55.5, to=97.5, by=0.03)
#print(vals.wind)
#print(vals.temp)

# Create a data frame with all combinations of the predictor values 
pred.grid = data.frame(expand.grid(Wind = vals.wind, Temp = vals.temp))


#Get fitted alcohol values for all predictor combinations
#in out grid using the predict() function
pred.ozone = predict(fit2, newdata = pred.grid)

open3d()
persp3d(x = vals.wind, y = vals.temp, z = pred.ozone, col="orange")
points3d(AQ$Ozone ~ AQ$Wind + AQ$Temp)
