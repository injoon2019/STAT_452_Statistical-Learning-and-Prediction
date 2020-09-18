# Tutorial 1: Clone Coding
# Injun Son
# September 18, 2020

#Read-in dataset and inspect first few lines
data.in = read.csv("Wine Quality.csv")
head(data.in)

# Extract only some columns

#Method 1: Indexing Columns
print(colnames(data.in))
# We want 4, 8, 9, 10, 11

# When indexing a data frame in R, the format is [rows, columns]
data = data.in[, c(4, 8, 9, 10, 11)]
head(data)

# Method 2: The select() function
# Need dplyr function
# When working with packages, always activate them
#install.packages("dplyr")
library(dplyr)
data = select(data.in, residual.sugar, density, pH, sulphates, alcohol)
head(data)

#change column names
colnames(data)[1] = "sugar"
head(data)

# Scatterplot matrix
pairs(data)

# Let's remove outlier

# Remove Outlier Method 1: Negative Indexing
# find the index of the point with the largest value of sulphates
ind.outlier = which.max(data$sulphates)
# There should be ',' 
data1 = data[-ind.outlier, ]
head(data1)

nrow(data) # Sample size of original dataset
nrow(data1) #Sample size after removing the outlier

# Remove Outlier Method 2: The filter() function
# The filter() function takes your data frame as its first input, then
# all the conditions you want the output data frame to satisfy,
# with different conditions separated by commas.

data1 = filter(data, sulphates < 1.4) # 1.4 comes from the scatterplot matrix

nrow(data)
nrow(data1)

# Now that we have removed outlier, let's make another scatterplot matrix
pairs(data1)

#Let's fit a separate simple linear regression model
# to predict alcohol using each of our explanatory variables
fit.sugar = lm(alcohol ~ sugar, data = data1)
fit.density = lm(alcohol ~ density, data=data1)
fit.pH = lm(alcohol ~ pH, data = data1)
fit.sulphates = lm(alcohol ~ sulphates, data = data1)

#Noe that we've fit these models
summary(fit.sugar)
summary(fit.density)
summary(fit.pH)
summary(fit.sulphates)


# Each of these regression models only concerns two variables
with(data1, plot(sugar, alcohol))
abline(fit.sugar)

with(data1, plot(density, alcohol))
abline(fit.density)

with(data1, plot(pH, alcohol))
abline(fit.pH)

with(data1, plot(sulphates, alcohol))
abline(fit.sulphates)

#These simple linear regression models are great, but they only describe
# a single predictor. Let's build a model with pH and density

#Let's plot in 3D using "rgl"
#install.packages("rgl")
library(rgl)
open3d()
plot3d(data1$alcohol ~ data1$density + data1$pH, col="blue")


fit2 = lm(alcohol ~ density+pH, data=data1)
summary(fit2)


#Finally let's make a new 3d plot and add our regression surface
# First, we construct a grid of values for our two predictors 
#using seq() and expand.grid()
# Next, compue the fitted values of our regression model for all the predictor values in this grid
#Finally, use persp3d() function to plot the regression surface using the predictor and 
#fitted values on our grid

summary(data1) #get a range of values for the predictors
vals.density = seq(from=0.98, to=1.005, by=0.001)
vals.pH = seq(from=2.7, to=4.1, by=0.03)
print(vals.density)
print(vals.pH)

# Create a data frame with all combinations of the predictor values 
pred.grid = data.frame(expand.grid(density = vals.density, pH = vals.pH))


#Get fitted alcohol values for all predictor combinations
#in out grid using the predict() function
pred.alcohol = predict(fit2, newdata = pred.grid)

open3d()
persp3d(x = vals.density, y = vals.pH, z = pred.alcohol, col="orange")
points3d(data1$alcohol ~ data1$density + data1$pH)
