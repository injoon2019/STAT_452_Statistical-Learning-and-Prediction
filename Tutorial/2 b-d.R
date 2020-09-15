#####################################################################
###                  Tutorial 1: Lecture 2b-d                     ###
### We will analyze a dataset containing various lab measurements ###
### on 500 wines. Our goal will be to predict alcohol content     ###
### using density, pH, sulfates, and residual sugar content       ###
#####################################################################

### Read-in dataset and inspect first few lines
data.in = read.csv("Datasets/Wine Quality.csv")
head(data.in)

### We will focus on just our response, alcohol, and four predictors
### I will show two different ways to extract these. Both get to the
### same result, but sometimes one method will be easier than the
### other.

### Method 1: Indexing columns
print(colnames(data.in))
### We want variables 4, 8, 9, 10, and 11. Use "[]" to extract them.
### When indexing a data frame in R, the format is [rows, columns]
data = data.in[,c(4, 8, 9, 10, 11)]
head(data)

### Method 2: The select() function
### This method uses a function from the dplyr package. You will
### need to install this package if you haven't already (un-comment
### and run the first line of this section to do so). When working
### with packages, you always have to activate them using the 
### library() function, but only once per R file.
###
### The select() function takes your data frame as its first
### input, then the names of the variables you want to keep,
### separated by commas, WITHOUT QUOTES. 
# install.packages("dplyr")
library(dplyr)
data = select(data.in, residual.sugar, density, pH, sulphates, 
  alcohol)
head(data)


### Regardless of which method you used, we now have a data frame
### with only the variables we need. Let's rename the sugar variable
### to save ourselves some typing
colnames(data)[1] = "sugar"
head(data)


### Now onto some actual analysis. Let's make a scatterplot matrix
pairs(data)


### There is an outlier on the sulphates variable. Let's remove this
### point. I will again show two ways to do this, one using base R
### indexing, the other using a function from dplyr. Both methods
### get you to the same place, so use whichever one you prefer 
### (although it is really helpful to know both).

### Method 1: Negative indexing
### First, we will find the index of the point with the largest 
### value of sulphates, then we will remove this point from data
ind.outlier = which.max(data$sulphates)
data1 = data[-ind.outlier,]
head(data1)

nrow(data)  # Sample size of original dataset
nrow(data1) # Sample size after removing the outlier

### Method 2: The filter() function
### This method uses another function from dplyr: filter(). We do
### not need to use the library() function again because we did so
### previously. 
### The filter() function takes your data frame as its
### first input, then all the conditions you want the output 
### data frame to satisfy, with different conditions separated
### by commas.
data1 = filter(data, sulphates < 1.4) # 1.4 comes from the
                                      # scatterplot matrix

nrow(data)  # Sample size of original dataset
nrow(data1) # Sample size after removing the outlier


### Now that we have removed our outlier, let's make another 
### scatterplot matrix
pairs(data1)


### Next, let's fit a separate simple linear regression model
### to predict alcohol using each of our explanatory variables
fit.sugar = lm(alcohol ~ sugar, data = data1)
fit.density = lm(alcohol ~ density, data = data1)
fit.pH = lm(alcohol ~ pH, data = data1)
fit.sulphates = lm(alcohol ~ sulphates, data = data1)

### Now that we've fit these models, let's see how they do
summary(fit.sugar)
summary(fit.density)
summary(fit.pH)
summary(fit.sulphates)

### Question: Does the regression output match what you learned from
###           the scatterplot matrix? If yes, why? If no, how 
###           do they differ?


### Each of these regression models only concerns two variables.
### We can plot that. The with() function can save us some typing
### here. Rather than explain the details, it's best to just
### see the with() function in action. We also use the abline()
### function to add our regression line to the plot
with(data1, plot(sugar, alcohol))
abline(fit.sugar)
with(data1, plot(density, alcohol))
abline(fit.density)
with(data1, plot(pH, alcohol))
abline(fit.pH)
with(data1, plot(sulphates, alcohol))
abline(fit.sulphates)


### These simple linear regression models are great, but they only
### describe a single predictor. Let's see what happens when we 
### build a model with pH and density. 

### It's always good to start with a plot. Plotting in 3 dimensions
### is a bit harder than in 2. Fortunately, there is a package
### for 3d plotting called rgl. You will need to install the 
### rgl package if you haven't alread (un-comment and run my
### first line below). Otherwise, just use library() to activate 
### the package.
### The functions we will use from rgl are called open3d() and
### plot3d(). The first, open3d(), just creates a plotting window.
### The second, plot3d() uses formula syntax, much like lm()
### The plot3d() function takes a formula describing the 
### relationship between vectors, not variables in a data frame,
### so we need to extract the relevant variables (or use with()).
### You can also specify the colour of your plot using a second
### input.
# install.packages("rgl")
library(rgl)
open3d()
plot3d(data1$alcohol ~ data1$density + data1$pH, col = "blue")


### Let's fit a regression model to describe the relationship
### we just saw.
fit2 = lm(alcohol ~ density + pH, data = data1)
summary(fit2)


### Finally, let's make a new 3d plot and add our regression
### surface. This will take a few steps because we have to compute
### the values for the regression surface.
### First, we construct a grid of values for our two predictors
### using seq() and expand.grid(). Next, we compute the fitted 
### values of our regression model for all the predictor values in 
### this grid. Finally, we use the persp3d() function to plot the 
### regression surface using the predictor and fitted values on our 
### grid.
summary(data1) # Get a range of values for the predictors
vals.density = seq(from = 0.98, to = 1.005, by = 0.001)
vals.pH = seq(from = 2.7, to = 4.1, by = 0.03)
print(vals.density)
print(vals.pH)

### Create a data frame with all combinations of the predictor
### values we just made
pred.grid = data.frame(expand.grid(density = vals.density,
                                   pH = vals.pH))
### Get fitted alcohol values for all predictor combinations
### in our grid using the predict() function.
pred.alcohol = predict(fit2, newdata = pred.grid)

### Plot our regression surface using persp3d(), and add in the
### data using points3d()
open3d()
persp3d(x = vals.density, y = vals.pH, z = pred.alcohol, 
  col = "orange")
points3d(data1$alcohol ~ data1$density + data1$pH)



