# Title: STAT 452 Exercise 7 L11Q2
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

par(mfrow = c(1,1))
#################################################
#################################################
#################################################


get.folds = function(n, K) {
  ### Get the appropriate number of fold labels
  n.fold = ceiling(n / K) # Number of observations per fold (rounded up)
  fold.ids.raw = rep(1:K, times = n.fold) # Generate extra labels
  fold.ids = fold.ids.raw[1:n] # Keep only the correct number of labels
  
  ### Shuffle the fold labels
  folds.rand = fold.ids[sample.int(n)]
  
  return(folds.rand)
}


### Number of folds
K = 10

### Construct folds
n = nrow(data) # Sample size
folds = get.folds(n, K)

### Create a container for MSPEs. Let's include ordinary least-squares
### regression for reference
all.models = c("LS", "Hybrid", "Ridge", "LASSO-Min", "LASSO-1se", "GAM")
all.MSPEs = array(0, dim = c(K, length(all.models)))
colnames(all.MSPEs) = all.models
### Begin cross-validation
for(i in 1:K){
  ### Split data
  data.train = data[folds != i,]
  data.valid = data[folds == i,]
  n.train = nrow(data.train)
  
  ### Get response vectors
  Y.train = data.train$Ozone
  Y.valid = data.valid$Ozone
  
  # LS
  fit.ls = lm(Ozone ~ ., data = data.train)
  pred.ls = predict(fit.ls, newdata = data.valid)
  MSPE.ls = get.MSPE(Y.valid, pred.ls)
  all.MSPEs[i, "LS"] = MSPE.ls
  
  #Hybrid Stepwise
  
  fit.start = lm(Ozone ~ 1, data = data.train)
  fit.end = lm(Ozone ~ ., data = data.train)
  
  step.BIC = step(fit.start, list(upper = fit.end), k = log(n.train),
                  trace = 0)
  
  pred.step.BIC = predict(step.BIC, data.valid)
  
  err.step.BIC = get.MSPE(Y.valid, pred.step.BIC)
  
  all.MSPEs[i, "Hybrid"] = err.step.BIC
  
  
  
  #ridge regression
  lambda.vals = seq(from = 0, to = 100, by = 0.05)
  
  fit.ridge = lm.ridge(Ozone ~ ., lambda = lambda.vals, 
                       data = data.train)
  
  ind.min.GCV = which.min(fit.ridge$GCV)
  lambda.min = lambda.vals[ind.min.GCV]
  
  all.coefs.ridge = coef(fit.ridge)
  coef.min = all.coefs.ridge[ind.min.GCV,]
  
  matrix.valid.ridge = model.matrix(Ozone ~ ., data = data.valid)
  
  ### Now we can multiply the data by our coefficient vector. The
  ### syntax in R for matrix-vector multiplication is %*%. Note that,
  ### for this type of multiplication, order matters. That is,
  ### A %*% B != B %*% A. Make sure you do data %*% coefficients.
  ### For more information, see me in a Q&A session or, better still,
  ### take a course on linear algebra (it's really neat stuff)
  pred.ridge = matrix.valid.ridge %*% coef.min
  
  ### Now we just need to calculate the MSPE and store it
  MSPE.ridge = get.MSPE(Y.valid, pred.ridge)
  all.MSPEs[i, "Ridge"] = MSPE.ridge
  
  matrix.train.raw = model.matrix(Ozone ~ ., data = data.train)
  matrix.train = matrix.train.raw[,-1]
  
  ### LASSO
  all.LASSOs = cv.glmnet(x = matrix.train, y = Y.train)
  
  ### Get both 'best' lambda values using $lambda.min and $lambda.1se
  lambda.min = all.LASSOs$lambda.min
  lambda.1se = all.LASSOs$lambda.1se
  
  ### Get the coefficients for our two 'best' LASSO models
  coef.LASSO.min = predict(all.LASSOs, s = lambda.min, type = "coef")
  coef.LASSO.1se = predict(all.LASSOs, s = lambda.1se, type = "coef")
  
  ### Get which predictors are included in our models (i.e. which 
  ### predictors have non-zero coefficients)
  included.LASSO.min = predict(all.LASSOs, s = lambda.min, 
                               type = "nonzero")
  included.LASSO.1se = predict(all.LASSOs, s = lambda.1se, 
                               type = "nonzero")
  
  matrix.valid.LASSO.raw = model.matrix(Ozone ~ ., data = data.valid)
  matrix.valid.LASSO = matrix.valid.LASSO.raw[,-1]
  pred.LASSO.min = predict(all.LASSOs, newx = matrix.valid.LASSO,
                           s = lambda.min, type = "response")
  pred.LASSO.1se = predict(all.LASSOs, newx = matrix.valid.LASSO,
                           s = lambda.1se, type = "response")
  
  ### Calculate MSPEs and store them
  MSPE.LASSO.min = get.MSPE(Y.valid, pred.LASSO.min)
  all.MSPEs[i, "LASSO-Min"] = MSPE.LASSO.min
  
  MSPE.LASSO.1se = get.MSPE(Y.valid, pred.LASSO.1se)
  all.MSPEs[i, "LASSO-1se"] = MSPE.LASSO.1se
  
  ## GAM
  fit.gam = gam(Ozone ~ s(Solar.R) + s(Wind) + s(Temp) + s(TWcp) + s(TWrat),
                data = data.train)
  
  pred.gam = predict(fit.gam, data.valid)
  MSPE.gam = get.MSPE(Y.valid, pred.gam) # Our helper function
  all.MSPEs[i, "GAM"] = MSPE.gam
}

all.MSPEs

### Make a boxplot of MSPEs. I would like to include the number of folds
### in the title. This can be done by using the paste0() function,
### which concatenates strings (i.e. attaches them end-to-end), and
### can be provided numeric variables.
boxplot(all.MSPEs, main = paste0("CV MSPEs over ", K, " folds"))



### Calculate RMSPEs
all.RMSPEs = apply(all.MSPEs, 1, function(W){
  best = min(W)
  return(W / best)
})
all.RMSPEs = t(all.RMSPEs)

### Make a boxplot of RMSPEs
boxplot(all.RMSPEs, main = paste0("CV RMSPEs over ", K, " folds"))