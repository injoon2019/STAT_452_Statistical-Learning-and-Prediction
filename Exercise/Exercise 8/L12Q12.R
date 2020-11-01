# Title: STAT 452 Exercise 8 L12Q12
# Author: Injun Son
# Date: October 31, 2020

library(dplyr)
library(MASS)   # For ridge regression
library(glmnet) # For LASSO
library(nnet) # Fits neural net models
library(rgl)  
source("Helper Functions.R")
data = na.omit(airquality[, 1:4])
data$TWcp = data$Temp*data$Wind
data$TWrat = data$Temp/data$Wind


# 1. Create a matrix of the five explanatory variables. Rescale each of them to lie between
# 0 and 1 and same them as another matrix. Print a summary() of each object to
# confirm that you have scaled properly.

### 75%/25% split into training and validation sets
n = nrow(data)
n.train = floor(n*0.75)
n.valid = n - n.train

inds = c(rep(1, times = n.train), rep(2, times = n.valid))
inds.rand = inds[sample.int(n)]

data.train = data[inds.rand == 1,]
X.train.raw = data.train[,-1]
Y.train = data.train[,1]

data.valid = data[inds.rand == 2,]
X.valid.raw = data.valid[,-1]
Y.valid = data.valid[,1]

### When fitting neural networks, it's best to scale the training set so
### that all predictors lie between 0 and 1. We then have to apply the same
### scaling to the validation set, so the validation set might have some 
### observations below 0 or above 1. This is fine.
### We can use Tom's function, which scales each column in x1 so that
### the min and max in each column of x2 are 0 and 1 respectively.
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

X.train = rescale(X.train.raw, X.train.raw)
X.valid = rescale(X.valid.raw, X.train.raw) # Be careful with the order

# y.1 = as.matrix(Y.train)
# y.2 = as.matrix(Y.valid)

# Y.train = as.matrix(Y.train)
# Y.valid = as.matrix(Y.valid)

# 2. Fit 4 NNs nets using only Temp and Wind, using each combination of 2 and 6 hidden
# nodes with 0.001 and 1 shrinkage; i,e,; (2,0.001), (2,1), (6,0.001), (6,1)

# (a) Refit each one manually 20 times or more and compute the sMSE each time.
# i. Report the sMSE for the optimal fit for each model.
# ii. Comment on the stability of fits for different models. In other words, which
# models were most/least consistent with the sMSE values produced
# by different fits?


##################################### (2, 0.001)

n.hidden = 2
shrink = 0.001

fit.nnet = nnet(y = Y.train, x = X.train[,c(3,4)], linout = TRUE, size = n.hidden,
                decay = shrink, maxit = 500)

n.nnets = 20 # Number of times to re-fit
### Container for SSEs
all.SSEs = rep(0, times = 20)
all.MSEs = rep(0, times = 20)
all.nnets = list(1:20)
for(i in 1:n.nnets){
  ### Fit model. We can set the input "trace" to FALSE to suppress the
  ### printed output from the nnet() function.
  this.nnet = nnet(y = Y.train, x = X.train[,c(3,4)], linout = TRUE, size = n.hidden,
                   decay = shrink, maxit = 500, trace = FALSE)
  
  ### Get the model's SSE
  this.SSE = this.nnet$value
  this.MSE = this.nnet$value / nrow(X.train)
  
  ### Store results. We have to use double square brackets when storing or
  ### retrieving from a list.
  all.SSEs[i] = this.SSE
  all.MSEs[i] = this.MSE
  all.nnets[[i]] = this.nnet
}
all.MSEs

ind.best = which.min(all.SSEs)
fit.nnet.best = all.nnets[[ind.best]]

x1 <- seq(from=2.3, to=20.7, by=.5)
x2 = seq(from=57, to=97, by=.5)
xy1 <- data.frame(expand.grid(wind=x1, temp=x2))

pred2 <- predict(fit.nnet ,newdata=rescale(xy1, X.valid.raw[,c(3,4)]))
surface2 = matrix(pred2, nrow=length(x1))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Ozone")
points3d(data$Ozone ~ data$Wind + data$Temp, col="blue")


##################################### (2, 1)
n.hidden = 2
shrink = 1

fit.nnet = nnet(y = Y.train, x = X.train[,c(3,4)], linout = TRUE, size = n.hidden,
                decay = shrink, maxit = 500)

n.nnets = 20 # Number of times to re-fit
### Container for SSEs
all.SSEs = rep(0, times = 20)
all.MSEs = rep(0, times = 20)
all.nnets = list(1:20)
for(i in 1:n.nnets){
  ### Fit model. We can set the input "trace" to FALSE to suppress the
  ### printed output from the nnet() function.
  this.nnet = nnet(y = Y.train, x = X.train[,c(3,4)], linout = TRUE, size = n.hidden,
                   decay = shrink, maxit = 500, trace = FALSE)
  
  ### Get the model's SSE
  this.SSE = this.nnet$value
  this.MSE = this.nnet$value / nrow(X.train)
  
  ### Store results. We have to use double square brackets when storing or
  ### retrieving from a list.
  all.SSEs[i] = this.SSE
  all.MSEs[i] = this.MSE
  all.nnets[[i]] = this.nnet
}
all.MSEs
ind.best = which.min(all.SSEs)
fit.nnet.best = all.nnets[[ind.best]]

x1 <- seq(from=2.3, to=20.7, by=.5)
x2 = seq(from=57, to=97, by=.5)
xy1 <- data.frame(expand.grid(wind=x1, temp=x2))

pred2 <- predict(fit.nnet ,newdata=rescale(xy1, X.valid.raw[,c(3,4)]))
surface2 = matrix(pred2, nrow=length(x1))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Ozone")
points3d(data$Ozone ~ data$Wind + data$Temp, col="blue")



##################################### (6, 0.001)
n.hidden = 6
shrink = 0.001

fit.nnet = nnet(y = Y.train, x = X.train[,c(3,4)], linout = TRUE, size = n.hidden,
                decay = shrink, maxit = 500)

n.nnets = 20 # Number of times to re-fit
### Container for SSEs
all.SSEs = rep(0, times = 20)
all.MSEs = rep(0, times = 20)
all.nnets = list(1:20)
for(i in 1:n.nnets){
  ### Fit model. We can set the input "trace" to FALSE to suppress the
  ### printed output from the nnet() function.
  this.nnet = nnet(y = Y.train, x = X.train[,c(3,4)], linout = TRUE, size = n.hidden,
                   decay = shrink, maxit = 500, trace = FALSE)
  
  ### Get the model's SSE
  this.SSE = this.nnet$value
  this.MSE = this.nnet$value / nrow(X.train)
  
  ### Store results. We have to use double square brackets when storing or
  ### retrieving from a list.
  all.SSEs[i] = this.SSE
  all.MSEs[i] = this.MSE
  all.nnets[[i]] = this.nnet
}
all.MSEs
### Get the best model. We have to use double square brackets when storing or
### retrieving from a list.
# ind.best = which.min(all.SSEs)
# fit.nnet.best = all.nnets[[ind.best]]
ind.best = which.min(all.SSEs)
fit.nnet.best = all.nnets[[ind.best]]

x1 <- seq(from=2.3, to=20.7, by=.5)
x2 = seq(from=57, to=97, by=.5)
xy1 <- data.frame(expand.grid(wind=x1, temp=x2))

#pred.nnet = predict(fit.nnet.best, X.valid)
# pred.nnet = predict(fit.nnet.best, X.valid)
# MSPE.nnet = get.MSPE(Y.valid, pred.nnet)
# pred2 <- predict(pred.nnet ,newdata=rescale(xy1, X.valid.raw[,c(3,4)]))
pred2 <- predict(fit.nnet ,newdata=rescale(xy1, X.valid.raw[,c(3,4)]))
surface2 = matrix(pred2, nrow=length(x1))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Ozone")
points3d(data$Ozone ~ data$Wind + data$Temp, col="blue")


##################################### (6, 1)
n.hidden = 6
shrink = 1

fit.nnet = nnet(y = Y.train, x = X.train[,c(3,4)], linout = TRUE, size = n.hidden,
                decay = shrink, maxit = 500)

n.nnets = 20 # Number of times to re-fit
### Container for SSEs
all.SSEs = rep(0, times = 20)
all.MSEs = rep(0, times = 20)
all.nnets = list(1:20)
for(i in 1:n.nnets){
  ### Fit model. We can set the input "trace" to FALSE to suppress the
  ### printed output from the nnet() function.
  this.nnet = nnet(y = Y.train, x = X.train[,c(3,4)], linout = TRUE, size = n.hidden,
                   decay = shrink, maxit = 500, trace = FALSE)
  
  ### Get the model's SSE
  this.SSE = this.nnet$value
  this.MSE = this.nnet$value / nrow(X.train)
  
  ### Store results. We have to use double square brackets when storing or
  ### retrieving from a list.
  all.SSEs[i] = this.SSE
  all.MSEs[i] = this.MSE
  all.nnets[[i]] = this.nnet
}
all.MSEs
### Get the best model. We have to use double square brackets when storing or
### retrieving from a list.
# ind.best = which.min(all.SSEs)
# fit.nnet.best = all.nnets[[ind.best]]
ind.best = which.min(all.SSEs)
fit.nnet.best = all.nnets[[ind.best]]

x1 <- seq(from=2.3, to=20.7, by=.5)
x2 = seq(from=57, to=97, by=.5)
xy1 <- data.frame(expand.grid(wind=x1, temp=x2))

#pred.nnet = predict(fit.nnet.best, X.valid)
# pred.nnet = predict(fit.nnet.best, X.valid)
# MSPE.nnet = get.MSPE(Y.valid, pred.nnet)
# pred2 <- predict(pred.nnet ,newdata=rescale(xy1, X.valid.raw[,c(3,4)]))
pred2 <- predict(fit.nnet ,newdata=rescale(xy1, X.valid.raw[,c(3,4)]))
surface2 = matrix(pred2, nrow=length(x1))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Ozone")
points3d(data$Ozone ~ data$Wind + data$Temp, col="blue")