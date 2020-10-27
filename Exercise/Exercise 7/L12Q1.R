# Title: STAT 452 Exercise 7 L12Q1
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

### We fit PPR models using the ppr() function. This function uses data frame-
### model formula syntax. We also need to specify the number of terms to
### include in our model, and what type of smoother to use. Number of terms is
### set using the nterms input, and type of smoother is set using sm.method.
### Options for smoothers include "spline" (smoothing spline where you have
### to also specify the number of degrees of freedom using df) and "gcvspline"
### (smoothing spline with degrees of freedom chosen by GCV).
fit.ppr.1 = ppr(Ozone ~ ., data = data,
              max.terms = 5, nterms = 1, sm.method = "gcvspline")
plot(fit.ppr.1)
summary(fit.ppr.1)

fit.ppr.2 = ppr(Ozone ~ ., data = data,
                max.terms = 5, nterms = 2, sm.method = "gcvspline")
plot(fit.ppr.2)
summary(fit.ppr.2)



#####################################################
######################################################

max.terms = 5 # Maximum number of terms for PPR

### Everything starts off normally for CV.

### Create folds
K = 10 # Number of folds
folds = get.folds(nrow(data), K) # Our helper function

### Create container for CV MSPEs
all.models = c("PPR")
n.models = length(all.models)
CV.MSPEs = array(0, dim = c(n.models, K))
rownames(CV.MSPEs) = all.models


### Split data
n = nrow(data)
p.train = 0.75
n.train = round(n * p.train)
n.valid = n - n.train
sets = c(rep(1, times = n.train), rep(2, times = n.valid))
sets.rand = shuffle(sets) # Our helper function

data.train = data[sets.rand == 1,]
data.valid = data[sets.rand == 2,]
Y.valid = data.valid$Ozone

###########
### PPR ###
###########

### To fit PPR, we need to do another round of CV. This time, do 5-fold
K.ppr = 10
n.train = nrow(data.train)
folds.ppr = get.folds(n.train, K.ppr)

### Container to store MSPEs for each number of terms on each sub-fold
MSPEs.ppr = array(0, dim = c(K.ppr, max.terms))

for(j in 1:K.ppr){
  ### Split the training data.
  ### Be careful! We are constructing an internal validation set by 
  ### splitting the training set from outer CV.
  train.ppr = data.train[folds.ppr != j,]
  valid.ppr = data.train[folds.ppr == j,] 
  Y.valid.ppr = valid.ppr$Ozone
  
  ### We need to fit several different PPR models, one for each number
  ### of terms. This means another for loop (make sure you use a different
  ### index variable for each loop).
  for(l in 1:max.terms){
    ### Fit model
    fit.ppr = ppr(Ozone ~ ., data = train.ppr, 
                  max.terms = max.terms, nterms = l, sm.method = "gcvspline")
    
    ### Get predictions and MSPE
    pred.ppr = predict(fit.ppr, valid.ppr)
    MSPE.ppr = get.MSPE(Y.valid.ppr, pred.ppr) # Our helper function
    
    ### Store MSPE. Make sure the indices match for MSPEs.ppr
    MSPEs.ppr[j, l] = MSPE.ppr
  }
}

MSPEs.ppr

boxplot(MSPEs.ppr, main = paste0("PPR MSPEs over ", K, " folds"))

### Calculate RMSPEs
all.RMSPEs = apply(MSPEs.ppr, 1, function(W){
  best = min(W)
  return(W / best)
})
all.RMSPEs = t(all.RMSPEs)

### Make a boxplot of RMSPEs
boxplot(all.RMSPEs, main = paste0("CV RMSPEs over ", K, " folds"))



#####################################################################
#####################################################################
#####################################################################
#Q3
