# Title: STAT 452 Exercise 8 L12Q12
# Author: Injun Son
# Date: October 31, 2020

library(dplyr)
library(MASS)   # For ridge regression
library(glmnet) # For LASSO
library(nnet) # Fits neural net models
library(rgl)  
library(caret)
library(reshape)
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

###################################
library(caret)

# This function will do min-max scaling internally with preProcess="range"
# Need to use y as a numeric vector and not a matrix.
# Can change numbers used in tune.Grid as needed.

# Default is 25 bootstrap reps 
# Can be changed by adding
# trcon = trainControl(method=..., number=..., repeats=...)
#  Can do method= "boot", "cv", "repeatedcv", "LOOCV", and several others
#  number= is number of boot reps or cv folds
#  repeats= number of CV replicates
#  returnResamp="all" retains the error measures from each split.

#Specify 5-fold CV run twice
trcon = trainControl(method="repeatedcv", number=5, repeats=2,
                     returnResamp="all")
parmgrid = expand.grid(size=c(1,3,5,7,9),decay= c(0.001, .1, .5, 1 ,2))

tuned.nnet <- train(x=data[,-1], y=data[,1], method="nnet", 
                    trace=FALSE, linout=TRUE, 
                    trControl=trcon, preProcess="range", 
                    tuneGrid = parmgrid)
tuned.nnet
names(tuned.nnet)
tuned.nnet$bestTune

#If I want to make plots, I need to rearrange the resamples
(resamp.caret = tuned.nnet$resample[,-c(2,3)])

library(reshape)
RMSPE.caret = reshape(resamp.caret, direction="wide", v.names="RMSE",
                      idvar=c("size","decay"), timevar="Resample")


# Plot results. 
siz.dec <- paste("NN",RMSPE.caret[,1],"/",RMSPE.caret[,2])
x11(pointsize=10)
boxplot(x=as.matrix(RMSPE.caret[,-c(1,2)]), use.cols=FALSE, names=siz.dec,
        las=2, main="caret Root-MSPE boxplot for various NNs")

# Plot RELATIVE results. 
lowt = apply(RMSPE.caret[,-c(1,2)], 2, min)

x11(pointsize=10)
boxplot(x=t(as.matrix(RMSPE.caret[,-c(1,2)]))/lowt, las=2, 
        names=siz.dec)

#Focused 
x11(pointsize=10)
boxplot(x=t(as.matrix(RMSPE.caret[,-c(1,2)]))/lowt, las=2, 
        names=siz.dec, ylim=c(1,2))

R=2
V=5
relMSPE = t(RMSPE.caret[,-c(1,2)])/lowt
(RRMSPE = apply(X=relMSPE, MARGIN=2, FUN=mean))
(RRMSPE.sd = apply(X=relMSPE, MARGIN=2, FUN=sd))
RRMSPE.CIl = RRMSPE - qt(p=.975, df=R*V-1)*RRMSPE.sd/sqrt(R*V)
RRMSPE.CIu = RRMSPE + qt(p=.975, df=R*V-1)*RRMSPE.sd/sqrt(R*V)
(all.rrcv = cbind(RMSPE.caret[,1:2],round(sqrt(cbind(RRMSPE,RRMSPE.CIl, RRMSPE.CIu)),2)))
all.rrcv[order(RRMSPE),]

