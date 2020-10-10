# Title: Exercise 5
# Author: Injun Son
# Date: October 9, 2020

#import dataset
Sys.setenv(LANG = "en")
library(dplyr)
library(leaps) # This package contains the regsubsets() function,
# which does all subsets selection

shuffle = function(X){
  new.order = sample.int(length(X))
  new.X = X[new.order]
  return(new.X)
}

get.MSPE = function(Y, Y.hat){
  return(mean((Y - Y.hat)^2))
}


data = na.omit(airquality[, 1:4])
data$TWcp = data$Temp* data$Wind
data$TWrat = data$Temp/data$Wind

n = nrow(data)
n.train = floor(n * 0.75)
n.valid = n - n.train
groups = c(rep(1, times = n.train), rep(2, times = n.valid))
groups.shuffle = shuffle(groups)
data.train = data[groups.shuffle == 1,]
data.valid = data[groups.shuffle == 2,]

#1. Use all-subsets regression.
# (a) Report the variables in the best model of each size.

########################################################################

### Create a container to store MSPEs
all.models = c("Solar", "Wind", "Temp", "TWcp", "TWrat")
all.MSPEs = rep(0, times = length(all.models))
names(all.MSPEs) = all.models

#############################################
### All subsets selection via AIC and BIC ###
#############################################


### The model.matrix lets us specify the regression formula we want
### to use, and outputs the corresponding model matrix
data.matrix = model.matrix(Ozone ~ .^2, data = data.train)

### We also need the response variable (i.e. alcohol)
Y.train = data.train$Ozone

### Now we can run all.subsets. There are a couple of extra inputs
### here. nvmax is the largest number of variables we are willing
### to include. 30 seems like plenty. intercept specifies whether we 
### want regsubsets() to add an intercept term. Since model.matrix()
### already adds an intercept, we don't want regsubsets() to add
### another one.
all.subsets = regsubsets(x = data.matrix, y = Y.train, nvmax = 30)

### The output of regsubsets isn't really useful. We need to run the
### summary() function on it to get useful information
info.subsets = summary(all.subsets)

### The output of summary contains an array with columns corresponding
### to predictors and rows corresponding to model sizes. This array 
### tells us which variables are included at each size. 
all.subsets.models = info.subsets$which
all.subsets.models = all.subsets.models[, -1]
all.subsets.models
dim(all.subsets.models)
### We can get the AIC and BIC of each of these models by re-fitting
### the models and running extractAIC(). The extractAIC() function
### has an input called k, which is the coefficient on the penalty term.
### We can get AIC by setting k=2 (the default), and BIC by setting
### k equal to the logarithm of the sample size.
### Note: We are going to fit 15 models here. That's way too many
### to code manually, so we will use a for loop.
n.models = nrow(all.subsets.models) # Number of candidate models
all.AICs = rep(0, times = n.models) # Container to store AICs
all.BICs = all.AICs # Copy all.AICs to get a container for BICs

for(i in 1:n.models){
  ### We can actually supply a model matrix and response vector 
  ### to lm, without using a data frame. Remember that our model matrix
  ### already has an intercept, so we need to make sure lm doesn't
  ### include another one. We do this by including -1 in the right side
  ### of the model formula.
  this.data.matrix = data.matrix[,all.subsets.models[i,]]
  fit = lm(Y.train ~ this.data.matrix - 1)
  
  ### Get the AIC using extractAIC(). This function takes a regression
  ### model as input, as well as (optionally) an input called k, which
  ### specifies the penalty on the number of variables in our model.
  ### The AIC value is in the second component of the output object.
  this.AIC = extractAIC(fit)[2]
  all.AICs[i] = this.AIC
  
  ### Get the BIC using extractAIC(). This time, we need to set k equal
  ### to the log of the number of observations used to fit our model
  this.BIC = extractAIC(fit, k = log(n.train))[2]
  all.BICs[i] = this.BIC
}

### Get the optimal model for AIC and BIC
AIC.ind = which.min(all.AICs)
AIC.model = all.subsets.models[AIC.ind,]
BIC.ind = which.min(all.BICs)
BIC.model = all.subsets.models[BIC.ind,]


### Next, we need to fit these models and get predictions
### on the validation set. To do so, we will need to construct
### model matrices for the training and validation sets which
### correspond to the chosen models for AIC and BIC
data.matrix.valid = model.matrix(Ozone ~ .^2, data = data.valid)

AIC.train = data.matrix[,AIC.model]
BIC.train = data.matrix[,BIC.model]
AIC.valid = data.matrix.valid[,AIC.model]
BIC.valid = data.matrix.valid[,BIC.model]

### Now we can fit the optimal models on the training set, and get
### validation set MSPEs (remember the -1)
AIC.fit = lm(data.train$Ozone ~ AIC.train - 1)
BIC.fit = lm(data.train$Ozone ~ BIC.train - 1)

AIC.pred = predict.matrix(AIC.fit, AIC.valid)
BIC.pred = predict.matrix(BIC.fit, BIC.valid)

Y.valid = data.valid$Ozone # Response vector in validation set
AIC.err = get.MSPE(Y.valid, AIC.pred)
BIC.err = get.MSPE(Y.valid, BIC.pred)

### Finally, we can store these MSPEs in all.MSPEs
all.MSPEs["subsets.AIC"] = AIC.err
all.MSPEs["subsets.BIC"] = BIC.err



#(b) Compute BIC on each of these models and report the BIC values for the
#models.

#####Use stepwise
fit.start = lm(Ozone ~ 1, data = data.train)
fit.end = lm(Ozone ~ .^2, data = data.train)

step.AIC = step(fit.start, list(upper = fit.end), k = 2)
step.BIC = step(fit.start, list(upper = fit.end), k = log(n.train), trace = 0)

pred.step.AIC = predict(step.AIC, data.valid)
pred.step.BIC = predict(step.BIC, data.valid)

err.step.AIC = get.MSPE(Y.valid, pred.step.AIC)
err.step.BIC = get.MSPE(Y.valid, pred.step.BIC)

#(c) Identify the best model. What variables are in it?

set.seed(2928893)
### First we need to set the number of folds
K = 10

### Construct folds
### Don't attach fold labels to dataset because we would just have
### to remove this later
n = nrow(data)
n.fold = n/K # Approximate number of observations per fold
n.fold = ceiling(n.fold)
ordered.ids = rep(1:10, each = n.fold)
ordered.ids = ordered.ids[1:n]
fold.ids = shuffle(ordered.ids)


### Create a container to store CV MSPEs
### One column per model, and one row per fold
CV.models = c("stepwise.AIC", "stepwise.BIC")
errs.CV = array(0, dim = c(K,length(CV.models)))
colnames(errs.CV) = CV.models

### Perform cross-validation to estimate MSPE for each model
### It takes a few seconds to do all-subsets at each iteration, so
### the entire loop will take some time. I like to print out a
### status update at the start of each iteration. The paste0()
### function attaches all of its inputs into a single string.
for(i in 1:K){
  print(paste0(i, " of ", K))
  
  ### Construct training and validation sets by either removing
  ### or extracting the current fold. 
  ### Also, get the response vectors
  data.train = data[fold.ids != i,]
  data.valid = data[fold.ids == i,]
  Y.train = data.train$Ozone
  Y.valid = data.train$Ozone
  
  
  ##########################################
  ### Stepwise selection via AIC and BIC ###
  ##########################################
  
  fit.start = lm(Ozone ~ 1, data = data.train)
  fit.end = lm(Ozone ~ .^2, data = data.train)
  
  ### These functions will run several times each. We don't need
  ### to print out all the details, so set trace = 0.
  step.AIC = step(fit.start, list(upper = fit.end), k=2,
                  trace = 0)
  step.BIC = step(fit.start, list(upper = fit.end), k = log(n.train),
                  trace = 0)
  print(summary(step.BIC))
  
  pred.step.AIC = predict(step.AIC, data.valid)
  pred.step.BIC = predict(step.BIC, data.valid)
  
  err.step.AIC = get.MSPE(Y.valid, pred.step.AIC)
  err.step.BIC = get.MSPE(Y.valid, pred.step.BIC)
  print(paste0(i, "th model error ", err.step.BIC))
  
  
  pred.step.AIC = predict(step.BIC, data.valid)
  
  ### Store errors in errs.CV, which has two dimensions, so 
  ### we need two indices
  errs.CV[i, "stepwise.AIC"] = err.step.AIC
  errs.CV[i, "stepwise.BIC"] = err.step.BIC
  
}


