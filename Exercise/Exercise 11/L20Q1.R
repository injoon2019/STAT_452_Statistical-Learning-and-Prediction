# Title: STAT 452 Exercise L20Q1
# Author: Injun Son
# Date: November 27, 2020

library(dplyr)
library(MASS)   # For ridge regression
library(glmnet) # For LASSO
library(nnet) # Fits neural net models
library(rgl)
library(FNN)
library(rpart)        # For fitting classification trees
library(rpart.plot)   # For plotting classification trees
library(randomForest) # For random forests
library(gbm)          # For boosting
source("Helper Functions.R")


# 1. Reset the seed for the Mersenne Twister to 8646824. Code two models: a classification
# tree using all defaults, followed by a second tree setting cp=0. This way, every student
# should have the same CV results within rpart.
set.seed(8646824, kind = "Mersenne-Twister")
vehdata = read.csv("vehicle.csv")

perm <- sample (x= nrow ( vehdata ))
data.train <- vehdata [ which ( perm <= 3* nrow ( vehdata )/4) , ]
data.valid <- vehdata [ which ( perm > 3* nrow ( vehdata )/4) , ]
Y.train = data.train[,19]
Y.valid = data.valid[,19]

# ind.random = sample(1:n)
# data.train = data[ind.random <= n.train,]
# data.valid = data[ind.random > n.train,]
# Y.valid = data.valid[,1]
# 
# 
# X.train = set1[,-19]
# X.valid = set2[,-19]
# Y.train = set1[, 19]
# Y.train = as.factor(Y.train)
# Y.valid = set2[, 19]
# Y.valid = as.factor(Y.valid)

### Fitting classification trees is mostly the same as fitting regression
### trees. We use the rpart() function from the rpart package. This function
### uses model formula/data frame syntax. We can set the CP using the cp 
### input; usually this is set to 0. When doing classification, we have to
### set method="class".
fit.tree.default = rpart(class ~ ., data = data.train, method = "class")
fit.tree.full = rpart(class ~ ., data = data.train, method = "class", 
                      cp = 0)

# (a) Print out the CP table for the default tree. Is there any evidence that
# larger trees are needed than what the defaults allow? Explain.
### We can plot our tree using the prp() function from the rpart.plot package.
### As with regression trees, the prp() function has many specialized inputs, 
### but we will just set type=1 and extra=1. This is a plotting function,
### so you can also set things like main, xlab, ylab, etc.
prp(fit.tree.default, type = 1, extra = 1, main = "Default Tree")


# (b) Print out the CP table for the tree with cp=0. How many splits are
# suggested as best?
prp(fit.tree.full, type = 1, extra = 1, main = "Full Tree")


# (c) Use Tom¡¯s code to find the optimal CP value for pruning using minimum CV and
# using the 1-SE rule. Report these two numbers

### We can tune our tree using information from the CP table.
info.tree = fit.tree.full$cptable

### The actual process of tuning is the same. We can use Tom's code to do this

# Find location of minimum error
minrow <- which.min(info.tree[,4])
# Take geometric mean of cp values at min error and one step up 
cplow.min <- info.tree[minrow,1]
cpup.min <- ifelse(minrow==1, yes=1, no=info.tree[minrow-1,1])
cp.min <- sqrt(cplow.min*cpup.min)

# Find smallest row where error is below +1SE
se.row <- min(which(info.tree[,4] < info.tree[minrow,4]+info.tree[minrow,5]))
# Take geometric mean of cp values at min error and one step up 
cplow.1se <- info.tree[se.row,1]
cpup.1se <- ifelse(se.row==1, yes=1, no=info.tree[se.row-1,1])
cp.1se <- sqrt(cplow.1se*cpup.1se)

# Creating a pruned tree using the CV-min rule.
fit.tree.min <- prune(fit.tree.full, cp=cp.min)
# Creating a pruned tree using the CV-1se rule.
fit.tree.1se <- prune(fit.tree.full, cp=cp.1se)

### Now that we have our pruned trees, let's plot them.
prp(fit.tree.min, type = 1, extra = 1, main = "CV-min Tree")
prp(fit.tree.1se, type = 1, extra = 1, main = "CV-1se Tree")

# (d) Report training and test error of all three of these trees, full and two
# pruned.

### Finally, let's get predictions and evaluate performance for each tree.
### We have to specify what format we want predictions to be presented in.
### Setting type="class" gives us class labels, setting type="vector" gives
### us numeric class labels, and type="prob" gives us predicted class 
### probabilities.

#Y.train fit.tree.full
pred.tree.full = predict(fit.tree.full, data.train, type = "class")
table(Y.train, pred.tree.full, dnn = c("Obs", "Pred"))
(mis.tree.full = mean(Y.train != pred.tree.full))

#Y.test fit.tree.full
pred.tree.full = predict(fit.tree.full, data.valid, type = "class")
table(Y.valid, pred.tree.full, dnn = c("Obs", "Pred"))
(mis.tree.full = mean(Y.valid != pred.tree.full))

# Y.train fit.tree.min
pred.tree.min = predict(fit.tree.min, data.train, type = "class")
table(Y.train, pred.tree.min, dnn = c("Obs", "Pred"))
(mis.tree.min = mean(Y.train != pred.tree.min))

# Y.test fit.tree.min
pred.tree.min = predict(fit.tree.min, data.valid, type = "class")
table(Y.valid, pred.tree.min, dnn = c("Obs", "Pred"))
(mis.tree.min = mean(Y.valid != pred.tree.min))

# Y.train fit.tree.1se
pred.tree.1se = predict(fit.tree.1se, data.train, type = "class")
table(Y.train, pred.tree.1se, dnn = c("Obs", "Pred"))
(mis.tree.1se = mean(Y.train != pred.tree.1se))

# Y.valid fit.tree.1se
pred.tree.1se = predict(fit.tree.1se, data.valid, type = "class")
table(Y.valid, pred.tree.1se, dnn = c("Obs", "Pred"))
(mis.tree.1se = mean(Y.valid != pred.tree.1se))


# #2. Fit a default random forest
# (a) Report variable importance and comment: which variables seem most
# important and which seem least important?

### The function that fits random forests requires that our response
### variable be a factor. We need to make a copy of our dataset and
### use the factor() function on quality.
data.rf = vehdata
data.rf$class = factor(data.rf$class)

### Split the data into training and validation sets
data.train.rf = data.rf[which ( perm <= 3* nrow ( vehdata )/4) , ]
data.valid.rf = data.rf[which ( perm > 3* nrow ( vehdata )/4) , ]
Y.train.rf = data.train.rf$class
Y.valid.rf = data.valid.rf$class

this.fit.rf = randomForest(class ~ ., data = data.train.rf)
importance(this.fit.rf)
varImpPlot(this.fit.rf, main="Importance of variable")

#   (b) Report test error and compare it to other methods
pred.rf = predict(this.fit.rf, data.valid.rf)
(mis.rf = mean(Y.valid != pred.rf))


# 3. Tune the number of variables and the node sizes for the random forest using its internal
# out-of-bag error. Since p = 18 and the default is mtry= root(18) is about 4, use a grid of
# 2,4,6,10,18. For node size, default is 1, so try 1,3,5,7,10. Rerun 5 random forests of
# 500 trees each.


### Random forests for classification are mostly the same as when we use 
### them for regression. See Lecture 15 for more information.
### Remember that we have to use tuning if we want to get the
###  most out of this model. Conveniently, we can use out-of-bag
### error to tune, so we don't need to write a full CV loop.
### Tuning parameters are mtry and nodesize.

### Set tuning parameters
all.mtrys = c(2,4,6,10,18)
all.nodesizes = c(1, 3, 5, 7, 9)
all.pars.rf = expand.grid(mtry = all.mtrys, nodesize = all.nodesizes)
n.pars = nrow(all.pars.rf)

M = 5 # Number of times to repeat RF fitting. I.e. Number of OOB errors

### Container to store OOB errors. This will be easier to read if we name
### the columns.
all.OOB.rf = array(0, dim = c(M, n.pars))
names.pars = apply(all.pars.rf, 1, paste0, collapse = "-")
colnames(all.OOB.rf) = names.pars


for(i in 1:n.pars){
  ### Progress update
  print(paste0(i, " of ", n.pars))
  
  ### Get tuning parameters for this iteration
  this.mtry = all.pars.rf[i, "mtry"]
  this.nodesize = all.pars.rf[i, "nodesize"]
  
  for(j in 1:M){
    ### Fit RF, then get and store OOB errors
    this.fit.rf = randomForest(class ~ ., data = data.train.rf,
                               mtry = this.mtry, nodesize = this.nodesize)
    
    pred.this.rf = predict(this.fit.rf)
    this.err.rf = mean(Y.train.rf != pred.this.rf)
    
    all.OOB.rf[j, i] = this.err.rf
  }
}

### Make a regular and relative boxplot
boxplot(all.OOB.rf, las=2, main = "OOB Boxplot")
rel.OOB.rf = apply(all.OOB.rf, 1, function(W) W/min(W))
boxplot(t(rel.OOB.rf), las=2,  # las sets the axis label orientation
        main = "Relative OOB Boxplot") 

### Best model looks like mtry = 18 and nodesize = 3.
fit.rf = randomForest(class ~ ., data = data.train.rf,
                      mtry = 18, nodesize = 3)

### Get predictions and evaluate performance
pred.rf = predict(fit.rf, data.valid.rf)

table(Y.valid, pred.rf, dnn = c("Obs", "Pred"))
(mis.rf = mean(Y.valid != pred.rf))




