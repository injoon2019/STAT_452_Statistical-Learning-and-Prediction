# Title: STAT 452 Exercise 8 L13Q1
# Author: Injun Son
# Date: October 31, 2020

library(dplyr)
library(MASS)   # For ridge regression
library(glmnet) # For LASSO
library(nnet) # Fits neural net models
library(rgl)  
library(mgcv)
library(rpart)
library(rpart.plot)
source("Helper Functions.R")
data = na.omit(airquality[, 1:4])
data$TWcp = data$Temp*data$Wind
data$TWrat = data$Temp/data$Wind

set.seed(8598176)

# 1. Fit a regression tree using only Temp and Wind, using cp=0.
# (a) Print out the cp table.
# i. According to minimum CV, what value of cp should be used for optimal
# pruning, and what is the relative CV error at this value?

fit.tree = rpart(Ozone ~ Temp + Wind, data = data, cp=0)

### Get the CP table
info.tree = fit.tree$cptable
info.tree

### We have to prune the tree manually. First, get the CP value with minimum
### CV error
ind.min = which.min(info.tree[,"xerror"])
CP.min.raw = info.tree[ind.min, "CP"]


#######################################################
# (b) Plot both the min-CV pruned tree and the 1SE pruned tree.
# i. Interpret the first split on the root node: What variable and split location
# are used is used, how many observations go each way, and what are
# the new means in each node?

### It's best to average the minimum CP value with the one from the row above 
### using the geometric mean (i.e. multiply them together, then square root). 
### If we implement this procedure directly, and the minimum CP value is in   
### the first row, our code will probably give an error. We should write our  
### code so that it can cope with this weird situation. This attitude is      
### called defensive programming, and it is a very good habit to practice.    

### Check if minimum CP value is in row 1. We can do this using an if-else
### statement. See this tutorial's video for details.
if(ind.min == 1){
  ### If minimum CP is in row 1, store this value
  CP.min = CP.min.raw
} else{
  ### If minimum CP is not in row 1, average this with the value from the
  ### row above it.
  
  ### Value from row above
  CP.above = info.tree[ind.min-1, "CP"]
  
  ### (Geometric) average
  CP.min = sqrt(CP.min.raw * CP.above)
}

### We now have our chosen CP value. We can prune our tree using the prune()
### function. The first input to prune() is the tree object, and we set "cp"
### equal to the CP value where we want to prune
fit.tree.min = prune(fit.tree, cp = CP.min)

### Next, we want to prune using the 1SE rule. Fortunately, the CP table
### gives us the CV standard error. First, find the minimum CV error plus 1 
### standard error
err.min = info.tree[ind.min, "xerror"]
se.min = info.tree[ind.min, "xstd"]
threshold = err.min + se.min

### Next, get the smallest tree with CV error below our threshold. 
### Note: We limit our search to only trees which are no larger than our min CV
###       error tree.
ind.1se = min(which(info.tree[1:ind.min,"xerror"] < threshold))

### Get the corresponding CP value, averaging if necessary
CP.1se.raw = info.tree[ind.1se, "xerror"]
if(ind.1se == 1){
  ### If best CP is in row 1, store this value
  CP.1se = CP.1se.raw
} else{
  ### If best CP is not in row 1, average this with the value from the
  ### row above it.
  
  ### Value from row above
  CP.above = info.tree[ind.1se-1, "CP"]
  
  ### (Geometric) average
  CP.1se = sqrt(CP.1se.raw * CP.above)
}

### Prune the tree
fit.tree.1se = prune(fit.tree, cp = CP.1se)

###########################################################################
### A nice feature of tree models is that they make great plots. Let's  ###
### plot the full tree, and both pruned trees. We can plot trees using  ###
### the prp() function from the rpart.plot package.                     ###
###########################################################################
### The prp() function has many inputs We will just use two: type and extra.
### Setting both of these inputs to 1 gives a nice looking plot. Since
### prp() makes a plot, we can set the title using main. We also
### have to provide the fitted tree object which is being plotted.
prp(fit.tree, type = 1, extra = 1, main = "Full Tree")
prp(fit.tree.min, type = 1, extra = 1, main = "Pruned Tree - Min")
prp(fit.tree.1se, type = 1, extra = 1, main = "Pruned Tree - 1SE")



################################################################################
################################################################################

get.folds = function(n, K) {
  ### Get the appropriate number of fold labels
  n.fold = ceiling(n / K) # Number of observations per fold (rounded up)
  fold.ids.raw = rep(1:K, times = n.fold) # Generate extra labels
  fold.ids = fold.ids.raw[1:n] # Keep only the correct number of labels
  
  ### Shuffle the fold labels
  folds.rand = fold.ids[sample.int(n)]
  
  return(folds.rand)
}

# i=1
### Number of folds
K = 10

### Construct folds
n = nrow(data) # Sample size
folds = get.folds(n, K)

### Create a container for MSPEs. Let's include ordinary least-squares
### regression for reference
all.models = c("LS", "Hybrid", "Ridge", "LASSO-Min", "LASSO-1se", "GAM", "Full-tree", "Min-cv tree", "1-se tree")
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
  
  
  # Full-tree 
  fit.tree = rpart(Ozone ~ ., data = data, cp=0)
  fit.tree.pred = predict(fit.tree, data.valid)
  MSPE.fit.tree = get.MSPE(Y.valid, fit.tree.pred)
  all.MSPEs[i, "Full-tree"] = MSPE.fit.tree
  
  
  # Min-cv tree
  info.tree = fit.tree$cptable
  info.tree
  ind.min = which.min(info.tree[,"xerror"])
  CP.min.raw = info.tree[ind.min, "CP"]
  if(ind.min == 1){
    ### If minimum CP is in row 1, store this value
    CP.min = CP.min.raw
  } else{
    ### If minimum CP is not in row 1, average this with the value from the
    ### row above it.
    
    ### Value from row above
    CP.above = info.tree[ind.min-1, "CP"]
    
    ### (Geometric) average
    CP.min = sqrt(CP.min.raw * CP.above)
  }
  fit.tree.min = prune(fit.tree, cp = CP.min)
  fit.tree.min.pred = predict(fit.tree.min, data.valid)
  MSPE.fit.tree.min = get.MSPE(Y.valid, fit.tree.min.pred)
  all.MSPEs[i, "Min-cv tree"] = MSPE.fit.tree.min
  
  
  #"1-se tree"
  err.min = info.tree[ind.min, "xerror"]
  se.min = info.tree[ind.min, "xstd"]
  threshold = err.min + se.min
  ind.1se = min(which(info.tree[1:ind.min,"xerror"] < threshold))
  
  ### Get the corresponding CP value, averaging if necessary
  CP.1se.raw = info.tree[ind.1se, "xerror"]
  if(ind.1se == 1){
    ### If best CP is in row 1, store this value
    CP.1se = CP.1se.raw
  } else{
    ### If best CP is not in row 1, average this with the value from the
    ### row above it.
    
    ### Value from row above
    CP.above = info.tree[ind.1se-1, "CP"]
    
    ### (Geometric) average
    CP.1se = sqrt(CP.1se.raw * CP.above)
  }
  
  fit.tree.1se = prune(fit.tree, cp = CP.1se)
  fit.tree.1se.pred = predict(fit.tree.1se, data.valid)
  MSPE.fit.tree.1se = get.MSPE(Y.valid, fit.tree.1se.pred)
  all.MSPEs[i, "1-se tree"] = MSPE.fit.tree.1se
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

mean(all.MSPEs[,7])
mean(all.MSPEs[,8])
mean(all.MSPEs[,9])
