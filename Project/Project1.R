# Title: STAT 452 Project 1
# Author: Injun Son
# Date: November 13, 2020

library(dplyr)
library(MASS)   # For ridge regression
library(glmnet) # For LASSO
library(splines)
library(nnet) # Fits neural net models
library(rgl)  
library(mgcv)
library(rpart)
library(rpart.plot)
library(gbm)
library(randomForest)
source("Helper Functions.R")

set.seed(2385660, kind="Mersenne-Twister")

#Read data
data = read.csv("Data2020.csv")

# Simple EDA
summary(data)
dim(data)

# Split train and valid
n = nrow(data)
reorder = sample.int(n)

size_train = floor(n*0.75)
ind_train = reorder[1:size_train]
ind_valid = reorder[(size_train+1):n]

data_train = data[ind_train, ]
data_valid = data[ind_valid, ]

Y.valid = data_valid$Y

# Linear Model
fit.linear.all = lm(Y ~ ., data = data_train)
fit.linear.inter = lm(Y ~ .^2, data = data_train)

pred.linear.all = predict(fit.linear.all, data_valid)
pred.linear.inter = predict(fit.linear.inter, data_valid)

MSPE.linear.all = get.MSPE(Y.valid, pred.linear.all)
MSPE.linear.inter = get.MSPE(Y.valid, pred.linear.inter)


################################

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
all.models = c("LS", "Hybrid", "Ridge", "LASSO-Min", "LASSO-1se", "GAM", "Full-tree", "Min-cv tree", "1-se tree", "randomForest")
all.MSPEs = array(0, dim = c(K, length(all.models)))
colnames(all.MSPEs) = all.models
### Begin cross-validation
for(i in 1:K){
  ### Split data
  data.train = data[folds != i,]
  data.valid = data[folds == i,]
  n.train = nrow(data.train)
  
  ### Get response vectors
  Y.train = data.train$Y
  Y.valid = data.valid$Y
  
  # LS
  fit.ls = lm(Y ~ ., data = data.train)
  pred.ls = predict(fit.ls, newdata = data.valid)
  MSPE.ls = get.MSPE(Y.valid, pred.ls)
  all.MSPEs[i, "LS"] = MSPE.ls
  
  #Hybrid Stepwise
  
  fit.start = lm(Y ~ 1, data = data.train)
  fit.end = lm(Y ~ ., data = data.train)
  
  step.BIC = step(fit.start, list(upper = fit.end), k = log(n.train),
                  trace = 0)
  
  pred.step.BIC = predict(step.BIC, data.valid)
  
  err.step.BIC = get.MSPE(Y.valid, pred.step.BIC)
  
  all.MSPEs[i, "Hybrid"] = err.step.BIC
  
  
  
  #ridge regression
  lambda.vals = seq(from = 0, to = 100, by = 0.05)
  
  fit.ridge = lm.ridge(Y ~ ., lambda = lambda.vals, 
                       data = data.train)
  
  ind.min.GCV = which.min(fit.ridge$GCV)
  lambda.min = lambda.vals[ind.min.GCV]
  
  all.coefs.ridge = coef(fit.ridge)
  coef.min = all.coefs.ridge[ind.min.GCV,]
  
  matrix.valid.ridge = model.matrix(Y ~ ., data = data.valid)
  
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
  
  matrix.train.raw = model.matrix(Y ~ ., data = data.train)
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
  
  matrix.valid.LASSO.raw = model.matrix(Y ~ ., data = data.valid)
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
  fit.gam = gam(Y ~ s(X1, k=5) + s(X2, k=5) + s(X3, k=5) + s(X4, k=5) + s(X5, k=5) + s(X6, k=5) + s(X7, k=5) + s(X8, k=5)
                + s(X9, k=5) + s(X10, k=5) + s(X11, k=5) + s(X12, k=5) + s(X13, k=5) + s(X14, k=5) +s(X15, k=5), 
                data = data.train)
  
  pred.gam = predict(fit.gam, data.valid)
  MSPE.gam = get.MSPE(Y.valid, pred.gam) # Our helper function
  all.MSPEs[i, "GAM"] = MSPE.gam
  
  
  # Full-tree 
  fit.tree = rpart(Y ~ ., data = data.train, cp=0)
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
  
  # Random-forest
  #############
  ##############
  ############
  # i = 1
  ## Set parameter values
  # all.mtry = 5:13
  # all.nodesize = c(2, 5, 8, 11, 15)
  # all.pars = expand.grid(mtry = all.mtry, nodesize = all.nodesize)
  # n.pars = nrow(all.pars)
  # 
  # ### Number of times to replicate process. OOB errors are based on bootstrapping,
  # ### so they are random and we should repeat multiple runs
  # M = 5
  # 
  # ### Create container for OOB MSPEs
  # OOB.MSPEs = array(0, dim = c(M, n.pars))
  # 
  # for(i in 1:n.pars){
  #   ### Print progress update
  #   print(paste0(i, " of ", n.pars))
  # 
  #   ### Get current parameter values
  #   this.mtry = all.pars[i,"mtry"]
  #   this.nodesize = all.pars[i,"nodesize"]
  # 
  #   ### Fit random forest models for each parameter combination
  #   ### A second for loop will make our life easier here
  #   for(j in 1:M){
  #     ### Fit model using current parameter values. We don't need variable
  #     ### importance measures here and getting them takes time, so set
  #     ### importance to F
  #     fit.rf = randomForest(Y ~ ., data = data, importance = F,
  #                           mtry = this.mtry, nodesize = this.nodesize)
  # 
  #     ### Get OOB predictions and MSPE, then store MSPE
  #     OOB.pred = predict(fit.rf)
  #     OOB.MSPE = get.MSPE(data$Y, OOB.pred)
  # 
  #     OOB.MSPEs[j, i] = OOB.MSPE # Be careful with indices for OOB.MSPEs
  #   }
  # }
  # 
  # 
  # ### We can now make an MSPE boxplot. First, add column names to indicate
  # ### which parameter combination was used. Format is mtry-nodesize
  # names.pars = paste0(all.pars$mtry,"-",
  #                     all.pars$nodesize)
  # colnames(OOB.MSPEs) = names.pars
  # 
  # ### Make boxplot
  # boxplot(OOB.MSPEs, las = 2, main = "MSPE Boxplot")
  # 
  # 
  # ### Get relative MSPEs and make boxplot
  # OOB.RMSPEs = apply(OOB.MSPEs, 1, function(W) W/min(W))
  # OOB.RMSPEs = t(OOB.RMSPEs)
  # boxplot(OOB.RMSPEs, las = 2, main = "RMSPE Boxplot")
  
  ############################
  ## Result is 9-8
  fit.rf.2 = randomForest(Y ~ ., data = data.train, importance = T,
                          mtry = 9, nodesize = 8)
  OOB.pred.2 = predict(fit.rf.2)
  OOB.MSPE.2 = get.MSPE(data.valid$Y, OOB.pred.2)
  all.MSPEs[i, "randomForest"] = OOB.MSPE.2
  
  
  
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







######## Actual Prediction
data2 = read.csv("Data2020testX.csv")

fit.gam2 = gam(Y ~ s(X1, k=5) + s(X2, k=5) + s(X3, k=5) + s(X4, k=5) + s(X5, k=5) + s(X6, k=5) + s(X7, k=5) + 
                 s(X8, k=5) + s(X9, k=5) + s(X10, k=5) + s(X11, k=5) + s(X12, k=5) + s(X13, k=5) + s(X14, k=5) +s(X15, k=5), 
              data = data)

pred.gam2 = predict(fit.gam2, data2)

write.table(pred.gam2, "prediction1.csv", sep = ",", row.names = F, col.names = F)



