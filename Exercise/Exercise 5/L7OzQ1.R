library(caret)
library(pls) 
source("Helper Functions.R")
ins = read.csv("Insurance.csv", header = TRUE)
ins$zone = as.factor(ins$zone)
ins$make = as.factor(ins$make)
ins.dv = data.frame(predict(dummyVars("~.", data=ins), newdata = ins))
head(ins.dv)

dim(ins.dv)
# > dim(ins.dv)
# [1] 2182   21


data.matrix.raw = model.matrix(per ~ ., data = ins.dv)
data.matrix = data.matrix.raw[,-1]

fit.PCA = prcomp(data.matrix, scale. = T)
print(fit.PCA)


#Scree plot
vars = fit.PCA$sdev^2
plot(1:length(vars), vars, main = "Variability Explained", 
     xlab = "Principal Component", ylab = "Variance Explained")
abline(h = 1)

# Cumulative variance plot
c.vars = cumsum(vars)   ### Cumulative variance explained
rel.c.vars = c.vars / max(c.vars)   ### Cumulative proportion of 
### variance explained
plot(1:length(rel.c.vars), rel.c.vars,
     main = "Proportion of Variance Explained by First W PCs",
     xlab = "W", ylab = "Proportion of Variance Explained")





########################################
########################################
#########################################
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
data =ins.dv
n = nrow(data) # Sample size
folds = get.folds(n, K)

### Create a container for MSPEs. Let's include ordinary least-squares
### regression for reference
all.models = c("LS", "Hybrid", "Ridge", "LASSO-Min", "LASSO-1se", "PLS")
all.MSPEs = array(0, dim = c(K, length(all.models)))
colnames(all.MSPEs) = all.models



### Begin cross-validation
for(i in 1:K){
  ### Split data
  data.train = data[folds != i,]
  data.valid = data[folds == i,]
  n.train = nrow(data.train)
  
  ### Get response vector
  Y.train = data.train$per
  Y.valid = data.valid$per
  
  # LS
  fit.ls = lm(per ~ ., data = data.train)
  pred.ls = predict(fit.ls, newdata = data.valid)
  MSPE.ls = get.MSPE(Y.valid, pred.ls)
  all.MSPEs[i, "LS"] = MSPE.ls
  
  #Hybrid Stepwise
  
  fit.start = lm(per ~ 1, data = data.train)
  fit.end = lm(per ~ ., data = data.train)
  
  step.BIC = step(fit.start, list(upper = fit.end), k = log(n.train),
                  trace = 0)
  
  pred.step.BIC = predict(step.BIC, data.valid)
  err.step.BIC = get.MSPE(Y.valid, pred.step.BIC)
  all.MSPEs[i, "Hybrid"] = err.step.BIC
  
  #Ridge regression
  lambda.vals = seq(from = 0, to = 100, by = 0.05)
  
  fit.ridge = lm.ridge(per ~ ., lambda = lambda.vals, 
                       data = data.train)
  
  ind.min.GCV = which.min(fit.ridge$GCV)
  lambda.min = lambda.vals[ind.min.GCV]
  
  all.coefs.ridge = coef(fit.ridge)
  coef.min = all.coefs.ridge[ind.min.GCV,]
  
  matrix.valid.ridge = model.matrix(per ~ ., data = data.valid)
  pred.ridge = matrix.valid.ridge %*% coef.min
  MSPE.ridge = get.MSPE(Y.valid, pred.ridge)
  all.MSPEs[i, "Ridge"] = MSPE.ridge
  
  #LASSO
  matrix.train.raw = model.matrix(per ~ ., data = data.train)
  matrix.train = matrix.train.raw[,-1]
  
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
  
  matrix.valid.LASSO.raw = model.matrix(per ~ ., data = data.valid)
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
  
  #
  fit.pls = plsr(per ~ ., data = data.train, validation = "CV",
                 segments = 5)
  
  
  #Code from L7 Question
  mp.cv = fit.pls$validation
  Opt.Comps = which.min(sqrt(mp.cv$PRESS/nrow(data)))
  print(Opt.Comps)
  #PLS
  
  ### Investigate the fitted PLS model. Comment out the next two 
  ### lines when running a CV loop
  
  ### The summary function gives us lots of information about how
  ### errors change as we increase the number of components
  # summary(fit.pls)
  
  ### The validationplot() function shows how MSPE from the internal 
  ### CV of plsr() changes with the number of included components.
  # validationplot(fit.pls)
  
  ### Get the best model from PLS. To do this, we need to find the model
  ### that minimizes MSPE for the plsr() function's internal CV. It 
  ### takes a few steps, but all the information we need is contained
  ### in the output of plsr().
  CV.pls = fit.pls$validation # All the CV information
  PRESS.pls = CV.pls$PRESS    # Sum of squared CV residuals
  CV.MSPE.pls = PRESS.pls / nrow(data.train)  # MSPE for internal CV
  ind.best.pls = which.min(CV.MSPE.pls) # Optimal number of components
  
  get.MSPE = function(Y, Y.hat){
    return(mean((Y - Y.hat)^2))
  }
  
  ### Get predictions and calculate MSPE on the validation fold
  ### Set ncomps equal to the optimal number of components
  pred.pls = predict(fit.pls, data.valid, ncomp = ind.best.pls)
  MSPE.pls = get.MSPE(Y.valid, pred.pls)
  all.MSPEs[i, "PLS"] = MSPE.pls
}

#MSPE for full data 

### Make boxplots
boxplot(all.MSPEs, main = "MSPE for 10-fold CV")

#RMSPE
all.RMSPEs = apply(all.MSPEs, 1, function(W){
  best = min(W)
  return(W / best)
})
all.RMSPEs = t(all.RMSPEs)

### Make a boxplot of RMSPEs
boxplot(all.RMSPEs, main = paste0("CV RMSPEs over ", K, " folds"))
