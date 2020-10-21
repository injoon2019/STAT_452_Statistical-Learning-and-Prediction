library(dplyr)
library(MASS)   # For ridge regression
library(glmnet) # For LASSO
source("Helper Functions.R")
data = na.omit(airquality[, 1:4])
data$TWcp = data$Temp*data$Wind
data$TWrat = data$Temp/data$Wind

set.seed(93425633)


### Create a container for MSPEs. Let's include ordinary least-squares
### regression for reference
all.models = c("Ridge", "LASSO-Min", "LASSO-1se")
all.MSPEs = array(0, dim = c(K, length(all.models)))
colnames(all.MSPEs) = all.models

# Ridge Regression
lambda.vals = seq(from = 0, to = 100, by = 0.05)

fit.ridge = lm.ridge(Ozone ~ ., lambda = lambda.vals, 
                     data = data)


### Get best lambda value and its index
### Note: Best is chosen according to smallest GCV value. We can 
###       get GCV from a ridge regression object using $GCV
ind.min.GCV = which.min(fit.ridge$GCV)
lambda.min = lambda.vals[ind.min.GCV]

# Optimal value for lambda: 0.2


### Get coefficients corresponding to best lambda value
### We can get the coefficients for every value of lambda using
### the coef() function on a ridge regression object
all.coefs.ridge = coef(fit.ridge)
coef.min = all.coefs.ridge[ind.min.GCV,]
#                   Solar.R          Wind          Temp          TWcp         TWrat 
# -161.63123617    0.06284069    6.82529896    2.45651021   -0.10883212    1.64685249 

# Lm parameters
fit.ls = lm(Ozone ~ ., data = data)
# (Intercept)      Solar.R         Wind         Temp         TWcp        TWrat  
# -191.19856      0.06384      9.56187      2.89466     -0.14751      1.36619  

#######################
#########################
#Q2

#######################################################################
### Now we can do the LASSO. This model is fit using the glmnet()   ###
### or cv.glmnet() functions in the glmnet package. LASSO also has  ###
### a tuning parameter, lambda, which we have to choose.            ###
### Fortunately, the cv.glmnet() function does CV internally, and   ###
### lets us automatically find the 'best' value of lambda.          ###
#######################################################################

matrix.train.raw = model.matrix(Ozone ~ ., data = data)
matrix.train = matrix.train.raw[,-1]

all.LASSOs = cv.glmnet(x = matrix.train, y = data$Ozone)

### Get both 'best' lambda values using $lambda.min and $lambda.1se
lambda.min = all.LASSOs$lambda.min
lambda.1se = all.LASSOs$lambda.1se
#lambda: 0.366 lambda.min 0.3658798   
#lambda: 7.182 lambda.1se 7.18237


### Get the coefficients for our two 'best' LASSO models
coef.LASSO.min = predict(all.LASSOs, s = lambda.min, type = "coef")
# (Intercept) -87.08234278
# Solar.R       0.05712184
# Wind          .         
# Temp          1.36664563
# TWcp         -0.01234339
# TWrat         2.29832095

coef.LASSO.1se = predict(all.LASSOs, s = lambda.1se, type = "coef")
# (Intercept) -51.4433762
# Solar.R       .        
# Wind          .        
# Temp          0.9551653
# TWcp          .        
# TWrat         2.0423891

### Get which predictors are included in our models (i.e. which 
### predictors have non-zero coefficients)
included.LASSO.min = predict(all.LASSOs, s = lambda.min, 
                             type = "nonzero")
included.LASSO.1se = predict(all.LASSOs, s = lambda.1se, 
                             type = "nonzero")


############
### Step wise
fit.start = lm(Ozone ~ 1, data = data)
fit.end = lm(Ozone ~ ., data = data.train)

step.BIC = step(fit.start, list(upper = fit.end), k = log(n.train), trace = 0)

# (Intercept)        TWrat         Temp      Solar.R  
# -93.3042       2.8633       1.2523       0.0596 


# ### Get predictions from both models on the validation fold. First,
# ### we need to create a predictor matrix from the validation set.
# ### Remember to include the intercept in model.matrix(), then delete
# ### it in the next step. 
# matrix.valid.LASSO.raw = model.matrix(alcohol ~ ., data = data.valid)
# matrix.valid.LASSO = matrix.valid.LASSO.raw[,-1]
# pred.LASSO.min = predict(all.LASSOs, newx = matrix.valid.LASSO,
#                          s = lambda.min, type = "response")
# pred.LASSO.1se = predict(all.LASSOs, newx = matrix.valid.LASSO,
#                          s = lambda.1se, type = "response")
# 
# ### Calculate MSPEs and store them
# MSPE.LASSO.min = get.MSPE(Y.valid, pred.LASSO.min)
# all.MSPEs[i, "LASSO-Min"] = MSPE.LASSO.min
# 
# MSPE.LASSO.1se = get.MSPE(Y.valid, pred.LASSO.1se)
# all.MSPEs[i, "LASSO-1se"] = MSPE.LASSO.1se