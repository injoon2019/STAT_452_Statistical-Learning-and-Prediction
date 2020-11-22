# Title: STAT 452 Exercise 10 L18A1
# Author: Injun Son
# Date: November 21, 2020

library(dplyr)
library(MASS)   # For ridge regression
library(glmnet) # For LASSO
library(nnet) # Fits neural net models
library(rgl)
library(FNN)
source("Helper Functions.R")


###### 1. Run logistic regression using multinom().
vehdata = read.csv("vehicle.csv")
vehdata$class = factor(vehdata$class, labels=c("2D", "4D", "BUS", "VAN"))


set.seed(46685326, kind = "Mersenne-Twister")
perm <- sample (x= nrow ( vehdata ))
set1 <- vehdata [ which ( perm <= 3* nrow ( vehdata )/4) , ]
set2 <- vehdata [ which ( perm > 3* nrow ( vehdata )/4) , ]

data.train = set1
data.valid = set2
Y.valid = data.valid[, 19]

### Rescale x1 using the means and SDs of x2
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

### Rescale our training and validation sets
data.train.scale = data.train
data.valid.scale = data.valid
data.train.scale[,-19] = rescale(data.train.scale[,-19], data.train[,-19])
data.valid.scale[,-19] = rescale(data.valid.scale[,-19], data.train[,-19])

summary(data.train.scale)
summary(data.valid.scale)

head(data.train.scale, 3)
head(data.valid.scale, 3)



# (b) Run the logistic regression model using all explanatory variables.
### Fit a logistic regression model using the multinom() function from the
### nnet package.
fit.log.nnet = multinom(class ~ ., data = data.train.scale, maxit = 200)

summary(fit.log.nnet)

# i. Run Anova() on the object. Report the table of test results and comment
# on which variables seem to be important or unimportant.
Anova(fit.log.nnet)


# ii. Compute and report training and test error. Does test error seem
# better or worse than optimal KNN? (Use the standard error computed
#                                    before to help you make a sensible comment here.)

pred.class.1 <- predict(fit.log.nnet, newdata=data.train.scale, 
                        type="class")
pred.class.2 <- predict(fit.log.nnet, newdata=data.valid.scale, 
                        type="class")
(mul.misclass.train <- mean(ifelse(pred.class.1 == set1$class, 
                                   yes=0, no=1)))
(mul.misclass.test <- mean(ifelse(pred.class.2 == set2$class, 
                                  yes=0, no=1)))



### Next, let's investigate the LR's performance on the test set
pred.log.nnet = predict(fit.log.nnet, data.valid.scale)

table(Y.valid, pred.log.nnet,        ### Confusion matrix
      dnn = c("Observed", "Predicted"))

(misclass.log.nnet = mean(pred.log.nnet != Y.valid)) ### Misclass rate







### 2. Run a LASSO version of logistic regression, using CV to estimate optimal shrinkage
# in the logistic regression parameters using minimum CV-error.

# (a) Report a list of the variables included/excluded in each logit. Does
# the pattern seem somewhat consistent with the ANOVA results from
# earlier? Explain in a sentence.
X.train.scale = as.matrix(data.train.scale[,-19])
Y.train = data.train.scale[,19]
X.valid.scale = as.matrix(data.valid.scale[,-19])
Y.valid = data.valid.scale[,19]


### Let's repeat our logistic regression analysis using the glmnet package.
#fit.log.glmnet = cv.glmnet(X.train.scale, Y.train, family = "multinomial")
fit.CV.lasso = cv.glmnet(X.train.scale, Y.train, family = "multinomial")

lambda.min = fit.CV.lasso$lambda.min
lambda.1se = fit.CV.lasso$lambda.1se

coef(fit.CV.lasso, s = lambda.min)
coef(fit.CV.lasso, s = lambda.1se)
### Get predictions and investigate performance. The predict() function
### for glmnet() can give several different types of output. To get
### predicted values, set type="class". Remember to set s=0 for logistic
### regression.
# pred.log.glmnet = predict(fit.log.glmnet, X.valid.scale, type = "class",
#                           s = 0)


pred.lasso.min = predict(fit.CV.lasso, X.valid.scale, s = lambda.min,
                         type = "class")
pred.lasso.1se = predict(fit.CV.lasso, X.valid.scale, s = lambda.1se,
                         type = "class")

table(Y.valid, pred.lasso.min, dnn = c("Obs", "Pred"))
table(Y.valid, pred.lasso.1se, dnn = c("Obs", "Pred"))

(miss.lasso.min = mean(Y.valid != pred.lasso.min))
(miss.lasso.1se = mean(Y.valid != pred.lasso.1se))


##### 3. Run LDA on these data.
# (a) Make a colour plot of the classes against pairs of linear discriminants
scale.1 <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- mean(x2[,col])
    b <- sd(x2[,col])
    x1[,col] <- (x1[,col]-a)/b
  }
  x1
}


X.train.DA = scale.1(set1[,-19], set1[,-19])
X.valid.DA = scale.1(set2[,-19], set1[,-19])

### Fit an LDA model using the lda() funtion from the MASS package. This
### function uses predictor/response syntax.
class.col <- ifelse(set1$class==1,y=53,n= ifelse(set1$class==2,y=68,n=
                                                   ifelse(set1$class==3,y=203,n=464)))
fit.lda = lda(X.train.DA, Y.train)

plot(fit.lda, col = class.col)


pred.lda = predict(fit.lda, X.valid.DA)$class

table(Y.valid, pred.lda, dnn = c("Obs", "Pred"))

(miss.lda = mean(Y.valid != pred.lda))

# 4. Run QDA on these data. Report training and test error. How does test error
# compare to other methods?
fit.qda = qda(X.train.DA, Y.train)

pred.qda = predict(fit.qda, X.valid.DA)$class

table(Y.valid, pred.qda, dnn = c("Obs", "Pred"))
(miss.qda = mean(Y.valid != pred.qda))
