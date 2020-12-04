# Title: STAT 452 Project 2
# Author: Injun Son
# Date: December 2, 2020

library(dplyr)
library(MASS)   # For ridge regression
library(glmnet) # For LASSO
library(nnet) # Fits neural net models
library(rgl)
library(FNN)
library(car)    # For ANOVA after logistic regression with nnet
library(mgcv)   # For GAM
library(klaR)   # For naive Bayes
library(rpart)        # For fitting classification trees
library(rpart.plot)   # For plotting classification trees
library(randomForest) # For random forests
library(gbm)          # For boosting
source("Helper Functions.R")


### Some of our code will be random, so we have to set the seed. 
### Use Mersenne-Twister for compatibility.
set.seed(46685326, kind = "Mersenne-Twister")
data = read.csv("P2Data2020.csv")
# Convert it to a factor
data$Y = factor(data$Y, labels=c("A", "B", "C", "D", "E"))
print(summary(data$Y))

perm <- sample (x= nrow ( data ))
set1 <- data [ which ( perm <= 3* nrow ( data )/4) , ]
set2 <- data [ which ( perm > 3* nrow ( data )/4) , ]

X.train.raw = set1[,-1]
X.valid.raw = set2[,-1]
Y.train = as.matrix(set1[,1])
Y.valid = as.matrix(set2[,1])


################################################ KNN analysis
# Rescale x1 using the means and SDs of x2
scale.1 <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- mean(x2[,col])
    b <- sd(x2[,col])
    x1[,col] <- (x1[,col]-a)/b
  }
  x1
}

# Rescale our training and validation sets
X.train.scale = scale.1(X.train.raw, X.train.raw)
X.valid.scale = scale.1(X.valid.raw, X.train.raw) # Watch the order

K.max = 40 # Maximum number of neighbours

### Container to store CV misclassification rates
mis.CV = rep(0, times = K.max)

for(i in 1:K.max){
  ### Progress update
  print(paste0(i, " of ", K.max))
  
  ### Fit leave-one-out CV
  this.knn = knn(X.train.scale, X.valid.scale, cl = set1[,1], k=i)
  
  ### Get and store CV misclassification rate
  this.mis.CV = mean(this.knn != Y.train)
  mis.CV[i] = this.mis.CV
}

### Get SEs
SE.mis.CV = sapply(mis.CV, function(r){
  sqrt(r*(1-r)/nrow(X.train.scale))
})

plot(1:K.max, mis.CV, xlab = "number of neighbours", ylab = "Misclassification Rate",
     ylim = c(0.6, 1))

for(i in 1:K.max){
  lower = mis.CV[i] - SE.mis.CV[i]
  upper = mis.CV[i] + SE.mis.CV[i]
  
  lines(x = c(i, i), y = c(lower, upper))
}

### Get CV min value for K
k.min = which.min(mis.CV)

thresh = mis.CV[k.min] + SE.mis.CV[k.min]
abline(h = thresh, col = "red")

### Get CV 1SE value for K
k.1se = max(which(mis.CV <= thresh))

### Finally, let's see how our tuned KNN models do
knn.min = knn(X.train.scale, X.valid.scale, Y.train, k.min)
knn.1se = knn(X.train.scale, X.valid.scale, Y.train, k.1se)

(mis.min = mean(Y.valid != knn.min))
(mis.1se = mean(Y.valid != knn.1se))

#mis.min = 0.244
#mis.1se = 0.252

################################################ Logistic Rregression
data$Y = factor(data$Y, labels=c("A", "B", "C", "D", "E"))
perm <- sample (x= nrow ( data ))
set1 <- data [ which ( perm <= 3* nrow ( data )/4) , ]
set2 <- data [ which ( perm > 3* nrow ( data )/4) , ]

data.train = set1
data.valid = set2
Y.valid = data.valid[, 1]

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
data.train.scale[,-1] = rescale(data.train.scale[,-1], data.train[,-1])
data.valid.scale[,-1] = rescale(data.valid.scale[,-1], data.train[,-1])

fit.log.nnet = multinom(Y ~ ., data = data.train.scale, maxit = 200)

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
(mul.misclass.train <- mean(ifelse(pred.class.1 == set1$Y, 
                                   yes=0, no=1)))
(mul.misclass.test <- mean(ifelse(pred.class.2 == set2$Y, 
                                  yes=0, no=1)))

pred.log.nnet = predict(fit.log.nnet, data.valid.scale)
(misclass.log.nnet = mean(pred.log.nnet != Y.valid)) ### Misclass rate
#0.232

################################################  LASSO version of logistic regression
X.train.scale = as.matrix(data.train.scale[,-1])
Y.train = data.train.scale[,1]
X.valid.scale = as.matrix(data.valid.scale[,-1])
Y.valid = data.valid.scale[,1]

fit.CV.lasso = cv.glmnet(X.train.scale, Y.train, family = "multinomial")

lambda.min = fit.CV.lasso$lambda.min
lambda.1se = fit.CV.lasso$lambda.1se

coef(fit.CV.lasso, s = lambda.min)
coef(fit.CV.lasso, s = lambda.1se)

pred.lasso.min = predict(fit.CV.lasso, X.valid.scale, s = lambda.min,
                         type = "class")
pred.lasso.1se = predict(fit.CV.lasso, X.valid.scale, s = lambda.1se,
                         type = "class")

(miss.lasso.min = mean(Y.valid != pred.lasso.min)) #0.236
(miss.lasso.1se = mean(Y.valid != pred.lasso.1se)) #0.232


################################################  LDA
scale.1 <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- mean(x2[,col])
    b <- sd(x2[,col])
    x1[,col] <- (x1[,col]-a)/b
  }
  x1
}

X.train.DA = scale.1(set1[,-1], set1[,-1])
X.valid.DA = scale.1(set2[,-1], set1[,-1])

class.col <- ifelse(set1$class==1,y=53,n= ifelse(set1$class==2,y=68,n=
                                                   ifelse(set1$class==3,y=203,n=464)))
fit.lda = lda(X.train.DA, Y.train)

plot(fit.lda, col = class.col)


pred.lda = predict(fit.lda, X.valid.DA)$class

table(Y.valid, pred.lda, dnn = c("Obs", "Pred"))

(miss.lda = mean(Y.valid != pred.lda)) #0.236

################################################  QDA
fit.qda = qda(X.train.DA, Y.train)
pred.qda = predict(fit.qda, X.valid.DA)$class
table(Y.valid, pred.qda, dnn = c("Obs", "Pred"))
(miss.qda = mean(Y.valid != pred.qda)) #0.164


################################################  Naive Bayes
perm <- sample (x= nrow ( data ))
set1 <- data [ which ( perm <= 3* nrow ( data )/4) , ]
set2 <- data [ which ( perm > 3* nrow ( data )/4) , ]
X.train = set1[,-1]
X.valid = set2[,-1]
Y.train = set1[, 1]
Y.train = as.factor(Y.train)
Y.valid = set2[, 1]
Y.valid = as.factor(Y.valid)

fit.NB.userkernel = NaiveBayes(X.train, Y.train, usekernel = T)
fit.NB.notuserkernel = NaiveBayes(X.train, Y.train, usekernel = F)

#Kernel
pred.NB.userkernel.raw = predict(fit.NB.userkernel, X.valid)
pred.NB = pred.NB.userkernel.raw$class

table(Y.valid, pred.NB, dnn = c("Obs", "Pred"))
(mis.NB = mean(Y.valid != pred.NB)) #0.212


#NOT Kernel
pred.NB.notuserkernel.raw = predict(fit.NB.notuserkernel, X.valid)
pred.NB = pred.NB.notuserkernel.raw$class

table(Y.valid, pred.NB, dnn = c("Obs", "Pred"))
(mis.NB = mean(Y.valid != pred.NB)) #0.24

##### PC
fit.PCA = prcomp(X.train, scale. = T)

X.train.PC = fit.PCA$x  # Extract the PCs
X.valid.PC = predict(fit.PCA, set2)

fit.NB.PC.userkernel = NaiveBayes(X.train.PC, Y.train, usekernel = T)
fit.NB.PC.notuserkernel = NaiveBayes(X.train.PC, Y.train, usekernel = F)

#Kernel PC
pred.NB.PC.userkernel.raw = predict(fit.NB.PC.userkernel, X.valid.PC)
pred.NB = pred.NB.PC.userkernel.raw$class
(mis.NB = mean(Y.valid != pred.NB))

#NOT Kernel PC
pred.NB.PC.notuserkernel.raw = predict(fit.NB.PC.notuserkernel, X.valid.PC)
pred.NB = pred.NB.PC.notuserkernel.raw$class
(mis.NB = mean(Y.valid != pred.NB))  #0.2

################################################  Classification Trees
p.train = 0.75
n = nrow(data)
n.train = floor(p.train*n)

ind.random = sample(1:n)
data.train = data[ind.random <= n.train,]
data.valid = data[ind.random > n.train,]
Y.valid = data.valid[,1]

fit.tree.full = rpart(Y ~ ., data = data.train, method = "class", 
                      cp = 0)
prp(fit.tree.full, type = 1, extra = 1, main = "Full Tree")
info.tree = fit.tree.full$cptable

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


pred.tree.full = predict(fit.tree.full, data.valid, type = "class")
table(Y.valid, pred.tree.full, dnn = c("Obs", "Pred"))
(mis.tree.full = mean(Y.valid != pred.tree.full)) #0.284

pred.tree.min = predict(fit.tree.min, data.valid, type = "class")
table(Y.valid, pred.tree.min, dnn = c("Obs", "Pred"))
(mis.tree.min = mean(Y.valid != pred.tree.min)) #0.296

pred.tree.1se = predict(fit.tree.1se, data.valid, type = "class")
table(Y.valid, pred.tree.1se, dnn = c("Obs", "Pred"))
(mis.tree.1se = mean(Y.valid != pred.tree.1se)) #0.296

################################################  Random Forest
data.rf = data
data.rf$Y = factor(data.rf$Y)

data.train.rf = data.rf[ind.random <= n.train,]
data.valid.rf = data.rf[ind.random > n.train,]
Y.train.rf = data.train.rf$Y
Y.valid.rf = data.valid.rf$Y

### Set tuning parameters
all.mtrys = 1:6
all.nodesizes = c(1, 5, 10, 15, 20)
all.pars.rf = expand.grid(mtry = all.mtrys, nodesize = all.nodesizes)
n.pars = nrow(all.pars.rf)

M = 5 # Number of times to repeat RF fitting. I.e. Number of OOB errors

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
    this.fit.rf = randomForest(Y ~ ., data = data.train.rf,
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
        main = "Relative OOB Boxplot") #4-1 has the lowest value
