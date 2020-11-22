# Title: STAT 452 Exercise 8 L17A1
# Author: Injun Son
# Date: November 21, 2020

library(dplyr)
library(MASS)   # For ridge regression
library(glmnet) # For LASSO
library(nnet) # Fits neural net models
library(rgl)
library(FNN)
source("Helper Functions.R")


### Some of our code will be random, so we have to set the seed. 
### Use Mersenne-Twister for compatibility.
set.seed(46536737, kind = "Mersenne-Twister")

###### 1. Get to know the data
# (a) Read the data and print a summary.
vehdata = read.csv("vehicle.csv")
print(summary(vehdata))

#(b) Notice that class has been read as numeric. Convert it to a factor using
# vehdata$class = factor(vehdata$class, labels=c(¡°2D¡±, ¡°4D¡±, ¡°BUS¡±, ¡°VAN¡±))
# Show a summary of this new version of class.
vehdata$class = factor(vehdata$class, labels=c("2D", "4D", "BUS", "VAN"))
print(summary(vehdata))

# (c) Print the correlation matrix for the explanatory variables. Comment
# on any strong correlations (maybe beyond ¡¾0.7 and note especially any
#                             beyond ¡¾0.9)
mydata.cor = cor(vehdata[1:18], method = c("spearman"))
mydata.cor <- data.matrix(mydata.cor)
mydata.cor



##### 2. Split the data using the code below, where set1 will be the training set for future
# analyses and set2 the test set:
set.seed(46685326, kind = "Mersenne-Twister")
perm <- sample (x= nrow ( vehdata ))
set1 <- vehdata [ which ( perm <= 3* nrow ( vehdata )/4) , ]
set2 <- vehdata [ which ( perm > 3* nrow ( vehdata )/4) , ]

print(head(set1, 6))
print(head(set2, 6))



##### 3. Run a KNN analysis on the training data using m = 1.
# (a) Show the confusion matrix for the test data. Comment on how well
# separated the four classes are. In particular, are there classes that are
# easier/harder to separate?
X.train.raw = set1[,-19]
X.valid.raw = set2[,-19]
Y.train = as.matrix(set1[,19])
Y.valid = as.matrix(set2[,19])

### Rescale x1 using the means and SDs of x2
scale.1 <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- mean(x2[,col])
    b <- sd(x2[,col])
    x1[,col] <- (x1[,col]-a)/b
  }
  x1
}

### Rescale our training and validation sets
X.train = scale.1(X.train.raw, X.train.raw)
X.valid = scale.1(X.valid.raw, X.train.raw) # Watch the order

pred.knn = knn(X.train, X.valid, cl = set1[,19], k=1)

### Let's make a confusion matrix. 
table(pred.knn, Y.valid, dnn = c("Predicted", "Observed"))



# (b) Compute and report the test misclassification rate and approximate standard
# error

### Next, let's get the misclassification rate
(misclass.knn = mean(pred.knn != Y.valid))



##### 4. Reset the seed to set.seed(9910314, kind="Mersenne-Twister"). Tune the KNN
# using CV error with knn.cv(). Use a grid from m = 1, . . . , 40. This may take a few
# seconds or minutes.
set.seed(9910314, kind="Mersenne-Twister")


# (a) Plot the validation error with standard errors against the number of neighbours.
# Show the plot and comment: is there a clear best m or is there a broad
# range of similar values, according to the SE?


K.max = 40 # Maximum number of neighbours

### Container to store CV misclassification rates
mis.CV = rep(0, times = K.max)

for(i in 1:K.max){
  ### Progress update
  print(paste0(i, " of ", K.max))
  
  ### Fit leave-one-out CV
  this.knn = knn(X.train, X.valid, cl = set1[,19], k=i)
  
  ### Get and store CV misclassification rate
  this.mis.CV = mean(this.knn != Y.train)
  mis.CV[i] = this.mis.CV
}

### Get SEs
SE.mis.CV = sapply(mis.CV, function(r){
  sqrt(r*(1-r)/nrow(X.train))
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
knn.min = knn(X.train, X.valid, Y.train, k.min)
knn.1se = knn(X.train, X.valid, Y.train, k.1se)

(mis.min = mean(Y.valid != knn.min))
(mis.1se = mean(Y.valid != knn.1se))
