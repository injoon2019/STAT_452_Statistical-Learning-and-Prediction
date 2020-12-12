# The libraries we need
library(caret)
library(nnet)
library(randomForest)
library(glmnet)

#Read the data and preprocess
ins <- read.csv("Insurance.csv", header=TRUE)
ins = ins[ins$claims>0,]
ins$zone = as.factor(ins$zone)
ins$make = as.factor(ins$make)
##### Replacing factors with dummy variables
#####   One for every level.
ins.dv = data.frame(predict(dummyVars("~.", data=ins), newdata=ins))
head(ins.dv)
summary(ins.dv)

# Creating 2 sets
set.seed(7703185, kind="Mersenne-Twister")
reorder = sample.int(nrow(ins.dv))
set1 = ins.dv[reorder <= 1300,]
set2 = ins.dv[reorder > 1300,]
#######################################################################

# Helper function for later
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

#######################################################################
## Fitting Machines
#######################################################################

## Machine 1: 
set.seed(49128990, kind="Mersenne-Twister")
mod1 = lm(per~., data=set1)
summary(mod1)

# Remove columns from X that "cause problems"
set1a = set1[,-c(9,19)]
mod1a = lm(per~., data=set1a)
summary(mod1a)

p1a = predict(mod1a, newdata=set1)
e1a = mean((set1$per - p1a)^2)
p1b = predict(mod1a, newdata=set2)
e1b = mean((set2$per - p1b)^2)

#######################################################################
## Machine 2: 

set.seed(49128990, kind="Mersenne-Twister")
mod2 = cv.glmnet(y=set1$per, x=as.matrix(set1[,-c(1,9,19)]), 
                 family="gaussian")
mod2
x11()
plot(mod2) 

p2a.1 = predict(mod2, newx=as.matrix(set1[,-c(1,9,19)]), s=mod2$lambda.min)
p2a.2 = predict(mod2, newx=as.matrix(set1[,-c(1,9,19)]), s=mod2$lambda.1se)
e2a.1 = mean((set1$per - p2a.1)^2)
e2a.2 = mean((set1$per - p2a.2)^2)
p2b.1 = predict(mod2, newx=as.matrix(set2[,-c(1,9,19)]), s=mod2$lambda.min)
p2b.2 = predict(mod2, newx=as.matrix(set2[,-c(1,9,19)]), s=mod2$lambda.1se)
e2b.1 = mean((set2$per - p2b.1)^2)
e2b.2 = mean((set2$per - p2b.2)^2)

#######################################################################
## Machine 3

# Will use 2 reps of 5-fold cross-validation to tune this
#### Tune
set.seed(8415478, kind="Mersenne-Twister")
R = 2
V = 5
n1 = nrow(set1)

#### Set up CV Folds
Index1 = matrix(NA, nrow=n1, ncol=R)
for(r in 1:R){
  Index1[,r]=floor((sample.int(n1)-1)*V/n1) + 1
}

#### Set up tuning parameter grid
grid3 = c(1,2,3,5,8)

#### Set up matrices for storing error measures
Mat3 = matrix(NA, nrow=length(grid3), ncol=V*R+1)
Mat3[,1] = grid3

#### Run CV
for (r in 1:R){ 
  for(v in 1:V){
    set1.1 = set1[Index1[,r]!=v,-c(9,19)]
    set1.2 = set1[Index1[,r]==v,-c(9,19)]
    # Start counter for storing grid results in rows
    qq=1
    # Start Analysis Loop for all tuning parameters
    for(g in grid3){
      fit = ppr(per~., data=set1.1, nterms=g, 
                  max.terms=10, sm.method="gcvspline") 
      pfit = predict(fit, newdata=set1.2)
      Mat3[qq,(r-1)*V+v+1] = mean((pfit - set1.2$per)^2)
      qq = qq+1
    }
  }
}
#### Full matrix of results
Mat3
#### Mean error for each tuning parameter
mean.Mat3 = apply(Mat3[,-1], 1, mean)
cbind(grid3,mean.Mat3)

#### Plot results. 
x11(pointsize=10)
boxplot(x=sqrt(Mat3[,-1]), use.cols=FALSE, names=grid3,
        main="Plot of Mat3 by grid3")

#### Plot RELATIVE results. 
lowt = apply(Mat3[,-1], 2, min)

x11(pointsize=10)
boxplot(x=sqrt(t(Mat3[,-1])/lowt), names=grid3,
        main="Plot of Relative Mat3 by grid3")

#### Refit model with lowest mean error
mod3 = ppr(per~., data=set1, nterms=grid3[which.min(mean.Mat3)], 
           max.terms=10, sm.method="gcvspline") 
summary(mod3)

p3a = predict(mod3, newdata=set1)
e3a = mean((set1$per - p3a)^2)
p3b = predict(mod3, newdata=set2)
e3b = mean((set2$per - p3b)^2)

#######################################################################
## Machine 4

# Will use 2 reps of 5-fold cross-validation to tune this
#### Tune
set.seed(1851128, kind="Mersenne-Twister")
R = 2
V = 5
n1 = nrow(set1)

#### Set up CV Folds
Index2 = matrix(NA, nrow=n1, ncol=R)
for(r in 1:R){
  Index2[,r] = floor((sample.int(n1)-1)*V/n1) + 1
}

#### Set up tuning parameter grid
nrounds = 10
grid4 = expand.grid(c(1,2,3),c(.01,.1,1))

#### Set up matrices for storing error measures
Mat4 = matrix(NA, nrow=nrow(grid4), ncol=V*R+ncol(grid4))
Mat4[,c(1,2)] = as.matrix(grid4)

#### Run CV
for (r in 1:R){ 
  for(v in 1:V){
    set1.1y = as.matrix(set1[Index2[,r]!=v,]$per)
    set1.1xu = as.matrix(set1[Index2[,r]!=v,-c(1,9,19)]) 
    set1.1x = rescale(set1.1xu, set1.1xu) 

    set1.2y = as.matrix(set1[Index2[,r]==v,]$per)
    set1.2xu = as.matrix(set1[Index2[,r]==v,-c(1,9,19)]) 
    set1.2x = rescale(set1.2xu, set1.1xu) 
    
    # Start counter for storing grid results in rows
    for(qq in 1:nrow(Mat4)){
      ## Restart nnet nrounds times to get best fit for each set of parameters 
      val.final = 9e99
      #  check <- MSE.final
      for(i in 1:nrounds){
        fit = nnet(y=set1.1y, x=set1.1x, linout=TRUE, 
                   size=grid4[qq,2], decay=grid4[qq,1], 
                   maxit=2000, trace=FALSE)
        val = fit$value/nrow(set1.1x)
        if(val < val.final){ 
          val.final = val
          fit.final = fit
        }
      pfit = predict(fit.final, newdata=set1.2x)
      Mat4[qq,(r-1)*V+v+2] = mean((set1.2y - pfit)^2)
      }
    }
  }
}
#### Full matrix of results
Mat4

#### Mean error for each tuning parameter
mean.Mat4 = apply(Mat4[,-c(1,2)], 1, mean)
cbind(grid4,mean.Mat4)

g4names = paste(Mat4[,2],"/",Mat4[,1])

#### Plot results. 
x11(pointsize=10)
boxplot(x=sqrt(Mat4[,-c(1,2)]), use.cols=FALSE, names=g4names,
        main="Plot of Mat4 by grid4")

#### Plot RELATIVE results. 
lowt = apply(Mat4[,-c(1,2)], 2, min)

x11(pointsize=10)
boxplot(x=sqrt(t(Mat4[,-c(1,2)])/lowt), names=g4names,
        main="Plot of Relative Mat4 by grid4")

#### Plot FOCUSED RELATIVE results. 

x11(pointsize=10)
boxplot(x=sqrt(t(Mat4[,-c(1,2)])/lowt), names=g4names, ylim=c(1,1.2),
        main="Plot of Relative Mat4 by grid4")

mean.rMat4 = apply(t(Mat4[,-c(1,2)])/lowt, 2, mean)
cbind(grid4,mean.rMat4)
best.parms = as.numeric(grid4[which.min(mean.rMat4),])


#### Fit Chosen Model
set1.y = as.matrix(set1$per)
set1.xu = as.matrix(set1[,-c(1,9,19)]) 
set1.x = rescale(set1.xu, set1.xu) 

set2.y = as.matrix(set2$per)
set2.xu = as.matrix(set2[,-c(1,9,19)]) 
set2.x = rescale(set2.xu, set1.xu) 


mod4 = nnet(y=set1.y, x=set1.x, linout=TRUE, 
            size=best.parms[2], decay=best.parms[1], 
            maxit=2000, trace=FALSE)
summary(mod4)

p4a = predict(mod4, newdata=set1.x)
e4a = mean((set1.y - p4a)^2)
p4b = predict(mod4, newdata=set2.x)
e4b = mean((set2.y - p4b)^2)
