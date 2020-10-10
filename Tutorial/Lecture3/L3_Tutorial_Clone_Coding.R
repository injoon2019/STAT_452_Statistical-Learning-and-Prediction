# Title: STAT 452 L3 Tutorial Clone Coding
# Author: Injun Son
# Date: September 28, 2020
# setwd("C:/Users/William/Desktop/STAT 452-652/Lecture 3")
source("Read Wine Data.R")

### Set R's pseudo-random number generator to start at the same
### place every time we run this script. The actual number used
### doesn't matter.
set.seed(17648372)

### Split the data into training and test sets.
### 75%/25% seems reasonable
n = nrow(data)
new.order = sample.int(n) ### Shuffled numbers from 1 to n
size.train = floor(n*0.75) ### Number of observations in our training
### set. Use floor() to round down
ind.train = new.order[1:size.train] ### Indices of observations
### to put in training set
ind.valid = new.order[(size.train + 1):n] ### Indices of observations
### to put in validation set
data.train = data[ind.train, ] ### Keep only observations in ind.train
data.valid = data[ind.valid, ] ### Keep only observations in ind.valid


### Fit linear models to predict alcohol using each predictor
### individually, all predictors together, and all interactions
### Note: These models must be fit using data.train so that we can
### evaluate their MSPE on data.valid
fit.sugar = lm(alcohol ~ sugar, data = data.train)
fit.density = lm(alcohol ~ density, data = data.train)
fit.pH = lm(alcohol ~ pH, data = data.train)
fit.sulphates = lm(alcohol ~ sulphates, data = data.train)
fit.all = lm(alcohol ~ ., data = data.train)
fit.int = lm(alcohol ~ . ^ 2, data = data.train)



### Get predictions on the validation set for each model using the
### predict() function.
pred.sugar = predict(fit.sugar, data.valid)
pred.density = predict(fit.density, data.valid)
pred.pH = predict(fit.pH, data.valid)
pred.sulphates = predict(fit.sulphates, data.valid)
pred.all = predict(fit.all, data.valid)
pred.int = predict(fit.int, data.valid)

### When we calculate validation set MSPEs for our models, we will
### end up repeating the same calculation 5 times. Let's make a
### function to do this for us.
### I will discuss the syntax of writing functions in my video
get.MSPE = function(Y, Y.hat) {
  residuals = Y - Y.hat
  resid.sq = residuals ^ 2
  SSPE = sum(resid.sq)
  MSPE = SSPE / length(Y)
  return(MSPE)
}


### Use our get.MSPE() function to calculate the validation set MSPE
### of each model
Y.valid = data.valid$alcohol
MSPE.sugar = get.MSPE(Y.valid, pred.sugar)
MSPE.density = get.MSPE(Y.valid, pred.density)
MSPE.pH = get.MSPE(Y.valid, pred.pH)
MSPE.sulphates = get.MSPE(Y.valid, pred.sulphates)
MSPE.all = get.MSPE(Y.valid, pred.all)
MSPE.int = get.MSPE(Y.valid, pred.int)


### Let's compare these validation set MSPEs
print(MSPE.sugar)
print(MSPE.density)
print(MSPE.pH)
print(MSPE.sulphates)
print(MSPE.all)
print(MSPE.int)

# It looks like the most complicated model has the best validation set.
#Will this always happend?

#Let's try 10 splits. That is, we want to repeat out previous analysis to 10 times
#There are a few different ways to do this. There are two methods.
#First, for loops, then the replicate() function

#1. for loop
M = 10 # number of times to split
all.split.MSPEs = array(0, c(M, 6)) # An array to store calculated MSPEs
                                    #fill array with 0's and M rows and 6 columns

colnames(all.split.MSPEs)  = c("sugar", "density", "pH", 
                               "suphates", "all", "int") #Set model names

for (i in 1:M){
  #Split the data into training and test sets
  new.order = sample.int(n) #shuffled numbers from 1 to n
  ind.train = which(new.order <= n*0.75) 
  ind.valid = which(new.order > n*0.75)
  data.train = data[ind.train, ]
  data.valid = data[ind.valid, ]
  
  fit.sugar = lm(alcohol ~ sugar, data = data.train)
  fit.density = lm(alcohol ~ density, data = data.train)
  fit.pH = lm(alcohol ~ pH, data = data.train)
  fit.sulphates = lm(alcohol ~ sulphates, data = data.train)
  fit.all = lm(alcohol ~ ., data = data.train)
  fit.int = lm(alcohol ~ . ^ 2, data = data.train)
  
  ### Get predictions on the validation set for each model using the
  ### predict() function.
  pred.sugar = predict(fit.sugar, data.valid)
  pred.density = predict(fit.density, data.valid)
  pred.pH = predict(fit.pH, data.valid)
  pred.sulphates = predict(fit.sulphates, data.valid)
  pred.all = predict(fit.all, data.valid)
  pred.int = predict(fit.int, data.valid)
  
  ### Use our get.MSPE() function to calculate the validation set MSPE
  ### of each model
  Y.valid = data.valid$alcohol
  MSPE.sugar = get.MSPE(Y.valid, pred.sugar)
  MSPE.density = get.MSPE(Y.valid, pred.density)
  MSPE.pH = get.MSPE(Y.valid, pred.pH)
  MSPE.sulphates = get.MSPE(Y.valid, pred.sulphates)
  MSPE.all = get.MSPE(Y.valid, pred.all)
  MSPE.int = get.MSPE(Y.valid, pred.int)
  
  #This part is neW! Store calculated MSPEs in our array
  all.split.MSPEs[i, 1] = MSPE.sugar
  all.split.MSPEs[i, 2] = MSPE.density
  all.split.MSPEs[i, 3] = MSPE.pH
  all.split.MSPEs[i, 4] = MSPE.sulphates
  all.split.MSPEs[i, 5] = MSPE.all
  all.split.MSPEs[i, 6] = MSPE.int
}

# Another approach to iteration is using the replicate() function
# This function requires that we specify some code, and how many
# times we want the code to run. If we put all out MSPEs in a vector,
# replicate() will stack these vectors into an arrow for us

all.split.MSPEs = replicate(M, {
  ### Split the data into training and test sets.
  ### 75%/25% seems reasonable
  new.order = sample.int(n) ### Shuffled numbers from 1 to n
  ind.train = which(new.order <= n * 0.75) ### Indices of observations
  ### to put in training set
  ind.valid = which(new.order > n * 0.75) ### Indices of observations
  ### to put in validation set
  data.train = data[ind.train, ] ### Keep only observations in ind.train
  data.valid = data[ind.valid, ] ### Keep only observations in ind.valid
  
  
  ### Fit linear models to predict alcohol using each predictor
  ### individually, all predictors together, and all interactions
  ### Note: These models must be fit using data.train so that we can
  ### evaluate their MSPE on data.valid
  fit.sugar = lm(alcohol ~ sugar, data = data.train)
  fit.density = lm(alcohol ~ density, data = data.train)
  fit.pH = lm(alcohol ~ pH, data = data.train)
  fit.sulphates = lm(alcohol ~ sulphates, data = data.train)
  fit.all = lm(alcohol ~ ., data = data.train)
  fit.int = lm(alcohol ~ . ^ 2, data = data.train)
  
  
  ### Get predictions on the validation set for each model using the
  ### predict() function.
  pred.sugar = predict(fit.sugar, data.valid)
  pred.density = predict(fit.density, data.valid)
  pred.pH = predict(fit.pH, data.valid)
  pred.sulphates = predict(fit.sulphates, data.valid)
  pred.all = predict(fit.all, data.valid)
  pred.int = predict(fit.int, data.valid)
  
  ### Use our get.MSPE() function to calculate the validation set MSPE
  ### of each model
  Y.valid = data.valid$alcohol
  MSPE.sugar = get.MSPE(Y.valid, pred.sugar)
  MSPE.density = get.MSPE(Y.valid, pred.density)
  MSPE.pH = get.MSPE(Y.valid, pred.pH)
  MSPE.sulphates = get.MSPE(Y.valid, pred.sulphates)
  MSPE.all = get.MSPE(Y.valid, pred.all)
  MSPE.int = get.MSPE(Y.valid, pred.int)
  
  # This part is new ! Put calculated MSPEs into a single vector, then return this vector
  
  this.MSPEs = c(MSPE.sugar, MSPE.density, MSPE.pH, MSPE.sulphates, MSPE.all, MSPE.int)
  names(this.MSPEs) = c("sugar", "density", "pH", "sulphates", "all", "int")
  
  return(this.MSPEs)
})

### Note: replicate() returns an array with different models
### corresponding to different rows. I would rather have models
### correspond to columns. We can swap rows and columns of an array
### (or similar object) using the t() function.
all.split.MSPEs = t(all.split.MSPEs)

#We now have validation set MSPEs for our models, let's make boxplots.
#We can give out boxplot a title using an optional input called "main"
boxplot(all.split.MSPEs, main="Boxplot of validation set MSPEs for 10 data splits")

# It would also be nice to see how well each model is doing relative to
# the others. For each data split, we will find the lowest MSPE, and 
# divide all model's MSPEs by this lowest value. This gives the best
# model a value of 1, and all other models scores are much larger

rel.split.MSPEs = apply(all.split.MSPEs, 1, function(W){
  best = min(W)
  return(W/best)
})

#apply() returns an array with models as rows, so use t() to re-orient our results
rel.split.MSPEs = t(rel.split.MSPEs)

boxplot(rel.split.MSPEs, main="Boxplot of relative validation set MSPEs for 10 data splits")


#Now that we 've covered data splitting, let's do cross validation
#The difference between repeated data splitting and cross validation is that, 
# with CV, we decide in advance which values t include in the validation set
# for each iteration. This will make more sense when you see it in action.

# Let's do 10-fold CV. We need to divide the dataset into 10 folds.
# We can do this by ranomly sampling the numbers from 1 to 10 and attaching
# these to our dataset as fold labels. 
n.fold = n/10 # Number of observations in each fold
n.fold = ceiling(n.fold) # Round up to make sure we get enough labels
                         # We can remove any excess later

ordered.ids = rep(1:10, times = n.fold)
ordered.ids = ordered.ids[1:n] # Remove excess label
shuffle = sample.int(n) #Randomly permute the numbers 1 to n
shuffle = smaple.int(n) #Randomly permute the numbers 1 to n
shuffled.ids = ordered.ids[shuffle] #Use shuffle to permute the fold labels

data.CV = data # Crerate a copy of our dataset
data.CV$fold = shuffled.ids # Add a column to our new dataset containing the fold labels

# Next, let's actaully do the cross validation. 
CV.MSPEs = array(0, dim = c(10, 6)) # An array to store valeus 
colnames(CV.MSPEs) = colnames(all.split.MSPEs) #

for(i in 1:10){
  data.train = filter(data.CV, fold != i)
  data.valid = filter(data.CV, fold==i)
  
  #Remove fold from training and validation sets since it isn't a real predictor
  data.train = select(data.train, -fold)
  data.valid = select(data.valid, -fold)
  
  fit.sugar = lm(alcohol ~ sugar, data = data.train)
  fit.density = lm(alcohol ~ density, data = data.train)
  fit.pH = lm(alcohol ~ pH, data = data.train)
  fit.sulphates = lm(alcohol ~ sulphates, data = data.train)
  fit.all = lm(alcohol ~ ., data = data.train)
  fit.int = lm(alcohol ~ . ^ 2, data = data.train)
  
  
  ### Get predictions on the validation set for each model using the
  ### predict() function.
  pred.sugar = predict(fit.sugar, data.valid)
  pred.density = predict(fit.density, data.valid)
  pred.pH = predict(fit.pH, data.valid)
  pred.sulphates = predict(fit.sulphates, data.valid)
  pred.all = predict(fit.all, data.valid)
  pred.int = predict(fit.int, data.valid)
  
  ### Use our get.MSPE() function to calculate the validation set MSPE
  ### of each model
  Y.valid = data.valid$alcohol
  MSPE.sugar = get.MSPE(Y.valid, pred.sugar)
  MSPE.density = get.MSPE(Y.valid, pred.density)
  MSPE.pH = get.MSPE(Y.valid, pred.pH)
  MSPE.sulphates = get.MSPE(Y.valid, pred.sulphates)
  MSPE.all = get.MSPE(Y.valid, pred.all)
  MSPE.int = get.MSPE(Y.valid, pred.int)
  
  ### Store MSPEs
  CV.MSPEs[i, 1] = MSPE.sugar
  CV.MSPEs[i, 2] = MSPE.density
  CV.MSPEs[i, 3] = MSPE.pH
  CV.MSPEs[i, 4] = MSPE.sulphates
  CV.MSPEs[i, 5] = MSPE.all
  CV.MSPEs[i, 6] = MSPE.int
}

boxplot(CV.MSPEs, main="Boxplot of CV Error With 10 Folds")

rel.CV.MSPEs = apply(CV.MSPEs, 1, function(W){
  best = min(W)
  return (W/best)
})

rel.CV.MSPEs = t(rel.CV.MSPEs)

boxplot(rel.CV.MSPEs, main="Boxplot of Relative CV Error with 10 folds")

########################################################################
### Next, let's do boostrap resampling. This will look very similar  ###
### to CV, but the way we choose the training and validation sets is ###
### different. To be consistent, let's do 10 resamples.              ###
########################################################################

### I like to generate indices for boostrap sampling inside the 
### for loop, so all we need now is the array.
boot.MSPEs = array(0, dim = c(10, 6))
colnames(boot.MSPEs) = colnames(CV.MSPEs)

for(i in 1:10){
  ### Generate the indices for our training sample. Boostrap resampling
  ### is done with replacement, so we need to set replace = T (i.e. TRUE)
  ids.train = sample.int(n, replace = T)
  
  ### There is a nice shortcut we can use to get the validation set
  ### indices. The setdiff() function gives us all the elements of
  ### the first list which are not also in the second list.
  ids.valid = setdiff(1:n, ids.train)
  
  ### Now we can get our training/validation sets and get the MSPEs
  ### Using ids.train as a row index for data will copy any rows
  ### that are copied in ids.train.
  data.train = data[ids.train,]
  data.valid = data[ids.valid,]
  
  ###################################################################
  ### This code is essentially identical to the for loop we used  ###
  ### for data splitting. If you understood that, don't worry too ###
  ### much about going through this code in-depth                 ###
  ###################################################################
  
  ### Fit linear models to predict alcohol using each predictor
  ### individually, all predictors together, and all interactions
  ### Note: These models must be fit using data.train so that we can
  ### evaluate their MSPE on data.valid
  fit.sugar = lm(alcohol ~ sugar, data = data.train)
  fit.density = lm(alcohol ~ density, data = data.train)
  fit.pH = lm(alcohol ~ pH, data = data.train)
  fit.sulphates = lm(alcohol ~ sulphates, data = data.train)
  fit.all = lm(alcohol ~ ., data = data.train)
  fit.int = lm(alcohol ~ . ^ 2, data = data.train)
  
  
  ### Get predictions on the validation set for each model using the
  ### predict() function.
  pred.sugar = predict(fit.sugar, data.valid)
  pred.density = predict(fit.density, data.valid)
  pred.pH = predict(fit.pH, data.valid)
  pred.sulphates = predict(fit.sulphates, data.valid)
  pred.all = predict(fit.all, data.valid)
  pred.int = predict(fit.int, data.valid)
  
  ### Use our get.MSPE() function to calculate the validation set MSPE
  ### of each model
  Y.valid = data.valid$alcohol
  MSPE.sugar = get.MSPE(Y.valid, pred.sugar)
  MSPE.density = get.MSPE(Y.valid, pred.density)
  MSPE.pH = get.MSPE(Y.valid, pred.pH)
  MSPE.sulphates = get.MSPE(Y.valid, pred.sulphates)
  MSPE.all = get.MSPE(Y.valid, pred.all)
  MSPE.int = get.MSPE(Y.valid, pred.int)
  
  ### Store MSPEs
  boot.MSPEs[i, 1] = MSPE.sugar
  boot.MSPEs[i, 2] = MSPE.density
  boot.MSPEs[i, 3] = MSPE.pH
  boot.MSPEs[i, 4] = MSPE.sulphates
  boot.MSPEs[i, 5] = MSPE.all
  boot.MSPEs[i, 6] = MSPE.int
}


### Make a boxplot of the scores
boxplot(boot.MSPEs, 
        main = "Boxplot of Bootstrap Error With 10 Resamples")


### Calculate relative errors and make boxplots
rel.boot.MSPEs = apply(boot.MSPEs, 1, function(W){
  best = min(W)
  return(W / best)
})

rel.boot.MSPEs = t(rel.boot.MSPEs)

boxplot(rel.boot.MSPEs, 
        main = "Boxplot of Relative Bootstrap Error With 10 Resamples")

### Calculate relative errors and make boxplots
rel.boot.MSPEs = apply(boot.MSPEs, 1, function(W){
  best = min(W)
  return(W / best)
})

rel.boot.MSPEs = t(rel.boot.MSPEs)

boxplot(rel.boot.MSPEs, 
        main = "Boxplot of Relative Bootstrap Error With 10 Resamples")


#########################################################################
### Finally, let's replicate the CV and boostrap procedures 20 times  ###
### each to get a sense of the uncertainty in our error estimates.    ###
### Much of the code for this section is just copy/pasted from above. ###
### In the interest of readability, I will remove comments that we    ###
### have already seen and just explain what's happening with the new  ###
### code.                                                             ###
#########################################################################

n.rep = 20 # Number of times to repeat CV/boostrap


### Start with CV. First, we need a container to store the average CV
### errors
ave.CV.MSPEs = array(0, dim = c(n.rep, 6))
colnames(ave.CV.MSPEs) = colnames(CV.MSPEs)

### We will put the entire CV section from above inside another
### for loop. This will repeat the entire CV process
### Note: we need to use a different loop variable for the outer
### for loop. It's common to use j when you have already used i
for (j in 1:n.rep) {
  n.fold = n / 10
  n.fold = ceiling(n.fold)
  
  ordered.ids = rep(1:10, times = n.fold)
  ordered.ids = ordered.ids[1:n]
  shuffle = sample.int(n)
  shuffled.ids = ordered.ids[shuffle]
  
  data.CV = data
  data.CV$fold = shuffled.ids
  
  CV.MSPEs = array(0, dim = c(10, 6))
  colnames(CV.MSPEs) = colnames(all.split.MSPEs)
  
  for (i in 1:10) {
    data.train = filter(data.CV, fold != i)
    data.valid = filter(data.CV, fold == i)
    
    ### In tutorial, I was getting an error because I wrote -folds
    ### instead of -fold. Whoops!
    data.train = select(data.train, -fold)
    data.valid = select(data.valid, -fold)
    
    fit.sugar = lm(alcohol ~ sugar, data = data.train)
    fit.density = lm(alcohol ~ density, data = data.train)
    fit.pH = lm(alcohol ~ pH, data = data.train)
    fit.sulphates = lm(alcohol ~ sulphates, data = data.train)
    fit.all = lm(alcohol ~ ., data = data.train)
    fit.int = lm(alcohol ~ . ^ 2, data = data.train)
    
    pred.sugar = predict(fit.sugar, data.valid)
    pred.density = predict(fit.density, data.valid)
    pred.pH = predict(fit.pH, data.valid)
    pred.sulphates = predict(fit.sulphates, data.valid)
    pred.all = predict(fit.all, data.valid)
    pred.int = predict(fit.int, data.valid)
    
    Y.valid = data.valid$alcohol
    MSPE.sugar = get.MSPE(Y.valid, pred.sugar)
    MSPE.density = get.MSPE(Y.valid, pred.density)
    MSPE.pH = get.MSPE(Y.valid, pred.pH)
    MSPE.sulphates = get.MSPE(Y.valid, pred.sulphates)
    MSPE.all = get.MSPE(Y.valid, pred.all)
    MSPE.int = get.MSPE(Y.valid, pred.int)
    
    CV.MSPEs[i, 1] = MSPE.sugar
    CV.MSPEs[i, 2] = MSPE.density
    CV.MSPEs[i, 3] = MSPE.pH
    CV.MSPEs[i, 4] = MSPE.sulphates
    CV.MSPEs[i, 5] = MSPE.all
    CV.MSPEs[i, 6] = MSPE.int
  }
  
  ### We now have MSPEs for each fold of one iteration of CV. Let's
  ### get the average error across these folds (think of each fold
  ### as a data split), and store the result in ave.CV.MSPEs
  this.ave.MSPEs = apply(CV.MSPEs, 2, mean)
  ave.CV.MSPEs[j,] = this.ave.MSPEs # We are replacing a whole 
  # row at once
}

boxplot(ave.CV.MSPEs,
        main = "Boxplot of 20 Replicates of Average 10-Fold CV Error")

rel.ave.CV.MSPEs = apply(ave.CV.MSPEs, 1, function(W){
  best = min(W)
  return(W / best)
})
rel.ave.CV.MSPEs = t(rel.ave.CV.MSPEs)

boxplot(rel.ave.CV.MSPEs,
        main = "Boxplot of 20 Replicates of Relative Average 10-Fold CV Error")



### Lastly, let's put the boostrap error estimation procedure inside
### a for loop.
ave.boot.MSPEs = array(0, dim = c(n.rep, 6))
colnames(ave.boot.MSPEs) = colnames(boot.MSPEs)

### We will put the entire bootstrap section from above inside another
### for loop. This will repeat the entire bootstrap process
### Note: we need to use a different loop variable for the outer
### for loop. It's common to use j when you have already used i
for (j in 1:n.rep) {
  boot.MSPEs = array(0, dim = c(10, 6))
  colnames(boot.MSPEs) = colnames(CV.MSPEs)
  
  for(i in 1:10){
    ids.train = sample.int(n, replace = T)
    ids.valid = setdiff(1:n, ids.train)
    
    data.train = data[ids.train,]
    data.valid = data[ids.valid,]
    
    fit.sugar = lm(alcohol ~ sugar, data = data.train)
    fit.density = lm(alcohol ~ density, data = data.train)
    fit.pH = lm(alcohol ~ pH, data = data.train)
    fit.sulphates = lm(alcohol ~ sulphates, data = data.train)
    fit.all = lm(alcohol ~ ., data = data.train)
    fit.int = lm(alcohol ~ . ^ 2, data = data.train)
    
    pred.sugar = predict(fit.sugar, data.valid)
    pred.density = predict(fit.density, data.valid)
    pred.pH = predict(fit.pH, data.valid)
    pred.sulphates = predict(fit.sulphates, data.valid)
    pred.all = predict(fit.all, data.valid)
    pred.int = predict(fit.int, data.valid)
    
    Y.valid = data.valid$alcohol
    MSPE.sugar = get.MSPE(Y.valid, pred.sugar)
    MSPE.density = get.MSPE(Y.valid, pred.density)
    MSPE.pH = get.MSPE(Y.valid, pred.pH)
    MSPE.sulphates = get.MSPE(Y.valid, pred.sulphates)
    MSPE.all = get.MSPE(Y.valid, pred.all)
    MSPE.int = get.MSPE(Y.valid, pred.int)
    
    boot.MSPEs[i, 1] = MSPE.sugar
    boot.MSPEs[i, 2] = MSPE.density
    boot.MSPEs[i, 3] = MSPE.pH
    boot.MSPEs[i, 4] = MSPE.sulphates
    boot.MSPEs[i, 5] = MSPE.all
    boot.MSPEs[i, 6] = MSPE.int
  }
  
  ### We now have MSPEs for each resample of one iteration of the
  ### boostrap. Let's get the average error across these resamples
  ### (think of each sample as a data split), and store the result 
  ### in ave.boot.MSPEs
  this.ave.MSPEs = apply(boot.MSPEs, 2, mean)
  ave.boot.MSPEs[j,] = this.ave.MSPEs # We are replacing a whole 
  # row at once
}

boxplot(ave.boot.MSPEs,
        main = "Boxplot of 20 Replicates of Average Bootstrap Error")

rel.ave.CV.MSPEs = apply(ave.CV.MSPEs, 1, function(W){
  best = min(W)
  return(W / best)
})
rel.ave.CV.MSPEs = t(rel.ave.CV.MSPEs)

boxplot(rel.ave.CV.MSPEs,
        main = "Boxplot of 20 Replicates of Relative Average Bootstrap Error")
