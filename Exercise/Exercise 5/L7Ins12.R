library(caret)
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