install.packages("PRROC")
library("PRROC")

#read in file
image_data <- read.csv("C://Users//siddm//Downloads//imageFixed.csv")

imagedata_y <- image_data$recurrence
image_data <- subset(image_data, select=-c(X, patients, recurrence))

# imbalanced outcome value
# we only have 85 positive values for recurrence
table(imagedata_y)

# reduce data dimensionality via PCA, so many variables
# Perform PCA 
pr.out <- prcomp(image_data , scale = TRUE)
phi <- pr.out$rotation # save for next predicting dataset so that predictions are appropriately handled
Z <- pr.out$x

dfZ <- data.frame(Z[,1:20]) # find how to do elbow
dfZ$recurrence <- imagedata_y

#split dataset into training and testing
set.seed(2023)

# test-train split set-up
index.train <- sample(1:dim(dfZ)[1], 0.8*dim(dfZ)[1])
data.train <- dfZ[index.train,]
data.test <- dfZ[-index.train,]

# train model
logistic.model <- glm(recurrence~., data=data.train, family='binomial')


# predict on the test set, obtain the predicted P(Y=1)

p.predict <- predict(logistic.model, data.test, type='response')
y.predict <- ifelse(p.predict>=0.5, 1, 0) # find a way to select for the best threshold
y.test <- data.test$recurrence
table(y.predict, y.test)
acc.test <- mean(y.predict==y.test)
acc.test

#AIC selection
step(logistic.model, method='backward')

#model from step above (AIC selection)
selected.model <- glm(recurrence ~ PC1 + PC3 + PC4 + PC10 + PC11 + PC12 + 
                        PC14 + PC15, family = "binomial", data = data.train)


#accuracy of AIC selected model 
selected.p.predict <- predict(selected.model, data.test, type='response')
selected.y.predict <- ifelse(selected.p.predict>=0.5, 1, 0) # find a way to select for the best threshold
y.test <- data.test$recurrence
table(selected.y.predict, y.test)
selected.acc.test <- mean(selected.y.predict==y.test)
selected.acc.test

# metrics like accuracy and ROC/AUROC
plot(roc.curve(scores.class0 = p.predict[y.test==1], 
               scores.class1 = p.predict[y.test==0], curve = TRUE),
     ylab='True Postive Rate', xlab='False Negative Rate (1 - True Negative Rate)')

# imbalanced dataset metrics
# true positive
TP <- intersect(which(y.test==1), which(y.predict==1))
# true negative
TN <- intersect(which(y.test==0), which(y.predict==0))
# false positive
FP <- which(y.test[which(y.predict==1)]==0)
# false negative
FN <- which(y.test[which(y.predict==0)]==1)

# true postive rate
TPR <- length(TP) / (length(TP) + length(FN))
TPR
# true negative rate
TNR <- length(TN) / (length(TN) + length(FP))
TNR
# precision
prec <- length(TP) / (length(TP) + length(FP))
prec


# randomly shuffle the index
index.random <- sample(1:dim(image_data)[1])

# split the data (index) into 5 folds 
groups <- cut(1:dim(dfZ)[1], 5, labels = FALSE)
index.fold <- split(index.random, groups)

# an empty vector to save individual MSE
aCCs <- c()

# 5-fold cross-validation
for(index.test in index.fold){
  
  # creat training and test set
  data.test <- dfZ[index.test,]
  data.train <- dfZ[-index.test,]
  
  # fit a linear model on the training set
  lm.model <- glm(recurrence ~ PC1 + PC3 + PC4 + PC10 + PC11 + PC12 + 
                    PC14 + PC15, family = "binomial", data = data.train)
  
  # predict on the test set
  yhat.test <- predict(lm.model, data.test)
  
  
  p.predict <- predict(lm.model, data.test, type='response')
  y.predict <- ifelse(p.predict>=0.5, 1, 0) # find a way to select for the best threshold
  y.test <- data.test$recurrence
  table(y.predict, y.test)
  acc.test <- mean(y.predict==y.test)
  aCCs <- c(aCCs, acc.test)
  acc.test
  
}
# plot 5 Accuracies

aCCsPercent <- aCCs * 100
plot(1:5, aCCsPercent, type='b', col='red', xlab='Fold', ylab='Accuracy', ylim=c(10,100))

# Average 5 Accuracies
mean(aCCs)