# load packages
install.packages("comprehenr")
install.packages("ROSE")
install.packages("PRROC")
install.packages("ggplot2")
library("comprehenr")
library("ggplot2")

# set working directory
# setwd("PATH TO FOLDER")

# helper functions
# cross validation accuracy score
crossVal <- function(data, threshold){
  # randomly shuffle the index
  
  index.random <- sample(1:dim(data)[1])
  
  # split the data (index) into 5 folds 
  groups <- cut(1:dim(data)[1], 5, labels = FALSE)
  index.fold <- split(index.random, groups)
  
  # an empty vector to save individual MSE
  aCCs <- c()
  
  # 5-fold cross-validation
  for(index.test in index.fold){
    
    # creat training and test set
    test <- data[index.test,]
    train <- data[-index.test,]
    
    # fit a linear model on the training set
    model <- glm(recurrence ~ ., family = "binomial", data = train)
    
    # predict on the test set
    prob <- predict(model, test, type='response')
    pred <- ifelse(prob>=threshold, 1, 0) # find a way to select for the best threshold
    truth <- test$recurrence
    accuracy <- mean(pred==truth)
    aCCs <- c(aCCs, accuracy)
    table(pred, truth)
  }
  # plot 5 Accuracies
  
  aCCsPercent <- aCCs * 100
  plot(1:5, aCCsPercent, type='b', col='red', xlab='Fold', ylab='Accuracy', ylim=c(10,100))
  
  # Average 5 Accuracies
  return(mean(aCCs))
}

# precision, TPR, TNR
accuracies <- function(prediction, truth){
  
  accuracy <- mean(prediction==truth)
  
  ## true positive
  TP <- intersect(which(truth==1), which(prediction==1))
  ## true negative
  TN <- intersect(which(truth==0), which(prediction==0))
  ## false positive
  FP <- which(truth[which(prediction==1)]==0)
  ## false negative
  FN <- which(truth[which(prediction==0)]==1)
  ## true postive rate
  TPR <- length(TP) / (length(TP) + length(FN))
  ## true negative rate
  TNR <- length(TN) / (length(TN) + length(FP))
  ## precision
  prec <- length(TP) / (length(TP) + length(FP))
  ## f1score
  f1 <- TP/(TP+0.5*(FP+FN))

  return(c(accuracy, prec, TPR, TNR, f1))
}



# open dataset
dforig <- read.csv('./data/comboFixed.csv')
dfy <- dforig$recurrence
df <- subset(dforig, select=-c(X, patients, recurrence))

# set random seed for consistent comparison between analyses
set.seed(2022)

# examine imbalanced outcome value
table(dfy)

# reduce data dimensionality
## Perform PCA 
pr.out <- prcomp(df , scale = TRUE)
phi <- pr.out$rotation # save for next predicting dataset so that predictions are appropriately handled
Z <- pr.out$x

## Graph PC1/PC2
pcs <- data.frame(Z[,1:2])
pcs <- cbind(pcs, dforig$recurrence)
ggplot(data = pcs)+
  geom_point(aes(x=PC1,y=PC2, color=dforig$recurrence), size=2)+
  theme_bw()

## Evaluate PCA Data Variance using elbow method
PVE.matrix <- summary(pr.out)$importance
PVE <- PVE.matrix[2,]
plot(PVE, xlab='Principle Component', ylab='Proportion of Variance Explained', cex.lab=1.5)
abline(a=PVE[10], b=0)
text(x=10, y=0.04)

# Isolate important PCs for graphing
dfZ <- data.frame(Z[,1:20]) # find how to do elbow
dfZ$recurrence <- dfy

# test-train split set-up
index.train <- sample(1:dim(dfZ)[1], 0.8*dim(dfZ)[1])
data.train <- dfZ[index.train,]
data.test <- dfZ[-index.train,]

# up and down sampling
library("ROSE")
data.train <- ROSE(recurrence ~ ., data = data.train, seed=2022)$data

# weighting outputs based on proportions
w0 <- length(data.train$recurrence)/(length(data.train$recurrence[data.train$recurrence==0]))
w1 <- length(data.train$recurrence)/(length(data.train$recurrence[data.train$recurrence==1]))
ws <- to_vec(for(r in data.train$recurrence) if (r == 0) w0 else w1)

# train model
logistic.model <- glm(recurrence~., family='binomial', data=data.train, weights=ws)

# basic evaluation of model
p.predict <- predict(logistic.model, data.test, type='response')
y.predict <- ifelse(p.predict>=0.65, 1, 0)
y.test <- data.test$recurrence
table(y.predict, y.test)
evaluators <- accuracies(prediction = y.predict, truth = y.test)
evaluators

# cross validate accuracy
CVAcc <- crossVal(dfZ, 0.65)

# ROC Curve
library("PRROC")
plot(roc.curve(scores.class0 = p.predict[y.test==1], 
               scores.class1 = p.predict[y.test==0], curve = TRUE),
     ylab='True Postive Rate', xlab='False Negative Rate (1 - True Negative Rate)')

# backward select using AIC
step(logistic.model, method='backward')

# train selected model
selected.model <- glm(formula = recurrence ~ PC2 + PC3 + PC4 + PC5 + PC9 + PC10 + 
                        PC12 + PC14 + PC16 + PC17 + PC18 + PC19, family = "binomial", 
                      data = data.train, weights = ws)

# run evaluation on selected model
selected.p.predict <- predict(selected.model, data.test, type='response')
selected.y.predict <- ifelse(selected.p.predict>=0.65, 1, 0) # find a way to select for the best threshold
y.test <- data.test$recurrence
table(selected.y.predict, y.test)
selected.acc.test <- mean(selected.y.predict==y.test)
selected.acc.test
plot(roc.curve(scores.class0 = selected.p.predict[y.test==1], 
               scores.class1 = selected.p.predict[y.test==0], curve = TRUE),
     ylab='True Postive Rate', xlab='False Negative Rate (1 - True Negative Rate)')
