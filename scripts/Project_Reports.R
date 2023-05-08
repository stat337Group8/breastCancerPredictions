
install.packages("comprehenr")
install.packages("ROSE")
install.packages("PRROC")
install.packages("ggplot2")
library("comprehenr")
library("ggplot2")


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
  f1 <- length(TP)/(length(TP)+0.5*(length(FP)+length(FN)))
  
  return(c(accuracy, prec, TPR, TNR, f1))
}






#read in file
report_data <- read.csv("reportFixed.csv")

report_data <- subset(report_data, select=-c(X, patients))

#split dataset into training and testing
set.seed(2022)

index.train <- sample(1:dim(report_data)[1], 0.8 * dim(report_data)[1])
data.train <- report_data[index.train,]
data.test <- report_data[-index.train,]

# up and down sampling
library("ROSE")
data.train <- ROSE(recurrence ~ ., data = data.train, seed=2022)$data

# weighting outputs based on proportions
w0 <- length(data.train$recurrence)/(length(data.train$recurrence[data.train$recurrence==0]))
w1 <- length(data.train$recurrence)/(length(data.train$recurrence[data.train$recurrence==1]))
ws <- to_vec(for(r in data.train$recurrence) if (r == 0) w0 else w1)

logistic.model <- glm(recurrence~tumorER+tumorPR+tumorHER2+tumorSubType+tumorOS+tumorTNM+tumorGrade+tumorNG+tumorHist+tumorLoc+tumorPos+tumorBBC+tumorSide+mriFocal+mriContra+mriLymph+mriSkin+mriPectoral, data=data.train, family='binomial', weights=ws)
summary(logistic.model)
# predict on the test test
p.pred <- predict(logistic.model, data.test, type='response')

y.pred <- ifelse(p.pred>=0.5, 1, 0)

# calculate classification accuracy
y.truth <- data.test$recurrence
acc.test <- mean(y.pred==y.truth)
acc.test

# confusion matrix
table(y.pred, y.truth)

evaluators <- accuracies(prediction = y.pred, truth = y.truth)
evaluators

# cross validate accuracy
CVAcc <- crossVal(data.train, 0.5)
CVAcc
# plot ROC Curve
library(PRROC)
plot(roc.curve(scores.class0 = p.pred[y.truth==1],
               scores.class1 = p.pred[y.truth==0], curve = TRUE),
     ylab='True Postive Rate', xlab='False Negative Rate (1 - True Negative Rate)')

# Backwards Selection
step(logistic.model, method='backward')

selected.model <- glm(formula = recurrence ~ tumorER+tumorPR+tumorHER2+tumorSubType+tumorOS+tumorTNM+tumorGrade+tumorNG+tumorHist+tumorLoc+tumorPos+tumorBBC+tumorSide+mriFocal+mriContra+mriLymph+mriSkin+mriPectoral, family = "binomial", data = data.train)

selected.p.predict <- predict(selected.model, data.test, type='response')
selected.y.predict <- ifelse(selected.p.predict>=0.65, 1, 0) # find a way to select for the best threshold
y.test <- data.test$recurrence
table(selected.y.predict, y.truth)
selected.acc.test <- mean(selected.y.predict==y.test)
selected.acc.test
