
#read in file
report_data <- read.csv("reportFixed.csv")

#split dataset into training and testing
set.seed(2022)

index.train <- sample(1:dim(report_data)[1], 0.8 * dim(report_data)[1])
data.train <- report_data[index.train,]
data.test <- report_data[-index.train,]

logistic.model <- glm(recurrence~tumorER+tumorPR+tumorHER2+tumorSubType+tumorOS+tumorTNM+tumorGrade+tumorNG+tumorHist+tumorLoc+tumorPos+tumorBBC+tumorSide+mriFocal+mriContra+mriLymph+mriSkin+mriPectoral, data=data.train, family='binomial')
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

# true positive
TP <- intersect(which(y.truth==1), which(y.pred==1))

# true negative
TN <- intersect(which(y.truth==0), which(y.pred==0))

# false positive
FP <- which(y.truth[which(y.pred==1)]==0)

# false negative
FN <- which(y.truth[which(y.pred==0)]==1)

# true positive rate
TPR <- length(TP) / (length(TP) + length(FN))
TPR

# true negative rate
TNR <- length(TN) / (length(TN) + length(FP))
TNR

# precision
prec <- length(TP) / (length(TP) + length(FP))
prec


# plot ROC Curve
library(PRROC)
plot(roc.curve(scores.class0 = p.pred[y.truth==1],
               scores.class1 = p.pred[y.truth==0], curve = TRUE),
     ylab='True Postive Rate', xlab='False Negative Rate (1 - True Negative Rate)')

# Backwards Selection
step(logistic.model, method='backward')

selected.model <- glm(formula = recurrence ~ tumorER+tumorPR+tumorHER2+tumorSubType+tumorOS+tumorTNM+tumorGrade+tumorNG+tumorHist+tumorLoc+tumorPos+tumorBBC+tumorSide+mriFocal+mriContra+mriLymph+mriSkin+mriPectoral, family = "binomial", data = data.train)

selected.p.predict <- predict(selected.model, data.test, type='response')
selected.y.predict <- ifelse(selected.p.predict>=0.5, 1, 0) # find a way to select for the best threshold
y.test <- data.test$recurrence
table(selected.y.predict, y.test)
selected.acc.test <- mean(selected.y.predict==y.test)
selected.acc.test
