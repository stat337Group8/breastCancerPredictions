setwd("C:/Users/A_juc/OneDrive - Loyola University Chicago/Desktop/Homework/Spring 2023/STAT 337/Project")
demo = read.csv("demographics.csv") # set the name of the data as 'demo'

lmodel = glm(recurrence~dob+menopause+race+metastatic, data = demo, family = "binomial")
# binomial logistic regression model created
summary(lmodel)

# fitted P(Y=1)
prob.one = lmodel$fitted.values
prob.one

# random split the data into 80% training and 20% test
set.seed(2022)
index_train = sample(1:dim(demo)[1], 0.8 * dim(demo)[1])
data_train = demo[index_train,]
data_test = demo[-index_train,]

train_lmodel = lmodel = glm(recurrence~dob+menopause+race+metastatic, data = data_train, family = "binomial")
summary(train_lmodel)
# predict on the test test, obtain the predicted P(Y=1|X)
ppred = predict(train_lmodel, data_test, type='response')

# calculate classification accuracy
# ypred = ppredifelse()
ypred = ifelse(ppred>= 0.5, 1, 0)
ypred
ytruth = data_test$recurrence
ytruth
accuracy_test = mean(ypred==ytruth)
accuracy_test

# confusion matrix
table(ypred, ytruth)

# true positive
TP = intersect(which(ytruth==1), which(ypred==1))

# true negative
TN = intersect(which(ytruth==0), which(ypred==0))

# false positive
FP = which(ytruth[which(ypred==1)]==0)

# false negative
FN = which(ytruth[which(ypred==0)]==1)

# true postive rate
TPR = length(TP) / (length(TP) + length(FN))
TPR

# true negative rate
TNR = length(TN) / (length(TN) + length(FP))
TNR

# precision
prec = length(TP) / (length(TP) + length(FP))
prec

# adjust threshold
threshold = seq(0, 1, 0.1)
TPRs = c()
TNRs = c()

for(x in threshold){
  ypred = ifelse(ppred>=t, 1, 0)
  TP = intersect(which(ytruth==1), which(ypred==1))
  TN = intersect(which(ytruth==0), which(ypred==0))
  FP = which(ytruth[which(ypred==1)]==0)
  FN = which(ytruth[which(ypred==0)]==1)
  TPR = length(TP) / (length(TP) + length(FN))
  TNR = length(TN) / (length(TN) + length(FP))
  TPRs = c(TPRs, TPR)
  TNRs = c(TNRs, TNR)
}
TPRs
TNRs

#ROC Curve
library(PRROC)
plot(roc.curve(scores.class0 = ppred[ytruth==1], 
               scores.class1 = ppred[ytruth==0], curve = TRUE),
     ylab='True Postive Rate', xlab='False Negative Rate (1 - True Negative Rate)')

# Backward Selection
# Order based on using the automatic 
# step(lmodel, direction = 'backward')
# Order is removing: [PID]>metastatic>menopause>race>dob

back1 = lm(recurrence~metastatic+menopause+race+dob, data = demo)
summary(back1)
back2 = lm(recurrence~menopause+race+dob, data = demo)
summary(back2)
back3 = lm(recurrence~race+dob, data = demo)
summary(back3)
back4 = lm(recurrence~dob, data = demo)
summary(back4)



