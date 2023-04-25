# load packages
install.packages("comprehenr")
install.packages("ROSE")
install.packages("PRROC")
setwd("C:/Users/HOME PC/Desktop/spring2023/STAT337/breastCancerPredictions/")
library("comprehenr")

# open dataset
df <- read.csv('./data/comboFixed.csv')
dfy <- df$recurrence
df <- subset(df, select=-c(X, patients, recurrence))
set.seed(2022)

# imbalanced outcome value
table(dfy)

# reduce data dimensionality
# Perform PCA 
pr.out <- prcomp(df , scale = TRUE)
phi <- pr.out$rotation # save for next predicting dataset so that predictions are appropriately handled
Z <- pr.out$x
# Evaluate PCA
PVE.matrix <- summary(pr.out)$importance
PVE <- PVE.matrix[2,]
plot(PVE, xlab='Principle Component', ylab='Proportion of Variance Explained', cex.lab=1.5)
abline(a=PVE[10], b=0)
text(x=10, y=0.04)

dfZ <- data.frame(Z[,1:20]) # find how to do elbow
dfZ$recurrence <- dfy

# test-train split set-up
index.train <- sample(1:dim(dfZ)[1], 0.8*dim(dfZ)[1])
data.train <- dfZ[index.train,]
data.test <- dfZ[-index.train,]

library("ROSE")
data.train <- ROSE(recurrence ~ ., data = data.train, seed=2022)$data

w0 <- length(data.train$recurrence)/(length(data.train$recurrence[data.train$recurrence==0]))
w1 <- length(data.train$recurrence)/(length(data.train$recurrence[data.train$recurrence==1]))
ws <- to_vec(for(r in data.train$recurrence) if (r == 0) w0 else w1)

# train model
logistic.model <- glm(recurrence~., family='binomial', data=data.train, weights=ws)
# logistic.model <- glm(recurrence~., family='binomial', data=data.train)

# evaluate model
p.predict <- predict(logistic.model, data.test, type='response')
y.predict <- ifelse(p.predict>=0.65, 1, 0)
y.test <- data.test$recurrence
table(y.predict, y.test)
acc.test <- mean(y.predict==y.test)
acc.test

# cross validation accuracy score

# metrics like accuracy and ROC/AUROC
library("PRROC")
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


# backward select using AIC
step(logistic.model, method='backward')

selected.model <- glm(formula = recurrence ~ PC2 + PC3 + PC4 + PC5 + PC9 + PC10 + 
                        PC12 + PC14 + PC16 + PC17 + PC18 + PC19, family = "binomial", 
                      data = data.train, weights = ws)

selected.p.predict <- predict(selected.model, data.test, type='response')
selected.y.predict <- ifelse(selected.p.predict>=0.65, 1, 0) # find a way to select for the best threshold
y.test <- data.test$recurrence
table(selected.y.predict, y.test)
selected.acc.test <- mean(selected.y.predict==y.test)
selected.acc.test
plot(roc.curve(scores.class0 = selected.p.predict[y.test==1], 
               scores.class1 = selected.p.predict[y.test==0], curve = TRUE),
     ylab='True Postive Rate', xlab='False Negative Rate (1 - True Negative Rate)')
