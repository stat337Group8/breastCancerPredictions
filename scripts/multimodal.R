# load packages
install.packages("comprehenr")
install.packages("ROSE")
install.packages("PRROC")
install.packages("ggplot2")
library("comprehenr")
library("ggplot2")

# set working directory
setwd("C:/Users/HOME PC/Desktop/spring2023/STAT337/breastCancerPredictions/")

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
acc.test <- mean(y.predict==y.test)
acc.test

# cross validation accuracy score


# metrics like precision, TPR, TNR, and ROC/AUROC
## ROC Curve
library("PRROC")
plot(roc.curve(scores.class0 = p.predict[y.test==1], 
               scores.class1 = p.predict[y.test==0], curve = TRUE),
     ylab='True Postive Rate', xlab='False Negative Rate (1 - True Negative Rate)')

## true positive
TP <- intersect(which(y.test==1), which(y.predict==1))
## true negative
TN <- intersect(which(y.test==0), which(y.predict==0))
## false positive
FP <- which(y.test[which(y.predict==1)]==0)
## false negative
FN <- which(y.test[which(y.predict==0)]==1)
## true postive rate
TPR <- length(TP) / (length(TP) + length(FN))
TPR
## true negative rate
TNR <- length(TN) / (length(TN) + length(FP))
TNR
## precision
prec <- length(TP) / (length(TP) + length(FP))
prec

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