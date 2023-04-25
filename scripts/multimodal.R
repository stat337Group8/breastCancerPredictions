# load packages
install.packages("PRROC")
library("PRROC")

# open dataset
df <- read.csv('./data/comboFixed.csv')
dfy <- df$recurrence
df <- subset(df, select=-c(X, patients, recurrence))
set.seed(2022)

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

dfZ <- data.frame(Z[,1:15]) # find how to do elbow
dfZ$recurrence <- dfy

# test-train split set-up
index.train <- sample(1:dim(dfZ)[1], 0.8*dim(dfZ)[1])
data.train <- dfZ[index.train,]
data.test <- dfZ[-index.train,]

# train model
logistic.model <- glm(recurrence~., data=data.train, family='binomial')

# evaluate model
y.predict <- predict(logistic.model, data.test, type='response')
y.predict <- ifelse(y.predict>=0.5, 1, 0) # find a way to select for the best threshold
y.test <- data.test$recurrence
table(y.predict, y.test)
acc.test <- mean(y.predict==y.test)
acc.test

# backward select using AIC


# metrics like accuracy and ROC/AUROC

