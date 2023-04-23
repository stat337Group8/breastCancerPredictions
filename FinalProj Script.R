library(mice)
library(missForest)

#read in file
image_data <- read.csv("C://Users//siddm//Downloads//image_data.csv")

#remove rows with empty values for Reccurence
image_data <- image_data[c(-325,-373),]

#find which rows have na
rowsWithNA <- impage_data[rowSums(is.na(image_data)) > 0,]
#see which columns have na
columnsWithNA <- names(which(colSums(is.na(image_data))>0))
columnsWithNA


#three options to impute the data via mice library
# https://appsilon.com/imputation-in-r/

#the only one that works without error is cart, but it takes an incredibly long time
#imputed_pmm = complete(mice(image_data[,-1], method = "pmm"))
imputed_cart = complete(mice(image_data[,-1], method = "cart"))
#imputed_lasso = complete(mice(image_data[,-1], method = "lasso.norm"))

#can also impute via random forest but haven't tried it yet
#imputed_missForest = missForest(image_data[,-1])


#remove rows with empty values
#image_data <- image_data[complete.cases(image_data), ]
#lose 63 patients after doing this

#split dataset into training and testing
set.seed(2022)

#right now using imputed_cart, change to image_data if you want to
index.train <- sample(1:dim(imputed_cart)[1], 0.8 * dim(imputed_cart)[1])
data.train <- imputed_cart[index.train,]
data.test <- imputed_cart[-index.train,]

logistic.model <- glm(Recurrence~., data=data.train[,-1], family='binomial')

# predict on the test test, obtain the predicted P(Y=1)
p.pred <- predict(logistic.model, data.test[-1], type='response')

y.pred <- ifelse(p.pred>=0.5, 1, 0)

# calculate classification accuracy
y.truth <- data.test$Recurrence
acc.test <- mean(y.pred==y.truth)
acc.test