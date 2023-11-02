library(ggplot2)
library(dplyr)
library(boot)
library(MASS)
library(car)
library(caret)
library(glmnet)

set.seed(1)
data <-read.csv("C:\\Users\\aris\\Documents\\Learning is fun\\Projects\\R projects\\data\\heart.csv")
head(data)
#data cleaning
colSums(is.na(data))
#we don't have missing data
#we check the type of our data, convert every qualitative variable to factor
data <- data %>% mutate_at(c("sex","cp", "fbs", "restecg","exng", "slp", "caa", "thall", "output"), as.factor)

#let's have a quick summary of our data
summary(data)

#split the data to training and test data
size <- nrow(data)
train_index<-sample(size, 0.75*size)
train_data <-data[train_index,]
test_data <-data[-train_index,]

test_target <-data[-train_index,"output"]
test_data <- test_data[, !names(test_data) %in% "output"]

#EDA
ggplot(data = train_data, aes(x = age, fill = output))+geom_histogram()

logistic.model<-glm(output~., data = data, subset = train_index, family = binomial)
result <-predict(logistic.model, test_data, type = "response")

thresholded <- function(x, thresh = 0.5){
  predictions <-rep(0,length(x))
  predictions[x>thresh] <-1
  return (as.factor(predictions))
}
conf_matrix <-confusionMatrix(data = thresholded(result, 0.5), reference = test_target)
conf_matrix

#Regularization
#The parameter estimations are calculated using maximum likehood estimation, which is prone to overfitting.
#Therefore we can try a regularization method like lasso and ridge regression in order to mitigate that

x<-model.matrix(output~., train_data)[,-1] #lm method automatically creates one hot encoding for factors,
#in this method we have to do the one hot encoding with model.matrix
y<-train_data$output

cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial") # a = 1 is the l1 norm
#cv.glmnet chooses the strength of regularization automatically

strength <-cv.lasso$lambda.min

lasso_model <-glmnet(x, y, alpha = 1, family = "binomial", lambda = strength)

#predictions
x_test <-model.matrix(output~., data[-train_index,])[,-1]
probabilities <-lasso_model %>% predict(x_test, type = "response")
predicted_classes <-as.factor(ifelse(probabilities>0.5, 1,0))

#confusionmatrix
confusionMatrix(data = predicted_classes, reference = test_target)
