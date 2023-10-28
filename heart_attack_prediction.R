library(ggplot2)
library(dplyr)
library(boot)
library(MASS)
library(car)
library(caret)

set.seed(42)
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
conf_matrix <-confusionMatrix(data = test_target, reference = thresholded(result, 0.5))
conf_matrix

#ggplot(data = train_data, aes(x = age))+geom_histogram()
#ggplot(data = train_data, aes(x = oldpeak, fill = output))+geom_histogram()
#sex = 1 corresponds to male
#ggplot(data = data, aes(x = as.factor(sex), fill = as.factor(sex)))+geom_bar()+scale_x_discrete(labels = c("Female", "Male"))+xlab("Sex")+theme(legend.position="none")