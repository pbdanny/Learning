library(caret)
library(kernlab)
data("spam")
View(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelfit <- train(type ~ ., data = training, methods = "glm")
dim(training)

set.seed(32323)
fold <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = TRUE)


#Wage data
library(ISLR)
library(ggplot2)
library(caret)
data("Wage")
summary(Wage)

g <- ggplot(data = Wage) +
  geom_histogram(aes(x = age, fill = maritl), alpha = 0.5)
g

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

g <- qplot(data = Wage, x = age, y = wage, color = education) +
  geom_smooth(method = 'lm')
g
