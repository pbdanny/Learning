library(rpart)
library(dplyr)

setwd("C:/Users/Thanakrit.B/Downloads")
bankData <- read.csv("2-bank-data.csv", sep = ";")
bankData.train <- sample_frac(bankData, 0.6)

training.idx <- as.integer(rownames(bankData.train))
bankData.test <- bankData[-training.idx,]
tree <- rpart(y ~ ., data = bankData.train)
res <- predict(tree, newdata = bankData.test, type = "class")

library(caret)
confusionMatrix(res, bankData.test$y, positive = "yes")

# Lift
res.p <- predict(tree, newdata = bankData.test)[,"yes"]
lift_result <- data.frame(prob = res.p, y = bankData.test$y)
lift_obj <- lift(y ~ prob, data = lift_result, class = "yes")
plot(lift_obj, values = 60)
plot(lift_obj, plot = "lift")

# cross validation
train_control <- trainControl(method = "cv", number = 5)
model <- train(y ~ . , data = bankData.train,
               trControl = train_control,
               method = "rpart")
model

# retrive final model
model$finalModel
model$resample

# tune hyperparameter, grid search
train_control <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
cp <- c(0.001, 0.002, 0.005, 0.007, 0.01)
tune_grid <- expand.grid(.cp = cp)
model_tune <- train(y ~ . , data = bankData.train,
               trControl = train_control,
               metric = metric,
               tuneGrid = tune_grid,
               method = "rpart")
model_tune
library(rpart.plot)
rpart.plot(model_tune$finalModel)


# tune hyperparameter, random search
train_control <- trainControl(method = "cv", number = 5, 
                              search = "random")
metric <- "Accuracy"
#cp <- c(0.001, 0.002, 0.005, 0.007, 0.01)
#tune_grid <- expand.grid(.cp = cp)
model_tune <- train(y ~ . , data = bankData.train,
                    trControl = train_control,
                    metric = metric,
                    tuneLength = 20,
                    method = "rpart")
model_tune
library(rpart.plot)
rpart.plot(model_tune$finalModel)

# Workshop
library(caret)
library(dplyr)

setwd("C:/Users/Thanakrit.B/Downloads")
bankData <- read.csv("2-bank-data.csv", sep = ";")
bankData.train <- sample_frac(bankData, 0.6)

training.idx <- as.integer(rownames(bankData.train))
bankData.test <- bankData[-training.idx,]

# rpart
library(rpart)
library(rpart.plot)
model_rpart <- rpart(y ~ ., data = bankData.train)

# RF
library(randomForest)
model_rf <- randomForest(y ~ ., data = bankData.train, ntree = 200)

# Tune Control
train_control <- trainControl(method = "cv", number = 3)
param <- seq(2, 6, 1)
tune_grid <- expand.grid(.mtry = param)
metric <- "Accuracy"
model_rf_tune <- train(y ~ ., data = bankData.train,
                            trControl = train_control,
                            metric = metric,
                            method = "rf",
                            tuneGrid = tune_grid,
                            ntree = 50)

# predict
pred_rpart <- predict(model_rpart, newdata = bankData.test)
pred_rf <- predict(model_rf, newdata = bankData.test, type = "prob")
pred_rf_tune <- predict(model_rf_tune, newdata = bankData.test, type = "prob")

# lift rpart
lift_rpart <- data.frame(prob = pred_rpart[,"yes"], y = bankData.test$y)
lift_rpart_obj <- lift(y ~ prob, data = lift_rpart, class = "yes")
plot(lift_rpart_obj, main = "CART, untuned")

#lift rf
lift_rf <- data.frame(prob = pred_rf[,"yes"], y = bankData.test$y)
lift_rf_obj <- lift(y ~ prob, data = lift_rf, class = "yes")
plot(lift_rf_obj, main = "Random forest, untuned")

#lift rf_tune
lift_rf_tune <- data.frame(prob = pred_rf_tune[,"yes"], y = bankData.test$y)
lift_rf_tune_obj <- lift(y ~ prob, data = lift_rf_tune, class = "yes")
plot(lift_rf_tune_obj, main = "Random forest, tuned")

#compare all lift
lift_all <- data.frame(y = bankData.test$y, 
                       rpart = pred_rpart[,"yes"],
                       rf = pred_rf[,"yes"],
                       rf_tune = pred_rf_tune[,"yes"])
lift_all_obj <- lift(y ~ rpart + rf + rf_tune, data = lift_all,
                     class = "yes")
plot(lift_all_obj, main = "Gain", auto.key = list(column = 2))
plot(lift_all_obj, plot = "lift", main = "Threshold", auto.key = list(column = 2))
