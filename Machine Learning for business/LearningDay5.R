# entropy & information gain
# root entropy
y <- 7/12
n <- 5/12
ent_root <- -y*log(y)-n*log(n)

# split body-shape 
round_y <- 2
round_n <- 4
N_round <- round_y+round_n

square_y <- 5
square_n <- 1
N_square <- round_y+round_n

ent_round <- -round_y/N_round*log(round_y/N_round)-round_n/N_round*log(round_n/N_round)
ent_square <- -square_y/N_square*log(square_y/N_square)-square_n/N_square*log(square_n/N_square)
ent_body <- ent_root-(N_round/12*ent_round  + N_square/12*ent_square)
ent_body

## Decision tree
# Data prep
bank_data <- read.csv(file = "/Users/Danny/Documents/Learning/Machine Learning for business/data/2-bank-data.csv", sep = ";")
library(dplyr)

train <- sample_frac(bank_data, 0.6)
train.idx <- as.integer(rownames(train))
test <- bank_data[-train.idx,]

prop.table(table(train$y))
prop.table(table(test$y))

# decision tree
library(rpart)
library(rpart.plot)
tree <- rpart(y ~ ., data = train)
rpart.plot(tree)
tree$variable.importance

# predict
res <- predict(tree, newdata = test, type = "class")
library(caret)
confusionMatrix(res, test$y,
                positive = "yes", mode = "prec_recall")

# model tuning with cp parameter
tree1 <- rpart(y ~ ., data = train,
              control = rpart.control(cp = 0.001))
rpart.plot(tree1)

res1 <- predict(tree1, newdata = test,
               type = "class")
confusionMatrix(res1, test$y,
                positive = "yes", mode = "prec_recall")
# small cp -> more split data ; not help

# Adjust threshold
res_p <- predict(tree, newdata = test)
res2 <- factor(
  ifelse(res_p[,"yes"] > 0.2, "yes", "no"),
  levels = c("no", "yes"))

confusionMatrix(res2, test$y,
                positive = "yes", mode = "prec_recall")


## Random forest
set.seed(123)
train <- sample_frac(bank_data, size = 0.6)
train.idx <- as.integer(rownames(train))
test <- bank_data[-train.idx, ]

library(randomForest)
rf <- randomForest(y ~ ., data = train)
res <- predict(rf, newdata = test)
confusionMatrix(res, test$y,
                positive = "yes", mode = "prec_recall")


# tune parameter, ntree

rf <- randomForest(y ~ ., data = train, ntree = 200)
res <- predict(rf, newdata = test)
confusionMatrix(res, test$y,
                positive = "yes", mode = "prec_recall")

# tune parameter, important
rf <- randomForest(y ~ ., data = train, importance = TRUE)
varImpPlot(rf)

# tune mtry

rf <- randomForest(y ~ ., data = train, ntree = 100, mtry = 2)
res <- predict(rf, newdata = test)
confusionMatrix(res, test$y,
                positive = "yes", mode = "prec_recall")


# Activiy
credit <- read.csv(file = "/Users/Danny/Documents/Learning/Machine Learning for business/data-day3/3-credit-approval.csv",
                   header = FALSE, na.strings = "?")
set.seed(1234)
train <- sample_frac(credit, size = 0.6)
train.idx <- as.integer(rownames(train))
test <- credit[-train.idx,]
prop.table(table(train$V16))
prop.table(table(test$V16))

# decision tree
library(rpart)
library(rpart.plot)
tree <- rpart(V16 ~ . , data = credit)
rpart.plot(tree)
res <- predict(tree, newdata = test, type = "class")
library(caret)
confusionMatrix(res, test$V16,
                positive = "+", mode = "prec_recall")

# random forest
library(randomForest)
credit_na <- na.omit(credit)
set.seed(1234)
train <- sample_frac(credit_na, size = 0.6)
train.idx <- as.integer(rownames(train))
test <- credit_na[-train.idx,]
rf <- randomForest(V16 ~ . , data = train)
rf
res <- predict(rf, newdata = test)
confusionMatrix(res, test$V16,
                positive = "+", mode = "prec_recall")
