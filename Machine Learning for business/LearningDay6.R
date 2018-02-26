setwd("/Users/Danny/Documents/Learning/Machine Learning for business/")

library(proxy)
x <- c(23, 3, 2)
y <- c(40, 10, 1)
dist(rbind(x, y))

bank <- read.csv(file = "data/2-bank-data.csv", sep = ";")

# Euclidean
dist(bank[1:5, c("age", "balance")], method = "Euclidean")
# Manhatton
dist(bank[1:5, c("age", "balance")], method = "Manhattan")


# look-alike
# find target group from decision tree
library(rpart)
library(rpart.plot)
set.seed(123)
tree <- rpart(y ~ . , data = bank)
rpart.plot(tree)
table(tree$where)

# choose group 6 as target group
target_idx <- tree$where == 6

# extract group 6 , 5 obs
library(dplyr)
set.seed(555)
target <- sample_n(bank[target_idx, ], 5)

# extract prospect from not in group 6, 20 obs
prospect <- sample_n(bank[!target_idx, ], 20)

# find nearest neighbour
library(proxy)
d <- dist(prospect, target)

# rank each prospect obs by mean of distance to each target obs
score_d <- apply(d, 1, mean)
sort(score_d, decreasing = T)

# Use kknn 
library(kknn)
train_idx <- sample(nrow(bank), 0.3*nrow(bank))
train <- bank[train_idx,]
test <- bank[-train_idx,]

result <- kknn(y ~ . , train = train, test = test, k = 11)

library(caret)
confusionMatrix(result$fitted.values, test$y, positive = 'yes',
                mode = 'prec_recall')

# model tuning
f1 <- NULL
for (i in seq(4, 10, 2)) {
  result <- kknn(y ~ . , train = train, test = test, k = i)
  confMet <- confusionMatrix(result$fitted.values, test$y, 
                             positive = 'yes', mode = 'prec_recall')
  f1 <- c(f1, confMet$byClass[7])
}

## Kmeans clustering
df <- NULL
for (j in 1:100) {
  ttwth <- NULL
  for (i in 2:30) {
    km <- kmeans(mtcars, i)
    ttwth <- c(ttwth, km$tot.withinss)
  }
  df <- rbind(df, ttwth)
}

rownames(df) <- NULL
colnames(df) <- 2:30

# Clustering
# Hierachical

library(proxy)
library(dplyr)
bank <- read.csv(file = "data/2-bank-data.csv", sep = ";")
set.seed(555)
bank_samp <- sample_n(bank, 100)
dist <- dist(bank_samp) # gover distance matrix
hclust(dist)
plot(hclust(dist))
cluster <- cutree(hclust(dist), k = 4)
rect.hclust(hclust(dist), k = 4,  border = "red")
# assign cluster
bank_samp$cluster <- cluster
by(bank_samp[, c(1,6)], bank_samp[, 18], FUN = mean)

## Project telco churn
telco <- read.csv(file = "./data-day6/12-Telco-Customer-Churn.csv" )

# clear customer id
telco$customerID <- NULL
telco$SeniorCitizen <- as.factor(telco$SeniorCitizen)

library(rpart)
library(rpart.plot)

# Decision tree
tree <- rpart(Churn ~ ., data = telco)
rpart.plot(tree)
tree$variable.importance

library(randomForest)
telco_rf <- na.omit(telco)
rf <- randomForest(Churn ~ ., data = telco_rf)
vu <- varUsed(rf, count = TRUE)
vu_sorted <- sort(vu, decreasing = TRUE, index.return = TRUE)
dotchart(vu_sorted$x, names(rf$forest$xlevels[vu_sorted$ix]))
varImpPlot(rf)

# Chose important data with PCA
# pca <- prcomp(telco, scale = TRUE)
# Cannot use pca since some data are categorical

# Choose data that impact churn rate , Contract, tenre, TotalCharges, TechSupport
# OnlineSecurity, MonthlyCharges, DeviceProtecton, InternetService

library(dplyr)
telco %>%
  select(Contract, tenure, TotalCharges, TechSupport, 
         OnlineSecurity, MonthlyCharges, DeviceProtection, InternetService) -> df

library(proxy) # for Gower distance
dist <- dist(df)
