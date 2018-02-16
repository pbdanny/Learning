library(dplyr)
library(tidyr)
library(ggplot2)

lin_reg <- lm(data = mtcars,
              mpg ~ wt)
g <- ggplot(data = mtcars, mapping = aes(y = mpg, x = wt))
g + geom_point() + labs(title = lin_reg$call)

lin_reg1 <- lm(data = mtcars, 
               hp ~ disp)
g1 <- ggplot(data = mtcars, mapping = aes(y = hp, x = disp))
g1 + geom_point() + labs(title = lin_reg1$call)


lin_reg2 <- lm(data = mtcars, 
               mpg ~ wt + hp)

# Forwards
lin_reg_min <- lm(data = mtcars, mpg ~ 1)
lin_reg_max <- formula(lm(data = mtcars, mpg ~ .))

step(lin_reg_min, direction = "forward", scope = lin_reg_max)

# Backward
lin_reg_min <- formula(lm(data = mtcars, mpg ~ 1))
lin_reg_max <- lm(data = mtcars, mpg ~ .)


step(lin_reg_max, direction = "backward", scope = lin_reg_min)


demand <- read.csv(file = file.choose(), sep = ";", stringsAsFactors = F)
full_name <- names(demand)
names(demand) <- paste0("V", 1:13)

cor(demand)

# backward
lin_reg_min <- formula(lm(V13 ~ 1, data = demand))
lin_reg_max <- lm(V13 ~ ., data = demand)
step(lin_reg_max, direction = "backward", scope = lin_reg_min)

# forward
lin_reg_min <- lm(V13 ~ 1, data = demand)
lin_reg_max <- formula(lm(V13 ~ ., data = demand))
step(lin_reg_min, direction = "forward", scope = lin_reg_max)


# Logistic regression
bank_data <- read.csv(file = file.choose(), sep = ";", stringsAsFactors = F)
table(bank_data$y)
table(bank_data$job)


bank_data %>%
  group_by(marital, y)  %>%
  summarise(n = n()) %>%
  spread(y, n) -> d

chisq.test(as.matrix(d[, -1]))

logit <- glm(data = bank_data, 
             y ~ age + job,
             family = "binomial")

res <- predict(logit, newdata = bank_data, type = 'response')
res_c <- ifelse(res > 0.2, "yes", "no")

table(bank_data$y, res_c)
acc <- (37505+785)/nrow(bank_data)

library(caret)


logit1 <- glm(data = bank_data,
              y ~ ., family = binomial)
res1 <- predict(logit1, newdata = bank_data, type = 'response')
res1_c <- ifelse(res1 > 0.1, "yes", "no")
confusionMatrix(res1_c, bank_data$y, mode = "prec_recall", positive = "yes")

# split test & train
bank_data %>% sample_frac(0.1) -> bank_data_train
logit2 <- glm(y ~ ., data = bank_data_train, family = 'binomial')
res2 <- predict(logit2, newdata = bank_data, type = "response")
res2_c <- ifelse(res2 > 0.2, "yes", "no")
confusionMatrix(res2_c, bank_data$y, mode = "prec_recall", positive = "yes")

# credit approval data set
credit <- read.csv(file = file.choose(), header = F, stringsAsFactors = F, na.strings = "?")
credit <- na.omit(credit)


table(credit$V16)
credit$V16 <- as.factor(credit$V16)

logit3 <- glm(data = credit,
              V16 ~ ., family = "binomial")

res3 <- predict(logit3, newdata = credit, type = "response")
res3_c <- ifelse(res3 > 0.2, "+", "-")
confusionMatrix(res3_c, credit$V16, mode = "prec_recall", positive = "+")
