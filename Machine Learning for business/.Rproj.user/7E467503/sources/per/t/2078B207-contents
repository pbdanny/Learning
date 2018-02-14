library(dplyr)

flight <- read.csv(file = "./data/flights.csv", header = T, stringsAsFactors = F)

flight %>%
    select(carrier, dep_delay, arr_delay) %>%
    filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
    group_by(carrier) %>%
    summarise(mean_dep = mean(dep_delay), mean_arr = mean(arr_delay)) %>%
    arrange(mean_dep) -> data2

# ggplot2
library(ggplot2)

ggplot(data = flight, mapping = aes(x = distance/air_time)) + 
    geom_histogram()

ggplot(data = data2,
       mapping = aes(x = mean_dep, y = mean_arr)) +
    geom_point(aes(color = carrier)) +
    geom_smooth(method = "lm")

ggplot(data = flight,
       mapping = aes(x = distance/air_time)) +
    geom_histogram(color = "gray")

ggplot(data = data2,
       mapping = aes(x = mean_dep)) +
    geom_dotplot()

ggplot(data = flight,
       mapping = aes(x = distance/air_time)) +
    geom_density()

ggplot(data = flight,
       mapping = aes(x = air_time)) +
    geom_freqpoly()

ggplot(data = flight,
       aes(x = carrier)) +
    geom_bar()

ggplot(data = data2,
       aes(x = mean_dep, y = mean_arr)) +
    geom_point()

ggplot(data = data2,
       aes(x = mean_dep, y = mean_arr)) +
    geom_line()

ggplot(data = data2,
       mapping = aes(x = mean_dep, y = mean_arr)) +
    geom_quantile() +
    geom_point()

ggplot(data = flight, 
       mapping = aes(x = dep_delay, y = arr_delay)) +
    geom_density_2d()

ggplot(data = flight, 
       mapping = aes(x = air_time, y = distance)) +
    geom_bin2d()

ggplot(data = data2, 
       mapping = aes(x = carrier, y = mean_dep, fill = carrier)) +
    geom_bar(stat = "identity")

# Sampling data
bankData <- read.csv(file = "./data/2-bank-data.csv", 
                     header = T, sep = ";", stringsAsFactors = F)
idx <- sample(1:nrow(bankData), size = 10)
test <- bankData[idx,]
train <- bankData[-idx,]

# dplyr
test <- sample_n(bankData, 5)
test <- sample_frac(bankData, size = 0.01)

bankData %>% sample_n(400) %>%
    group_by(education) %>%
    summarise(sample_age = mean(age))
# stratify
bankData %>% group_by(y, education) %>% sample_n(2) -> d2

# test random sample

bankData %>% sample_frac(size = 0.1) -> d1
prop.table(table(bankData$y))
prop.table(table(d1$y))

# muli-stage cluster
kclust <- kmeans(mtcars, 5)
data1 <- mtcars
data1$cluster <- kclust$cluster

data1 %>% group_by(cluster) %>% sample_n(2) -> d1

# EDA
bankData %>% group_by(job) %>% summarise(n = n()) %>% arrange(n)

bankData %>%
    group_by(education) %>%
    summarise(n = n(),
              Q1 = quantile(balance, 0.25),
              med_bal = median(balance),
              mean_bal = mean(balance),
              sd = sd(balance),
              Q3 = quantile(balance, 0.75),
              IQR = IQR(balance))
library(plotly)
bankData %>% 
    ggplot(aes(x = age)) +
    geom_histogram(binwidth = 1) -> g
ggplotly(g)

bankData %>% 
    ggplot(aes(x = education, y = age, fill = education)) +
    geom_violin() +
    geom_boxplot(alpha = 0.5)
