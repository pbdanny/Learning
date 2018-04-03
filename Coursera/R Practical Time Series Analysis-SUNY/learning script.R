library(RODBC)
con <- odbcConnectAccess2007('C:/Users/Thanakrit.B/Downloads/OSS DataSales 201802.mdb')
df <- sqlFetch(con, "AppControlReport", as.is = TRUE)
head(df)
str(df)
library(dplyr)
rcvd <- df %>% 
  select(Receive_Date, Product, Source_Code, Agent_Code, bundle) %>%
  mutate(rcvd_date = as.Date(Receive_Date, format = "%Y-%m-%d"))
rm(df)
odbcCloseAll()

write.csv(rcvd, file = gzfile("oss_rcvd_2018.csv.gz"), 
          fileEncoding = 'UTF-8', row.names = F)

## read in data

rcvd <- read.csv(file = "oss_rcvd_2018.csv.gz",
                 stringsAsFactors = F)
# convert string to 
rcvd$rcvd_date <- as.Date(rcvd$rcvd_date, format = "%Y-%m-%d")
library(dplyr)

rcvd_daily <- rcvd %>%
  group_by(rcvd_date) %>% 
  summarise(n = n())

ts <- ts(rcvd_daily$n)
plot(ts, main = 'App rcvd daily')
plot(log(ts), main = 'App rcvd daily (log x)')

# test if all autocorelation fn <> 0
Box.test(ts, log(length(ts))) # quite signitficant
Box.test(log(ts), log(length(ts)))  # quite significant

# preliminary test if some model fitted
library(forecast)
auto.arima(log(ts))  # could extract some model

# Try stationalized data
par(mfrow = c(3,1))
# plot(ts, main = 'App rcvd daily')
plot(log(ts), main = 'Log (ts)')
acf(log(ts), main = 'acf of Log (ts)')
pacf(log(ts), main = 'pacf of Log (ts)')

p <- 0
d <- 0
q <- 0
P <- 2
D <- 2
Q <- 2
S <- 5
df <- NULL

for(i in 0:P) {
  for(j in 0:D) {
    for(k in 0:Q) {
      model <- arima(log(ts), order = c(0,0,0), 
                     seasonal = list(order = c(i,j,k), period = S))
      aic <- model$aic
      sse <- sum(model$residuals^2)
      resid_test <- Box.test(model$residuals)
      cat('0 0 0 ', i, ' 1 ',j, ' 5 ',
          'AIC ', aic, ' SSE ',sse ,' pval ', resid_test$p.value, '\n')
      row <- data.frame('p' = 0, 'd' = 0, 'q' = 0,
                        'P' = i, 'D' = j, 'Q' = k, 'S' = S,
                        'AIC' = aic, 'SSE' = sse, 'pval' = resid_test$p.value)
      df <- rbind(df, row)
    }
  }
}

library(astsa)
sarima(log(ts), 0,0,0, 1,0,1, 5)

## Try to cut some part of data, with less varience
rcvd_daily %>%
  filter(as.numeric(format(rcvd_date, "%Y")) >= 2017) -> rcvd_daily_2017

rcvd_daily %>% 
  filter(rcvd_date >= "2017-05-31") -> rcvd_daily_2017_may

ts <- ts(rcvd_daily$n)
ts_2017 <- ts(rcvd_daily_2017$n)
ts_2017_may <- ts(rcvd_daily_2017_may$n)

# plot to test
par(mfrow = c(3,1))
plot(ts, main = 'rcvd daily : all')
plot(ts_2017, main = 'rcvd daily : from 2017 onwards')
plot(ts_2017_may, main = 'rcvd daily : May 2017 onwards')

# test data 2017
Box.test(ts, lag = log(length(ts)))
Box.test(ts_2017, lag = log(length(ts_2017)))
Box.test(ts_2017_may, lag = log(length(ts_2017_may)))

# from the Box test, show ts_2017 have higher chance of autocorrelation <>0
# use auto.arima to find some crude model
library(forecast)
model1 <- auto.arima(ts)
model2 <- auto.arima(ts_2017)
model3 <- auto.arima(ts_2017_may)
model1
model2
model3

# all the data could make some model
## Warning!! since the data frequency was not set
## The auto arima would not detect any seasonality in the model
## the prediction will aloso be straight line
dev.off()
plot(forecast(model1, 10))
pred <- forecast(model1, 10)
actual <- c(7013,4831,4708,3834,5920,7906,3476,3818,3203,3370)
mse <- sum((pred$mean - actual)^2)/length(pred$mean)
sqrt(mse)/mean(pred$mean)
## Quite large avg err 40% from actual data

## Use acf , pacf to determine seasonality
## choose ts_2017 since the Box.text yield small p value

par(mfrow = c(3,1))
plot(ts_2017, main = 'rcvd daily : from 2017 onwards')
acf(ts_2017)
pacf(ts_2017)

# try parameter p, d, q, P, D, Q, S = 0,0,0,2,1,2,5
# trial parameter to fit model

p <- 0
d <- 0
q <- 0
P <- 2
D <- 1
Q <- 2
S <- 5
df <- NULL

for(i in 0:P) {
  for(j in 0:D) {
    for(k in 0:Q) {
      model <- arima(ts_2017, order = c(p, d, q), 
                     seasonal = list(order = c(i,j,k), period = S))
      aic <- model$aic
      sse <- sum(model$residuals^2)
      resid_test <- Box.test(model$residuals)
      cat(p, d, q, i, j, k, S, 
          'AIC ', aic, ' SSE ',sse ,' pval ', resid_test$p.value, '\n')
      row <- data.frame('p' = 0, 'd' = 0, 'q' = 0,
                        'P' = i, 'D' = j, 'Q' = k, 'S' = S,
                        'AIC' = aic, 'SSE' = sse, 'pval' = resid_test$p.value)
      df <- rbind(df, row)
    }
  }
}

df[order(df$AIC),]

# best model is 0 0 0 2 1 1 5
# predict with astsa
library(astsa)
model <- sarima(ts_2017, 0, 0, 0, 2, 1, 1, 5)  # P - value still not satisfied

# model accuaracy
pred <- sarima.for(ts_2017, n.ahead = 10, 
                   p = 0, d = 0, q =0, 
                   P = 2, D = 1, Q = 1, S = 5)
actual <- c(7013,4831,4708,3834,5920,7906,3476,3818,3203,3370)
mse <- sum((pred$pred - actual)^2)/length(pred$pred)
sqrt(mse)/mean(pred$pred)
# error 44% 
pred$pred
pred$se

## Non parametric with Exponential Smoothing
# convert string to 
rcvd$rcvd_date <- as.Date(rcvd$rcvd_date, format = "%Y-%m-%d")

library(dplyr)
rcvd_daily <- rcvd %>%
  group_by(rcvd_date) %>% 
  summarise(n = n())

rcvd_daily %>% 
  filter(rcvd_date >= "2017-06-01") -> rcvd_daily_2017_jun


ts <- ts(rcvd_daily$n)
acf(ts)
pacf(ts)
# expected seanonality in model
ts_s <- ts(rcvd_daily$n, frequency = 5)
ts_2017_jun <- ts(rcvd_daily_2017_jun$n)
ts_2017_jun_s <- ts(rcvd_daily_2017_jun$n, frequency = 5)

# create model
model1 <- HoltWinters(ts, gamma = F)
model2 <- HoltWinters(ts_s)
model3 <- HoltWinters(ts_2017_jun, gamma = F)
model4 <- HoltWinters(ts_2017_jun_s)

# test of error
model1$SSE
model2$SSE
model3$SSE
model4$SSE

# predict
pred <- predict(model4, 10)

# Validate model
actual <- c(7013,4831,4708,3834,5920,7906,3476,3818,3203,3370)
err <- pred - actual
mse <- sum(err^2)/length(pred)
sqrt(mse)/mean(actual)
# err at 34% , which is the best of all 3 methods
