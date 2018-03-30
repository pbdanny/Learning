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
rcvd_ts <- rcvd %>%
  group_by(rcvd_date) %>% 
  summarise(n = n())
ts <- ts(rcvd_ts$n)
plot(ts[1:90], main = 'App rcvd daily', type = 'l')

library(forecast)
Box.test(log(ts), log(length(ts)))  # quite significant
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
