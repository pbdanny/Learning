# Statistic Review ----

plot(co2, main = "Atmospheric CO2 concentration")
co2_values <- as.numeric(co2)
co2_times <- as.numeric(time(co2))

# Linear regression univariate x = times, y = values
# Layman computation 
ss_xx <- sum((co2_times - mean(co2_times))^2)
ss_xy <- sum((co2_times - mean(co2_times))*(co2_values - mean(co2_values)))
slope <- ss_xy/ss_xx
intercept <- mean(co2_values) - slope*mean(co2_times)

# Regression by R function
lin_reg <- lm(co2_values ~ co2_times)
plot(co2, main = "Atmospheric CO2 concentration")
abline(lin_reg, col = "red")

# Fitted value & residual
fitted <- intercept +  slope*co2_times 
error <- co2_values - fitted
ss_err <- sum(error^2)
ss_yy <- sum((co2_values - mean(co2_values))^2)
R_sqr <- 1-ss_err/ss_yy

# EDA on residuals, test normal distribution
par(mfrow = c(1, 3))
co2_resid <- resid(lin_reg)
hist(co2_resid, main = 'Histogram of CO2 regression residuals')
qqnorm(co2_resid, main = 'Normal Q-Q plot')
qqline(co2_resid)
plot(co2_resid ~ co2_times, main = 'Regression residual on time')
dev.off()
plot(co2_resid ~ co2_times, xlim = c(1960, 1963), main = 'Zoomin Residual on Time')

# Sleep data
sleep
# summarized data
tapply(sleep$extra, sleep$group, mean)
plot(data = sleep, extra ~ group, main = "Extra sleep by Gosset data by group")

extra1 <- sleep$extra[sleep$group == 1]
extra2 <- sleep$extra[sleep$group == 2]
t.test(extra1, extra2, paired = T, alternative = "two.sided")

# test if sampling distribution (mean of difference) is normal
diff <- extra1 - extra2
qqnorm(diff, main = "Normal Probability plot")
qqline(diff)

# Trun data from test to paired regression
plot(extra1, extra2, xlab = "Extra sleep drug I", ylab = "Extra sleep drug II",
     main = "Extra Sleep drug II against drug I")
sleep_lin_reg <- lm(extra2 ~ extra1)
# plot fitted line
abline(sleep_lin_reg)

# residual normalcy analysis
plot(resid(sleep_lin_reg))

qqnorm(resid(sleep_lin_reg))
qqline(resid(sleep_lin_reg), col = "red")
resid(sleep_lin_reg)

data(trees)
View(trees)
pairs(trees, pch = 21, bg = "brown", col = NA, cex = 1.1)
# Week 2 : Visualize & describe time series data ----
library(astsa)
help(astsa)
help(jj)
plot(jj, type = "o", main = "Johnson & Johnson Q earning per share",
     xlab = "Years", ylab = "Earning")

help(flu)
plot(flu, main = "Monthly Flu. & Pneu. deth in US", xlab = "Months",
     ylab = "Death per 10000")

plot(globtemp, type = "l", main = "Globe temp deviation",
     xlab = "Year", ylab = "Temp deviation")
plot(globtempl)
plot(star)


# Auto covarince
pure_rand_process <- ts(rnorm(100))
acf(pure_rand_process, type = "covariance")
print(acf(pure_rand_process, type = "covariance"))

# Random walk
x <- NULL
x[1] <- 0
for (i in 2:1000) {
  x[i] <- x[i-1] + rnorm(1)
}
rand_walk <- ts(x)
plot(x, main = 'Random walk', ylab = '', xlab = 'Days', 
     type = 'l', col = "blue", lwd = 2)
print(acf(rand_walk))
# high correlation = no stationality, in stochastic process

# Detrend data with 'diff()' = x2-x1, x3-x2 ...
plot(diff(rand_walk))

# diff of rand_walk = pure random process , stationality
acf(diff(rand_walk))
# confirm stationality with acf 

# Moving average

# Generate noise
noise=rnorm(10000)

# Introduce a variable
ma_2=NULL

# Loop for generating MA(2) process

for(i in 3:10000){
  ma_2[i]=noise[i]+0.7*noise[i-1]+0.2*noise[i-2]
}

# Shift data to left by 2 units
moving_average_process=ma_2[3:10000]

# Put time series structure on a vanilla data
moving_average_process=ts(moving_average_process)

# Partition output graphics as a multi frame of 1 rows and 2 column
par(mfrow=c(1,2))

# plot the process and plot its ACF
plot(moving_average_process, main='A moving average process of order 2', 
     ylab=' ', col='blue')
acf(moving_average_process, main='Correlogram of a moving average process of order 2')

# sampling time series data
set.seed(123)
autocov <- acf(arima.sim(n=1000, model=list(ma=c(0.5, 0.5))), type = "covariance")
# autocovarience at lag (0)
autocov$acf[1] # R start time index = rownames - 1
# autocorrelation 
autocorr <- (acf(arima.sim(n=1000, model=list(ma=c(0.5, 0.5)))))
autocorr$acf[3] # autocorr at lag (2)
autocorr$acf[2] # autocorr at lag (1)

## Simulate AR(p) process
set.seed(2016)
phi_1 <- 0.5
phi_2 <- -0.4
X_ts <- arima.sim(model = list(ar = c(phi_1, phi_2)), n = 1000)
par(mfrow = c(2,1))
plot(X_ts, main = paste0('AR Time Series , phi 1 : ', phi_1, ' phi 2 : ', phi_2))
acf(X_ts, main = 'ACF of AR')
dev.off()
## week 4 Partial ACF ----
# ACF of MA(q) coefficent will cease at time = q
set.seed(123)
par(mfrow = c(3 ,1))
phi_1 <- 0.9
phi_2 <- -0.6
phi_3 <- 0.3
data_ts <- arima.sim(n = 500, list(ar = c(phi_1, phi_2, phi_3)))
plot(data_ts, main=
       paste("Autoregressive Process with phi1 =", phi_1, " phi2 =", phi_2, " phi3 = ", phi_3))
acf(data_ts, main="Autocorrelation Function")
acf(data_ts, type="partial", main="Partial Autocorrelation Function")


## Beveridge
b <- read.csv(file = "/Users/Danny/Documents/Learning/Coursera/R Practical Time Series Analysis-SUNY/beveridge-wheat-price-index-1500.csv", 
              header = T, stringsAsFactors = F)
b <- b[1:370, ]
b_ts <- ts(as.numeric(b[,2]), start = 1500)
plot(b_ts, ylab = 'price', main = "Beveridge Wheat Price Data")

# MA30 of data, MA from avg of left 15 + right 15 
b_ma <- filter(b_ts, rep(1/31, 31), sides = 2)
# plot MA30
lines(b_ma, col = "red")

par(mfrow = c(3,1))
# smooth data with MA30
Y <- b_ts/b_ma
plot(Y, ylab = "scaled price", main = "Detrended data")
# remove cannot smooth data, head & tails of data that could not find MA
acf(na.omit(Y), main = "Autocorrelation of detrend data")
acf(na.omit(Y), type = 'partial', main = "Partial Autocorrelation of detrend data")

## use ar analysis AR process
ar(na.omit(Y), order.max = 5)


## Theory of Partial Auto Correlation
library(isdals)
data("bodyfat")
str(bodyfat)
pairs(bodyfat)
cor(bodyfat)

# try step-wise for best varible predict Fat
fit_min <- lm(data = bodyfat, Fat ~ 1)
fit_max <- formula(lm(data = bodyfat, Fat ~ .))
step(fit_min, direction = "forward", scope = fit_max)
# formula = Fat ~ Thight

# Find correlation of Fat ~ Triceps, without Thigh effect
fat_hat <- predict(lm(Fat ~ Thigh, data = bodyfat))
triceps_hat <- predict(lm(Triceps ~ Thigh, data = bodyfat))
# correlation on residule
cor(bodyfat$Fat - fat_hat, bodyfat$Triceps - triceps_hat)

# could use pcor.test
library(ppcor)
pcor.test(bodyfat$Fat, bodyfat$Triceps, bodyfat$Thigh)

# Find correlation of Fat ~ Triceps, without Thight, Midarm effect
fat_hat <- predict(lm(Fat ~ Thigh + Midarm, data = bodyfat))
triceps_hat <- predict(lm(Triceps ~ Thigh + Midarm, data = bodyfat))
# correlation on residule
cor(bodyfat$Fat - fat_hat, bodyfat$Triceps - triceps_hat)

# use pcor
pcor(bodyfat)
## Week 4 : simulation AR(2) and coefficient estimation ----
set.seed(2017)

# AR parameter
sigma <- 4
phi <- NULL
phi[1:2] <-c (1/3, 1/2)
phi

n <- 10000

ar_process <- arima.sim(n, model = list(ar = c(1/3, 1/2)), sd = 4)
ar_process[1:5]

# auto correlation function for Yule-walker matrix equation
r <- NULL
r[1:2] <- acf(ar_process, plot = F)$acf[2:3]
r

R <- matrix(1, nrow = 2, ncol = 2) # matrix of dimension 2 by 2, with entries all 1's.
R

R[1,2] <- r[1] # only diagonal entries are edited
R[2,1] <- r[1] # only diagonal entries are edited
R

b <- matrix(r, nrow = 2, ncol = 1)# b- column vector with no entries
b

# solution for phi_hat
phi_hat <- solve(R,b)
phi_hat

# calculate var of white noise
c0 <- acf(ar_process, type = 'covariance', plot = F)$acf[1]
var_hat <- c0*(1 - sum(phi_hat*r))
var_hat

par(mfrow = c(3,1))
plot(ar_process, main = 'Simulated AR(2)')
acf(ar_process, main = 'ACF')
pacf(ar_process, main = 'PACF')

# x_t=phi1*x_(t-1)+phi2* x_(t-2)+\phi_3*x_(t-3)+z_t
# z_t~ N(0, sigma^2)

set.seed(2017)
sigma <- 4
phi <- NULL
phi[1:3] <- c(1/3, 1/2, 7/100)
n <- 100000

ar3_process <- arima.sim(n, model = list(ar = phi), sd = 4)

r <- NULL
r[1:3] <- acf(ar3_process, plot = F)$acf[2:4]
r

R <- matrix(1, nrow = 3, ncol = 3) 
R[1,2] <- r[1] 
R[1,3] <- r[2]
R[2,1] <- r[1]
R[2,3] <- r[1]
R[3,1] <- r[2]
R[3,2] <- r[1]
R

# b-column vector on the right
b <- matrix(NA, nrow = 3 , ncol = 1) # b- column vector with no entries
b[1,1] <- r[1]
b[2,1] <- r[2]
b[3,1] <- r[3]
b

# solve Rx=b and find phi's
phi_hat <- solve(R,b)
phi_hat

# sigme estimation
c0 <- acf(ar3_process, type = 'covariance', plot = F)$acf[1]
var_hat <- c0*(1 - sum(phi_hat*r))
var_hat

par(mfrow = c(3,1))
plot(ar3_process, main = 'Simulated AR(3)')
acf(ar3_process, main = 'ACF')
pacf(ar3_process, main = 'PACF')
# Question Week 4 - Yule-Walker Eq. matrix -----
r <- matrix(c(0.8, 0.6, 0.2), ncol = 1)
phi <- matrix(1, ncol = 3, nrow = 3)
phi[1,2] <- r[1]
phi[1,3] <- r[2]
phi[2,1] <- r[1]
phi[2,3] <- r[1]
phi[3,1] <- r[2]
phi[3,2] <- r[1]
phi
phi_hat <- solve(phi, r)
c0 <- 5
var_hat <- c0*(1 - sum(phi_hat*r))
var_hat

# Modeling recruitment time series from 'astsa' package as an AR process

library(astsa)
my_data <- rec

# Plot rec 
plot(rec, main = 'Recruitment time series', col = 'blue', lwd = 3)

# subtract mean to get a time series with mean zero
ar_process = my_data - mean(my_data)

# ACF and PACF of the process
par(mfrow=c(2,1))
acf(ar_process, main = 'Recruitment', col = 'red', lwd = 3)
pacf(ar_process, main = 'Recruitment', col = 'green', lwd = 3)

# order
p <- 2

# sample autocorreleation function r
r <- NULL
r[1:p] <- acf(ar_process, plot = F)$acf[2:(p+1)]
cat('r =', r, '\n')

# matrix R
R <- matrix(1, p, p) # matrix of dimension 2 by 2, with entries all 1's.

# define non-diagonal entires of R
for(i in 1:p){
  for(j in 1:p){
    if(i!=j)
      R[i,j] <- r[abs(i-j)]
  }
}
R

# b-column vector on the right
b <- NULL
b <- matrix(r, nrow = p , 1) # b- column vector with no entries
b

# solve(R,b) solves Rx=b, and gives x=R^(-1)b vector
phi_hat <- NULL
phi_hat <- solve(R,b)[,1]
phi_hat

#variance estimation using Yule-Walker Estimator
c0 <- acf(ar_process, type = 'covariance', plot = F)$acf[1]
c0

var_hat <- c0*(1 - sum(phi_hat*r))
var_hat

# constant term in the model
phi0_hat <- mean(my_data)*(1 - sum(phi_hat))
phi0_hat

cat("Constant:", phi0_hat," Coeffcinets:", phi_hat, " and Variance:", var_hat, '\n')
## Johnson & Johnson quarterly earnings per share ----
library(astsa)

# Time plot for Johnson&Johnson
plot(JohnsonJohnson, main = 'Johnson&Johnosn earnings per share', 
     col = 'blue', lwd = 3)

# log-return of Johnson&Johnson
jj.log.return <- diff(log(JohnsonJohnson))
jj.log.return.mean.zero <- jj.log.return-mean(jj.log.return)

# Plots for log-returns
par(mfrow = c(3,1))
plot(jj.log.return.mean.zero, main = 'Log-return (mean zero) of Johnson&Johnosn earnings per share')
acf(jj.log.return.mean.zero, main = 'ACF')
pacf(jj.log.return.mean.zero, main = 'PACF')

# Order
p <- 4

# sample autocorreleation function r
r <- NULL
r[1:p] <- acf(jj.log.return.mean.zero, plot = F)$acf[2:(p+1)]
r

# matrix R
R <- matrix(1,p,p) # matrix of dimension 4 by 4, with entries all 1's.

# define non-diagonal entires of R
for(i in 1:p){
  for(j in 1:p){
    if(i != j)
      R[i,j] <- r[abs(i-j)]
  }
}
R

# b-column vector on the right
b <- matrix(r,p,1) # b- column vector with no entries
b

phi.hat <- solve(R,b)[,1]
phi.hat

# Variance estimation using Yule-Walker Estimator
c0 <- acf(jj.log.return.mean.zero, type = 'covariance', plot = F)$acf[1]
c0
var.hat <- c0*(1 - sum(phi.hat*r))
var.hat

# Constant term in the model
phi0.hat <- mean(jj.log.return)*(1 - sum(phi.hat))
phi0.hat

cat("Constant:", phi0.hat," Coeffcinets:", phi.hat, " and Variance:", var.hat, '\n')


## ARMA
rm(list=ls(all=TRUE))
set.seed(500) # Beginning of Heptarchy: Kent, Essex, Sussex,
# Wessex, East Anglia, Mercia, and Northumbria. 
data <- arima.sim( list(order = c(1,0,1), ar =.7, ma=.2), n = 1000000)
par(mfcol = c(3,1 ))
plot(data, main="ARMA(1,1) Time Series: phi1=.7, theta1=.2", xlim=c(0,400)) #first terms 
acf(data, main="Autocorrelation of ARMA(1,1), phi1=.7, theta1=.2")
acf(data, type="partial", main="Partial Autocorrelation of ARMA(1,1), phi1=.7, theta1=.2")
## Major scientific discovery ----
plot(discoveries,
     main = "Time Series of Number of Major Scientific Discoveries in a Year")
stripchart(discoveries, method = "stack", offset=.5, at=.15,pch=19, main="Number of Discoveries Dotplot",
           xlab="Number of Major Scientific Discoveries in a Year", ylab="Frequency")
par(mfcol = c(2,1 ))
acf(discoveries, main="ACF of Number of Major Scientific Discoveries in a Year")
acf(discoveries, type="partial", main="PACF of Number of Major Scientific Discoveries in a Year")

# Fitting model with arima & AIC measure
# Assumption p = c(0, 1, 2, 3) and q = c(0, 1, 2, 3)
p <- c(0, 1, 2, 3)
q <- c(0, 1, 2, 3)

for (i in p) {
  for (j in q) {
    cat(paste0("ARMA (", i, ", 0, ",j, ") "))
    cat(AIC(arima(discoveries, order = c(i, 0, j))))
    cat("\n")
  }
}
# fitting coefficient
arima(discoveries, order = c(1, 0, 1))

# auto ARIMA
library(forecast)
ma(discoveries, d=0, approximation=FALSE)

# use approximation calculation for coefficient, approximation = TRUE
auto.arima(discoveries, d=0, approximation=TRUE)


# ARIMA(2,1,1) Simulation
# parameters
phi <- c(.7, .2)
beta <- 0.5
sigma <- 3
m <- 10000

set.seed(5)
Simulated.Arima <- arima.sim(n = m,
                             list(order = c(2,1,1),
                                  ar = phi, 
                                  ma = beta))

plot(Simulated.Arima, ylab = ' ', 
     main = 'Simulated time series from ARIMA(2,1,1) process', 
     col = 'blue', lwd = 2)

acf(Simulated.Arima)

Diff.Simulated.Arima <- diff(Simulated.Arima)

plot(Diff.Simulated.Arima)

acf(Diff.Simulated.Arima)

pacf(Diff.Simulated.Arima)

library(astsa)
sarima(Simulated.Arima, 2, 1, 1, 0, 0, 0) # astsa not installed

library(forecast)
auto.arima(Simulated.Arima) # forecast not installed

fit1 <- arima(Diff.Simulated.Arima, order = c(4,0,0))
fit1

fit2 <- arima(Diff.Simulated.Arima, order = c(2,0,1))
fit2

fit3 <- arima(Simulated.Arima, order = c(2,1,1))
fit3
## Female birth in 1959 ----
female_birth <- readxl::read_excel("/Users/Danny/Documents/Learning/Coursera/R Practical Time Series Analysis-SUNY/daily-total-female-births-in-cal.xlsx")

colnames(female_birth) <- c('date', 'no_birth')

plot(y = female_birth$no_birth, x = female_birth$date, type = "l",
     main = "Daily female births 1959", xlab = 'date', ylab = 'no birth')

# Roughly test if any autocorr coef. not = 0
n <- female_birth$no_birth
Box.test(n, lag = log(length(n)))

# create time series data with frequency = daily in year =365.25
ts_birth <- ts(female_birth$no_birth, start = c(1959, 1, 1), frequency = 365.25)

# Some trend, detrend with diff()
plot(diff(ts_birth), type = "l",
     main = "Diff - Daily female births 1959", xlab = 'date', ylab = '')

# Re-test if autocorrelation coef. not = 0
Box.test(diff(ts_birth), lag = log(length(ts_birth)))

# acf , pacf test for p, q
acf(diff(female_birth$no_birth), main = 'ACF of difference data', 50)
acf(diff(female_birth$no_birth), type = 'partial', main = 'PACF of difference data', 50)

# try to fit model
model1 <- arima(female_birth$no_birth, order = c(0, 1, 1))
SSE1 <- sum(model1$residuals^2)
# test if after model the residual is pure white noise, all autocorr = 0
model1.test <- Box.test(model1$residuals, 
                        lag = log(length(model1$residuals)))

model2<-arima(female_birth$no_birth, order=c(0,1,2))
SSE2<-sum(model2$residuals^2)
model2.test<-Box.test(model2$residuals, lag = log(length(model2$residuals)))

model3<-arima(female_birth$no_birth, order=c(7,1,1))
SSE3<-sum(model3$residuals^2)
model3.test<-Box.test(model3$residuals, lag = log(length(model3$residuals)))

model4<-arima(female_birth$no_birth, order=c(7,1,2))
SSE4<-sum(model4$residuals^2)
model4.test<-Box.test(model4$residuals, lag = log(length(model4$residuals)))

df<-data.frame(row.names=c('AIC', 'SSE', 'p-value'), c(model1$aic, SSE1, model1.test$p.value), 
               c(model2$aic, SSE2, model2.test$p.value), c(model3$aic, SSE3, model3.test$p.value),
               c(model4$aic, SSE4, model4.test$p.value))
colnames(df)<-c('Arima(0,1,1)','Arima(0,1,2)', 'Arima(7,1,1)', 'Arima(7,1,2)')

format(df, scientific=FALSE)

# Fit a SARIMA model
library(astsa)
sarima(female_birth$no_birth, 0,1,2,0,0,0)
# Quiz week 5 ----
plot(BJsales)
plot(diff(BJsales))
plot(diff(diff(BJsales)))
acf(diff(diff(BJsales)))
pacf(diff(diff(BJsales)))
d <- 2
for(p in 1:4){
  for(q in 1:2){
    if(p+d+q<=6){
      model<-arima(x=BJsales, order = c((p-1),d,(q-1)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}

d <- 2
for(p in 1:4){
  for(q in 1:2){
    if(p+d+q<=8){
      model<-arima(x=BJsales, order = c((p-1),d,(q-1)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}

model<-arima(BJsales, order=c(0,2,1))

par(mfrow=c(2,2))

plot(model$residuals)
acf(model$residuals)
pacf(model$residuals)
qqnorm(model$residuals)

sarima(BJsales,0,2,1,0,0,0)
## Week#6, SARIMA intro ----

x <- NULL
z <- NULL
n <- 10000

z <- rnorm(n)
x[1:13] <- 1

for(i in 14:n){
  x[i] <- z[i] + 0.7*z[i-1] + 0.6*z[i-12] + 0.42*z[i-13]
}

par(mfrow=c(2,1))
plot.ts(x[12:120], main='The first 10 months of simulation SARIMA(0,0,1,0,0)_12', ylab='') 

acf(x, main='SARIMA(0,0,1,0,0,1)_12 Simulation')
## SARIMA Johnson & Johnson ----
# arima simulation to find best p,d,q, P,D,Q, S

library(astsa)
plot(jj)
plot(log(jj))
plot(diff(jj))
plot(diff(log(jj)))
par(mfrow = c(3,1))
plot(diff(log(jj)), main = 'Johnson & Jonhson Earning : Log - return')
acf(diff(log(jj)), main = 'acf')
acf(diff(log(jj)), type = 'partial', main = 'partial')

# acf show seasonal difference, use diff = 4 to stabilize
plot(diff(diff(log(jj)), 4), main = 'Johnson & Jonhson Earning : Log - return - S = 4')
acf(diff(diff(log(jj)), 4), main = 'acf : S = 4')
acf(diff(diff(log(jj)), 4), type = 'partial', main = 'partial : S = 4')

# test transform data
Box.test(x = diff(diff(log(jj)), 4), lag = log(length(jj)))

d <- 1
DD <- 1
per <- 4

for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p + d + q + i + DD + j <= 10){
          model <- arima(x = log(jj),  # test model with season portion
                         order = c((p-1), d, (q-1)),  # p, d, q
                         seasonal = list(order = c((i-1), DD, (j-1)),  # P, D, Q
                                         period = per))  # seasonality
          pval <- Box.test(model$residuals, lag = log(length(model$residuals)))
          sse <- sum(model$residuals^2)
          cat(p-1, d, q-1, i-1, DD, j-1, per,
              'AIC=', model$aic, ' SSE=', sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

## find estimator coefficient from best simulation parameter
# p,d,q, P,D,Q = 0,1,1,1,1,0 with seasonality = 4

library(astsa)
sarima(log(jj), 0,1,1,1,1,0,4)

## SARIMA milk production ----

milk <- read.csv('./Documents/Learning/Coursera/R Practical Time Series Analysis-SUNY/monthly-milk-production-pounds-p.csv',
                 stringsAsFactors = FALSE)
colnames(milk) <- c('month', 'Pounds')
milk <- milk[-nrow(milk),]

Milk <- as.numeric(milk$Pounds)

par(mfrow = c(2,2))
plot(Milk, type = 'l', main = 'Milk data')
plot(diff(diff(Milk) ,12), type = 'l', main = 'Detrend, Deseasonal (S = 12)')
acf(diff(diff(Milk) ,12), lag.max = 50)
pacf(diff(diff(Milk), 12), lag.max = 50)

# fast guess paramter
library(astsa)
sarima(Milk, 0,1,0,0,1,1,12)


library(astsa)

d <- 1  # non seasonal difference
DD <- 1  # seasonal difference
per <- 12  # S = 12
df <- NULL

for(p in 1:1){
  for(q in 1:1){
    for(i in 1:3){
      for(j in 1:4){
        if(p+d+q+i+DD+j <= 10){
          model <- arima(x = Milk, order = c((p-1), d, (q-1)), 
                         seasonal = list(order = c((i-1), DD, (j-1)), 
                                         period = per))
          pval <- Box.test(model$residuals, lag = log(length(model$residuals)))
          sse <- sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 
              'AIC=', model$aic, ' SSE=', sse,' p-VALUE=', pval$p.value,'\n')
          m <- data.frame('p' = p-1, 'd' = d, 'q' = q-1, 'P' = i-1, 'D' = DD, 'Q' = j-1, 'S' = per, 
                    'AIC' = model$aic, 'SSE' = sse, 'pval' = pval$p.value)
          df <- rbind(df, m)
        }
      }
    }
  }
}
# order by min aic, min SSE
df[order(df$AIC, df$SSE),]

# create best model
model<- arima(x = Milk, order = c(0, 1, 0), 
              seasonal = list(order = c(0, 1, 1), period = 12))
# forecast with library forecast
library(forecast)
plot(forecast(model))
forecast(model)
## SARIMA Souvenior shop ----

SUV <- read.csv('./Documents/Learning/Coursera/R Practical Time Series Analysis-SUNY/monthly-sales-for-a-souvenir-sho.csv',
                stringsAsFactors = FALSE)
colnames(SUV) <- c('month', 'Sales')
SUV <- SUV[-nrow(SUV),]
suv <- ts(as.numeric(SUV$Sales))

library(astsa)
par(mfrow=c(2,2))

plot(suv, main='Monthly sales for a souvenir shop', ylab='', col='blue', lwd=3)
plot(log(suv), main='Log-transorm of sales', ylab='', col='red', lwd=3)
plot(diff(log(suv)), main='Differenced Log-transorm of sales', ylab='', col='brown', lwd=3)
plot(diff(diff(log(suv)),12), main='Log-transorm without trend and seasonaliy', ylab='', col='green', lwd=3)

data<-diff(diff((log(suv)),12))
acf2(data, 50)

d <- 1
DD <- 1
per <- 12
for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(suv), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

model<- arima(x=log(suv), order = c(1,1,0), 
              seasonal = list(order=c(0,1,1), period=12))

library(forecast)
dev.off()
plot(forecast(model))
forecast(model)

a<-sarima.for(log(suv),12,1,1,0,0,1,1,12)

plot.ts(c(suv,exp(a$pred)), main='Monthly sales + Forecast', ylab='', 
        col='blue', lwd=3)
## Quiz SARIMA ----
par(mfrow=c(2,2))
acData <- diff(diff(USAccDeaths, 12))
# obtain acf and pacf below
acf(acData, lag.max = 30, main = 'diff-12, diff')
pacf(acData, lag.max = 30, main = 'diff-12, diff')
acf(diff(diff(USAccDeaths), 12), lag.max = 30, main = 'diff, diff-12')
pacf(diff(diff(USAccDeaths), 12), lag.max = 30, main = 'diff, diff-12')

dev.off()
library(astsa)
model <- sarima(USAccDeaths, 0,1,1,0,1,1, 12)
model$ttable

sarima.for(USAccDeaths,n.ahead = 12 , 0,1,1,0,1,1, 12)
## Forecasting ----
rm(list = ls(all = TRUE))
rain.data <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat", skip = 1)
rain.ts <- ts(rain.data, start = c(1813))
# histogram plot
par(mfrow = c(1 , 2))
hist(rain.data, main = "Annual London Rainfall 1813-1912",
     xlab = "rainfall in inches")
qqnorm(rain.data, main = "Normal Plot of London Rainfall")
qqline(rain.data, col = 'red')
# time plot
par(mfrow = c(2, 1))
plot.ts(rain.ts, main="Annual London Rainfall 1813-1912",
        xlab="year", ylab="rainfall in inches")
acf(rain.ts, main="ACF: London Rainfall")

# check if auto.corr.coef exist
library(forecast)
auto.arima(rain.ts)
## Simple Exponential Smoothing & Double Exponential Smoothing----
rm(list = ls())
setwd('/Users/Danny/Documents/Learning/Coursera/R Practical Time Series Analysis-SUNY')
money <- read.csv(file = 'volume-of-money-abs-definition-m.csv', stringsAsFactors = F)
money <- money[-nrow(money), ]
money_ts <- ts(as.numeric(money[,2]), start = c(1960, 2), frequency = 12)
par(mfrow = c(3, 1))
plot(money_ts, main = "Time plot of Valume of Money")
acf(money_ts)
pacf(money_ts)
dev.off()
m <- HoltWinters(money_ts, gamma = FALSE)
plot(m)

## HoltWinter for Trend 
# set up our transformed data and smoothing parameters 
data = as.numeric(money[,2])
N <- length(data)
alpha <- 0.7
beta <- 0.5
## prepare empty arrays so we can store values
forecast <- NULL
level <- NULL
trend <- NULL
#initialize level and trend in a very simple way 
level[1] <- data[1]
trend[1] <- data[2] - data[1]

#initialize forecast to get started 
forecast[1] <- data[1] 
forecast[2] <- data[2]

#loop to build forecasts 
for(n in 2:N) {
  level[n] <- alpha*data[n] + (1-alpha)*(level[n-1] + trend[n-1])
  trend[n] <- beta*(level[n] - level[n-1]) + (1-beta)*trend[n-1]
  forecast[n+1] <- level[n] + trend[n]
}
# use HoltWinters function
m <- HoltWinters(data, alpha = 0.7, beta = 0.5, gamma = FALSE)
# compare loop function, HoltWinters
par(mfcol = c(1,2))
plot(forecast[3:N], type = 'l', main = "xhat from loop function")
plot(m$fitted[,1], main = 'xhat from HolWinters')

 

## Triple Exponential Smoothing ----
plot(AirPassengers)
plot(log(AirPassengers))
# fourier frequency decomposition
library(TSA)
peri <- periodogram(AirPassengers, log = TRUE)
df <- data.frame('frq' = peri[['freq']], 'spec' = peri[['spec']])
# try HoltWinter without trend & seasonal data
air_ses <- HoltWinters(log(AirPassengers), beta = FALSE, gamma = FALSE)
air_ses$SSE
# HoltWinter without seasonal
air_des <- HoltWinters(log(AirPassengers), gamma = FALSE)
air_des$SSE
# HoltWinter with seasonal
air_tes <- HoltWinters(log(AirPassengers))
air_tes$SSE
plot(air_ses)
par(mfrow = c(3,1))
plot(air_ses)
plot(air_des)
plot(air_tes)
air_tes
# forecast sunspots
library(forecast)

plot(sunspots)
sun_tes <- HoltWinters(sunspots)
sun_pred <- forecast(sun_tes)
sun_pred
# forecast air passenger
air_pred <- forecast(log(AirPassengers))
air_pred$model
plot(air_pred)