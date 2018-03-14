# Statistic Review

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

# Week 2 : Visualize & describe time series data
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


## week 4 Partial ACF
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

## Week 4 : simulation AR(2) and coefficient estimation
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

# Question Week 4 - Yule-Walker Eq. matrix
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
