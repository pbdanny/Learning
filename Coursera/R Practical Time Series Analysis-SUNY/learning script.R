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
