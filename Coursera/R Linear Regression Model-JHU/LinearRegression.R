library(UsingR)
library(ggplot2)
data("diamond")
g <- ggplot(data = diamond, aes(x = carat, y = price))
g <- g + xlab("Mass (carats)")
g <- g + ylab("Price (Sin$)")
g <- g + geom_point(size = 6, color = "black", alpha = 0.2)
g <- g + geom_smooth(method = "lm", color = "green")
g

#find fit line
fit <- lm(price ~ carat, data = diamond)
coef(fit)
summary(fit)

#move to carat mean
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)

#multiply by 10 -> divided by 10 for slop
fit3 <- lm(price ~ I(carat*10), data= diamond)
coef(fit3)

#predictin with predict function
newx <- c(0.16, 0.27, 0.34)
predict(fit, newdata = data.frame(carat = newx))

#Resisual Analysis
y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e - (y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2]*x)))

#plottting residual
plot <- data.frame(cbind(x, e))
e <- ggplot(aes(x = x, y = e), data = plot)
e <- e + geom_point(alpha = 0.5)
e <- e + geom_smooth(method = "lm")
e
mean(diamond$carat)

#Resisual Variation
library(UsingR)
data("diamond")
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y~x)
summary(fit)
summary(fit)$sigma
sum(x*resid(fit))

#confident interval for intercept & slope
summary(fit)$df
fit$df

sumCoef <- summary(fit)$coefficients

#CI of intercept
sumCoef[1,1] + c(-1,1)*qt(0.975, df = fit$df.residual)*sumCoef[1,2]
#CI of slope
sumCoef[2,1] + c(-1,1)*qt(0.975, df = fit$df.residual)*sumCoef[2,2]

#Quiz 2
#1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y~x)
sumConf <- summary(fit)$coefficients
sumConf[2,4]

#3
data("mtcars")
w <- mtcars$wt
m <- mtcars$mpg
fit2 <- lm(m~w)
summary(fit2)
predict(fit2, newdata = data.frame(w = mean(w)), interval = "confidence")

#5
data("mtcars")
w <- mtcars$wt
m <- mtcars$mpg
fit3 <- lm(m~w)
predict(fit2, newdata = data.frame(w = 3), interval = "prediction")

#6
data("mtcars")
w2 <- mtcars$wt/2
m <- mtcars$mpg
fit4 <- lm(m~w2)
sumConf <- summary(fit4)$coefficient
sumConf
#CI of slope
(sumConf[2,1] + c(-1,1)*qt(0.975, df = fit4$df.residual)*sumConf[2,2])

#9
summary(fit4)

1/.7528
