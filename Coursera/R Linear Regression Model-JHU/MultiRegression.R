n <- 100; x <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
y <- 1 + x + x2 + x3 + rnorm(n, sd = .1)
ey <- resid(lm(y ~ x2 + x3))
ex <- resid(lm(x ~ x2 + x3))
sum(ey*ex)/sum(ex^2)
coef(lm(y ~ x + x2 + x3))

#Example
require(datasets); data("swiss"); ?swiss
install.packages("GGally")

library(GGally); library(ggplot2)
g <- ggpairs(swiss, lower = list(continuous = "smooth"))
g

summary(lm(Fertility ~ ., data = swiss))$coefficient
summary(lm(Fertility ~ Agriculture, data = swiss))
install.packages("rgl")

library(rgl)
plot3d()


#Quiz 3
#1
data("mtcars")
str(mtcars)
fit <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit)
#2
fit1 <- lm(mpg ~ factor(cyl), data = mtcars)
summary(fit)$coef[3]
summary(fit1)$coef[3]

#3
fit2 <- lm(mpg ~ factor(cyl)*wt, data = mtcars)
summary(fit2)
anova(fit, fit2, test = "Chisq")

#4
fit3 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(fit3)

#5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit5 <- lm(y ~ x)
hatvalues(fit5)

#6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit6 <- lm(y ~ x)
hatvalues(fit6)
dfbetas(fit6)[,2]

download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda" ,destfile="./ravensData.rda",method="curl")
load("./ravensData.rda") 
head(ravensData)


lmRaven <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)
summary(lmRaven)

logRegRaven <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore, family = "binomial")
summary(logRegRaven)
