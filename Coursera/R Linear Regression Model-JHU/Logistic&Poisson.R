#Logistic & Poisson Regression

#Quiz
#1
library(MASS)
data("shuttle")
str(shuttle)

dat <- shuttle
#Transform use auto = 1, no = 2 -> auto = 0, no = 1
dat$use1 <- abs(as.integer(dat$use) - 2)
dat$use1 <- factor(dat$use1, labels = c("noauto","auto"))
#check use, use1
table(dat$use, dat$use1)

#Tranform wind head = 1, tail = 2 -> head = 2, tail = 1
dat$wind1 <- abs(as.integer(dat$wind) - 2) + 1
dat$wind1 <- factor(dat$wind1, labels = c("tail","head"))
#check
table(dat$wind, dat$wind1)

fit1 <- glm(dat$use1 ~ dat$wind1, family = "binomial")
summary(fit1)
#exp of(B1) is the odd ratio of odd(P when X + 1)/odd(P when X = 1)
exp(fit1$coefficients[2])

#Quiz 2
fit2 <- glm(dat$use1 ~ dat$wind1 + dat$magn, family = "binomial")
summary(fit2)

exp(fit2$coefficients)

#Quiz 4

data("InsectSprays")
str(InsectSprays)
dt <- InsectSprays

pfit <- glm(dt$count ~ dt$spray, family = "poisson")
summary(pfit)
#relative rate = E(Y|X)/E[Y|X+1] = exp(-b1)
exp(-pfit$coefficients[2])

#Quiz 6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
plot(x,y)
y.sub <- y[seq(6,11,1)]
x.sub <- x[seq(6,11,1)]
fit3 <- lm(y.sub ~ x.sub)
fit3$coefficients