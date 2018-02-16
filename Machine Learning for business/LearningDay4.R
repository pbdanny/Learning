## Poisson regression Start ----

library(dplyr)
library(tidyr)
library(ggplot2)

ceb <- read.csv(file = file.choose())
hist(ceb$n)

ggplot(data = ceb,
       mapping = aes(x = n)) +
  geom_histogram(bins = 20) +
  ggtitle(lab = "Histogram of n")

# sample poisson Bank data

bank <- read.csv(file = file.choose(), sep = ";")
ggplot(data = bank,
       mapping = aes(x = campaign)) +
  geom_histogram()

# Transform data with log()

ggplot(data = ceb,
       mapping = aes(x = n)) +
  geom_histogram(bins = 20) +
  #scale_x_log10() +
  ggtitle(lab = "Histogram of log(n)")

# Poison parameter
x <- 0:20
poi.mean <- 10
poi.probs <- dpois(x, poi.mean)
names(poi.probs) <- x
barplot(poi.probs, col = "green")

# Mining accident data
mine <- read.csv(file = file.choose())
mine.glm <- glm(COUNT ~ INB + EXTRP + AHS + AGE, family = poisson, data = mine)
summary(mine.glm)

# Step forward
mine.min <- glm(COUNT ~ 1, data = mine, family = poisson)
mine.max <- formula(glm(COUNT ~ ., data = mine, family = poisson))
step(mine.min, direction = "forward", scope = mine.max)

# Step backward
mine.max <- glm(COUNT ~ ., data = mine, family = poisson)
mine.min <- formula(glm(COUNT ~ 1, data = mine, family = poisson))
step(mine.max, direction = "backward", scope = mine.min)

# Poisson model prediction
resp <- predict(mine.glm, newdata = mine, type = "response")
pred <- predict(mine.glm, newdata = mine) # log(resp)
# Note : resp = exp(pred)
# Poisson model validation, p-value of goodness to fit 
1 - pchisq(37.717, df = 39)
# p-value > 0.05 , model distribution fit with poison distribution

# Prediction Error , with Absolute mean square err
mean(abs(mine$COUNT - resp))

# Poison & weight data & categorical test
onion <- read.csv(file = file.choose())
onion.glm <- glm(data = onion,
                 skins ~ factor(block) + maturity + cure,
                 family = poisson, weight = weight)

# Use ANOVA for factor analysis
anova(onion.glm, test = "Chisq")

# maturity most effect, refit data
onion.glm.2 <- glm(data = onion,
                   skins ~ maturity, family = poisson, weight = weight)
summary(onion.glm.2)

# offect , when data came in rate & population
cancer <- read.csv(file = file.choose())
cancer.glm <- glm(data = cancer,
                  n ~ Cytology * Residence * Age,
                  family = poisson, offset = log(pop/100000))
summary(cancer.glm)

# Features selection
anova(cancer.glm, test = "Chisq")

# Refif model
cancer.glm2 <- glm(data = cancer,
                  n ~ Cytology * Age + Residence ,
                  family = poisson, offset = log(pop/100000))

summary(cancer.glm2)
# measure goodness to fit of model
1 - pchisq(5.0598, df = 3)

# Ceb data
ceb <- read.csv(file = file.choose())

# create count data
ceb$y <- ceb$mean * ceb$n

# create offset data
ceb$os <- log(ceb$n)

# create model 
ceb.fit1 <- glm(data = ceb, formula = y ~ 1, 
                family = poisson, offset = os)
ceb.fit2 <- glm(data = ceb, formula = y ~ resident, 
                family = poisson, offset = os)
ceb.fit3 <- glm(data = ceb, formula = y ~ education, 
                family = poisson, offset = os)
ceb.fit4 <- glm(data = ceb, formula = y ~ duration, 
                family = poisson, offset = os)

ceb.fit5 <- glm(data = ceb, formula = y ~ duration*resident*education, 
                family = poisson, offset = os)
anova(ceb.fit5, test = "Chisq")
# all tree varible effect the model
ceb.fit6 <- glm(data = ceb, formula = y ~ duration + resident + education, 
                family = poisson, offset = os)
1 - pchisq(70.653, df = 59)

# error
resp <- predict(ceb.fit6, newdata = ceb, type = 'response')
mean(abs(ceb$y - resp))

## End----

## Servival analysis Start ----
library(survival)
data("ovarian")
head(ovarian)

# create survival object
S1 <- Surv(ovarian$futime, ovarian$fustat)

# Non parametric model
fit1 <- survfit(S1 ~ 1)
fit1
summary(fit1)
plot(fit1, xlab = "t", ylab = "S(t)")

# Parametric model
fit2 <- survreg(data = ovarian,
              Surv(futime, fustat) ~ 1,
              dist = 'exponential')
summary(fit2)
# Mean time to failure
lambda <- exp(-7.17)
MTTF <- 1/lambda

# Non parametric model with factor
fit3 <- survfit(data = ovarian,
              Surv(futime, fustat) ~ factor(rx))
summary(fit3)
plot(fit3, xlab = "t", ylab = "S(t)", col = 1:2)

# Parametric model, with factor
fit4 <- survreg(data = ovarian,
                Surv(futime, fustat) ~ age, dist = 'exponential')
summary(fit4)
predict(fit4, newdata = ovarian)

# Parametric model, with factor
fit5 <- survreg(data = ovarian,
                Surv(futime, fustat) ~ factor(rx) + factor(ecog.ps) , dist = 'exponential')
summary(fit5)
# p value > 0.05, survival model fit with exponential 
predict(fit4, newdata = ovarian)

# End-----