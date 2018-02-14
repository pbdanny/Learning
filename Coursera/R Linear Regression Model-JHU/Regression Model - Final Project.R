library(ggplot2)
data("mtcars")
str(mtcars)
summary(mtcars)

dt <- mtcars
#Test single-regression
fit1 <- lm(mpg ~ am, data = dt)
summary(fit1)

#Test with residual plot
g <- ggplot(data = dt) +
  geom_boxplot(aes(x = factor(am), y = resid(fit1)))
g

#? is this model omitted important varible
#re-check test with varience inflation factor
library(car)
fit2 <- lm(mpg ~ ., data = dt)
vif(fit2)

#Incluce highest vif factor : disp, cyl, wt in model
#use Anova for comparison
fit1.1 <- update(fit1, mpg ~ am + disp)
fit1.2 <- update(fit1, mpg ~ am + disp + cyl)
fit1.3 <- update(fit1, mpg ~ am + disp + cyl + wt)
anova(fit1, fit1.1, fit1.2, fit1.3)

#Incluce highest vif factor : drat, vs, gear in model
#use Anova for comparison
fit1.1 <- update(fit1, mpg ~ am + drat)
fit1.2 <- update(fit1, mpg ~ am + drat + vs)
fit1.3 <- update(fit1, mpg ~ am + drat + vs + gear)
anova(fit1, fit1.1, fit1.2, fit1.3)

#Recheck with varience inflation factor
vif(fit1.2)
