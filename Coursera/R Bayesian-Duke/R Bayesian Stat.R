# Confident interval
sample.p <- 0.5
sample.sd <- sqrt((0.5)*(0.5))
n <- 20
sample.p.se <- sample.sd/sqrt(n)
ha <- 4/20
ha <- 0.56

plot(rnorm(ha, mean = sample.p, sd = sample.p.se))
ha + c(-1,1)*qnorm(0.975, mean = ha, sd = sqrt(ha*(1-ha))/sqrt(n))
binom.test(56,100)$conf.int
ha + c(-1,1)*qnorm(0.975, mean = ha, sd = sqrt(ha*(1-ha))/sqrt(n))

plot(pbinom(0:4, size = 20, prob = 0.5))
qbinom(.95, size = 20, prob = 0.5)

# Week 1 
# Hypothesis testing Frequestist vs. Bayesian 
# Frequentist hypothesis
# assume H0 true -> p = 0.5
p <- 0.5
n <- 20
k <- 4

# p-value = probability extreme to the alternative hypothesis
sum(dbinom(0:k, size = n, prob = p))
# or
pbinom(k, size = n, prob = p)

# p-value = 0.005908966 if significant level = 0.05, reject H0 

# Bayesist hypothesis
# create p from range of model p = 0.1 to 0.9
# model p list
p <- c(seq(from = 0.1, to = 0.9, by = 0.1)) 
# P(model) = prior 
prior <- c(rep(0.06, 4), 0.52, rep(0.06, 4)) 
# data
n <- 20
k <- 4

# likelihood : P(data|model) : P(k = 4, n = 20, p = from p)
likelihood <- dbinom(k, size = n, p = p)

# Posterior : P(mode|data) <- after testing data come how it effect
# prior P(model)
# P(model|data) = P(data|model) x P(model) / P(data)
# P(mode|data) = likelihood x prior / sum(likelihood x prior)

numerator <- likelihood * prior
denominator <- sum(numerator) 
posterior <- numerator/denominator
sum(posterior)  # Recheck sum posterior = 1
p[which.max(posterior)]

# Quiz 1
# P(rain & pred rain) by
# P(pred rain) = p(pred rain|rain) + p(pred rain | not rain)
(50/365)*(0.8)/((50/365)*(0.8) + (315/365)*(0.3))


# Quiz 2
# Ans A
# If the posterior probability of H0 is less than .05, 
# the p-value under H0 will also be less than .05.

# Quiz 3
# The probability of observing exactly k males in 20 samples, 
# given p, the true population proportion of males.

# Quiz 4
# Probability is a measure of the likelihood that an event will occur. 

# Quiz 5
# model
p <- c(0.2, 0.4, 0.5, 0.6, 0.8)
# prior of model
prior <- c(1/8, 1/8, 1/2, 1/8, 1/8)
k <- 3
n <- 3

# Binomial distribution
likelihood <- dbinom(k, size = n, prob = p)
plot(likelihood)
# Find P(model | data) = Bayes' rules = P(data | model) * P(model) / P(data) 
# numerator : P(data | model) * P(model) = likelihood * prior
# denominator : p(data) = P(data | model1) + P(data | model2) + ... + P(data | modeln)
numerator <- likelihood * prior
denominator <- sum(numerator)
posterior <- numerator/denominator
sum(posterior)
plot(posterior)

# Week 1 Quiz

# Question 1
# Solution
# p(C|r,b) <- {P(r,b|C)*P(C)}/{P(r,b|A)*P(A) + P(r,b|B)*P(B) + P(r,b|C)*P(C)} 

ans <- (1/3*1/2)*(1/3)/((1/6*1/2*1/3) + (1/3*1/6*1/3) + (1/3*1/2*1/3))
# ans in fraction = 6/11

# Question 3 
p.h1 <- dbinom(0, 10, 1/1000)
p.h2 <- dbinom(0, 10, 1/1000000)
ans <- p.h1/(p.h1+p.h2)

# Question 7
ans <- sum(dbinom(5:300, size = 300, prob = 0.01))

# Question 8
N <- seq(1, 9, 1)
N <- N * 100e6
p.N <- 1/N
people <- 413271201
likelihood <- dbinom(3, people, p.N)
prior <- rep(1/9, 9)
denom <- sum(likelihood * 0.1)
ans <- sum(likelihood[1:5]*0.1)/denom

# Question 9

n <- 6000
p.h1 <- 1/6
p.h2 <- 0.175
c <- seq(0, 6000, 1)
p.c999.h1 <- dbinom(999, n , p.h1) 
p.c999.h2 <- dbinom(999, n , p.h2)
p.cMore.h1 <- 1-pbinom(999, n, p.h1)
p.cMore.h2 <- 1-pbinom(999, n, p.h2)
p.h1.c999 <- (p.c999.h1*0.8)/(p.c999.h1*0.8 + p.c999.h2*0.2)
p.h1.cMore <- (p.cMore.h1*0.8)/(p.cMore.h1*0.8 + p.cMore.h2*0.2)
p.h1.c999 + p.h1.cMore

# Quiz 
beta <- function (x) {factorial(26)/(factorial(22)*factorial(4))*(x^3)*((1-x)^21)}
pbeta(0.2, 4, 22)

plot(dbeta(seq(0.01, 0.99, 0.01), 0.5, 0.5))

1-pbeta(0.5, 143, 157)

# Posterior probability & p-value
n <- 104490000
k <- 52263471
p <- k/n
# Central Limit Theorem
mean <- n*0.5
sd <- sqrt(n*0.5*(1-0.5))
z <- (k - mean)/sd
(1-pnorm(k, mean, sd))*2

beta(1,1)
