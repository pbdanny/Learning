## Learning from Data
# Home work #4

## Question 2
N <- 10000
delta <- 0.05
dvc <- 50
# choice a) VC Bound
sqrt(8/N*log(4*(2*N)^dvc/delta))

# choice b) Redemacher Penalty Bound
sqrt(2*log(2*N*N^dvc)/N) + sqrt(2/N*log(1/delta)) + 1/N

# choice c) Parondo and Van den Broek 
# assume epsilon <- 0
sqrt((log(6*(2*N)^dvc/delta))/N)

# choice d) Devroye
sqrt(1/(2*N)*log(4*(N^2)^dvc/delta))

## Question 3
N <- 5
delta <- 0.05
dvc <- 50
# choice a) VC Bound
sqrt(8/N*log(4*(2*N)^dvc/delta))

# choice b) Redemacher Penalty Bound
sqrt(2*log(2*N*N^dvc)/N) + sqrt(2/N*log(1/delta)) + 1/N

# choice c) Parondo and Van den Broek 
# assume epsilon <- 0
sqrt((log(6*(2*N)^dvc/delta))/N)

# choice d) Devroye
sqrt(1/(2*N)*log(4*(N^2)^dvc/delta))

## Questino 4-6

# Find avg hypothesis (g bar) ----
all.g <- vector(mode = "numeric")

for(i in 1:1000){
  # create 2 points of sample , output
  x <- runif(2, min = -1, max = 1)
  y <- sin(pi*x)
  # create data frame for lm()
  data <- data.frame(x,y)
  # regressino to the origin use "x - 1"
  model <- lm(y ~ x - 1, data = data)
  all.g <- rbind(all.g, model$coefficients)
}
g.bar.coeff <- mean(all.g)

# Bias = Expected value of  (g.bar(x) - target f(x))^2
x <- runif(1000, min = -1, max = 1)
bias <- mean((g.bar.coeff*x - sin(pi*x))^2)

# Varience = Expected value of (g(x) - g.bar(x))
vari <- mean((all.g*x - g.bar.coeff*x)^2)

Eout <- bias + vari

# Question 7 ----

all.Eout <- data.frame(hypo = character(), E = numeric())

# Find avg hypothesis (g bar) of y = 0 ----

# Bias = Expected value of  (g.bar(x) - target f(x))^2
x <- runif(1000, min = -1, max = 1)
bias <- mean((0 - sin(pi*x))^2)

# Varience = Expected value of (g(x) - g.bar(x))
vari <- mean((0 - g.bar.coeff*x)^2)

Eout <- bias + vari
all.Eout <- rbind(all.Eout, data.frame(hypo = "y = b", E = Eout))

# Find avg hypothesis (g bar) of y = ax ----
all.g <- vector(mode = "numeric")

for(i in 1:1000){
  # create 2 points of sample , output
  x <- runif(2, min = -1, max = 1)
  y <- sin(pi*x)
  # create data frame for lm()
  data <- data.frame(x,y)
  # regressino to the origin use "x - 1"
  model <- lm(y ~ x -1, data = data)
  all.g <- rbind(all.g, model$coefficients)
}
g.bar.coeff <- mean(all.g)

# Bias = Expected value of  (g.bar(x) - target f(x))^2
x <- runif(1000, min = -1, max = 1)
bias <- mean((g.bar.coeff*x - sin(pi*x))^2)

# Varience = Expected value of (g(x) - g.bar(x))
vari <- mean((all.g*x - g.bar.coeff*x)^2)

Eout <- bias + vari

all.Eout <- rbind(all.Eout, data.frame(hypo = "y = ax", E = Eout))

# Find avg hypothesis (g bar) of y = ax+b ----
all.g.intercept <- vector(mode = "numeric")
all.g.coef <- vector(mode = "numeric")

for(i in 1:1000){
  # create 2 points of sample , output
  x <- runif(2, min = -1, max = 1)
  y <- sin(pi*x)
  # create data frame for lm()
  data <- data.frame(x,y)
  # regressino to the origin use "x - 1"
  model <- lm(y ~ x, data = data)
  all.g.intercept <- rbind(all.g.intercept, model$coefficients[1])
  all.g.coef <- rbind(all.g.coef, model$coefficients[2])
}
g.bar.intercept <- mean(all.g.intercept)
g.bar.coef <- mean(all.g.coef)

# Bias = Expected value of  (g.bar(x) - target f(x))^2
x <- runif(1000, min = -1, max = 1)
bias <- mean(((g.bar.coef*x + g.bar.intercept) - sin(pi*x))^2)

# Varience = Expected value of (g(x) - g.bar(x))
vari <- mean(((all.g.coef*x + all.g.intercept) - g.bar.coeff*x)^2)

Eout <- bias + vari
all.Eout <- rbind(all.Eout, data.frame(hypo = "y = ax+b", E = Eout))

# Find avg hypothesis (g bar) of y = ax^2 ----
all.g <- vector(mode = "numeric")

for(i in 1:1000){
  # create 2 points of sample , output
  x <- runif(2, min = -1, max = 1)
  y <- sin(pi*x)
  # create data frame for lm()
  data <- data.frame(x,y)
  # regressino to the origin use "x - 1"
  model <- lm(y ~ x*x -1, data = data)
  all.g <- rbind(all.g, model$coefficients)
}
g.bar.coeff <- mean(all.g)

# Bias = Expected value of  (g.bar(x) - target f(x))^2
x <- runif(1000, min = -1, max = 1)
bias <- mean((g.bar.coeff*x - sin(pi*x))^2)

# Varience = Expected value of (g(x) - g.bar(x))
vari <- mean((all.g*x - g.bar.coeff*x)^2)

Eout <- bias + vari
all.Eout <- rbind(all.Eout, data.frame(hypo = "y = ax^2", E = Eout))

# Find avg hypothesis (g bar) of y = ax^2 + b ----
all.g.intercept <- vector(mode = "numeric")
all.g.coef <- vector(mode = "numeric")

for(i in 1:1000){
  # create 2 points of sample , output
  x <- runif(2, min = -1, max = 1)
  y <- sin(pi*x)
  # create data frame for lm()
  data <- data.frame(x,y)
  # regressino to the origin use "x - 1"
  model <- lm(y ~ x*x, data = data)
  all.g.intercept <- rbind(all.g.intercept, model$coefficients[1])
  all.g.coef <- rbind(all.g.coef, model$coefficients[2])
}
g.bar.intercept <- mean(all.g.intercept)
g.bar.coef <- mean(all.g.coef)

# Bias = Expected value of  (g.bar(x) - target f(x))^2
x <- runif(1000, min = -1, max = 1)
bias <- mean(((g.bar.coef*x + g.bar.intercept) - sin(pi*x))^2)

# Varience = Expected value of (g(x) - g.bar(x))
vari <- mean(((all.g.coef*x + all.g.intercept) - g.bar.coeff*x)^2)

Eout <- bias + vari
all.Eout <- rbind(all.Eout, data.frame(hypo = "y = ax^2 + b", E = Eout))
