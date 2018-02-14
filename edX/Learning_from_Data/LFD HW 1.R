# Learning form Data
# Homework #1
# Question 7 - 10

## 1) One experiment ----
## Create dataset and target fn
# Create dataframe of input
n = 100
set.seed(1)
x <- data.frame(x0 = 1,
                x1 = runif(n, min = -1, max = 1), 
                x2 = runif(n, min = -1, max = 1))
plot(x$x1, x$x2)
# text(x, col = "green")

# Create target function
f <- data.frame(slop = runif(1, min = -1, max = 1), 
                intercept = runif(1, min = -1, max = 1))
abline(a = f$intercept, b = f$slop, col = "blue")

# Assign output from target function
# from formular x' = a + b*x1 if x2 > x' , then on the right side of line
# if x2 <  x', on the left side of line
x$color <- ifelse(x$x2 > (x$x1*f$slop + f$intercept), 1, 4)
plot(x$x1, x$x2, col = x$color, pch = ifelse(x$color == 1, 1, 2))
abline(a = f$intercept, b = f$slop, col = "green")

# Assign output value y = {-1, 1}
x$y <- ifelse(x$color == 1, 1, -1)

## Start PLA algorithm
# Assing weight w
w <- data.frame(x0 = 0, x1 = 0, x2 = 0)

for(i in 1:100) {
  # h(x) = transpose(w) * x
  h <- sign(as.matrix(x[,c("x0", "x1", "x2")]) %*% t(w))
  # find mismatced idx when sign(w*x) != y
  idx <- sign(h) != x[, "y"]
  cat(sprintf("iteration %d : avg err %f \n", i, sum(idx)/n))
  # plot error points
  plot(x$x1, x$x2, col = x$color, pch = ifelse(x$color == 1, 1, 2))
  abline(a = f$intercept, b = f$slop, col = "green")
  points(x = x[idx,"x1"], y = x[idx, "x2"], type = "p", pch = 3 , col = "red")
  
  # Assign new w
  w <- w + t(as.matrix(x[idx, "y"])) %*% as.matrix(x[idx, c("x0", "x1", "x2")])
}

## 2) 1000 experitment ----

# sum of no. iteration until converge
sum.i <- 0
# no. of data point in each iteration
n <- 10

# Start experiment for 1000 times
for (i in 1:1000){
  # Create dataframe of input
  x <- data.frame(x0 = 1,
                  x1 = runif(n, min = -1, max = 1), 
                  x2 = runif(n, min = -1, max = 1))
  
  # Create target function
  f <- data.frame(slop = runif(1, min = -1, max = 1), 
                  intercept = runif(1, min = -1, max = 1))
  
  # Assign output from target function
  # from formular x' = a + b*x1 if x2 > x' , then on the right side of line
  # if x2 <  x', on the left side of line
  # Assign output value y = {-1, 1}
  x$y <- ifelse(x$x2 > (x$x1*f$slop + f$intercept), 1, -1)
  
  # Assing weight w
  w <- data.frame(x0 = 0, x1 = 0, x2 = 0)
  
  # Initialized iteration
  i <- 0
  
  # Initalized error
  err <- 1
  
  while(err > 0.000001) {
    pre.err <- sum(idx)/n
    # h(x) = transpose(w) * x
    h <- sign(as.matrix(x[,c("x0", "x1", "x2")]) %*% t(w))
    # find missidx when sign(w*x) != y
    idx <- sign(h) != x[, "y"]
    err <- sum(idx)/n
    # Assign new w
    w <- w + t(as.matrix(x[idx, "y"])) %*% as.matrix(x[idx, c("x0", "x1", "x2")])
    i <- i+1
  }
  sum.i <- sum.i + i
}

# avg iteration until converge
sum.i/1000