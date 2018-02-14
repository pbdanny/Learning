# Learning from Data
# Home work #2

## Question 1-2 ----

ex <- data.frame()

for (i in 1:100000) {
  # filb 10 coins 1000 times
  c <- apply(as.data.frame(rep(0, 1000)), MARGIN = 1, FUN = function(x) {
    return(mean(rbinom(10, 1, prob = 0.5)))
  })
  # pick coin #1
  c.1 <- c[1]
  # pick random coin 
  c.rand <- sample(c, 1)
  # pick min mu
  c.min <- min(c)
  ex <- rbind(ex, c(c.1, c.rand, c.min))
}
colnames(ex) <- c("c.1", "c.rand", "c.min")
mean(ex$c.1)
mean(ex$c.rand)
mean(ex$c.min)

## Question 5 - 7 ----

# no. of data point in each iteration
n.in <- 100
n.out <- 1000
# Initialixed Error sample
Ein <- data.frame()
Eout <- data.frame()
# Start experiment for 1000 times
for (i in 1:1000){
  # Create dataframe of input
  x <- data.frame(x0 = 1,
                  x1 = runif(n.in, min = -1, max = 1), 
                  x2 = runif(n.in, min = -1, max = 1))
  
  x.out <- data.frame(x0 = 1,
                  x1 = runif(n.out, min = -1, max = 1), 
                  x2 = runif(n.out, min = -1, max = 1))
  
  # Create target function
  f <- data.frame(slop = runif(1, min = -1, max = 1), 
                  intecept = runif(1, min = -1, max = 1))
  
  # Assign output from target function
  # from formular x' = a + b*x1 if x2 > x' , then on the right side of line
  # if x2 <  x', on the left side of line
  # Assign output value y = {-1, 1}
  x$y <- ifelse(x$x2 > (x$x1*f$slop + f$intecept), 1, -1)
  x.out$y <- ifelse(x.out$x2 > (x.out$x1*f$slop + f$intecept), 1, -1)
  
  # Linear regression w from w = (t(x)*x)-1*t(x)*y
  X <- as.matrix(x[,c("x0","x1","x2")])
  w <- solve(t(X) %*% X) %*% t(X) %*% as.matrix(x$y)
  
  # forecasting f
  f <- sign(X %*% w)
  
  # Error in-sample
  Ein <- rbind(Ein, mean(f != x[, "y"]))
  
  # forecasting f out of sample
  f.out <- sign(as.matrix(x.out[,c("x0","x1","x2")]) %*% w)
  
  # Error out-of-sample
  Eout <- rbind(Eout, mean(f.out != x.out[, "y"]))
}
colnames(Ein) <- "err"
colnames(Eout) <- "err"
mean(Ein$err)
mean(Eout$err)

## Question 8-10 ----

# no. of data point in each iteration
n.in <- 1000
n.out <- 1000

# Initialixed Error sample
Ein <- data.frame()
Eout <- data.frame()
Ein.tr <- data.frame()
Eout.tr <- data.frame()
# Start experiment for 1000 times
for (i in 1:1000){
  
  # Create dataframe of input
  x <- data.frame(x0 = 1,
                  x1 = runif(n.in, min = -1, max = 1), 
                  x2 = runif(n.in, min = -1, max = 1))
  
  x.out <- data.frame(x0 = 1,
                      x1 = runif(n.out, min = -1, max = 1), 
                      x2 = runif(n.out, min = -1, max = 1))
  
  # Assign output value y = f(x1, x2) = sign(x1^2 + x2^2 -0.6)
  x$y <- sign(x$x1^2 + x$x2^2 - 0.6)
  x.out$y <- sign(x.out$x1^2 + x.out$x2^2 - 0.6)
  
  # Adding noise by flipping sign of 10% training set
  err.idx <- sample(1:n.in, n.in*0.1)
  x[err.idx, "y"] <- -x[err.idx, "y"]
  err.idx.out <- sample(1:n.out, n.out*0.1)
  x.out[err.idx.out, "y"] <- -x.out[err.idx.out, "y"]
  
  # Add transformation x3 = x1*x2, x4 = x1^2, x5 = x2^2
  x.tr <- x
  x.tr$x3 <- x.tr$x1*x.tr$x2
  x.tr$x4 <- x.tr$x1^2
  x.tr$x5 <- x.tr$x2^2

  x.out.tr <- x.out
  x.out.tr$x3 <- x.out.tr$x1*x.out.tr$x2
  x.out.tr$x4 <- x.out.tr$x1^2
  x.out.tr$x5 <- x.out.tr$x2^2

  # Linear regression w from w = (t(x)*x)-1*t(x)*y
  X <- as.matrix(x[,c("x0","x1","x2")])
  w <- solve(t(X) %*% X) %*% t(X) %*% as.matrix(x$y)
  
  # Linear regression w.tilda from on nonliner transformation 
  X.tr <- as.matrix(x.tr[,c("x0","x1","x2","x3","x4","x5")])
  w.tilda <- solve(t(X.tr) %*% X.tr) %*% t(X.tr) %*% as.matrix(x.tr$y)
  
  # forecasting f
  f <- sign(X %*% w)
  f.tr <- sign(X.tr %*% w.tilda)
  
  # Error in-sample
  Ein <- rbind(Ein, mean(f != x[, "y"]))
  Ein.tr <- rbind(Ein.tr, mean((f.tr != x.tr[, "y"])))
  
  # forecasting f out of sample
  f.out <- sign(as.matrix(x.out[,c("x0","x1","x2")]) %*% w)
  f.out.tr <- sign(as.matrix(x.out.tr[,c("x0","x1","x2","x3","x4","x5")]) %*% w.tilda)
  
  # Error out-of-sample
  Eout <- rbind(Eout, mean(f.out != x.out[, "y"]))
  Eout.tr <- rbind(Eout.tr, mean(f.out.tr != x.out.tr[, "y"]))
}
colnames(Ein) <- "err"
colnames(Eout) <- "err"
mean(Ein$err)
mean(Eout$err)

colnames(Ein.tr) <- "err"
mean(Ein.tr$err)
colnames(Eout.tr) <- "err"
mean(Eout.tr$err)