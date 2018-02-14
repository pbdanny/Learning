mean(runif(100))
1:10
2^10
2**10
abs(rnorm(10))

oddcount <- function(x) {
    k <- 0
    for (n in x) {
        if (n %% 2 == 1) k <- k+1
    }
    return(k)
}

oddcount(c(1, 2, 3, 5, 7, 10))

x <- c(88, NULL, 12, 168, 13)
length(x)

z <- NULL
for (i in 1:10) if (i %% 2 == 0) z <- c(z, i)
z

z <- c(5, 2, -3, 8)
w <- z[z*z > 8]
w

m <- matrix(1:4, nrow = 2, byrow = FALSE)
3*m
m %*% m

z <- matrix(1:16, nrow = 4)
z[,2:3]

z <- matrix(1:6, nrow = 3)
z
z <- cbind(z, rep(1,3))
z

x <- c(1:6)
x
as.character(x)
as.logical(x)
as.integer(x)

j <- list(name = "Joe", salary = 55000, union = T)
j$name
j[[1]]
j["name"]

j$location <- "bangkok"
j
j[["location"]]
j[["union"]] <- NULL
j

kid <- c("Jack", "Jill")
age <- c(12, 10)
d <- data.frame(kid, age)
d
str(d)

d[[1]]
d["kid"]
d[["kid"]]

head(mtcars)
mtcars$ratio <- mtcars$hp/mtcars$cyl
mtcars$ratio <- NULL

x <- rbind(m,m)


m <- rbind(mtcars, list(99, 10, 200, 999, 9, 9, 99, 1,1,9,9))
m <- rbind(mtcars, c(99, 10, 200, 999, 9, 9, 99, 1, 1, 9,9))

# Assignment
d <- read.csv(file.choose(), sep = ";", header = T, stringsAsFactors = F)
d[,d$age > 35 || d$age < 45]
