seed_parkmiller <- 1
seed_mygenerator <- 2^10

park_miller_generator <- function() {
  a <- 16807
  m <- 2^31 - 1
  
  seed_parkmiller <<- (a * seed_parkmiller) %% m
  return(seed_parkmiller)
}

my_generator <- function() {
  m <- 2^31
  a <- 65521
  c <- 3000
  
  seed_mygenerator <<- (a * seed_mygenerator + c) %% m
  return(seed_mygenerator)
}

n <- 500
m <- 2^31

generated <- numeric(n)

for (i in 1:n) {
  generated[i] <- my_generator()
}

X <- generated/m

plot(1:n, X, cex=0.5)

sorted_X <- sort(X)

plot(sorted_X,(1:n)/n, type='s', xlim=c(0,1), ylim=c(0,1))
abline(a=0,b=1,col="red")

x1<-X[1:(n-2)]
x2<-X[2:(n-1)]
x3<-X[3:n]
library(rgl)

plot3d(x1,x2,x3)
plot(x1,x3,cex=0.5)

