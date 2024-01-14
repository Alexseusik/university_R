seed_parkmiller <- 1
seed_mygenerator <- 2^10

park_miller_generator <- function() {
  a <- 16807
  m <- 2^31 - 1
  
  seed_parkmiller <<- (a * seed_parkmiller) %% m
  return(seed_parkmiller)
}

generator <- function() {
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
  print(generated[i])
}

X <- generated/m

plot(1:n, X, cex=0.3)

sorted_X <- sort(X)

dist_generator <- function() {
  
  u1 <- generator()/m
  u1 <- generator()/m
  
  xi <- rexp(u1, rate = 1)
  nu <- runif(u1, 0, 1)
  
  z <- xi + nu
  
  return(z)
}

n <- 150

generated <- numeric(n)

for (i in 1:n) {
  generated[i] <- dist_generator()
}

X <- generated
sorted_X <- sort(X)

plot(sorted_X,(1:n)/n,type="s")

