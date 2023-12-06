parkmiller_generator <- function() {
  
  a <- 16807  
  c <- 0
  m <- 2^31-1
  seed <- 123456789
  
  seed <<- (a * seed + c) %% m
  
  return(seed)
}

my_generator <- function() {

  a <- 65521  
  c <- 3000
  m <- 2^31
  seed <- 2^10

  seed <- (a * seed + c) %% m
  
  return(seed)
}

for (i in 1:10) {
  print(my_generator())
}