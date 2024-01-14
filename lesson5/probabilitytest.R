set.seed(5)


alpha_H0 <- 2
beta_H0 <- 5
alpha_H1 <- 2
beta_H1 <- 4
alpha_level <- 0.05
B <- 10000

generate_data <- function(a, b, n) {
  return(rbeta(n, a, b))
}

log_likelihood_ratio <- function(x, a_H0, b_H0, a_H1, b_H1) {
  return(sum(log(dbeta(x, a_H1, b_H1) / dbeta(x, a_H0, b_H0))))
}

estimate_errors <- function(n) {
  data_H0 <- replicate(B, generate_data(alpha_H0, beta_H0, n))
  lr_H0 <- apply(data_H0, 2, function(x) log_likelihood_ratio(x, alpha_H0, beta_H0, alpha_H1, beta_H1))
  Ca <- quantile(lr_H0, 1 - alpha_level)
  alpha_error <- mean(lr_H0 > Ca)
  data_H1 <- replicate(B, generate_data(alpha_H1, beta_H1, n))
  lr_H1 <- apply(data_H1, 2, function(x) log_likelihood_ratio(x, alpha_H0, beta_H0, alpha_H1, beta_H1))
  beta_error <- mean(lr_H1 < Ca)
  return(c(alpha_error, beta_error))
}

optimal_n <- NULL
for (n in seq(10, 500, by=2)) {
  errors <- estimate_errors(n)
  cat("n:", n, "Alpha error:", errors[1], "Beta error:", errors[2], "\n")
  if (errors[1] <= alpha_level && errors[2] <= alpha_level) {
    optimal_n <- n
    break
  }
}


optimal_n
