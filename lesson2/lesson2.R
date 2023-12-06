data <- read.table("lesson2/distr8.txt", header = TRUE)

X <- sort(data$X)

hist(X, breaks = 50, freq = FALSE, main = "Гістограма з Щільністю Розподілу", xlab = "Значення", ylab = "Щільність")
lines(density(X), col='red')

x <- seq(min(X), max(X), by = 0.1)
y <- dchisq(x, df)
lines(x, y, col = "blue")

plot(pnorm(X, mean = 5, sd = 0.1), (1:length(X))/length(X), asp=1, ylab="", xlab="")
abline(0,1,col=2)

plot(plnorm(X, meanlog = 2, sdlog = 0.1), (1:length(X))/length(X), asp=1, ylab="", xlab="")
abline(0,1,col=2)

plot(pexp(X, rate=5), (1:length(X))/length(X), asp=1, ylab="", xlab="")
abline(0,1,col=2)

plot(pchisq(X, df = 5), (1:length(X))/length(X), asp=1, ylab="", xlab="")
abline(0,1,col=2)

qqnorm(X, main = "QQ-діаграма для Нормального Розподілу")
qqline(X, col = "red")

qqplot(qlnorm(ppoints(X)), X, main = "QQ-діаграма для Логнормального Розподілу", xlab = "Теоретичні квантилі", ylab = "Емпіричні квантилі")
abline(a = 0, b = 1, col = "red")

qqplot(qexp(ppoints(X)), X, main = "QQ-діаграма для Експоненційного Розподілу", xlab = "Теоретичні квантилі", ylab = "Емпіричні квантилі")
abline(a = 0, b = 1, col = "red")

qqplot(qchisq(ppoints(X), df = 5), X, main = "QQ-діаграма для χ² Розподілу", xlab = "Теоретичні квантилі", ylab = "Емпіричні квантилі")
abline(a = 0, b = 1, col = "red")

QQplot <- function(x, df, K=1000, alpha=0.05) {
  n <- length(x)
  chiSquareQ <- qchisq(ppoints(n), df = df)
  sx <- sort(x)
  W <- matrix(rchisq(K*n, df = df), nrow = n, ncol = K)
  W <- apply(W, 2, sort)
  tops <- apply(W, 1, quantile, probs = 1 - alpha/2)
  bots <- apply(W, 1, quantile, probs = alpha/2)
  plot(c(chiSquareQ, chiSquareQ, chiSquareQ), c(tops, bots, sx), type = "n", xlab = "Теоретичні квантилі", ylab = "Емпіричні квантилі")
  points(chiSquareQ, sx, col = 2)
  segments(chiSquareQ, bots, chiSquareQ, tops, col = 4)
  abline(0, 1, col = 1)
}

QQplot(X, 5)