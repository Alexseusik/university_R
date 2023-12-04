
data <- read.table("lesson2/distr8.txt", header = TRUE, sep = "\t")

X <- sort(data$X)

hist(X, breaks=30, probability = TRUE)

lambda <- 4

# Додавання гладкої лінії логнормального розподілу
meanlog <- 0
sdlog <- 1
curve(dlnorm(x, meanlog = meanlog, sdlog = sdlog), from = 0, to = 20, col = "green", lwd = 2, add = TRUE)
