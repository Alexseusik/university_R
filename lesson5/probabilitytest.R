# Функція для обчислення відношення вірогідностей для одного спостереження
likelihood_ratio <- function(x) {
  # Густина ймовірності для H1: Beta(2,4)
  pdf_h1 <- dbeta(x, 2, 4)
  
  # Густина ймовірності для H0: Beta(2,5)
  pdf_h0 <- dbeta(x, 2, 5)
  
  # Відношення вірогідностей
  pdf_h1 / pdf_h0
}

# Генеруємо вибірку розміром 50 з розподілу Beta(2,5) - нульова гіпотеза
set.seed(1) # для відтворюваності
sample_data <- rbeta(50, 2, 5)

# Обчислюємо відношення вірогідностей для кожного спостереження у вибірці
lr_values <- sapply(sample_data, likelihood_ratio)

# Обчислюємо продукт усіх відношень вірогідностей для вибірки
likelihood_ratio_product <- prod(lr_values)

# Вивід результату
likelihood_ratio_product