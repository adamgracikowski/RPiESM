# Przykładowe zadania do kolokwium:

can_reject <- function(pvalue, alpha) {
  if(pvalue <= alpha) {
    message("Odrzucamy H0, ponieważ ", pvalue, " ≤ ", alpha)
  } else {
    message("Nie ma podstaw do odrzucenia H0, ponieważ ", 
            pvalue, " > ", alpha)
  }
}

# zadanie 1 ---------------------------------------------------------------

# Niech X będzie zmienną losową reprezentującą czas montażu podzespołu w minutach.
# X ma rozkład normalny o nieznanych parametrach.

X <- c(20.4, 24.2, 18.6, 19.6, 23.0, 21.8)
alpha <- 0.05

(test_result <- t.test(X, conf.level = 1 - alpha))
pvalue <- test_result$p.value

can_reject(pvalue, alpha)

# Odrzucamy H0, ponieważ 2.09589904067022e-06 ≤ 0.05.


# zadanie 2 ---------------------------------------------------------------

# Niech X będzie zmienną losową reprezentującą staż pracy pracownika.
# X ma rozkład normalny o nieznanych parametrach.

X <- rep(c(5, 15, 25), c(7, 15, 3))
alpha <- 0.05

if (!require("TeachingDemos", quietly = TRUE)) {
  install.packages("TeachingDemos")
}
library("TeachingDemos", character.only = TRUE)

# a)

(conf_interval <- sqrt(sigma.test(X, conf_level = 1 - alpha)$conf))

# b)

# H0: μ = 13 lat
# H1: μ > 13 lat

alpha <- 0.05
mu <- 13

(test_result <- t.test(X,
                       mu = mu,
                       alternative = "greater", 
                       conf.level = 1 - alpha))
pvalue = test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.375772831694894 > 0.05
# Możemy zatem stwierdzić, że średni staż pracy w tym przedsiębiorstwie nie
# przekracza 13 lat.

# zadanie 3 ---------------------------------------------------------------

# H0: badana próba losowa pochodzi z rozkładu równomiernego.
# H1: badana próba losowa nie pochodzi z rozkładu równomiernego.

x <- c(16, 28, 32, 14, 10)
p <- rep(1/length(x), length(x))
alpha <- 0.1

# Sprawdzamy, czy spełnione są założenia dla testu χ^2-Pearsona:

(all(sum(x)*p >= 10))

(test_result <- chisq.test(x = x, p = p))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Odrzucamy H0, ponieważ 0.0012340980408668 ≤ 0.1.
# Zatem badana próba losowa nie pochodzi z rozkładu równomiernego.


# zadanie 4 ---------------------------------------------------------------

# Niech X będzie zmienną losową reprezentującą wyniki pomiarów 
# rozważanej wielkości w cm.
# X ma rozkład normalny o nieznanych parametrach.

X <- c(1.017, 1.021, 10.015, 1.019, 1.022, 1.019)

# a) 

alpha <- 0.01

# H0: σ = 0.001
# H1: σ > 0.001

sigma <- 0.001

(test_result <- sigma.test(X, 
                           sigma = sigma, 
                           alternative = "greater", 
                           conf.level = 1 - alpha))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Odrzucamy H0, ponieważ 0 ≤ 0.01.

# b)

alpha <- 0.05
(test_result <- t.test(X, conf.level = 1 - alpha))
(round(test_result$conf.int, 3))