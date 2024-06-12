# Kolokwium 2022-2023:

can_reject <- function(pvalue, alpha) {
  if(pvalue <= alpha) {
    message("Odrzucamy H0, ponieważ ", pvalue, " ≤ ", alpha)
  } else {
    message("Nie ma podstaw do odrzucenia H0, ponieważ ", 
            pvalue, " > ", alpha)
  }
}

# zadanie 1 ---------------------------------------------------------------

set.seed(111)

lambda <- 0.25
N <- 25
X <- rexp(N, lambda)

# a)

library(MASS)
(fit <- fitdistr(X, "exponential"))

# b)

(pexp(7, fit$estimate, lower.tail = FALSE))


# zadanie 2 ---------------------------------------------------------------

# Niech X będzie zmienną losową o rozkładzie dwupunktowym.
# X = 1 oznacza, że losowo wybrany kibic wytypował Polaków.
# X = 0 oznacza, że losowo wybrany kibic wytypował inną drużynę.

x <- 97
n <- 250
alpha <- 0.01

# Sprawdzamy, czy możemy skorzystać z prop.test():

(all(c(x, n - x) > 5))

# Wyznaczamy przedział ufności:

(test_result <- prop.test(x, n, conf.level = 1 - alpha))
(conf_interval <- test_result$conf.int)

# zadanie 3 ---------------------------------------------------------------

library(lattice)
data(barley)

# Niech X będzie zmienną losową reprezentującą wysokość plonów na farmie Waseca.
# X ma rozkład normalny o nieznanych parametrach.

# Przyjęty model statystyczny to Model II z karty wzorów.

X <- barley$yield[barley$site == "Waseca"]
alpha <- 0.05
mu <- 50

# H0: μ = 50
# H1: μ < 50

# Stosujemy test t-studenta:

(test_result <- t.test(X,
                       mu = mu,
                       alternative = "less",
                       conf.level = 1 - alpha))
pvalue = (test_result$p.value)
can_reject(pvalue, alpha)

# Wartość statystyki testowej: t = -0.89388
# Wartość p-value: p-value = 0.1913
# Postać zbioru krytycznego: -Inf 51.76758

# Nie ma podstaw do odrzucenia H0, ponieważ 0.1913 > 0.05.
# Zatem stwierdzamy, że średni plon nie musi być mniejszy niż 50 jednostek.

# zadanie 4 ---------------------------------------------------------------

library(MASS)
data("cats")

# Niech X, Y będą zmiennymi losowymi reprezentującymi masę w kg odpowiednio 
# kocurów i kotów z rozważanego zbioru.
# X i Y mają rozkład normalny o nieznanych parametrach.
# Próby losowe z populacji są niezależne.

# Zastosowany model statystyczny to Model I z karty wzorów.

X <- cats$Bwt[cats$Sex == "M"]
Y <- cats$Bwt[cats$Sex == "F"]

# H0: σX^2 = σY^2
# H1: σX^2 != σY^2

alpha <- 0.05
(test_result <- var.test(X, Y, alternative = "two.sided"))

# Wartość statystyki testowej: F = 2.9112
# Wartość p-value: p-value = 0.0001157

pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Odrzucamy H0, ponieważ 0.0001157 ≤ 0.05.
# Możemy zatem stwierdzić, że wariancje te są istotnie różne.


# zadanie 5 ---------------------------------------------------------------

n <- 30
x <- c(-0.89, -0.13, -0.05, 1.18, 0.25)
alpha <- 0.05

# H0: badana próba losowa pochodzi z rozkładu t-studenta o n = 30 
#     stopniach swobody.
# H1: ~H0

(test_result <- ks.test(x, y = "pt", df = n))

# Wartość statystyki testowej: D = 0.24872
# Wartość p-value: p-value = 0.8485

pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.84855 > 0.05.
# Zatem stwierdzamy, że próba losowa może pochodzić z rozważanego rozkładu.

# zadanie 6 ---------------------------------------------------------------

# H0: μA = μB = μC = μD
# H1: ~H0

# Zmienna objaśniana to: czas działania baterii w godzinach.
# Zmienna objaśniająca to: typ baterii (czynnik występuje na 4 poziomach: A, B, C i D).

times <- c(163, 205, 197, 286, 172,
           87, 106, 101, 94, 123,
           82, 153, 87, 103, 96,
           104, 136, 98, 207, 146)
types <- as.factor(rep(c("A", "B", "C", "D"), rep(5, 4)))

(means <- tapply(times, types, mean))
plot(times ~ types, main = "Wykresy skrzynkowe",
     xlab = "type", ylab = "time [h]")
lines(1:4, means, type = "p", col = "red")

