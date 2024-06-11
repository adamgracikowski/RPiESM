# Lab05:

can_reject <- function(pvalue, alpha) {
  if(pvalue <= alpha) {
    message("Odrzucamy H0, ponieważ ", pvalue, " ≤ ", alpha)
  } else {
    message("Nie ma podstaw do odrzucenia H0, ponieważ ", 
            pvalue, " > ", alpha)
  }
}

# zadanie 1 ---------------------------------------------------------------

# H0: badana próba losowa pochodzi z rozkładu równomiernego.
# H1: badana próba losowa nie pochodzi z rozkładu równomiernego.

x <- c(380, 340, 380, 500)
p <- rep(1/length(x), length(x))
alpha <- 0.05

# Sprawdzamy, czy spełnione są założenia dla testu χ^2-Pearsona:

(all(sum(x)*p >= 10))

(test_result <- chisq.test(x = x, p = p))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Odrzucamy H0, ponieważ 7.48837694879548e-08 ≤ 0.05.
# Zatem badana próba losowa nie pochodzi z rozkładu równomiernego.

# zadanie 2 ---------------------------------------------------------------

# H0: prawdopodobieństwa wyst¸apienia grup krwi 0, A, B, AB w populacji są równe
#     odpowiednio: 0.4; 0.4; 0.1; 0.1.
# H1: ~H0

x <- c(36, 42, 14, 8)
p <- c(0.4, 0.4, 0.1, 0.1)
alpha <- 0.05

# Sprawdzamy, czy spełnione są założenia dla testu χ^2-Pearsona:

(all(sum(x)*p >= 10))

(test_result <- chisq.test(x = x, p = p))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.475291083343021 > 0.05.

# zadanie 3 ---------------------------------------------------------------

# H0: badana próba losowa pochodzi z rozkładu dwumianowego o parametrach 3 i 0.5
# h1: ~H0

n <- 3
p <- 0.5
values <- c(0, 1, 2, 3)
x <- c(24, 73, 77, 26)

# Ponieważ rozkład jest jednoznacznie określony, możemy obliczyć 
# prawdopodobieństwa poszczególnych wyników:

p <- dbinom(values, n, p)

alpha <- 0.05

# Sprawdzamy, czy spełnione są założenia dla testu χ^2-Pearsona:

(all(sum(x)*p >= 10))

(test_result <- chisq.test(x = x, p = p))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.979712499851545 > 0.05.

# zadanie 4 ---------------------------------------------------------------

# H0: badana próba losowa pochodzi z rozkładu Poissona (hipoteza złożona)
# H1: ~H0

particles <- 0:5
x <- c(140, 280, 235, 200, 100, 45)
observed_data <- rep(particles, x)

# Wyznaczamy estymatę parametru λ metodą największej wiarygodności:

library(MASS)
lambda <- fitdistr(observed_data, "Poisson")$estimate

# Na podstawie estymaty parametru λ znajdujemy prawdopodobieństwa wystąpienia
# poszczególnych wartości:

p <- c(dpois(particles[-length(particles)], lambda), 
                   ppois(4, lambda, lower.tail = FALSE))

# Sprawdzamy, czy spełnione są założenia dla testu χ^2-Pearsona:
(all(sum(x)*p >= 10))

(test_result <- chisq.test(x = x, p = p))

# Nie możemy bezpośrednio porównać poziom istotności z p-value, ponieważ
# p-value dotyczy hipotezy prostej. Musimy skorzystać ze zbioru krytycznego:

k <- length(x)
r <- 1
alpha <- 0.1
(chi_value <- qchisq(1 - alpha, k - 1 - r))

if(chi_value <= test_result$statistic){
  message("Odrzucamy H0 (statystyka testowa należy do zbioru krytycznego)")
} else {
  message("Nie ma podstaw do odrzucenia H0.")
}

# Nie ma podstaw do odrzucenia H0.
# Zatem próba losowa pochodzi z rozkładu Poissona.

# zadanie 5 ---------------------------------------------------------------

# H0: badana próba losowa pochodzi z rozkładu Gamma(a = 4.5, β = 4).
# H1: ~H0

path <- "Data/infolinia.txt"
infolinia <- read.csv(path, sep="")
times <- infolinia$czas
alpha <- 0.05

a <- 4.5
beta <- 4

# Ponieważ rozkład Gamma jest rozkładem absolutnie ciągłym, do przeprowadzenia
# testu korzystamy z testu Kołmogorowa-Smirnowa:

(test_result <- ks.test(x = times, 
                        y = "pgamma", 
                        shape = 4.5, 
                        rate = 4))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.374533989147458 > 0.05.
# Stwierdzamy zatem, że badana próba pochodzi z rozważanego rozkładu.
# Wartość statystyki testowej D = 0.091338.

# zadanie 6 ---------------------------------------------------------------

N <- 100

# Generujemy podane próby losowe:

d1 <- rnorm(N, mean = 20, sd = 5)
d2 <- runif(N, min = -1, max = 1)
d3 <- rexp(N, rate = 1/5)
d4 <- rpois(N, lambda = 3)

# Przygotowujemy okno graficzne do wyświetlania kilku wykresów w jednym wierszu:

par(mfrow = c(1, 4))

names <- c("rozkład normalny", "rozkład jednostajny",
           "rozkład wykładniczy", "rozkład Poissona")

# a)

qqnorm(d1, main = names[1])
qqline(d1, col = "blue")

qqnorm(d2, main = names[2])
qqline(d2, col = "blue")

qqnorm(d3, main = names[3])
qqline(d3, col = "blue")

qqnorm(d4, main = names[4])
qqline(d4, col = "blue")

# b)

boxplot(d1, main = names[1])

boxplot(d2, main = names[2])

boxplot(d3, main = names[3])

boxplot(d4, main = names[4])

# c)

hist(d1, freq = FALSE, main = names[1])
lines(density(d1), col = "red")

hist(d2, freq = FALSE, main = names[2])
lines(density(d2), col = "red")

hist(d3, freq = FALSE, main = names[3])
lines(density(d3), col = "red")

hist(d4, freq = FALSE, main = names[4])
lines(density(d4), col = "red")

# d)

# H0: zmienna losowa ma rozkład normalny.
# H1: zmienna losowa nie ma rozkładu normalnego.

alpha <- 0.05

test_result <- shapiro.test(d1)
(pvalue <- test_result$p.value)
(pvalue <= alpha)

# Odrzucamy hipotezę, że zmienna ma rozkład normalny.

test_result <- shapiro.test(d2)
(pvalue <- test_result$p.value)
(pvalue <= alpha)

# Brak podstaw do odrzucenia hipotezy, że zmienna ma rozkład normalny.