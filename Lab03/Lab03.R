# Lab03:

can_reject <- function(pvalue, alpha) {
  if(pvalue <= alpha) {
    message("Odrzucamy H0, ponieważ ", pvalue, " ≤ ", alpha)
  } else {
    message("Nie ma podstaw do odrzucenia H0, ponieważ ", 
            pvalue, " > ", alpha)
  }
}

# zadanie 1 ---------------------------------------------------------------

# Niech X oznacza zmienną losową reprezentującą wagę ptaków badanego gatunku.

X <- c(5.21, 5.15, 5.20, 5.48, 5.19, 5.25, 5.09, 5.17, 4.94, 5.11)

# X ma rozkład normalny, N(μ, σ^2), o nieznanych parametrach μ oraz σ.

# a)

# Tworzymy przedział ufności dla średniej zmiennej X:

conf_level <-  0.95
(conf_interval <- t.test(X, conf.level = conf_level)$conf.int)

# b)

# H0: μ = 5.15kg
# H1: μ != 5.15kg

alpha <- 0.05
mu <- 5.15

(test_result <- t.test(X,
                      mu = mu,
                      alternative = "two.sided", 
                      conf.level = 1 - alpha))
pvalue = test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.518801113547527 > 0.05.
# Stwierdzamy zatem, że można stwierdzić, że średnia waga ptaków badanego
# gatunku wynosi 5.15 kg.

# c)

alpha <- 0.05
mu <- 5.2

# H0: μ = 5.2
# H1: μ < 5.2

(test_result <- t.test(X,
                      mu = mu,
                      alternative = "less",
                      conf.level = 1 - alpha))
pvalue = (test_result$p.value)
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.3192172477902 > 0.05.
# Stwierdzamy, że nie można sądzić, że waga ptaków badanego gatunku jest
# mniejsza niż 5.20 kg.

# d)

# H0: μ = 5.2
# H1 μ < 5.2

alpha <- 0.05
mu_actual <- 5.15
mu_hypot <- 5.20

# Szukamy prawdopodobieństwa odrzucenia hipotezy zerowej, czyli mocy testu β:

(test_result <- power.t.test(n = length(X), 
                            delta = abs(mu_actual - mu_hypot),
                            sd = sd(X),
                            type = "one.sample",
                            alternative = "one.sided",
                            sig.level = alpha))
(power <- test_result$power)

# Prawdopodobieństwo to wynosi: 0.2830938.

# e)

# H0: μ = 5.2
# H1: μ < 5.2

alpha <- 0.05
mu_actual <- 5.15
mu_hypot <- 5.20
power <- 0.8

(test_result <- power.t.test(n = length(X),
                            power = power,
                            sd = sd(X),
                            type = "one.sample",
                            alternative = "one.sided",
                            sig.level = alpha))
delta = test_result$delta
(mu_required <- mu_hypot - delta)

# Średnia waga ptaków musiałaby wynosić: 5.083513 kg.

# f)

# H0: μ = 5.2
# H1: μ < 5.2

alpha <- 0.05
mu_actual <- 5.15
mu_hypot <- 5.20
power <- 0.8

(test_result <- power.t.test(power = power, 
                            delta = abs(mu_actual - mu_hypot),
                            sd = sd(X), 
                            type = "one.sample", 
                            alternative = "one.sided",
                            sig.level = alpha))
n <- test_result$n
(n_required <- ceiling(n))

# Minimalna liczność próby to: 48.

# g)

alpha <- 0.05

# Aby utworzyć przedział ufności dla wariancji wagi ptaków badanego gatunku,
# możemy postąpić na dwa różne sposoby:

# I sposób:

n <- length(X)
sq <- var(X)
chisq_left <- qchisq(1 - alpha/2, n - 1)
chisq_right <- qchisq(alpha/2, n - 1)

(conf_interval <- c((n - 1)*sq/chisq_left, (n - 1)*sq/chisq_right))

# II sposób:

# Korzystamy z funkcji sigma.test() z pakietu TeachingDemos:

if (!require("TeachingDemos", quietly = TRUE)) {
  install.packages("TeachingDemos")
}
library("TeachingDemos", character.only = TRUE)

(conf_interval <- sigma.test(X, conf_level = 1 - alpha)$conf)

# h)

# Wystarczy spierwiastkować końce przedziału wyznaczonego w poprzednim podpunkcie:

alpha <- 0.05
(sqrt(sigma.test(X, conf_level = 1 - alpha)$conf))

# i)

# H0: σ = 0.2
# H1: σ != 0.2

alpha <- 0.05
sigma <- 0.2

(test_result <- sigma.test(X, 
                          sigma = sigma, 
                          sigmasq = sigma^2, 
                          alternative = "two.sided", 
                          conf.level = 1 - alpha))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.204086442248777 > 0.05.

# zadanie 2 ---------------------------------------------------------------

# Wczytujemy dane z pliku goats.txt:

path <- "Data/goats.txt"
data <- read.csv(path, sep="")
weights <- data$WeightInitial

# Wiemy, że weights ma rozkład normalny o nieznanych parametrach.

# a)

conf_level <- 0.95
(conf_interval <- t.test(weights, conf.level = conf_level)$conf.int)

# b)

# H0: μ = 23kg
# H1: μ > 23kg

alpha <- 0.05
mu <- 23

(test_result <- t.test(weights, 
                      mu = mu, 
                      alternative = "greater", 
                      conf.level = 1 - alpha))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.393601988096709 > 0.05.

# c)

# H0: μ = 23 kg
# H1: μ > 23 kg

alpha <- 0.05
mu_actual <- 24
mu_hypot <- 23
n <- 40

(test_result <- power.t.test(n = n, 
                            delta = abs(mu_actual - mu_hypot),
                            sd = sd(weights),
                            type = "one.sample",
                            alternative = "one.sided",
                            sig.level = alpha))
power <- test_result$power

(result <- 1 - power)

# Prawdopodobieństwo to wynosi: 0.4460559

# d)

# H0: μ = 23 kg
# H1: μ > 23 kg

alpha <- 0.05
mu_actual <- 24
mu_hypot <- 23
power <- 0.8

(test_result <- power.t.test(power = power,
                            delta = abs(mu_actual - mu_hypot),
                            sd = sd(weights),
                            type = "one.sample",
                            alternative = "one.sided",
                            sig.level = alpha))
n <- test_result$n
(n <- ceiling(n))

# Należy zebrać co najmniej 77 pomiarów.

# e)

alpha <- 0.1

if (!require("TeachingDemos", quietly = TRUE)) {
  install.packages("TeachingDemos")
}
library("TeachingDemos", character.only = TRUE)

(conf_interval <- sigma.test(weights, conf.level = 1 - alpha)$conf)

# f)

# H0: σ^2 = 20 kg^2
# H1: σ^2 != 20 kg^2

sigmasq <- 20
alpha <- 0.1

(test_result <- sigma.test(weights, 
                          sigmasq = sigmasq, 
                          alternative = "two.sided", 
                          conf.level = 1 - alpha))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Odrzucamy H0, ponieważ 0.0518968755499783 ≤ 0.1.
# Stwierdzamy zatem, że nie można przyjąć stwierdzenia, że wariancja 
# młodych kóz hodowlanych w Australii wynosi 20 kg^2.

# g)

# H0: σ = 3
# H1: σ > 3

alpha <- 0.1
sigma <- 3

(test_result <- sigma.test(weights,
                          sigma = sigma,
                          alternative = "greater",
                          conf.level = 1 - alpha))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Odrzucamy H0, ponieważ 0.0692457788417857 ≤ 0.1.

# zadanie 3 ---------------------------------------------------------------

# Niech X będzie zmienną losową o rozkładzie dwupunktowym.
# X = 1 oznacza, że pracownik przedszkola jest mężczyzną.
# X = 0 oznacza, że pracownik przedszkola jest kobietą.

# p - prawdopodobieństwo, że losowo wybrany pracownik przedszkola jest mężczyzną

alpha <- 0.05
n <- 400
k <- 128
p0 <- 0.35

# a)

# H0: p = 0.35
# H1: p < 0.35

# Spełnione są założenia Modelu IV, zatem korzystamy z funkcji prop.test():

(test_result <- prop.test(x = k, 
                         n = n, 
                         alternative = "less",
                         p = p0))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.113999852189345 > 0.05.

# b)

(conf_interval <- prop.test(x = k, 
                            n = n, 
                            conf.level = 1 - alpha)$conf.int)

# c)

alpha <- 0.05
n <- 10
k <- 3
p0 <- 0.35

# W tym przypadku założenia Modelu IV nie są w pełni spełnione, ze względu na 
# małą liczność próby. Korzystamy zatem z funkcji binom.test():

(test_result <- binom.test(x = k,
                          n = n, 
                          alternative = "less",
                          p = p0))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.513827016355859 > 0.05.

# d)

(conf_interval <- binom.test(x = k, 
                             n = n, 
                             conf.level = 1 - alpha)$conf.int)

# zadanie 4 ---------------------------------------------------------------

# Niech X będzie zmienną losową o rozkładzie dwupunktowym.
# X = 1 oznacza, że pracownik legitymuje się stażem mniejszym niż 5 lat.
# X = 0 oznacza, że pracownik legitymuje się stażem >= 5 lat.

# p - prawdopodobieństwo, że losowo wybrany pracownik legitymuje się stażem
#     mniejszym niż 5 lat.

# H0: p = 0.8
# H1: p != 0.8

n <- 150
k <- 118
p_hypot <- 0.8
alpha <- 0.05

# Spełnione są założenia Modelu IV, zatem korzystamy z funkcji prop.test():

(test_result <- prop.test(x = k,
                         n = n,
                         p = p_hypot,
                         alternative = "two.sided",
                         conf.level = 1 - alpha))
pvalue <- test_result$p.value 
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.759462865379933 > 0.05.

# zadanie 5 ---------------------------------------------------------------

# a)

# Korzystamy ze wzoru na przedział ufności z Modelu III:

huge_test <- function(X, alpha){
  n <- length(X)
  if(n < 100){
    stop("Argument 'X' must have at least 100 elements.")
  }
  n_sqrt <- sqrt(n)
  s <- sd(X)
  u <- qnorm(1 - alpha/2)
  X_mean <- mean(X)
  half_width <- u*s/n_sqrt
  return (c(X_mean - half_width, X_mean + half_width))
}

# b)

# Wczytujemy dane ze zbioru 'geyser':

library(MASS)
data("geyser")
duration <- geyser$duration

# Ponieważ nie wiemy jaki rozkład mają powyższe dane, korzystamy z funkcji
# napisanej w punkcie a):

alpha <- 0.05
(conf_interval <- huge_test(duration, alpha))