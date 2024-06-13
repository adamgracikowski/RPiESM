# Kolokwium 2023-2024:

can_reject <- function(pvalue, alpha) {
  if(pvalue <= alpha) {
    message("Odrzucamy H0, ponieważ ", pvalue, " ≤ ", alpha)
  } else {
    message("Nie ma podstaw do odrzucenia H0, ponieważ ", 
            pvalue, " > ", alpha)
  }
}

# zadanie 1 ---------------------------------------------------------------

set.seed(123)
N <- 25
lambda <- 6

X <- rexp(N, lambda)

# a)

library(MASS)
(fit <- fitdistr(X, "exponential"))

#     rate  
#  6.807561 
# (1.361512)

# b)

(qexp(0.9, fit$estimate))

# 0.3382394

# zadanie 2 ---------------------------------------------------------------

library(MASS)
data("crabs")

X <- crabs$CW

# a)

summary(X)

(mean(X))          # 36.41
(var(X))           # 61.96768
(median(X))        # 36.8
(quantile(X, 0.8)) # 43.44

# b)

hist_data <- hist(X, freq = FALSE)
(mid_point <- hist_data$mids)

# Środki przedziałów klasowych: 17.5 22.5 27.5 32.5 37.5 42.5 47.5 52.5

(heights3 <- hist_data$counts[1:3])

# Wysokości pierwszych trzech słupków: 3 16 22

# c)

# H0: szerokość pancerza krabów ma rozkład normalny.
# H1: ~H0

(test_result <- shapiro.test(X))
pvalue <- test_result$p.value
can_reject(pvalue, 0.05)

# Wartość statystyki testowej: W = 0.99106
# Wartość p-value: p-value = 0.2542

# Nie ma podstaw do odrzucenia H0, ponieważ 0.2542 > 0.05.
# Szerokość pancerza może zatem mieć rozkład normalny.

# d)

# Zakładamy, że zmienna X ma rozkład normalny o nieznanych parametrach.
# Do skonstruowania przedziału ufności dla odchylenia standradowego skorzystamy 
# ze wzoru z karty wzorów:

n <- length(X)
s <- sd(X)
alpha <- 0.05

chi_left <- qchisq(1 - alpha/2, n - 1)
chi_right <- qchisq(alpha/2, n - 1)

(conf_int <- (n - 1) * s / sqrt(c(chi_left, chi_right)))

# Przedział ufności: 101.1269 123.1434


# zadanie 3 ---------------------------------------------------------------

# Niech X będzie zmienną losową o rozkładzie dwupunktowym.
# X = 1 oznacza, że losowo wybrana osoba oglądała obrady.
# X = 0 oznacza, że losowo wybrana osoba nie oglądała obrad.

# a)

# p - prawdopodobieństwo, że losowo wybrana osoba oglądała obrady.

n <- 500
x <- 24
p <- 0.04
alpha <- 0.05

# H0: p = 0.04
# H1: p > 0.04

# Spełnione są założenia Modelu IV, zatem korzystamy z funkcji prop.test():

(all(c(x, n - x) > 5))
(test_result <- prop.test(x, n, p = p, 
                          alternative = "greater", 
                          conf.level = 1 - alpha))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Wartość statystyki testowej: X-squared = 0.63802
# Wartość p-value: p-value = 0.2122

# Nie ma podstaw do odrzucenia H0, ponieważ 0.2122 > 0.05
# Transmisja obrad może mieć zatem oglądalność przekraczającą 4%.

# b)

alpha = 0.01
(binom.test(x, n, conf.level = 1 - alpha)$conf.int)

# Przedział ufności: 0.02677391 0.07823701

# zadanie 4 ---------------------------------------------------------------

# H0: badana próba losowa pochodzi z rozkładu Poissona z parametrem λ = 2.
# H1: ~H0

lambda <- 2

days <- c(22, 30, 22, 16, 10)
p <- c(dpois(0:3, lambda = lambda), 
       ppois(3, lambda = lambda, lower.tail = FALSE))

# Sprawdzamy, czy spełnione są założenia dla testu χ^2-Pearsona:

(all(sum(days) * p >= 10))

# Przeprowadzamy test χ^2-Pearsona przy pomocy funkcji chisq.test():

(test_result <- chisq.test(x = days, p = p))
pvalue = test_result$p.value

# Wartość statystyki testowej: X-squared = 8.0813
# Wartość p-value: p-value = 0.08864

can_reject(pvalue, 0.05)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.08864 > 0.01.
# Zatem badana próba losowa może pochodzić z rozważanego rozkładu.

# zadanie 5 ---------------------------------------------------------------

library(MASS)
data("anorexia")

# Niech X oznacza wagę przed kurację, a Y wagę po kuracji. Dane mamy połączone
# w pary (X, Y), a pary są wzajemnie niezależne.

# Z treści zadania wiemy, że łączny rozkład (X, Y ) to rozkład normalny
# (nie znamy jego parametrów), co pociąga za sobą, że rozkład X − Y też jest
# rozkładem normalnym (o nieznanych parametrach).

# Pobieramy odpowiednie dane ze zbioru anorexia:

X <- anorexia$Prewt[anorexia$Treat == "CBT"]
Y <- anorexia$Postwt[anorexia$Treat == "CBT"]

# a)

alpha <- 0.05

# H0: μX = μY
# H1: μX < μY

# Spełnione są założenia Modelu III z karty wzorów, zatem możemy wykorzystać
# paired t-test:

(test_result <- t.test(X, Y, 
                       alternative = "less", 
                       paired = TRUE, 
                       conf.level = 1 - alpha))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Wartość statystyki testowej: t = -2.2156
# Wartość p-value: p-value = 0.01751

# Odrzucamy H0, ponieważ 0.01751 ≤ 0.05.
# Nie można zatem twierdzić, że średnia waga pacjentek poddanych terapii uległa
# zwiększeniu.

# b)

# Do wyznaczenia prawdopodobieństwa popełnienia błędu II rodzaju wykorzystamy
# power.t.test():

# Zakładamy, że μy - μX = 3 funty.

delta <- 3
(test_result <- power.t.test(n = length(X), 
                            delta = delta,
                            sd = sd(Y - X),
                            sig.level = alpha,
                            type = "paired",
                            alternative = "one.sided"))
power <- test_result$power
(result <- 1 - power)

# Prawdopodobieństwo to wynosi: 0.3044165