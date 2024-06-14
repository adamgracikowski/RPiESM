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

# H0: Wartości próbki potwierdzają normalność rozkładu próbki.
# H1: ~H0

X <- c(3.75, 4.52, -3.88, 6.85, 8.15, 6.15)

# a)

# W celu sprawdzenia założenia o normalności rozkładu próbki skorzystamy z 
# testu Shapiro-Wilka, czyli funkcji shapiro.test():

alpha <- 0.05
(test_result <- shapiro.test(X))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Wartość statystyki testowej: W = 0.83202
# Wartość p-value: p-value = 0.1118

# Nie ma podstaw do odrzucenia H0, ponieważ 0.1118 > 0.05.
# Próba może zatem należeć do rozkładu normalnego.

# b)

library(MASS)
(fit <- fitdistr(X, "normal"))

#     mean        sd   
#  4.256667   3.915770 
# (1.598606) (1.130385)

# c)

if (!require("TeachingDemos", quietly = TRUE)) {
  install.packages("TeachingDemos")
}
library("TeachingDemos", character.only = TRUE)

alpha <- 0.05
(conf_interval <- sqrt(sigma.test(X, conf.level =  1 - alpha)$conf))

# Przedział ufności: 2.677547 10.520515

# zadanie 2 ---------------------------------------------------------------

# Pobieramy dane o erupcjach gejzera, po których wystąpieniu czas czekania 
# na kolejną był mniejszy niż 70:

X <- faithful$eruptions[faithful$waiting < 70]

# a)

# Wyświetlamy podstawowe statystyki:

summary(X)
sd(X)

# b)

# Szkicujemy wykres skrzynkowy:

box <- boxplot(X)

# Znajdujemy liczbę obserwacji odstających:

(outliers <- boxplot.stats(X)$out)
(length(outliers))

# 2.883 3.067 3.367 3.833 4.067 3.500 3.333 3.417 2.900
# Jest ich dokładnie 9.

# zadanie 3 ---------------------------------------------------------------

# Niech X będzie zmienną losową reprezentującą obwód pnia drzewek pomarańczowych.
# Zakładamy, że X ma rozkład normalny o nieznanych parametrach.

X <- Orange$circumference

# a)

# H0: μ = 100 mm
# H1: μ < 100 mm

# W celu przetestowania hipotezy korzystamy z testu t-Studenta, czyli funkcji
# t.test():

alpha <- 0.05
mu <- 100

(test_result <- t.test(X,
                       mu = mu,
                       alternative = "less",
                       conf.level = 1 - alpha))
pvalue = (test_result$p.value)
can_reject(pvalue, alpha)

# Wartość statystyki testowej: t = 1.6319
# Wartość p-value: p-value = 0.944

# Nie ma podstaw do odrzucenia H0, ponieważ 0.944 > 0.05.
# Zatem średnia obwodu pnia może nie być mniejsza niż 100 mm.

# b)

mu_actual <- 90

(test_result <- power.t.test(n = length(X), 
                             delta = abs(mu - mu_actual),
                             sd = sd(X),
                             type = "one.sample",
                             alternative = "one.sided",
                             sig.level = alpha))
(power <- test_result$power)

# Prawdopodobieństwo to wynosi: 0.262322

# c)

# H0: σ = 40 mm
# H1: σ != 40 mm

alpha <- 0.05
sigma <- 40

# W celu sprawdzenia hipotezy korzystamy z funkcji sigma.test():

(test_result <- sigma.test(X, 
                           sigma = sigma, 
                           sigmasq = sigma^2,
                           alternative = "two.sided", 
                           conf.level = 1 - alpha))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Wartość statystyki testowej: X-squared = 70.229
# Wartość p-value: p-value = 0.0005085

# Odrzucamy H0, ponieważ 0.0005085 ≤ 0.05.
# Możemy zatem twierdzić, że odchylenie standardowe obwodu pnia różni się
# istotnie od 40 mm.

# zadanie 4 ---------------------------------------------------------------

library(MASS)

# Wybieramy ze zbioru interesujące nas dane:

X <- Pima.te$glu[Pima.te$age > 40]
Y <- Pima.te$glu[Pima.te$age <= 40]
alpha <- 0.05

# Zmienne X, Y mają rozkład normalny o nieznanych parametrach ale równych 
# wariancjach. Dane te są niezależne. Spełnione są zatem założenia Modelu II
# z karty wzorów.

# a)

# H0: μX = μY
# H1: μX > μY

# W celu przetestowania hipotezy korzystamy z testu t-Studenta, czyli funkcji
# t.test():

(test_result <- t.test(X, Y, 
                       alternative = "greater", 
                       paired = FALSE, 
                       var.equal = TRUE))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Wartość statystyki testowej: t = 2.9646
# Wartość p-value: p-value = 0.001626

# Odrzucamy H0, ponieważ 0.001626 ≤ 0.05.
# Zatem możemy twierdzić, że wartośc średnia X jest istotnie wyższa niż wartość Y.

# b)

# Wyznaczamy wartość estymatora odchylenia standardowego X i Y:

nX = length(X)
nY = length(Y)
varX = var(X)
varY = var(Y)
(sd_XY = sqrt(((nX - 1) * varX + (nY - 1) * varY)/(nX + nY - 2)))

# Wartość tego estymatora to: 30.14848

# W celu wyznaczenia prawdopodobieństwa korzystamy z funkcji power.t.test():

(test_result <- power.t.test(power = 0.8,
                             delta = 3, 
                             sd = sd_XY,
                             sig.level = 0.01, 
                             type="two.sample",
                             alternative="one.sided"))
(n <- ceiling(test_result$n))

# Wartość n powinna być równa co najmniej 2029 dla każdej z X i Y.

# zadanie 5 ---------------------------------------------------------------

# H0: Rozkład prawdopodobieństwa preferencji klientów wynosi odpowiednio 
#     0.3, 0.4, 0.1, 0.1, 0.1 dla smaków waniliowe, czekoladowe, truskawkowe,
#     jagodowe, orzechowe.
# H1: ~H0

x <- c(234, 290, 76, 73, 69)
p <- c(0.3, 0.4, 0.1, 0.1, 0.1)
alpha <- 0.05

# Sprawdzamy, czy spełnione są założenia dla testu χ^2-Pearsona:

(all(sum(x)*p >= 10))

(test_result <- chisq.test(x = x, p = p))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Wartość statystyki testowej: X-squared = 1.1671
# Wartość p-value: p-value = 0.8835

# Nie ma podstaw do odrzucenia H0, ponieważ 0.8835 > 0.05.
# Możemy zatem twierdzić, że rozkład preferencji klientów może być taki jak
# w treści zadania.