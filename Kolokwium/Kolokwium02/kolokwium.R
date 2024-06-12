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
N <- 27
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
data("barley")

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
# kocurów i kotek z rozważanego zbioru.
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

# H0: badana próba losowa pochodzi z rozkładu t-Studenta o n = 30 
#     stopniach swobody.
# H1: ~H0

# Do rozwiązania zadania wykorzystujemy test Kołmogrowa-Smirnowa

(test_result <- ks.test(x, y = "pt", df = n))

# Wartość statystyki testowej: D = 0.24872
# Wartość p-value: p-value = 0.8485

pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.84855 > 0.05.
# Zatem stwierdzamy, że próba losowa może pochodzić z rozważanego rozkładu.

# zadanie 6 ---------------------------------------------------------------

# H0: μA = μB = μC = μD
# H1: ~H0, czyli występują istotne różnice w czasie działania baterii.

# Zmienna objaśniana to: czas działania baterii w godzinach.
# Zmienna objaśniająca to: typ baterii 
#                          (czynnik występuje na 4 poziomach: A, B, C i D).

times <- c(163, 205, 197, 286, 172,
           87, 106, 101, 94, 123,
           82, 153, 87, 103, 96,
           104, 136, 98, 207, 146)
types <- as.factor(rep(c("A", "B", "C", "D"), rep(5, 4)))
alpha <- 0.05

# Sprawdzamy założenia dla każdego poziomu czynnika:

# 1) Normalność rozkładu dla każdego poziomu czynnika:

simplify2array(tapply(
  times, 
  types, 
  function(x) { shapiro.test(x)[1:2] }))

# A         B         C          D        
# statistic 0.849288  0.9634323 0.8046868  0.8991561
# p.value   0.1922786 0.8316584 0.08843554 0.4052386

# Wszystkie p-value są >= alpha, zatem możemy założyć normalność rozkładów.

# 2) Przybliżona równość wariancji:

# H0: wariancje dla wszystkich typów baterii są równe.
# H1: ~H0

# Wykorzystujemy test Levene'a (ponieważ n < 10, więc nie są spełnione założenia
# dla testu Barletta):

if (!require("car", quietly = TRUE)) {
  install.packages("car")
}
library("car", character.only = TRUE)

(test_result <- leveneTest(times, types, center = mean))

# Levene's Test for Homogeneity of Variance (center = mean)
#       Df F value Pr(>F)
# group  3  1.0467 0.3989
#      16

# Wartość statystyki testowej: F value = 1.0467
# Wartość p-value: p-value = 0.3989

# Nie ma podstaw do odrzucenia H0, zatem możemy założyć równość wariancji.

# Przechodzimy do ANOVA-y:

# Wykorzystujemy funkcje lm() oraz summary():

model <- lm(times ~ types)
summary(model)

# Wartość statystyki F wynosi 8.689, a p-value wynosi 0.001189. 
# Ponieważ p-value jest znacznie mniejsze niż 0.05, możemy odrzucić hipotezę 
# zerową, że średnie czasów działania baterii są równe we wszystkich grupach. 
# Oznacza to, że typ baterii ma istotny wpływ na czas działania.

# Przechodzimy do testu porównań wielokrotnych:

# Wykorzystujemy procedurę Turkey'a, ponieważ próby mają taką samą liczność:

(tukey <- TukeyHSD(aov(model)))
plot(tukey)

#       diff     p adj
# B-A -102.4 0.0019956
# C-A -100.4 0.0023809
# D-A  -66.4 0.0468712
# C-B    2.0 0.9997518
# D-B   36.0 0.4224166
# D-C   34.0 0.4702214

# Dla par: B-A, C-A oraz A-D występuje istotna różnica w czasie działania 
# baterii, ponieważ p-value < alpha. 
# Wartości w kolumnie diff wskazują, że baterie typu A mają istotnie dłuższą
# średnią żywotność w porónaniu do pozostałych typów (B, C i D).
# Nie ma statystycznie istotnych różnic między czasem działania baterii 
# typu B i C, B i D oraz C i D.