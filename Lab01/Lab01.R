# Lab01:

# zadanie 1 ---------------------------------------------------------------

N <- 10000
n <- 1:N

zadanie1 <- function(X, n, h) {
  S <- cumsum(X)
  M <- S/n
  plot(M, type = 'l')
  abline(h = h, col = 'blue')
}

# a)

p <- 1/4
X <- rbinom(N, 1, p)
zadanie1(X, n, p)

# b)

lambda <- 1/3
X <- rexp(N, lambda)
zadanie1(X, n, 1/lambda)

# c)

location <- 0
scale <- 1
X <- rcauchy(N, location, scale)
zadanie1(X, n, location)


# zadanie 2 ---------------------------------------------------------------

N <- 500
n <- 1:N
mi <- 4
sigma <- 2

X <- rnorm(N, mi, sigma)
S <- cumsum(X)
M <- S/n

medians = c()
for(i in n)
  medians[i] <-  median(X[1:i])

# a)

plot(M,
     las = 1,      # Orientacja etykiet na osiach.
     xlab = 'N',   # Etykiety dla osi X.
     col = 'blue', # Kolor wykresu.
     type = 'l')   # Typ wykresu.
lines(medians,
      lty = 4,
      col = 'red')

# Zarówno średnie jak i mediany zbiegają do wartości μ (wartości oczekiwanej).

# b)

sds = c()
iqrs = c()
for(i in 2:N)
{
  sds[i-1] <- sd(X[1:i])
  iqrs[i-1] <- IQR(X[1:i])/1.35
}

plot(sds,
     las = 1,
     col = 'orange',
     ylim = c(0,3),  # Określenie zakresu osi Y.
     type = 'l',
     xlab = 'N')
lines(iqrs,
      lty = 4,
      col = 'green')

# Zarówno odchylenia standardowe jak i rozstępy międzykwartylowe zbiegają do 
# wartości σ (odchylenia standardowego).
# Na podstawie powyższych można wnioskować że wybrane statystyki są sensownymi 
# estymatorami odpowiadających im parametrów.

# zadanie 3 ---------------------------------------------------------------

# Rozwiązanie analogiczne do zadania 2.
# Wnioski:

# Mediany zbiegają do wartości "a", natomiast średnie nie zbiegają do niczego.
# Ochylenia ćwiartkowe zbiegają do wartości "d", natomiast ochylenia standardowe
# nie zbiegają do niczego.

# Na podstawie powyższych można wnioskować że mediany i ochylenia ćwiartkowe są 
# sensownymi estymatorami odpowiadających im parametrów, czego nie można 
# powiedzieć o średnich i odchyleniach standardowych.


# zadanie 4 ---------------------------------------------------------------

# fitdistr() - funkcja służy do dopasowywania parametrycznych rozkładów
#              prawdopodobieństwa do danych, można dzięki niej policzyć
#              wartość estymatora największej wiarygodności.

library(MASS)

times <- c(483, 705, 2623, 347, 620, 2719, 1035, 421)
fit <- fitdistr(times, "exponential")

# a)

(estimate <- fit$estimate)

# b)

(time_estimate <- 1/estimate)

(probability <- pexp(1000, rate = estimate))


# zadanie 5 ---------------------------------------------------------------

library(MASS)

N <- 100
alfa <- 3
beta <- 2

X <- rgamma(N, alfa, beta)
(fit <- fitdistr(X, "gamma")$estimate)

# zadanie 6 ---------------------------------------------------------------

# replicate() - funkcja służy do wielokrotnego wykonywania operacji na wektorze

N <- 10000
k <- 20
theta <- 2

estimates <- replicate(N, {
  x <- runif(k, 0, theta)
  return (c(2*mean(x), max(x))) # Wykorzystujemy gotowe wzory na ENW.
})

mean(estimates[1,] - theta)
mean(estimates[2,] - theta)

# Oba estymatory dobrze estymują wartość θ, przy czym warto zauważyć, że
# estymator metody momentów jest nieobciążony, a estymator największej
# wiarygodności ma zauważalne negatywne obciążenie.

par(mfrow=c(2,1), 
    mar = c(3, 3, 1, 1))

plot(estimates[1,],
     las = 1,
     xlab = '',
     ylab = 'EMM',
     ylim = c(0,3))

plot(estimates[2,],
     las = 1,
     xlab = '',
     ylab = 'ENW',
     ylim = c(0,3))