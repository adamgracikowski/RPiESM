# Lab04:

can_reject <- function(pvalue, alpha) {
  if(pvalue <= alpha) {
    message("Odrzucamy H0, ponieważ ", pvalue, " ≤ ", alpha)
  } else {
    message("Nie ma podstaw do odrzucenia H0, ponieważ ", 
            pvalue, " > ", alpha)
  }
}

# zadanie 1 ---------------------------------------------------------------

# Niech X, Y będą zmiennymi losowymi o rozkładzie dwupunktowym.
# X = 1 oznacza, że mieszkaniec Warszawy jest regularnym klientem sieci sklepów.
# X = 0 oznacza, że mieszkaniec Warszawy nie robi zakupów w sieci sklepów.
# Y = 1 oznacza, że mieszkaniec Krakowa jest regularnym klientem sieci sklepów.
# Y = 0 oznacza, że mieszkaniec Krakowa nie robi zakupów w sieci sklepów.

# p1 - prawdopodobieństwo, że losowo wybrany mieszkaniec Warszawy jest regularnym
#      klientem sieci sklepów Żuczek.
# p2 - prawdopodobieństwo, że losowo wybrany mieszkaniec Krakowa jest regularnym
#      klientem sieci sklepów Żuczek.

# a)

# H0: p1 = p2
# H1: p1 > p2

alpha <- 0.05
n1 <- 233
k1 <- 40
n2 <- 220
k2 <- 31

(test_result <- prop.test(x = c(k1, k2), 
                         n = c(n1, n2), 
                         alternative = "greater"))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.220385477834834 > 0.05.

# b.i)

(test_result <- power.prop.test(n = c(n1, n2), 
                               p1 = 0.17, 
                               p2 = 0.14, 
                               sig.level = 0.05, 
                               alternative = "one.sided"))
(power <- test_result$power)

# Prawdopodobieństwo zdarzenia należy do [0.2188411; 0.2263947].

# b.ii)

(test_result <- power.prop.test(power = 0.8, 
                               p1 = 0.17, 
                               p2 = 0.14, 
                               sig.level = 0.05, 
                               alternative = "one.sided"))
n <- test_result$n
(n <- ceiling(n))

# Należy wylosować co najmniej 1799 mieszkańców Warszawy oraz co najmniej 1799
# mieszkańców Krakowa.

# zadanie 2 ---------------------------------------------------------------

# Niech X, Y oznaczają zmienne losowe reprezentujące zawartość nikotyny
# W papierosach pierwszego i drugiego garunku papierosów.
# X i Y mają rozkłady normalne o nieznanych parametrach.

# Pomiary zawartości nikotyny zostały wykonane niezależnie od siebie, zatem
# skorzystamy z tzw. unpaired t-test.

X <- c(26.4, 22.5, 24.9, 23.7, 21.5)
Y <- c(25.1, 29.0, 23.4, 27.6, 22.3)

# a)

# Aby skorzystać z Modelu II musimy sprawdzić, czy σ1 = σ2:

# H0: σ1 = σ2
# H1: σ1 != σ2

alpha <- 0.1
(test_result <- var.test(X, Y, alternative = "two.sided"))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.489133763132729 > 0.1.

# Wiedząc, że σ1 = σ2 możemy przystąpić do kolejnej części zadania:

# H0: μ1 = μ2
# H1: μ1 < μ2

alpha <- 0.05
(test_result <- t.test(X, Y, 
                      alternative = "less", 
                      paired = FALSE, 
                      var.equal = TRUE))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Nie ma podstaw do odrzucenia H0, ponieważ 0.151122716439341 > 0.05.

# b)

# Odchylenie standardowe X i Y szacujemy koszystając z następującego estymatora:

sd_XY <- sqrt(0.5 * (var(X) + var(Y)))

(test_result <- power.t.test(n = length(X),
                            delta = 2,
                            sd = sd_XY,
                            sig.level = 0.05,
                            type = "two.sample",
                            alternative = "one.sided"))
power <- test_result$power
(result <- 1 - power)

# Prawdopodobieństwo błędnej odpowiedzi wynosi: 0.6710692

# c)

sd_XY <- sqrt(0.5 * (var(X) + var(Y)))

(test_result <- power.t.test(power = 0.75,
                            delta = 2,
                            sd = sd_XY,
                            sig.level = 0.05,
                            type = "two.sample",
                            alternative = "one.sided"))
n <- test_result$n
(n <- ceiling(n))

# Trzeba pobrać próby gatunków liczności co najmniej 17 każda.

# zadanie 3 ---------------------------------------------------------------

path <- "C:Data/hemoglobina.txt"
hemoglobina <- read.csv(path, sep="")

before <- hemoglobina$przed
after <- hemoglobina$po

# a)

# H0: μ_before = μ_after
# H1: μ_before > μ_after

alpha <- 0.05
(test_result <- t.test(before, 
                      after, 
                      alternative = "greater", 
                      paired = TRUE))
pvalue <- test_result$p.value
can_reject(pvalue, alpha)

# Odrzucamy H0, ponieważ 0.00286060968698234 ≤ 0.05.
# Stwierdzamy zatem, że (dane nie potwierdzają, że nowy lek obniża poziom
# hemoglobiny glikowanej.

# b)

# Pomiary są połączone w pary, zatem do obliczenia odchylenia standardowego
# korzystamy z następującego wzoru:

sd_XY = sd(before - after)

(test_result <- power.t.test(n = length(before),
                            delta = 1.5,
                            sd_XY,
                            sig.level = 0.05,
                            type = "paired",
                            alternative = "one.sided"))
(power <- test_result$power)

# zadanie 4 ---------------------------------------------------------------

# Wczytujemy dane ze zbioru nlschools:

library(MASS)
data(nlschools)

ses <- nlschools$SES
iq <- nlschools$IQ

median_ses <- median(ses, na.rm = TRUE)

# Znajdujemy dane poniżej oraz powyżej mediany:

iq_above_median <- iq[ses > median_ses]
iq_below_median <- iq[ses <= median_ses]

alpha <- 0.05

# H0: μ_iq_below = μ_iq_above
# H1: μ_iq_below < μ_iq_above

# Wykorzystujemy statystykę z Modelu IV, ponieważ nie mamy informacji o
# rozkładach danych pomiarowych oraz są one dostatecznie liczne:

U <- (mean(iq_above_median) - mean(iq_below_median)) / 
  sqrt(var(iq_above_median)/length(iq_above_median) + 
       var(iq_below_median)/length(iq_below_median))

# Obliczamy p-value:

pvalue <- pnorm(U, lower.tail = FALSE)
can_reject(pvalue, alpha)

# Odrzucamy H0, ponieważ 3.36370197064809e-34 ≤ 0.05.
# Nie możemy zatem stwierdzić, że wśród uczniów kończących ósmą klasę,
# ci pochodzący z domów o społeczno-ekonomicznym statusie powyżej mediany, 
# mają wyższy poziom inteligencji werbalnej niż pozostali.