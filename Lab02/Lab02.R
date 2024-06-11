# Lab02:

# zadanie 1 ---------------------------------------------------------------

sample20 <- rnorm(20)
sample100 <- rnorm(100)

# Rysujemy dystrybuantę empiryczną pierwszej próby:

plot(ecdf(sample20), 
     col  = 'red',
     xlim = c(-2.5,2.5), # Zakres dobrany tak, by pokazać to, co istotne.
     main = 'Dystrybuanty empiryczne i teoretyczna',
     xlab = 'x',
     ylab = 'F(x)',
     las  = 1)

# Dodajemy dystrybuantę empiryczną drugiej próby:

lines(ecdf(sample100),
      col = 'blue')

# Dodajemy dystrybuantę teoretyczną:

curve(pnorm(x),
      col = 'green',
      add = T) # Aby dodać do istniejącego wykresu ustawiamy add = T.

# Obserwujemy, że dystrybuanta empiryczna przypomina dystrybuantę teoretyczną,
# oraz wraz ze wzrosem obserwacji w próbie przybliża dystrybuantę teoretyczną
# coraz dokładniej.

# zadanie 2 ---------------------------------------------------------------

N <- 1000
sample <- rnorm(N)

hist(sample,
     prob = T,        # Histogram ma reprezentować gęstość prawdopodobieństwa.
     xlim = c(-4,4),  # Zakresy dobrane by pokazać to, co istotne.
     ylim = c(0,0.5),
     main = "Histogram i estymator jądrowy",
     xlab = 'x',
     ylab = 'P(x)',
     las  = 1)

# Tworzymy estymator jądrowy dla próby:

est = density(sample,
              bw = 0.3) # Parametr odpowiedzialny za wygładanie, 
                        # im większy tym bardziej wygładzony estymator.

# Dodajemy do wykresu estymator jądrowy:

lines(est,
      col = 'red',
      lwd = 2)

# Dodajemy do wykresu gęstość teoretyczną:

curve(dnorm(x),
      col = 'green',
      lwd = 2,
      add = T)

# zadanie 3 ---------------------------------------------------------------

par(mfrow=c(3,1), 
    mar = c(3, 3, 1, 1))

barplot(dbinom(0:10,10,0.5),
        col       = 'red',
        names.arg = 0:10,
        las       = 1,
        main      = "Wykersy funkcji prawdopodobieństwa rozkładów dwumianowych",
        ylab      = 'P(x)',
        xlab      = 'x')

barplot(dbinom(0:10,10,0.25),
        col       = 'green',
        names.arg = 0:10,
        las       = 1,
        ylab      = 'P(x)',
        xlab      = 'x')

barplot(dbinom(0:50,50,0.25),
        col       = 'blue',
        names.arg = 0:50,
        las       = 1,
        ylab      = 'P(x)',
        xlab      = 'x')

# zadanie 4 ---------------------------------------------------------------

# Rysujemy odpowiednie wykresy gęstości zmiennych losowych o rozkładzie 
# t-Studenta:

curve(dt(x,5),
      xlim = c(-4,4),
      ylim = c(0,0.4),
      col  = 'red',
      main = 'Wykresy funkcji gęstości rozkładu t-Studenta',
      ylab = 'P(x)',
      xlab = 'x')

curve(dt(x,10),
      add = T,
      col = 'green')

curve(dt(x,40),
      add = T,
      col = 'blue')

# Rysujemy wykres gęstości standardowego rozkładu normalnego N(0,1):
curve(dnorm(x,0,1),
      add = T,
      col = 'magenta',lwd = 2)


# zadanie 5 ---------------------------------------------------------------

# Rysujemy odpowiednie wykresy gęstości zmiennych losowych o rozkładzie χ^2:

curve(dchisq(x,5),
      xlim = c(0,80),
      ylim = c(0,0.15),
      col  = 'red',
      main = 'Wykresy funkcji gęstości rozkładu chi-kwadrat',
      ylab = 'P(x)',
      xlab = 'x')

curve(dchisq(x,10),
      add = T,
      col = 'green')

curve(dchisq(x,40),
      add = T,
      col = 'blue')

# Wykres gęstości rozkładu normalnego o następujących parametrach
# N('liczba stopni swobody',sqrt(2*'liczba stopni swobody')):

curve(dnorm(x,40,sqrt(2*40)), # Przykład dla 40 stopni swobody.
      add = T,
      col = 'magenta',
      lwd = 2)


# zadanie 6 ---------------------------------------------------------------

# Ładujemy pakiet MASS i wczytujemy zbiór danych Cars93:

library(MASS)
data("Cars93")

# a) 

# Dodajemy nową zmienną przy użyciu prostej konwersji z mili na galon
# do litrów na 100km:

mile2km <- 1.6
gallon2liter <- 3.8

(Cars93$zp.m <- 100 * gallon2liter / (mile2km * Cars93$MPG.city))

# b)

summary(Cars93$zp.m)

# Wyznaczamy kwantyl rzędu 0.95:
(quantile_zpm <- quantile(Cars93$zp.m, 0.95))

# Wartość ta oznacza, że około 95% danych znajduje się poniżej tej wartości, 
# a około 5% znajduje się powyżej niej.

# c)

par(mfrow = c(1, 2))  # Ustawienie układu na 1 wiersz i 2 kolumny.

(b1 <- boxplot(zp.m ~ Origin, 
               data = Cars93, 
               main = "Zużycie paliwa w mieście (litr/100km)",
               xlab = "Pochodzenie", 
               ylab = "Zużycie paliwa (litr/100km)"))

(b2 <- boxplot(MPG.city ~ Origin, 
               data = Cars93, 
               main = "Zużycie paliwa w mieście (mpg)",
               xlab = "Pochodzenie", 
               ylab = "Zużycie paliwa (mpg)"))

# Z wykresu pudełkowego można odczytać wartość pierwszego kwartyla, trzeciego
# kwartyla, rozstępu ćwiartkowego oraz mediany. Na takim wykresie możemy jeszcze
# wyróżnić takie elementy jak wąsy oraz wartości odstające.

# d)

par(mfrow = c(1, 2))

(type_counts <- table(Cars93$Type))

barplot(type_counts, 
        main = "Liczba samochodów w poszczególnych kategoriach", 
        xlab = "Typ samochodu", 
        ylab = "Liczba samochodów")

pie(type_counts, 
    main = "Procentowy udział samochodów w poszczególnych kategoriach")

# Do kategorii 'sportowe' należy 14 samochodów.

# zadanie 7 ---------------------------------------------------------------

# I sposób:

sample_count <- 10000
sample_size <- 10
conf_level <- 0.95
actual_mean <- 0
coverage_count <- 0

samples <- matrix(NA, 
                  nrow = sample_count, 
                  ncol = sample_size)
conf_intervals <- matrix(NA, 
                         nrow = sample_count, 
                         ncol = 2) 

for (i in 1:sample_count) {
  
  # Generujemy próbkę:
  samples[i,] <- rnorm(sample_size)
  
  # Znajdujemy przedział ufności na poziomie 0.95:
  conf_intervals[i,] <- t.test(samples[i,], conf.level = conf_level)$conf.int
  
  # Sprawdzamy, czy faktyczna wartość oczekiwana znajduje się w przedziale:
  if(actual_mean >= conf_intervals[i, 1] & actual_mean <= conf_intervals[i, 2])
    coverage_count <- coverage_count + 1
}

coverage_fraction <- coverage_count / sample_count

# Porównujemy frakcję pokryć z założonym poziomem ufności:
if (coverage_fraction >= conf_level) {
  message("Frakcja pokryć: ", coverage_fraction, 
          ", spełnia założony poziom ufności (", conf_level, ").")
} else {
  message("Frakcja pokryć: ", coverage_fraction, 
          ", nie spełnia założonego poziomu ufności (", conf_level, ").")
}

# II sposób:

n <- 1e4
m <- 10

# Funkcja losuje próbkę rozmiaru 'm' z rozkładu normalnego N(μ, σ^2),
# a następnie wyznacza przedział ufności na poziomie 'conf_level' zgodnie z
# modelem II z wykładów. Wartością zwracaną jest wartość logiczna, określająca
# czy μ należy do przedziału ufności:

generate_sample_and_check_coverage <- function(mi, sigma, m, conf_level) {
  sample <- rnorm(m, mi, sigma)
  conf_interval <- t.test(sample, conf.level = conf_level)$conf.int
  return (conf_interval[1] <= mi & mi <= conf_interval[2])
}

# Wykorzystujemy funkcję replicate() do wygenerowania wektora logicznego:

coverages <- replicate(n, generate_sample_and_check_coverage(0, 1, m, 0.95))
coverage_sum <- sum(coverages)
coverage_fraction <- coverage_sum / n

message("Frakcja pokryć dla poziomu ufności 0.95: ",  coverage_fraction)

# zadanie 8 ---------------------------------------------------------------

# Wybieramy wartości parametrów rozkładu normalnego:

mi <- 5
sigma <- 2

# I sposób:

N <- 50
n <- 10
conf_level <- 0.95
conf_intervals <- matrix(NA, nrow = N, ncol = 2)
contains_mi <- vector(mode = "logical", length = N)

for(i in 1:N) {
  
  # Generujemy próbę:
  sample_data <- rnorm(n, mean = mi, sd = sigma)
  
  # Znajdujemy i zapamiętujemy przedział ufności:
  conf_interval <- t.test(sample_data, conf.level = conf_level)$conf.int
  conf_intervals[i,] <- conf_interval
  
  # Sprawdzamy, czy przedział ufności zawiera wartość oczekiwaną:
  contains_mi[i] <- mi >= conf_interval[1] & mi <= conf_interval[2]
}

how_many_contains <- sum(contains_mi)

# Tworzenie wykresu przedziałów ufności:

plot(NULL, 
     xlim = c(1, N), 
     ylim = c(mi - 4*sigma, mi + 4*sigma), # Zakres wybrany na podstawie 
     xlab = "Próbki",                      # reguły 3-σ.
     ylab = "Przedział ufności dla µ", 
     main = "Przedziały ufności dla µ")

# Dodawanie przedziałów ufności do wykresu:

for (i in 1:N) {
  lines(c(i, i), 
        conf_intervals[i, ], 
        col = ifelse(contains_mi[i], "green", "red"))
}

message("Powinno zawierać µ: ≥ ", N * conf_level, 
        ", a faktycznie zawiera: ", how_many_contains)

# II sposób:

N <- 50
n <- 10
conf_level <- 0.95

# Funkcja generuje próbkę rozmiaru 'n' z rozkładu normalnego N(μ, σ^2),
# a następnie zwraca przedział ufności dla µ na poziomie 'conf_level', 
# wyznaczony zgodnie z modelem II z wykładów:

generate_conf_interval <- function(n, mi, sigma, conf_level){
  sample <- rnorm(n, mi, sigma)
  return (t.test(sample, conf.level = conf_level)$conf.int)
}

# Wykorzystujemy funkcję 'replicate' do wygenerowania macierzy 
# przedziałów ufności:

conf_intervals <- replicate(N, generate_conf_interval(n, mi, sigma, conf_level))
contains_mi <- conf_intervals[1,] <= mi & mi <= conf_intervals[2,]
contains_sum <- sum(contains_mi)

# Tworzymy podstawę do wykresu:

plot(NULL, 
     xlim = c(1, N), 
     ylim = c(mi - 3 * sigma, mi + 3 * sigma), # Zakres z reguły 3-σ.
     xlab = "Próbki",                          
     ylab = "Przedział ufności dla µ", 
     main = "Przedziały ufności dla µ")

# Do rysowania pionowych przedziałów ufności wybieramy funkcję 'segments',
# która rysuje odcinki pomiędzy podanymi parami punktów:

segments(1:N, conf_intervals[1,], 1:N, conf_intervals[2,], 
         col = ifelse(contains_mi, "green", "red"))

message("Powinno zawierać µ około ", N * conf_level, 
        ", a faktycznie zawiera: ", contains_sum)