# Lab06:

# zadanie 1 ---------------------------------------------------------------

data(iris)
summary(iris)
width <- iris$Sepal.Width
species <- iris$Species

# a) ----------------------------------------------------------------------

# H0: Średnia szerokość działki kielicha dla każdej odmiany irysa jest taka sama.
# H1: Istnieją odmiany irysa o różnej wartości średniej szerokości działki kielicha.

# Zmienna objaśniana: szerokość działki kielicha.
# Zmienna objaśniająca: odmiana irysa, czynnik występuje na trzech poziomach
# "versicolor-setosa", "virginica-setosa", "virginica-versicolor".

alpha <- 0.01

means <- tapply(width, species, mean)
boxplot(width ~ species, main = "Wykresy skrzynkowe")
lines(1:3, means, pch = 20, type = "p", cex = 2, col = "red")

# Czerwone kółka oznaczają średnie grupowe.
# Wykresy skrzynkowe i wartości średnich sugerują, że szerokość działki kielicha
# dla odmiany "setosa" średnio przyjmuje wartości większe od pozostałych odmian.

# b) ----------------------------------------------------------------------

# Sprawdzanie założeń:


# Czy dla każdego poziomu czynnika rozkład zmiennej odpowiedzi jest normalny?: 

# Ponieważ wartość n nie jest mała, rysujemy wykresy kwantylowe dla odpowiedzi 
# w każdej grupie:

par(mfrow = c(1, 3))
tapply(width, species, function(x) { qqnorm(x); qqline(x)})

# Następnie, przeprowadzamy testy normalności dla każdej grupy i
# wyświetlamy p-value, będące rezultatem testów:

tapply(width, 
       species, 
       function(x) { shapiro.test(x)$p.value })

# Otrzymujemy p-value >= alpha dla każdej grupy.

# Wyświetlamy wartość statystyki testowej oraz p-value dla każdej grupy:

simplify2array(tapply(
  width, 
  species, 
  function(x) { shapiro.test(x)[1:2] }))

# Czy wariancje w grupach są przynajmniej w przybliżeniu równe?:

# Ponieważ n >= 10 oraz rozkłady zmiennej odpowiedzi w grupach są normalne, 
# stosujemy test Barletta:

bartlett.test(width, species)

# W przeciwnym przypadku przeprowadzilibyśmy test Levene'a:

library(car)
leveneTest(width, species, center = mean)

# c) ----------------------------------------------------------------------

# Tworzymy model i dokonujemy analizy wariancji:

model <- lm(width ~ species)
summary(model)
anova(model)

pf(49.16, 2, 147, lower.tail = TRUE)

# Przeprowadzamy testy porównań wielokrotnych:

pairwise.t.test(width, species, p.adjust = "bonf")

# Alternatywnie korzystamy z procedury Turkey'a:

(tukey <- TukeyHSD(aov(model)))
plot(tukey)