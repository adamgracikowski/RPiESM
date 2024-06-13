**Uwaga**:
W zadaniach dotyczących testowania hipotez należy:

- Opisać przyjęty model statystyczny.
- Podać postać testowanych hipotez: hipotezy zerowej i alternatywnej.
- Podać nazwę zastosowanego testu statystycznego.
- Podać wartość otrzymanej statystyki testowej.
- Podać p-value lub postać zbioru krytycznego.
- Uzasadnić decyzję o odrzuceniu lub braku podstaw do odrzucenia weryfikowanej hipotezy zerowej.

### zadanie 01:

Ustawić ziarno generatora na 123.
Wylosować 25 obserwacji z rozkładu wykładniczego z parametrem λ = 6. Następnie udać, że się nie zna wartości parametru λ i wyznaczyć:

- Wartość estymatora największej wiarygodności parametru λ.
- Wartość estymatora największej wiarygodności kwantyla rzędu 0.9 rozkładu wykładniczego Exp(λ).

### zadanie 02:

Zbiór **crabs** z biblioteki **MASS** zawiera dane dotyczące krabów dwóch gatunków. Interesuje nas zmienna `CW`, czyli szerokość pancerza.

- Podać wartość średnią, wariancję, medianę i kwantyl rzędu 0.8 szerokości pancerza badanych krabów.
- Narysować histogram częstości dla badanej próbki. Podać środki przedziałów klasowych i wysokość pierwszych trzech słupków.
- Zweryfikować odpowiednim testem hipotezę, że szerokość pancerza krabów ma rozkład normalny. Przyjąć poziom istotności 0.05.
- Utworzyć przedział ufności dla odchylenia standardowego szerokości pancerza. Założyć, że interesująca nas zmienna ma rozkład normalny. Przyjąć poziom ufności 0.95.

### zadanie 03:

W losowej próbie 500 telewidzów znalazły się 24 osoby, które zadeklarowały, że oglądają obrady komisji śledczych.

- Na podstawie powyższych danych zweryfikować hipotezę, że transmisje z obrad mają oglądalność przekraczającą 4%. Przyjąć poziom istotności α = 0.05.
- Utworzyć 99% przedział ufności dla odsetka osób oglądających obrady komisji śledczych.

### zadanie 04:

Obserwyjąc awarię sieci wodno-kanalizacyjnej w ciągu 100 dni w pewnym mieście, otrzymano dane:

| dzienna liczba awarii | liczba dni |
| --------------------- | ---------- |
| 0                     | 22         |
| 1                     | 30         |
| 2                     | 22         |
| 3                     | 16         |
| 4                     | 10         |

Czy na podstawie powyższych danych można twierdzić, że liczba awarii ma rozkład Poissona z parametrem λ = 2? Zweryfikować odpowiednią hipotezę na poziomie istotności α = 0.05.

### zadanie 05:

Zbiór danych **anorexia** z biblioteki **MASS** zawiera dane pacjentek zmagających się z tym zaburzeniem odżywiania. Interesują nas osoby, wobec których zastosowano terapię poznawczo-begawioralną (oznaczaną jako `CBT` w zmiennej `Treat`). W zmiennej `Prewt` podano wagę pacjentek w funtach przed podjęciem leczenia, a w zmiennej `Postwt` wagę tych samych pacjentek po ustalonym okresie leczenia. Zakładamy, że łączny rozkład wagi przed i po terapii jest rozkładem normalnym.

- Czy na podstawie tych danych można twierdzić, że średnia waga pacjentek poddanych terapii **CBT** uległa zwiększeniu? Zweryfikować odpowiednią hipotezę na poziomie istotności α = 0.05.
- Zakładając, że średnia waga pacjentek po terapii jest wyższa o 3 funty, wyznaczyć prawdopodobieństwo, że przeprowadzony w poprzednim podpunkcie test da błędną odpowiedź.
