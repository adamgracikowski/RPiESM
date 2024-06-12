**Uwaga**:
W zadaniach 3, 4, 5 i 6 należy:

- Opisać przyjęty model statystyczny.
- Podać postać testowanych hipotez: hipotezy zerowej i alternatywnej.
- Podać nazwę zastosowanego testu statystycznego.
- Podać wartość otrzymanej statystyki testowej.
- Podać p-value lub postać zbioru krytycznego.
- Uzasadnić decyzję o odrzuceniu lub braku podstaw do odrzucenia weryfikowanej hipotezy zerowej.

### zadanie 01:

Ustawić ziarno generatora na 111.
Wylosować 27 obserwacji z rozkładu wykładniczego z parametrem λ = 1/4. Następnie udać, że się nie zna wartości parametru λ i wyznaczyć:

- Wartość estymatora największej wiarygodności parametru λ.
- Wartość estymatora największej wiarygodności prawdopodobieństwa P(X > 7), gdzie X ~ Exp(λ).

### zadanie 02:

Na 250 losowo wybranych do badania kibiców siatkówki, 97 wytypowało Polaków jako potencjalnych zwycięzców tegorocznej Ligi Narodów. Utworzyć 99% przedział ufności dla odsetka kibiców podzielających ten pogląd.

### zadanie 03:

Zbiór **barley** z biblioteki **lattice** zawiera dane dotyczące upraw jęczmienia w USA. Zmienna `yield` zawiera informację o wysokości plonów, a zmienna `site` o farmie, na której prowadzono pomiary. Interesuje nas wysokość plonów dla farmy _Waseca_. Można założyć, że badana zmienna ma rozkład normalny.
Na poziomie istotności α = 0.05 zweryfikować hipotezę, że średni plon uzyskany na farmie _Waseca_ jest mniejszy niż 50 jednostek.

### zadanie 04:

Zbiór **cats** z biblioteki **MASS** zawiera dane dotyczące kotów. Interesuje nas zmienna `Bwt` (masa ciała, mierzona w kg) oraz zmienna `Sex` (płeć, `F` - kotka, `M` - kocur).
Czy można twierdzić, że wariancja masy ciała kocurów i wariancja masy ciała kotek różnią się istotnie? Założyć, że w obu grupach rozkład badanej cech jest normalny. Przyjąć poziom istotności testu α = 0.05.

### zadanie 05:

W celu sprawdzenia, czy generator liczb losowych z rozkładu t-Studenta `t(30)` działa poprawnie, wygenerowano 5-cio elementową próbę z tego rozkładu i otrzymano następujące wartości: -0.89, -0.13, -0.05, 1.18, 0.25.
Czy można w związku z tym twierdzić, że badany generator działa poprawnie? Zweryfikować odpowiednią hipotezę, przyjmując poziom istotności testu α = 0.05.

### zadanie 06:

Badano czas działania 4 różnych baterii: A, B, C i D. Badanie przeprowadzono na losowej próbie 20 baterii i otrzymano następujące wyniki (czas działania w godzinach):

| A   | B   | C   | D   |
| --- | --- | --- | --- |
| 163 | 87  | 82  | 104 |
| 205 | 106 | 153 | 136 |
| 197 | 101 | 87  | 98  |
| 286 | 94  | 103 | 207 |
| 172 | 123 | 96  | 146 |

Czy na podstawie danych z tabeli można twierdzić, że występują istotne różnice w średniej długości czasu działania baterii A, B, C i D? Zweryfikować odpowiednie hipotezy na poziomie istotności α = 0.05.