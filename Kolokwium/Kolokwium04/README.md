**Uwaga**:
W zadaniach dotyczących testowania hipotez należy:

- Opisać przyjęty model statystyczny.
- Podać postać testowanych hipotez: hipotezy zerowej i alternatywnej.
- Podać nazwę zastosowanego testu statystycznego.
- Podać wartość otrzymanej statystyki testowej.
- Podać p-value lub postać zbioru krytycznego.
- Uzasadnić decyzję o odrzuceniu lub braku podstaw do odrzucenia weryfikowanej hipotezy zerowej.

### zadanie 01:

Niech $X_1, \dots, X_n$ oznacza próbkę prostą z rozkładu normalnego z nieznanymi parametrami.

- Przypuśćmy, że zaobserwowano 6 następujących wartości tej próbki: 3.75, 4.52, -3.88, 6.85, 8.15, 6.15. Czy zaobserwowane wartości potwierdzają założenie o normalności rozkładu próbki? Zweryfikować hipotezę na poziomie istotności α = 0.05.
- Zakładając, że można przyjąć założenie o normalności rozkładu próbki wyznaczyć wartość estymatora największej wiarygodności parametru σ na podstawie zaobserwowanych wartości z poprzedniego podpunktu.
- Wyznaczyć przedział ufności dla σ na podstawie zaobserwowanych wartości z pierwszego podpunktu zadania. Przyjąć poziom ufności 0.95.

### zadanie 02:

Zbiór _faithful_ zawiera dane doyczące czasu trwania erupcji (zmienna `eruptions`) i czasu oczekiwania na kolejną erupcję (zmienna `waiting`) gejzera _Old Faithful_ w parku _Yellowstone_. Czasy te mierzone są w minutach.

Interesują nas tylko te erupcje gejzera, po których wystąpieniu czas czekania na kolejną był mniejszy niż 70 minut.

- Podać wartość średnią, odchylenie standardowe, medianą oraz górny kwartyl czasu trwania takich erupcji.
- Narysować wykres skrzynkowy dla czasu trwania takich erupcji i na jego podstawie ocenić, czy w analizowanycm zbiorze występują obserwacje odstające (jeżeli występują, należy podać ile ich jest).

### zadanie 03:

Zbiów **Orange** zawiera dane dotyczące obwódu pnia (zmienna `circumference`) drzewek pomarańczowych, zmierzonego w mm. Zakładamy, że obwód pnia ma rozkład normalny.

- Czy na poziomie istotności α = 0.05 można twierdzić, że wartość średnia obwodu pnia jest mniejsza niż 100 mm? Zweryfikować odpowiednią hipotezę statystyczną.
- Wyznaczyć moc testu do weryfikacji hipotezy w poprzednim podpunkcie, zakładając, że prawdziwa wartość średnia obwodu pnia wynosi 90 mm.
- Czy na poziomie istotności α = 0.05 można twierdzić, że odchylenie standardowe obwodu pnia różni się istotnie od 40 mm? Zweryfikować odpowiednią hipotezę statystyczną.

### zadanie 04:

Zbiór **Pima.te** z biblioteki **MASS** zawiera dane dotyczące Indianek z plemienia Pima. Interesuje nas zmienna `age` (wiek, mierzony w latach) oraz `glu` (wynik testu glukozowego).

Niech X oznacza wynik testu glukozowego Indianek, mających więcej niż 40 lat, zaś Y wynik testu glukozowego Indianek mających co najwyżej 40 lat.

Zakładamy, że X i Y są normalne z tą samą wariancją.

- Czy można twierdzić, że wartość średnia X jest istotnie większa niż wartość średnia Y? Przyjąć poziom istotności testu α = 0.05.

### zadanie 05:

Przed otwarciem nowej lodziarni przeprowadzono sondaż na losowej grupie osób dotyczący ulubionych smaków lodów. Każdy badany miał wskazać jeden z 5 smaków znajdujących się na liście. Wyniki sondażu przedstawione są w tabelce:

| ulubione lody | liczba osób |
| ------------- | ----------- |
| waniliowe     | 234         |
| czekoladowe   | 290         |
| truskawkowe   | 76          |
| jagodowe      | 73          |
| orzechowe     | 69          |

Czy na podstawie powyższych danych można twierdzić, że rozkład preferencyjny jest następujący?:

| ulubione lody | prawdopodobieństwo |
| ------------- | ------------------ |
| waniliowe     | 0.3                |
| czekoladowe   | 0.4                |
| truskawkowe   | 0.1                |
| jagodowe      | 0.1                |
| orzechowe     | 0.1                |

Zweryfikować odpowiednią hipotezę na poziomie istotności α = 0.05.
