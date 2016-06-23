Spacer po torusie

Wprowadzenie

Wszystkie liczby w treści tego zadania są całkowite. Resztą z dzielenia liczby a przez dodatnią, czyli większą od zera, liczbę b nazywamy wartość mod(a, b), spełniającą warunki 0 ≤ mod(a, b) < b oraz a = q b + mod(a, b) dla pewnej liczby q.

Dyskretny torus jest wielowymiarowym uogólnieniem dwukierunkowej listy cyklicznej.

Dla niepustego ciągu dodatnich liczb s ≡ (n1, … nk), torus o kształcie s jest wymiaru k. Elementami tego torusa są punkty, jednoznacznie identyfikowane przez współrzędne (i1, … ik) takie, że 0 ≤ ij < nj dla każdego indeksu 1 ≤ j ≤ k.

Torus nie ma punktów brzegowych. Każdy punkt, po każdej współrzędnej, ma określonego poprzednika i następnika. W torusie o kształcie (n1, … nk) następnikiem punktu o współrzędnych (i1, … ik) po j-tej współrzędnej jest punkt o współrzędnych (i1, … ij - 1, mod(ij + 1, nj), ij + 1, … ik) a poprzednikiem punkt o współrzędnych (i1, … ij - 1, mod(ij - 1, nj), ij + 1, … ik).

Kierunkiem w torusie wymiaru k nazywamy liczbę o wartości bezwzględnej nie przekraczającej k. Kolejnym punktem w kierunku i po punkcie p jest następnik p po i-tej współrzędnej, jeśli i jest dodatnie, poprzednik p po abs(i)-tej współrzędnej, jeśli i jest ujemne oraz punkt p, jeśli i jest równe 0.

Spacer po torusie to, być może nieskończona, kolekcja jego punktów z określoną kolejnością. Kolejne punkty spaceru nie muszą sąsiadować ze sobą w torusie. Spacer może być pusty.

W torusie, traktowanym jak struktura danych, punkty przechowują wartości.

Za reprezentację torusa może służyć dowolny jego punkt.

Polecenie

Zdefiniuj klasę Torus, spełniającą poniższe warunki:

Klasa Torus reaguje na komunikat #shape:, z argumentem s będącym niepustą uporządkowaną kolekcją dodatnich liczb, tworząc nowy torus kształtu s i dając jeden z jego punktów.

Punkt torusa, w odpowiedzi na komunikat #value, daje przechowywaną przez siebie wartość. Początkową wartością przechowywaną przez punkt jest nil.

Komunikat #value:, wysłany do punktu torusa, powoduje zapisanie w nim, jako wartości, obiektu będącego argumentem komunikatu.

Metoda #printOn: punktu torusa wypisuje na strumień, który dostaje jako argument, wartość przechowywaną w tym punkcie.

Punkt p torusa, w odpowiedzi na komunikat #+ z argumentem j, daje punkt po p w kierunku j.

Na komunikat #- z argumentem j punkt torusa reaguje tak, jak na komunikat #+ z argumentem j negated.

Wysłanie komunikatu #@ do punktu p torusa wymiaru k, z argumentem będącym uporządkowaną kolekcją liczb v o długości k, daje punkt odległy od p o wektor v. W torusie o kształcie (n1, … nk), dla punktu o współrzędnych (i1, … ik) i wektora (d1, … dk) jest to punkt o współrzędnych (mod(i1 + d1, n1), … mod(ik + dk, nk)).

Odpowiedzią na komunikat #| wysłany do punktu p torusa, z argumentem będącym kierunkiem j, jest spacer po wszystkich punktach tego torusa, które mają współrzędne o indeksie innym niż abs(j) takie, jak punkt p. Punkty w tym spacerze nie powtarzają się. Pierwszym punktem spaceru jest p a każdy następny jest kolejnym punktem, po swoim poprzedniku, w kierunku j. Zwracamy uwagę, że dla j = 0, spacer p | j jest jednoelementowy.

Dla nieujemnej liczby i oraz kierunku j, do punktu p torusa można wysłać komunikat #% z argumentem i -> j, będącym obiektem standardowej, Smalltalkowej klasy Association. Jako wartość wyrażenia p % (i -> j) dostajemy i-elementowy spacer po torusie zaczynający się od punktu p, w którym każdy następny punkt jest kolejnym, po swoim poprzedniku, w kierunku j. Uwaga - w tak utworzonym spacerze punkty mogą się powtarzać.

Metoda #do: spaceru s odwiedza punkty torusa w kolejności określonej przez s. Odwiedzając punkt p, wykonuje ona blok, który jest argumentem #do:, przekazując mu jako argument cały punkt p, a nie przechowywaną przez niego wartość.

Spacer s, w odpowiedzi na komunikat #, (przecinek) z argumentem t będącym spacerem, daje sklejenie (konkatenację) spacerów s i t. Punkty spaceru s poprzedzają w niej punkty spaceru t a kolejność punktów w ramach obu sklejanych spacerów jest zachowana. Jeśli spacer s jest nieskończony, odwiedzając punkty w kolejności wyznaczonej przez spacer s , t, do punktów spaceru t nigdy nie dojdziemy.

Spacer s rozumie komunikat #| z argumentem x. W odpowiedzi daje sklejenie, z zachowaniem kolejności, wszystkich spacerów będących wynikiem wysłania komunikatu #| z argumentem x do punktów spaceru s.

Analogicznie, spacer s w odpowiedzi na komunikat #% z argumentem x daje sklejenie, z zachowaniem kolejności, wszystkich spacerów będących wynikiem wysłania komunikatu #% z argumentem x do punktów spaceru s.

Punkt p torusa, w odpowiedzi na komunikat #& z jednoargumentowym blokiem b, daje spacer s. Pierwszym punktem spaceru s jest p. Podczas odwiedzania punktów spaceru s metodą #do:, po odwiedzeniu p obliczana jest wartość w równa b value: p. Jeśli w to nil, spacer się kończy. W przeciwnym przypadku w jest spacerem, którego punkty mają być odwiedzone w następnej kolejności, po punkcie p. Zwracamy uwagę, że metoda ta umożliwia zbudowanie nieskończonego spaceru.

Spacer rozumie wszystkie komunikaty odpowiadające metodom klasy Collection. Jego klasowe metody #new i #new: oraz obiektowe metody #add: i #remove:ifAbsent: są zablokowane za pomocą #shouldNotImplement a obiektowa metoda #species daje jako wynik klasę OrderedCollection.
Przykłady

Jeden z przykładów korzysta z pomocniczej metody spaceru:

first: anInteger

    | answer i |
    answer := OrderedCollection new.
    anInteger > 0 ifFalse: [^answer].
    i := anInteger.
    self do:
        [:each |
        answer add: each.
        i := i - 1.
        i = 0 ifTrue: [^answer]].
    ^answer

Każdy z poniższych fragmentów kodu, obliczony w Workspace, powinien dać wartość true.

t := Torus shape: #(21).
i := 1.
t % (10 -> 1) do: [:p | p value: i. i := i + 1].
i := -1.
(t - 1) % (10 -> -1) do: [:p | p value: i. i := i - 1].
(t | 1 collect: [:p | p value]) asArray = #(1 2 3 4 5 6 7 8 9 10 nil -10 -9 -8 -7 -6 -5 -4 -3 -2 -1)

t := Torus shape: #(3 5).
i := 1.
t | 1 | 2 do: [:p | p value: i. i := i + 1].
(t | 2 | 1 collect: [:p | p value]) asArray = #(1 6 11 2 7 12 3 8 13 4 9 14 5 10 15)

t := Torus shape: #(7 5).
t | 1 | 2 do: [:p | p value: 0].
i := 1.
t @ #(4 -3) % (4 -> 1) % (2 -> -2) do: [:p | p value: i. i := i + 1].
w := t | 1 collect: [:p | String withAll: (p | 2 collect: [:q | Character digitValue: q value])].
w asArray = #('08700' '00000' '00000' '00000' '02100' '04300' '06500')

t := Torus shape: #(3 2 5).
w := (OrderedCollection new)
    add: t | 1;
    add: t | 2;
    add: t | 3;
    add: t | 1 | 2;
    add: t | 1 | 3;
    add: t | 2 | 3;
    add: t | 1 | 2 | 3;
    yourself.
(w collect: [:c | c size]) asArray = #(3 2 5 6 15 10 30)

t := Torus shape: #(5 3).
1 to: 5 do: 
    [:k |
    t value: k.
    t + 2 value: k * 10.
    t + 2 + 2 value: k * 100.
    t := t + 1].
(((t + 1) % (2 -> 1) , (t - 2 | 0)) % (4 -> -2) collect: [:p | p value]) asArray = #(2 200 20 2 3 300 30 3 100 10 1 100)

t := Torus shape: #(5).
1 to: 5 do: [:k | t value: k. t := t + 1].
b := nil.
b := [:p | p - 1 - 1 & b].
(((b value: t + 1 + 1 + 1) % (2 -> 1) first: 13) collect: [:p | p value]) asArray = #(2 3 5 1 3 4 1 2 4 5 2 3 5)

t := Torus shape: #(5).
1 to: 5 do: [:k | t value: k. t := t + 1].
b := nil.
b := [:p | p value = 2 ifFalse: [p + 1 + 1 & b]].
((b value: t - 1 - 1) collect: [:p | p value]) asArray = #(1 3 5 2)

((((Torus shape: #(1000)) | 1 | 1 | 1 | 1 | 1) anyOne) value: 11; value) = 11

Wskazówki

Nie trzeba sprawdzać poprawności argumentów komunikatu.

W komunikatach o błędzie, tekstowa reprezentacja obiektu jest budowana za pomocą #printOn:. Metoda #printOn: w klasie Collection korzysta z #do:. Ewentualny błąd w metodzie #do: może więc spowodować kaskadę kolejnych błędów. By tego uniknąć, warto na czas pracy nad programem tymczasowo przedefiniować metodę #printOn: spaceru tak, by nie korzystała z #do:.

Uwagi

Rozwiązanie powinno mieć formę pakietu (pliku .pac) implementacji Dolphin Smalltalk 7. Oprócz definicji klasy Torus, w pakiecie należy umieścić wszystkie potrzebne niestandardowe klasy i metody.