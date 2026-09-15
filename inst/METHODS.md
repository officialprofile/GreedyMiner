# Metody i założenia

## Wnioskowanie

`discovery_confirmation` wybiera reguły, zbiory kontrolne i parametry modeli na treningu. Testy potwierdzające używają odłożonych obserwacji. Skan par nie wybiera najniższego p spośród metod: każda metoda jest osobną hipotezą. Domyślna korekta obejmuje wszystkie zaplanowane rekordy etapu, również pomocnicze testy; brakujące wyniki liczą się do rozmiaru rodziny. `p_adjust_scope = "layer"` kontroluje osobne rodziny warstw, nie ich unię.

W trybie `exploratory` testy par i dHSIC korzystają z całych danych. Reguły i modele nadal mają odłożoną ocenę. Sam wybór zależności do pokazania nie stanowi niezależnej replikacji. Nie należy wielokrotnie dostrajać analizy po obejrzeniu części potwierdzającej.

BH wymaga odpowiednich założeń o zależności testów. BY jest zachowawcza wobec dowolnej zależności poprawnych wartości p. Żadna korekta nie gwarantuje kontroli błędu dla źle skalibrowanych testów.

## Statystyki

Distance correlation używa podwójnie centrowanych macierzy odległości i estymatora V. Dla kategorii nominalnych odległość wynosi 0 lub 1; porządkowe są kodowane rangą poziomu. Populacyjna charakterystyka niezależności wymaga odpowiednich warunków momentowych i metryki. HSIC używa jądra gaussowskiego z medianowym pasmem dla liczb oraz jądra równości dla kategorii. Raportowany HSIC jest normalizowany; dHSIC nie jest.

Permutacyjne p wynosi `(1 + liczba_statystyk_nie_mniejszych) / (B + 1)`. Permutowane są całe indeksy macierzy. dHSIC permutuje zmienne niezależnie, pozostawiając pierwszą. Odrzucenie łącznej niezależności nie stanowi dowodu interakcji z konkretnym celem.

Bootstrap przedziałów jest percentylowy. Dla danych blokowych losowane są całe bloki. Dla czasu nie ma automatycznego bootstrapu. Przedziały nieliniowych miar zależności, szczególnie blisko niezależności, nie mają tu gwarancji dokładnego pokrycia; pasmo jądra jest utrzymywane stałe. Istotność rozstrzyga test, nie samo położenie przedziału.

Welch bada średnie, Kruskal–Wallis rozkłady rang, a test niezależności pełniejszy rodzaj związku. Eta-kwadrat przy wielu grupach jest opisową miarą udziału zróżnicowania, nie efektem specyficznym dla statystyki Welcha. V Craméra zawiera korektę obciążenia małej próby. Kierunek kontrastu i tablice liczebności są w `details`.

## Modele

Siatka parametrów jest stała, wybierana wyłącznie w walidacji wewnętrznej. Recepta imputacji, kodowania, standaryzacji i baz spline jest tworzona osobno w każdym treningowym foldzie. Ostateczny model jest dopasowany do części odkrywczej i dopiero potem oceniany na potwierdzającej. Metryki: MSE i Brier. Nieznana klasa zmiennej objaśnianej w walidacji daje pominięcie zadania, a nie ukryte usunięcie wierszy.

`heldout_gain = 1 - mean(loss_model) / mean(loss_reference)`. Dla bloków średnie nadają jednakowe wagi blokom. Ujemne wartości pozostają w wyniku. Przy zerowej stracie odniesienia gain jest nieokreślony. Przedziały gain powstają przez bootstrap jednostek oceny. Wartość p pochodzi z przybliżonego jednostronnego testu t sparowanych strat, warunkowo względem zamrożonego treningu. Nie uwzględnia całej zmienności ponownego treningu. Stała różnica strat lub zbyt mało jednostek nie daje automatycznej wartości p.

Interakcje porównują model addytywny z modelem zawierającym iloczyny baz do zadanego rzędu. To hipoteza względna wobec bazy, regularyzacji i metryki. Dobór kombinacji nie wymaga sygnału pojedynczych predyktorów. Limit projektu wynosi 5000 kolumn.

Korelacja cząstkowa usuwa liniową projekcję na zbiór kontrolny. Test t wymaga odpowiednich założeń Gaussa i niezależności obserwacji. GCM używa iloczynu reszt dwóch modeli nuisance, ocenionych na odłożonych wierszach. Wymaga warunków momentowych i odpowiednio szybkiej estymacji modeli nuisance; pakiet ich nie sprawdza. Zerowanie tego momentu nie jest ogólną równoważnością niezależności warunkowej. Przyrost predykcyjny jest odrębną hipotezą zależną od modelu.

## Braki i struktura

Analiza kompletnych par nie jest uniwersalnym rozwiązaniem problemu brakowania. Wynik może być obciążony przy niewłaściwym mechanizmie braków. Modele stosują treningową medianę oraz jawne wskaźniki braków/nieznanych kategorii. GAM odwzorowuje nieznane kategorie na treningową dominantę; ta konwencja nie dodaje wiedzy o nowej kategorii.

Automatyczny tryb blokowy traktuje bloki jako niezależne jednostki. Permutacja całych bloków wymaga jednakowych długości i wymienialności bloków. `within_block` dotyczy wymienialności wewnątrz bloków, a nie bezwarunkowej niezależności. `circular` wymaga właściwej niezmienniczości na przesunięcie i założeń stacjonarności. Żaden tryb nie jest automatycznym testem przyczynowości.

## Stabilność i koszt

Stabilność jest częstością przekroczenia ustalonego progu efektu dla wspieranych miar przy podpróbkowaniu części odkrywczej. Wynik odnosi się do zestawu zmiennych i metody, nie do identycznej definicji reguły. Część miar o nieporównywalnej skali jest pomijana. Nie jest to formalne stability selection. Mianowniki `attempted` i `evaluated` pozwalają odróżnić brak wyboru od nieudanej oceny.

Macierze odległości i jąder są budowane raz na zadanie i ponownie używane w permutacjach. Koszt takiego testu wynosi O(B n²), pamięć O(n²). Pakiet ogranicza n dla tej warstwy; nie używa szybkiego algorytmu O(n log n) z `energy`. Budżety ograniczają wyszukiwanie kombinacji. Wznowienie odczytuje wyniki zakończonych zadań; nie wznawia pojedynczej permutacji lub dopasowania przerwanego w połowie. Na Windows wykonywanie jest szeregowe.

## Źródła

- R, `cor.test`, `t.test`, `oneway.test`, `fisher.test`, `chisq.test`, `p.adjust`: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html
- Székely, Rizzo, Bakirov (2007), *Measuring and testing dependence by correlation of distances*. DOI: 10.1214/009053607000000505.
- Gretton et al. (2008), *A Kernel Statistical Test of Independence*. https://papers.nips.cc/paper/3201-a-kernel-statistical-test-of-independence
- Pfister et al. (2018), *Kernel-based tests for joint independence*. DOI: 10.1111/rssb.12235.
- Shah, Peters (2020), *The hardness of conditional independence testing and the generalised covariance measure*. https://arxiv.org/abs/1804.07203
- Cawley, Talbot (2010), *On Over-fitting in Model Selection and Subsequent Selection Bias in Performance Evaluation*. https://www.jmlr.org/papers/v11/cawley10a.html
- Phipson, Smyth (2010), *Permutation P-values Should Never Be Zero*. DOI: 10.2202/1544-6115.1585.
- Dokumentacje silników: https://glmnet.stanford.edu/articles/glmnet.html ; https://cran.r-universe.dev/ranger/doc/manual.html ; https://cran.r-universe.dev/partykit/doc/manual.html ; https://cran.r-universe.dev/arules/doc/manual.html
- Bazy interakcyjne GAM: https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/te.html
