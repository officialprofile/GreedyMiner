# Stan kontroli — 2026-09-15

R i Rscript nie były dostępne w środowisku wykonania.

| Kontrola | Stan |
|---|---|
| Statyczny przegląd 23 plików R | Bez wykrytych błędów w zastosowanym narzędziu |
| Definicje 22 eksportów i odwołania do funkcji wewnętrznych | Zgodne |
| Pokrycie eksportów dokumentacją, 10 plików Rd | Zgodne |
| Składnia YAML i macierz CI: Linux, Windows, macOS | Sprawdzone statycznie |
| Niezależne sprawdzenie wzorów w Pythonie | Zaliczone |
| 41 grup testów w tests/regression.R | Przygotowane, nieuruchomione |
| Instalacja pakietu i R CMD check | Nieuruchomione |
| GitHub Actions | Konfiguracja dołączona, nieuruchomiona |

Kontrola składni używała uproszczonej gramatyki Lark, nie parsera R. Nie zastępuje uruchomienia pakietu.

Niezależny kod w Pythonie sprawdził distance correlation, wzór dHSIC, zerowe zależności marginalne XOR, sygnał łączny XOR, bazy tensorowe XOR/XOR3 oraz zgodność pierwotnego i dualnego rozwiązania ridge. Nie wykonywał kodu R ani opcjonalnych silników.

Archiwum źródłowe utworzono bez R CMD build. Przed użyciem wyników należy wykonać instalację, testy i R CMD check w środowisku R. Dołączony workflow robi to po uruchomieniu w GitHub Actions.
