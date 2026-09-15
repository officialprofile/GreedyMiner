# GreedyMiner 0.2.0

R package for exploratory data analysis. GreedyMiner creates a report with a set of all statistically significant relationships between variables.

```r
install.packages("GreedyMiner_0.2.0.tar.gz", repos = NULL, type = "source")
```

### Example


```r
library(GreedyMiner)

wynik <- mine(dane, seed = 42)

associations(wynik)
predictability(wynik)
conditional_associations(wynik)
interactions(wynik)
rules(wynik)
joint_dependencies(wynik)
discoveries(wynik)
diagnostics(wynik)

report(wynik, "raport.html")
write_results(wynik, "wyniki")
```

```r
wynik <- mine(dane, layers = "pairwise", seed = 42)
```

```r
wynik$tests
coverage(wynik)
```
