library(GreedyMiner)

wynik <- mine(iris, control = gm_control(permutations = 199, bootstrap = 30), seed = 42)
associations(wynik)
predictability(wynik)
interactions(wynik)
rules(wynik)
diagnostics(wynik)
write_results(wynik, "wyniki")
