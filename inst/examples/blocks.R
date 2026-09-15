library(GreedyMiner)

d <- simulate_dependencies(n = 300, scenario = "repeated", p_noise = 0)$data
wynik <- mine(d, id = "id", layers = c("pairwise", "predictive"),
  control = gm_control(learners = "ridge", permutations = 199, bootstrap = 30), seed = 42)
coverage(wynik)
diagnostics(wynik)
