library(GreedyMiner)

d <- simulate_dependencies(n = 400, scenario = "xor", p_noise = 0, seed = 42)$data
wynik <- mine(d, targets = "y", layers = c("pairwise", "interactions", "joint"),
  control = gm_control(permutations = 999, bootstrap = 50), seed = 42)
associations(wynik)
interactions(wynik)
joint_dependencies(wynik)
