library(GreedyMiner)

wynik <- benchmark_miner(repetitions = 20, n = 300, permutations = 999, seed = 42)
write.csv(wynik, "benchmark.csv", row.names = FALSE)
print(aggregate(cbind(false_discovery_proportion, elapsed_seconds) ~ scenario,
  data = wynik, FUN = mean))
