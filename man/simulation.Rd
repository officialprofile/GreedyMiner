\name{simulation}
\alias{simulate_dependencies}
\alias{benchmark_miner}
\title{Synthetic dependencies and benchmarks}
\description{Generate known dependency patterns and measure detection, pairwise false discoveries and runtime.}
\usage{
simulate_dependencies(n = 300L, scenario = c("linear", "quadratic", "periodic", "variance",
  "xor", "xor3", "confounded", "independent", "missing", "outliers", "repeated"),
  noise = 0.3, p_noise = 3L, seed = 1L)

benchmark_miner(repetitions = 10L, n = 300L, p_noise = 3L,
  scenarios = c("independent", "linear", "quadratic", "periodic", "variance", "xor", "missing", "outliers"),
  permutations = 199L, seed = 1L)
}
\arguments{
  \item{n}{Simulation sample size.}
  \item{scenario}{Simulation scenario.}
  \item{noise}{Gaussian noise standard deviation where applicable.}
  \item{p_noise}{Number of independent noise columns.}
  \item{seed}{An integer random seed. The caller RNG state is restored.}
  \item{repetitions}{Number of independent simulation repetitions.}
  \item{scenarios}{Simulation scenarios evaluated by the benchmark.}
  \item{permutations}{Fixed number of Monte Carlo permutations. The minimum p-value is 1/(B+1).}
}
\details{Averaging replicate false-discovery proportions estimates FDR in the chosen simulation setting; it is not a general guarantee. Benchmarks are not run automatically on package load.}
\value{Simulation data and truth metadata, or one benchmark row per scenario and repetition.}
