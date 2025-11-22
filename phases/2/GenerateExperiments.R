library(DoE.base)
library(tidyverse)

generate_experiment <- function(factors, seed, path) {
  replications <- 3
  DoE.base::fac.design(
    nfactors = length(factors),
    replications = replications,
    repeat.only = FALSE,
    blocks = 1,
    randomize = TRUE,
    seed = seed,
    nlevels = sapply(factors, length),
    factor.names = factors
  ) |> readr::write_csv(path)
}

seed <- 0
cpu_factors <- list(
  Size = c(100, 250, 500),
  Iterations = c(30, 300, 3000, 30000),
  Threads = c(1, 2, 4, 8, 16, 32)
)
gpu_factors <- list(
  Size = c(100, 250),
  Iterations = c(30, 300, 3000, 30000, 300000)
)
generate_experiment(cpu_factors, seed, "./cpu.csv")
generate_experiment(gpu_factors, seed, "./gpu.csv")
