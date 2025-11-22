library(DoE.base)
library(tidyverse)

generate_experiment <- function(factors, seed, path) {
    replications <- 100
    DoE.base::fac.design(nfactors = length(factors), replications = replications,
        repeat.only = FALSE, blocks = 1, randomize = TRUE, seed = seed, nlevels = sapply(factors,
            length), factor.names = factors) |>
        readr::write_csv(path)
}

sizes <- c(32, 64, 128)
iterations <- c(10, 100, 1000)

seed <- 0
cpu_factors <- list(Size = sizes, Iterations = iterations, Threads = c(1, 2, 4, 8,
    16, 32))
gpu_factors <- list(Size = sizes, Iterations = iterations)
generate_experiment(cpu_factors, seed, "./cpu.csv")
generate_experiment(gpu_factors, seed, "./gpu.csv")
