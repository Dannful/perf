library(DoE.base)
library(tidyverse)

generate_experiment <- function(factors, seed, path, replications) {
    DoE.base::fac.design(nfactors = length(factors), replications = replications,
        repeat.only = FALSE, blocks = 1, randomize = TRUE, seed = seed, nlevels = sapply(factors,
            length), factor.names = factors) |>
        readr::write_csv(path)
}

sizes <- c(24, 56, 120)
iterations <- c(10, 100, 1000)
replications <- 100
seed <- 0
cpu_factors <- list(Size = sizes, Iterations = iterations, Threads = c(1, 2, 4, 8, 16))
gpu_factors <- list(Size = sizes, Iterations = iterations)
generate_experiment(cpu_factors, seed, "./cpu.csv", replications = replications)
generate_experiment(gpu_factors, seed, "./gpu.csv", replications = replications)


###################################################################################################

sizes <- c(24, 88, 152, 248, 376, 504)
replications <- 10
cpu_factors <- list(Size = sizes, Threads = c(1, 2, 4, 8, 16))
gpu_factors <- list(Size = sizes)

# fac.design does not support single level factors (as in the gpu experiment)
generate_experiment_gpu_2 <- function(factors, seed, path, replications) {
  set.seed(seed)
  base_design <- expand.grid(factors)
  
  # replications with internal randomization
  final_design <- map_dfr(1:replications, function(r) {
    base_design |>
      slice_sample(prop = 1) |> 
      dplyr::mutate(Blocks = paste0(".", r))
  }) |> 
  readr::write_csv(path)
}

generate_experiment(cpu_factors, seed, "./cpu_2.csv", replications = replications)
generate_experiment_gpu_2(gpu_factors, seed, "./gpu_2.csv", replications = replications)

# Add iterations to match previous format
cpu <- read_csv("./cpu_2.csv", col_types = cols(Blocks = col_character()))|>
  dplyr::mutate(Iterations = 25) |>
  dplyr::select(Size, Iterations, Threads, Blocks)

readr::write_csv(cpu, "./cpu_2.csv")

gpu <- read_csv("./gpu_2.csv", col_types = cols(Blocks = col_character()))|>
  dplyr::mutate(Iterations = 25) |>
  dplyr::select(Size, Iterations, Blocks)

readr::write_csv(gpu, "./gpu_2.csv")
