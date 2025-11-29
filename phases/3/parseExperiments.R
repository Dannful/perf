library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(tibble)
library(here)

read_experiment_file <- function(full_file_path) {
    tryCatch(
        readLines(full_file_path, warn = FALSE),
        error = function(e) {
            warning(sprintf("File read failed: %s | %s", full_file_path, e$message))
            NULL
        }
    )
}

extract_metrics <- function(log_lines) {
    if (is.null(log_lines) || length(log_lines) == 0) {
        return(tibble(metric_name = character(), value = numeric()))
    }

    patterns <- tibble(
        pattern = c(
            "Execution time [(]s[)] is (\\d+\\.?\\d*)",
            "Total execution time [(]s[)] is (\\d+\\.?\\d*)",
            "MSamples/s (\\d+\\.?\\d*)"
        ),
        metric_name = c("computation_time_s", "total_time_s", "msamples_per_sec")
    )

    log_text <- paste(log_lines, collapse = "\n")

    patterns |>
        rowwise() |>
        mutate(value_str = str_extract(log_text, pattern),
               value     = str_extract(value_str, "(\\d+\\.?\\d*)")) |>
        ungroup() |>
        select(metric_name, value) |>
        mutate(value = as.numeric(value))
}

read_cpu_results <- function(folder, machine_name, experiment_number) {
  cat("Looking in:", folder, "\n")
  
  files <- list.files(folder, pattern = "\\.out$", recursive = TRUE, 
                      full.names = TRUE, all.files = TRUE)
  
  cat("Files found:", length(files), "\n")
  
  if (length(files) == 0) {
    return(NULL)
  }
  
  results_list <- lapply(files, function(f) {
    rel_path <- sub(paste0("^", folder, "/?"), "", f)
    parts <- strsplit(rel_path, "/")[[1]]
    
    if (length(parts) != 4) {
      warning(sprintf("Unexpected CPU path structure: %s (expected 4 parts, got %d)", 
                      rel_path, length(parts)))
      return(NULL)
    }
    
    problem_size <- as.numeric(parts[1])
    iterations <- as.numeric(parts[2])
    threads <- as.numeric(parts[3])
    replication <- as.numeric(gsub("^\\.(\\d+)\\.out$", "\\1", parts[4]))
    
    log_lines <- read_experiment_file(f)
    metrics <- extract_metrics(log_lines)
    
    if (nrow(metrics) == 0) {
      return(NULL)
    }
    
    metrics |>
      mutate(
        experiment = experiment_number,
        device = "cpu",
        machine = machine_name,
        problem_size = problem_size,
        num_iterations = iterations,
        num_threads = threads,
        replication_index = replication
      )
  })
  
  do.call(rbind, Filter(Negate(is.null), results_list))
}

read_gpu_results <- function(folder, machine_name, experiment_number) {
  cat("Looking in:", folder, "\n")
  
  files <- list.files(folder, pattern = "\\.out$", recursive = TRUE, 
                      full.names = TRUE, all.files = TRUE)
  
  cat("Files found:", length(files), "\n")
  
  if (length(files) == 0) {
    return(NULL)
  }
  
  results_list <- lapply(files, function(f) {
    rel_path <- sub(paste0("^", folder, "/?"), "", f)
    parts <- strsplit(rel_path, "/")[[1]]
    
    if (length(parts) != 3) {
      warning(sprintf("Unexpected GPU path structure: %s (expected 3 parts, got %d)", 
                      rel_path, length(parts)))
      return(NULL)
    }
    
    problem_size <- as.numeric(parts[1])
    iterations <- as.numeric(parts[2])
    replication <- as.numeric(gsub("^\\.(\\d+)\\.out$", "\\1", parts[3]))
    
    log_lines <- read_experiment_file(f)
    metrics <- extract_metrics(log_lines)
    
    if (nrow(metrics) == 0) {
      return(NULL)
    }
    
    metrics |>
      mutate(
        experiment = experiment_number,
        device = "gpu",
        machine = machine_name,
        problem_size = problem_size,
        num_iterations = iterations,
        num_threads = 1,
        replication_index = replication
      )
  })
  
  do.call(rbind, Filter(Negate(is.null), results_list))
}

base_dir <- here::here("phases/3/experiments")

cat("\n=== Reading Experiment 1 ===\n")
exp1_draco1 <- read_gpu_results(file.path(base_dir, "draco1-gpu"), "draco1", 1)
exp1_draco2 <- read_cpu_results(file.path(base_dir, "draco2-cpu"), "draco2", 1)

cat("\n=== Reading Experiment 2 ===\n")
exp2_draco1 <- read_gpu_results(file.path(base_dir, "exp2_draco1-gpu"), "draco1", 2)
exp2_draco2 <- read_cpu_results(file.path(base_dir, "exp2_draco2-cpu"), "draco2", 2)
exp2_beagle <- read_gpu_results(file.path(base_dir, "beagle"), "beagle", 2) 

all_results <- dplyr::bind_rows(
    exp1_draco1,
    exp1_draco2,
    exp2_draco1,
    exp2_draco2,
    exp2_beagle
) |>
  select(experiment, device, machine, problem_size, num_iterations, 
         num_threads, replication_index, metric_name, value) |>
  arrange(experiment, device, machine, problem_size, num_iterations, 
          num_threads, replication_index, metric_name)

output_file <- file.path(base_dir, "../clean_dataset.csv")
write.csv(all_results, output_file, row.names = FALSE)

cat("\n=== Summary ===\n")
cat("Final dataset has", nrow(all_results), "rows\n")
cat("Columns:", paste(names(all_results), collapse = ", "), "\n")
cat("\nFirst few rows:\n")
print(head(all_results, 10))
cat("\nSaved to:", output_file, "\n")