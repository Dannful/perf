library(dplyr)
library(stringr)
library(tibble)
library(here)
library(purrr)
library(grid)

base_dir <- here::here("phases/3")

cpu_machine_name <- "draco2-cpu"
gpu_machine_name <- "draco1-gpu"

read_experiment_file <- function(full_file_path) {
    file_content <- tryCatch({
        readLines(full_file_path, warn = FALSE)
    }, error = function(e) {
        warning(sprintf("File read failed for: %s | Reason: %s", full_file_path,
            e$message), call. = FALSE)
        return(NULL)
    })

    return(file_content)
}

extract_metrics <- function(log_lines) {
    if (is.null(log_lines) || length(log_lines) == 0) {
        return(tibble(metric_name = character(), value = numeric()))
    }

    patterns <- tibble(pattern = c("Execution time [(]s[)] is (\\d+\\.?\\d*)", "Total execution time [(]s[)] is (\\d+\\.?\\d*)",
        "MSamples/s (\\d+\\.?\\d*)"), metric_name = c("computation_time_s", "total_time_s",
        "msamples_per_sec"))

    log_text <- paste(log_lines, collapse = "\n")

    extracted_data <- patterns |>
        rowwise() |>
        mutate(value_str = str_extract(log_text, pattern), value = str_extract(value_str,
            "(\\d+\\.?\\d*)")) |>
        ungroup() |>
        select(metric_name, value) |>
        mutate(value = as.numeric(value))

    return(extracted_data)
}

process_experiment_data <- function() {
    all_files <- list.files(base_dir, recursive = TRUE, pattern = "\\.out$", full.names = TRUE,
        all.files = TRUE)

    if (length(all_files) == 0) {
        stop(paste("No .out files found in the base directory:", base_dir))
    }

    all_paths_tbl <- tibble(full_file_path = all_files) |>
        mutate(file_path_relative = str_extract(full_file_path, paste0("(", cpu_machine_name,
            "|", gpu_machine_name, ").*"))) |>
        filter(!is.na(file_path_relative))

    cpu_regex <- paste0(cpu_machine_name, "/([^/]+)/([^/]+)/([^/]+)/\\.([^/]+)\\.out$")
    gpu_regex <- paste0(gpu_machine_name, "/([^/]+)/([^/]+)/\\.([^/]+)\\.out$")

    cpu_matches <- str_match(all_paths_tbl$file_path_relative, cpu_regex)
    gpu_matches <- str_match(all_paths_tbl$file_path_relative, gpu_regex)

    all_paths_tbl <- all_paths_tbl |>
        mutate(machine = str_extract(file_path_relative, paste0("^", cpu_machine_name,
            "|", gpu_machine_name)), device = as.factor(ifelse(machine == cpu_machine_name,
            "cpu", "gpu")),
            problem_size = as.integer(coalesce(cpu_matches[, 2], gpu_matches[, 2])),
            num_iterations = as.integer(coalesce(cpu_matches[, 3], gpu_matches[, 3])),
            num_threads = as.integer(ifelse(device == "gpu", 1, cpu_matches[, 4])),
            replication_index = as.integer(coalesce(cpu_matches[, 5], gpu_matches[, 4]))) |>
        mutate(log_content = map(full_file_path, read_experiment_file), extracted_metrics = map(log_content,
            extract_metrics)) |>
        select(-file_path_relative) |>
        tidyr::unnest(extracted_metrics) |>
        select(device, machine, problem_size, num_iterations, num_threads, replication_index,
            metric_name, value) |>
        mutate(metric_name = as.factor(metric_name))

    print(all_paths_tbl)

    return(all_paths_tbl)
}

experiment_results <- process_experiment_data()

experiment_results_clean <- experiment_results |>
    filter(!is.na(value))
experiment_results_clean <- experiment_results_clean |>
    filter(is.na(num_threads) | num_threads != 32)

experiment_results_clean |>
    write.csv("./clean_dataset.csv")
