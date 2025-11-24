library(dplyr)
library(stringr)
library(tibble)
library(here)
library(purrr)
library(ggplot2)
library(grid)

base_dir <- here::here("phases/3")

cpu_machine_name <- "draco2-cpu"
gpu_machine_name <- "draco3-gpu"

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
        filter(!is.na(file_path_relative)) |>
        mutate(machine = str_extract(file_path_relative, paste0("^", cpu_machine_name,
            "|", gpu_machine_name)), device = as.factor(ifelse(machine == cpu_machine_name,
            "cpu", "gpu")), cpu_match = str_match(file_path_relative, paste0(cpu_machine_name,
            "/([^/]+)/([^/]+)/([^/]+)/\\.([^/]+)\\.out$")), gpu_match = str_match(file_path_relative,
            paste0(gpu_machine_name, "/([^/]+)/([^/]+)/\\.([^/]+)\\.out$"))) |>
        mutate(problem_size = as.integer(coalesce(cpu_match[, 2], gpu_match[, 2])),
            num_iterations = as.integer(coalesce(cpu_match[, 3], gpu_match[, 3])),
            num_threads = as.integer(cpu_match[, 4]), replication_index = as.integer(coalesce(cpu_match[,
                5], gpu_match[, 4]))) |>
        mutate(log_content = map(full_file_path, read_experiment_file), extracted_metrics = map(log_content,
            extract_metrics)) |>
        select(-cpu_match, -gpu_match, -log_content) |>
        tidyr::unnest(extracted_metrics) |>
        select(device, machine, problem_size, num_iterations, num_threads, replication_index,
            metric_name, value) |>
        mutate(metric_name = as.factor(metric_name))

    return(all_paths_tbl)
}

experiment_results <- process_experiment_data()

experiment_results_clean <- experiment_results |>
    filter(!is.na(value))
experiment_results_clean <- experiment_results_clean |>
    filter(is.na(num_threads) | num_threads != 32)

cpu_data <- experiment_results_clean |>
    filter(device == "cpu", metric_name == "computation_time_s")

cpu_baseline <- cpu_data |>
    filter(num_threads == 1) |>
    select(problem_size, num_iterations, baseline_time = value)

cpu_speedup <- cpu_data |>
    filter(num_threads > 1) |>
    left_join(cpu_baseline, by = c("problem_size", "num_iterations")) |>
    mutate(speedup = baseline_time/value)

gpu_data <- experiment_results_clean |>
    filter(device == "gpu", metric_name == "computation_time_s")

if (nrow(gpu_data) > 0 && nrow(cpu_baseline) > 0) {
    gpu_speedup <- gpu_data |>
        left_join(cpu_baseline, by = c("problem_size", "num_iterations")) |>
        mutate(speedup = baseline_time/value)
} else {
    gpu_speedup <- tibble()
}

pdf("analysis_plots.pdf", width = 10, height = 8)

p1 <- ggplot(filter(experiment_results_clean, metric_name == "computation_time_s"),
    aes(x = problem_size, y = value, color = as.factor(num_threads))) + geom_point(alpha = 0.8) +
    geom_line(alpha = 0.8) + facet_grid(num_iterations ~ device, scales = "free_y") +
    scale_y_log10() + labs(title = "Computation Time vs Problem Size", x = "Problem Size",
    y = "Computation Time (s, log scale)", color = "Number of Threads") + theme_minimal()
print(p1)

p2 <- ggplot(filter(experiment_results_clean, device == "cpu", metric_name == "msamples_per_sec"),
    aes(x = num_threads, y = value, color = as.factor(problem_size))) + geom_point() +
    geom_line() + facet_wrap(~problem_size, scales = "free_y") + labs(title = "CPU Performance vs Number of Threads",
    x = "Number of Threads", y = "Performance (MSamples/s)", color = "Problem Size") +
    theme_minimal() + theme(legend.position = "none")
print(p2)

if (nrow(cpu_speedup) > 0) {
    p3 <- ggplot(cpu_speedup, aes(x = num_threads, y = speedup, color = as.factor(problem_size))) +
        geom_point() + geom_line() + geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        facet_wrap(~problem_size, scales = "free_y") + labs(title = "CPU Thread Speedup",
        x = "Number of Threads", y = "Speedup Factor", color = "Problem Size") +
        theme_minimal() + theme(legend.position = "none")
    print(p3)
}

if (nrow(gpu_speedup) > 0) {
    p4 <- ggplot(gpu_speedup, aes(x = as.factor(problem_size), y = speedup)) + geom_col(fill = "skyblue") +
        labs(title = "GPU vs Single-Threaded CPU Speedup", x = "Problem Size", y = "Speedup Factor") +
        theme_minimal()
    print(p4)
}

lm_data <- experiment_results_clean |>
    filter(device == "cpu", metric_name == "msamples_per_sec")

if (nrow(lm_data) > 0) {
    model <- lm(value ~ num_threads + problem_size + num_iterations, data = lm_data)

    cat("\n--- Linear Model Summary ---\n")
    print(summary(model))

    grid.newpage()
    grid.text("Linear Model Diagnostics", y = 0.9, gp = gpar(fontsize = 16))

    lm_data$predicted <- predict(model)
    p_lm1 <- ggplot(lm_data, aes(x = value, y = predicted)) + geom_point() + geom_abline(slope = 1,
        intercept = 0, color = "red") + labs(title = "Actual vs Predicted MSamples/s",
        x = "Actual", y = "Predicted") + theme_minimal()
    print(p_lm1)

    p_lm2 <- ggplot(model, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept = 0,
        color = "red") + labs(title = "Residuals vs Fitted", x = "Fitted values",
        y = "Residuals") + theme_minimal()
    print(p_lm2)

    p_lm3 <- ggplot(model, aes(sample = .stdresid)) + stat_qq() + stat_qq_line() +
        labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Standardized Residuals") +
        theme_minimal()
    print(p_lm3)
}

dev.off()

print("Analysis complete. Plots saved to analysis_plots.pdf")

