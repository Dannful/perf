library(ggplot2)
library(dplyr)
library(grid)
library(purrr)

experiment_results_clean <- read.csv("./clean_dataset.csv")

cpu_data <- experiment_results_clean |>
    filter(device == "cpu", metric_name == "computation_time_s")

cpu_baseline <- cpu_data |>
    filter(num_threads == 1) |>
    group_by(problem_size, num_iterations) |>
    summarise(baseline_time = mean(value), .groups = "drop")

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

experiment_results_clean |>
    write.csv("./clean_dataset.csv")

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


# --- 99% CONFIDENCE INTERVALS FOR GPU ---

compute_ci <- function(x, conf = 0.99) {
    n <- length(x)
    mean_x <- mean(x)
    se <- sd(x)/sqrt(n)
    alpha <- 1 - conf
    tcrit <- qt(1 - alpha/2, df = n - 1)

    tibble(mean = mean_x, lower = mean_x - tcrit * se, upper = mean_x + tcrit * se,
        n = n)
}

# Use all GPU metrics that exist
gpu_times <- experiment_results_clean %>%
    filter(device == "gpu")

# --------- CI WITH ALL REPLICATIONS ---------
gpu_ci_all <- gpu_times %>%
    group_by(metric_name, problem_size, num_iterations) %>%
    summarise(ci = list(compute_ci(value)), .groups = "drop") %>%
    tidyr::unnest_wider(ci)

cat("\n=== 99% Confidence Intervals (ALL REPLICATIONS) ===\n")
print(gpu_ci_all)


# --------- CI WITH SAMPLE OF 10 REPLICATIONS ---------
set.seed(123)

gpu_ci_sample10 <- gpu_times %>%
    group_by(metric_name, problem_size, num_iterations) %>%
    summarise(sample_values = list({
        v <- value
        if (length(v) > 10) sample(v, 10) else v
    }), .groups = "drop") %>%
    mutate(ci = map(sample_values, compute_ci)) %>%
    select(-sample_values) %>%
    tidyr::unnest_wider(ci)

cat("\n=== 99% Confidence Intervals (SAMPLE OF 10) ===\n")
print(gpu_ci_sample10)

print(gpu_ci_all, n = Inf)
print(gpu_ci_sample10, n = Inf)

