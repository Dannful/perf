library(ggplot2)
library(dplyr)
library(grid)
library(purrr)
library(tidyr)

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
    geom_line(alpha = 0.8) + facet_grid(num_iterations ~ device) + labs(title = "Computation Time vs Problem Size",
    x = "Problem Size", y = "Computation Time (seconds)", color = "Number of Threads") +
    theme_minimal()
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

lm_data <- experiment_results_clean |>
    filter(device == "cpu", metric_name == "msamples_per_sec")

if (nrow(lm_data) > 0) {
    model <- lm(value ~ num_threads + problem_size + num_iterations, data = lm_data)

    cat("\n--- Linear Model Summary ---\n")
    print(summary(model))

    # Create a long-format data frame for plotting
    lm_data_long <- lm_data |>
        mutate(predicted = predict(model)) |>
        tidyr::pivot_longer(cols = c("value", "predicted"), names_to = "type", values_to = "msamples_per_sec")

    # Create the new plot
    p_lm1 <- ggplot(lm_data_long, aes(x = num_threads, y = msamples_per_sec, color = type)) +
        geom_point(alpha = 0.6) + geom_line(aes(group = type), alpha = 0.6) + facet_grid(problem_size ~
        num_iterations, scales = "free_y") + labs(x = "Number of Threads", y = "Performance (MSamples/s)",
        color = "Type") + theme_minimal()

    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(0.1, 0.9),
        "npc"))))
    grid.text("Linear Model Diagnostics: Actual vs. Predicted", gp = gpar(fontsize = 16),
        vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(p_lm1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
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
