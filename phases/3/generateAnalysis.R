library(ggplot2)
library(dplyr)
library(grid)
library(purrr)
library(tidyr)

my_style <- function() {
  list(
    theme_bw(base_size = 12),
    theme(
      legend.title = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.spacing = unit(1, "mm"),
      legend.position = "right",
      legend.justification = "left",
      legend.box.spacing = unit(0, "pt"),
      legend.box.margin = margin(0, 0, 0, 0),
      axis.text.x = element_text(angle=45, vjust=1, hjust=1)    
    ))
}

experiment_results_clean <- read.csv("./clean_dataset.csv") |>
    filter(experiment == 1, machine %in% c("draco1", "draco2"))

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

pdf("analysis_plots.pdf", width = 10, height = 8)

p1 <- ggplot(filter(experiment_results_clean, metric_name == "computation_time_s"),
    aes(x = problem_size, y = value, color = as.factor(num_threads))) + 
    geom_point(alpha = 0.8) +
    geom_line(alpha = 0.8) + 
    facet_grid(num_iterations ~ device) + 
    labs(title = "Computation Time vs Problem Size",
         x = "Problem Size", 
         y = "Computation Time (seconds)", 
         color = "Number of Threads") +
    my_style()
print(p1)

# Plot 2: CPU Thread Speedup
if (nrow(cpu_speedup) > 0) {
    p2 <- ggplot(cpu_speedup, aes(x = num_threads, y = speedup, color = as.factor(problem_size))) +
        geom_point() + 
        geom_line() + 
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        facet_wrap(~problem_size, scales = "free_y") + 
        labs(title = "CPU Thread Speedup",
             x = "Number of Threads", 
             y = "Speedup Factor", 
             color = "Problem Size") +
        my_style() + 
        theme(legend.position = "none")
    print(p2)
}

# Plot 3: Linear Model
lm_data <- experiment_results_clean |>
    filter(device == "cpu", metric_name == "msamples_per_sec")

if (nrow(lm_data) > 0) {
    model <- lm(value ~ num_threads + problem_size + num_iterations, data = lm_data)

    cat("\n--- Linear Model Summary ---\n")
    print(summary(model))

    lm_data_long <- lm_data |>
        mutate(predicted = predict(model)) |>
        tidyr::pivot_longer(cols = c("value", "predicted"), 
                           names_to = "type", 
                           values_to = "msamples_per_sec")

    p_lm1 <- ggplot(lm_data_long, aes(x = num_threads, y = msamples_per_sec, color = type)) +
        geom_point(alpha = 0.6) + 
        geom_line(aes(group = type), alpha = 0.6) + 
        facet_grid(problem_size ~ num_iterations, scales = "free_y") + 
        labs(x = "Number of Threads", 
             y = "Performance (MSamples/s)",
             color = "Type") + 
        my_style()

    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(0.1, 0.9), "npc"))))
    grid.text("Linear Model Diagnostics: Actual vs. Predicted", 
              gp = gpar(fontsize = 16),
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

    tibble(mean = mean_x, 
           lower = mean_x - tcrit * se, 
           upper = mean_x + tcrit * se,
           n = n)
}

# Use all GPU metrics that exist
gpu_times <- experiment_results_clean |>
    filter(device == "gpu")

# --------- CI WITH ALL REPLICATIONS ---------
gpu_ci_all <- gpu_times |>
    group_by(metric_name, problem_size, num_iterations) |>
    summarise(ci = list(compute_ci(value)), .groups = "drop") |>
    tidyr::unnest_wider(ci)

cat("\n=== 99% Confidence Intervals (ALL REPLICATIONS) ===\n")
print(gpu_ci_all)

# --------- CI WITH SAMPLE OF 10 REPLICATIONS ---------
set.seed(123)

gpu_ci_sample10 <- gpu_times |>
    group_by(metric_name, problem_size, num_iterations) |>
    summarise(sample_values = list({
        v <- value
        if (length(v) > 10) sample(v, 10) else v
    }), .groups = "drop") |>
    mutate(ci = map(sample_values, compute_ci)) |>
    select(-sample_values) |>
    tidyr::unnest_wider(ci)

cat("\n=== 99% Confidence Intervals (SAMPLE OF 10) ===\n")
print(gpu_ci_sample10)

print(gpu_ci_all, n = Inf)
print(gpu_ci_sample10, n = Inf)

# Total time and execution time analysis

dir.create("plots", showWarnings = FALSE, recursive = TRUE)

experiment_results_clean <- read.csv("./clean_dataset.csv") |>
    filter(is.na(value) == FALSE) |>
    mutate(
        machine_label = case_when(
            machine == "draco2" ~ paste0("draco2 (CPU, ", num_threads, " cores)"),
            machine == "draco1" ~ "draco1 (GPU)",
            machine == "beagle" ~ "beagle (GPU)",
            TRUE ~ machine
        ),
        metric_label = case_when(
            metric_name == "computation_time_s" ~ "Tempo de Computação",
            metric_name == "total_time_s" ~ "Tempo Total",
            TRUE ~ metric_name
        )
    ) 

time_summary <- experiment_results_clean |>
    filter(metric_name %in% c("computation_time_s", "total_time_s")) |>
    group_by(experiment, device, machine, machine_label, problem_size, 
             num_iterations, num_threads, metric_label) |>
    summarise(
        mean_value = mean(value),
        se = sd(value) / sqrt(n()),
        ci_lower = mean_value - qt(0.995, n()-1) * se,
        ci_upper = mean_value + qt(0.995, n()-1) * se,
        n = n(),
        .groups = "drop"
    )

plot_time_analysis <- function(data, exp) {
  ggplot(time_summary |> filter(experiment == exp) |> filter(!is.na(mean_value)), 
              aes(x = problem_size, y = mean_value, 
                  color = machine_label, 
                  linetype = metric_label,
                  group = interaction(machine_label, metric_label))) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 5, alpha = 1.0, linewidth = 0.5) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    facet_wrap(vars(device, num_iterations), 
               scales = "free_y", 
               ncol = 3, 
               labeller = labeller(device = c("cpu" = "CPU","gpu" = "GPU") , 
                    num_iterations = function(x) { paste0("Iterações: ", x) })) +
    labs(
        title = paste("Experimento", exp, ": Tempo de computação vs Tempo total"),
        x = "Tamanho do problema",
        y = "Tempo (s)",
        color = "Máquina / Configuração",
        linetype = "Métrica"
    ) +
    my_style() +
    scale_linetype_manual(values = c("Tempo de Computação" = "solid", "Tempo Total" = "dashed")) +
    scale_x_continuous(breaks = seq(0, 500, 50))
}

p1a <- plot_time_analysis(time_summary, exp = 1)
p1b <- plot_time_analysis(time_summary, exp = 2)
ggsave("plots/plot1a_execution_vs_total_exp1.pdf", plot = p1a, width = 8, height = 5)
ggsave("plots/plot1b_execution_vs_total_exp2.pdf", plot = p1b, width = 8, height = 5)

# Throughput plot

throughput_summary <- experiment_results_clean |>
  filter(metric_name == "msamples_per_sec") |>
  group_by(experiment, machine_label, problem_size, num_iterations) |>
  summarise(
    mean_value = mean(value),
    se = sd(value) / sqrt(n()),
    ci_lower = mean_value - qt(0.995, n()-1) * se,
    ci_upper = mean_value + qt(0.995, n()-1) * se,
    n = n(),
    .groups = "drop"
  )

plot_throughput <- function(data, exp) {
  ggplot(throughput_summary |> filter(experiment == exp), 
              aes(x = problem_size, 
                  y = mean_value, 
                  color = machine_label, 
                  group = machine_label)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 5, alpha = 1.0, linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  
  facet_wrap(~ num_iterations, labeller = label_value) +
  
  labs(
    title = paste("Experimento", exp, ": Throughput"),
    x = "Tamanho do problema",
    y = "Throughput (MSamples/s)",
    color = "Máquina / Configuração"
  ) +
  my_style()
}

p2a <- plot_throughput(throughput_summary, exp = 1)
p2b <- plot_throughput(throughput_summary, exp = 2)

ggsave("plots/plot2a_throughput_exp1.pdf", plot = p2a, width = 8, height = 5)
ggsave("plots/plot2b_throughput_exp2.pdf", plot = p2b, width = 8, height = 5)

# CPU Speedup and Efficiency Analysis

cpu_data <- experiment_results_clean |>
    filter(device == "cpu") |>
    filter(metric_name %in% c("computation_time_s", "total_time_s"))

baseline_cpu <- cpu_data |>
     filter(num_threads == 1) |>
     group_by(experiment, problem_size, num_iterations, metric_label) |> 
     summarise(
        baseline_mean = mean(value),
        baseline_se = sd(value) / sqrt(n()),
        n_baseline = n(),
        .groups = "drop"
     )
  
cpu_speedup <- cpu_data |>
    group_by(experiment, num_threads, problem_size, num_iterations, metric_label) |> 
    summarise(
        mean_time = mean(value),
        se = sd(value) / sqrt(n()),
        n = n(),
        .groups = "drop"
    ) |>
    inner_join(baseline_cpu, by = c("experiment", "problem_size", "num_iterations", "metric_label")) |> 
    mutate(
        speedup = baseline_mean / mean_time,
        efficiency = speedup / num_threads
    )

plot_cpu_speedup <- function(data, exp) {
    max_threads <- max(data$num_threads)
  
    ideal_line <- data.frame(
      num_threads = c(1, max_threads),
      speedup = c(1, max_threads)
    )

    ggplot(data |> filter(experiment == exp),
         aes(x = num_threads,
             y = speedup,
             color = factor(problem_size),
             linetype = metric_label,
             shape = metric_label,
             group = interaction(problem_size, metric_label))) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    geom_line(data = ideal_line, 
              aes(x = num_threads, y = speedup), 
              inherit.aes = FALSE, 
              linetype = "dashed", 
              color = "gray50", 
              linewidth = 0.8) +
    facet_wrap(~ num_iterations, labeller = label_value) +
    scale_x_continuous(
      breaks = sort(unique(data$num_threads))
    ) +
    scale_shape_manual(values = c(16, 4)) +
    labs(
      title = paste("Experimento", exp, ": Speedup (CPU)"),
      x = "Número de Threads",
      y = "Speedup (baseline = 1 thread)",
      color = "Tamanho do problema",
      linetype = "Métrica",  
      shape = "Métrica"
    ) +
    my_style()
}

p_speedup1 <- plot_cpu_speedup(cpu_speedup, 1)
p_speedup2 <- plot_cpu_speedup(cpu_speedup, 2)

ggsave("plots/plot3a_cpu_speedup_exp1.pdf", plot = p_speedup1, width = 8, height = 5)
ggsave("plots/plot3b_cpu_speedup_exp2.pdf", plot = p_speedup2, width = 8, height = 5)

plot_cpu_efficiency <- function(data, exp) {
    max_threads <- max(data$num_threads)
  
    ideal_line <- data.frame(
      num_threads = c(1, max_threads),
      efficiency = c(1, 1)
    )

    ggplot(data |> filter(experiment == exp),
         aes(x = num_threads,
             y = efficiency,
             color = factor(problem_size),
             linetype = metric_label,
             shape = metric_label,
             group = interaction(problem_size, metric_label))) +
      
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    
    geom_line(data = ideal_line, 
              aes(x = num_threads, y = efficiency), 
              inherit.aes = FALSE, 
              linetype = "dashed", 
              color = "gray50", 
              linewidth = 0.8) +
      
    facet_wrap(~ num_iterations, labeller = label_value) +
    
    scale_x_continuous(
      breaks = sort(unique(data$num_threads))
    ) +
    
    scale_y_continuous(
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1.1) 
    ) +
    
    scale_shape_manual(values = c(16, 4)) +
    
    labs(
      title = paste("Experimento", exp, ": Eficiência (CPU)"),
      x = "Número de Threads",
      y = "Eficiência (Speedup / Threads)",
      color = "Tamanho do problema",
      linetype = "Métrica",
      shape = "Métrica"
    ) +
    my_style()
}

p_efficiency1 <- plot_cpu_efficiency(cpu_speedup, 1)
p_efficiency2 <- plot_cpu_efficiency(cpu_speedup, 2)

ggsave("plots/plot4a_cpu_efficiency_exp1.pdf", plot = p_efficiency1, width = 8, height = 5)
ggsave("plots/plot4b_cpu_efficiency_exp2.pdf", plot = p_efficiency2, width = 8, height = 5)

plot_cpu_vs_gpu_speedup <- function(data, exp) {
    ggplot(data |> filter(experiment == exp),
         aes(x = num_threads,
             y = speedup,
             color = factor(problem_size),
             linetype = metric_label,
             shape = metric_label,
             group = interaction(problem_size, metric_label))) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    geom_line(data = ideal_line, 
              aes(x = num_threads, y = speedup), 
              inherit.aes = FALSE, 
              linetype = "dashed", 
              color = "gray50", 
              linewidth = 0.8) +
    facet_wrap(~ num_iterations, labeller = label_value) +
    scale_x_continuous(
      breaks = sort(unique(data$num_threads))
    ) +
    scale_shape_manual(values = c(16, 4)) +
    labs(
      title = paste("Experimento", exp, ": Speedup (CPU)"),
      x = "Número de Threads",
      y = "Speedup (baseline = 1 thread)",
      color = "Tamanho do problema",
      linetype = "Métrica",  
      shape = "Métrica"
    ) +
    my_style()
}

# Overhead analysis

overhead_df <- experiment_results_clean |>
    filter(metric_name %in% c("computation_time_s", "total_time_s")) |>
    tidyr::pivot_wider(
        names_from = metric_name,
        values_from = value,
        id_cols = c(experiment, device, machine, machine_label, num_threads,
                    problem_size, num_iterations, replication_index)
    ) |>    
    group_by(experiment, machine_label, num_threads, problem_size, num_iterations) |>
    summarise(
        mean_comp = mean(computation_time_s),
        mean_total = mean(total_time_s),
        n = n(),
        mean_overhead_pct = 100 * (mean_total - mean_comp) / mean_total,
        .groups = "drop"
    )

    

plot_overhead_pct <- function(data, exp) {
    ggplot(data |> filter(experiment == exp), aes(x = problem_size, y = mean_overhead_pct, color = machine_label)) +
    geom_point(size = 2) +
    geom_line(linewidth = 1.2) +

    facet_grid( ~ num_iterations, scales = "free_x") +
    
    geom_hline(yintercept = c(10, 25, 50), linetype = "dotted", color = "gray50", alpha = 0.8) +
    annotate("text", x = min(data$problem_size), y = 52, label = "50% Overhead", 
             color = "gray50", size = 3, hjust = 0, vjust = 0) +
    
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    
    labs(
      title = paste("Experimento", exp, ": Overhead Médio vs Tamanho do Problema"),
      y = "Overhead Médio (% do tempo total)",
      x = "Tamanho do Problema",
      color = "Máquina / Configuração"
    ) +
    
    my_style() +
    theme(
      legend.position = "bottom",
    )
}

p_overhead1 <- plot_overhead_pct(overhead_df, 1)
p_overhead2 <- plot_overhead_pct(overhead_df, 2)

ggsave("plots/plot5a_overhead_percent_exp1.pdf", plot = p_overhead1, width = 10, height = 6)
ggsave("plots/plot5b_overhead_percent_exp2.pdf", plot = p_overhead2, width = 10, height = 6)