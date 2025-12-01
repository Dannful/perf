library(ggplot2)
library(dplyr)
library(grid)
library(purrr)
library(tidyr)
library(here)

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

base_dir <- here::here("phases/3/analysis")
plots_dir <- file.path(base_dir, "plots")
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================================== #
# Total and execution time analysis 
# =============================================================================================== #

experiment_results_clean <- read.csv(file.path(base_dir, "clean_dataset.csv")) |>
    filter(is.na(value) == FALSE) |>
    mutate(
        machine_label = case_when(
            machine == "draco2" ~ paste0("draco2 (CPU, ", num_threads, " cores)"),
            machine == "draco1" ~ "draco1 (GPU)",
            machine == "beagle" ~ "beagle (GPU)",
            TRUE ~ machine
        ),
        metric_label = case_when(
            metric_name == "computation_time_s" ~ "Tempo de Comp.",
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
    df <- data |> filter(experiment == exp) |> filter(!is.na(mean_value))
    p <- ggplot(df, 
        aes(x = problem_size, y = mean_value, 
            color = machine_label, 
            linetype = metric_label,
            group = interaction(machine_label, metric_label))) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 5, alpha = 1.0, linewidth = 0.5) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    labs(
        x = "Tamanho do problema",
        y = "Tempo (s)",
        color = "Máquina / Configuração",
        linetype = "Métrica"
    ) +
    my_style() +
    scale_linetype_manual(values = c("Tempo de Comp." = "solid", "Tempo Total" = "dashed")) +
    scale_x_continuous(breaks = seq(0, 500, 50))

    if (length(unique(df$num_iterations)) > 1) {
        p <- p + facet_wrap(
            ~ device + num_iterations,
            scales = "free_y",
            labeller = labeller(
                device = c(cpu = "CPU", gpu = "GPU"),
                num_iterations = function(x) paste0("Iterações: ", x)
            )
        )
    } else {
        p <- p + facet_grid(
            rows = vars(device),
            cols = vars(num_iterations),
            scales = "free_y",
            labeller = labeller(
                device = c(cpu = "CPU", gpu = "GPU"),
                num_iterations = function(x) paste0("Iterações: ", x)
            )
        )
    }

    return (p)
}

p1a <- plot_time_analysis(time_summary, exp = 1)
p1b <- plot_time_analysis(time_summary, exp = 2)
ggsave(file.path(plots_dir, "plot1a_execution_vs_total_exp1.pdf"), plot = p1a, width = 8, height = 5)
ggsave(file.path(plots_dir, "plot1b_execution_vs_total_exp2.pdf"), plot = p1b, width = 8, height = 5)

# =============================================================================================== #
# Throughput plot
# =============================================================================================== #

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
    ggplot(data |> filter(experiment == exp), 
        aes(x = problem_size, 
            y = mean_value, 
            color = machine_label, 
            group = machine_label)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 5, alpha = 1.0, linewidth = 0.5) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    facet_wrap(~ num_iterations, labeller = 
        labeller(num_iterations = function(x) paste0("Iterações: ", x))
    ) +
    labs(
      x = "Tamanho do problema",
      y = "Throughput (MSamples/s)",
      color = "Máquina / Configuração"
    ) +
    my_style()
}

p2a <- plot_throughput(throughput_summary, exp = 1)
p2b <- plot_throughput(throughput_summary, exp = 2)

ggsave(file.path(plots_dir, "plot2a_throughput_exp1.pdf"), plot = p2a, width = 8, height = 5)
ggsave(file.path(plots_dir, "plot2b_throughput_exp2.pdf"), plot = p2b, width = 8, height = 5)

# =============================================================================================== #
# CPU Speedup and Efficiency Analysis
# =============================================================================================== #

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
    facet_wrap(~ num_iterations, labeller = 
        labeller(num_iterations = function(x) paste0("Iterações: ", x))
    ) +
    scale_x_continuous(
      breaks = sort(unique(data$num_threads))
    ) +
    scale_shape_manual(values = c(16, 4)) +
    labs(
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

ggsave(file.path(plots_dir, "plot3a_cpu_speedup_exp1.pdf"), plot = p_speedup1, width = 8, height = 5)
ggsave(file.path(plots_dir, "plot3b_cpu_speedup_exp2.pdf"), plot = p_speedup2, width = 8, height = 5)

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
      
    facet_wrap(~ num_iterations, labeller = 
        labeller(num_iterations = function(x) paste0("Iterações: ", x))
    ) +
    
    scale_x_continuous(
      breaks = sort(unique(data$num_threads))
    ) +
    
    scale_y_continuous(
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1.1) 
    ) +
    
    scale_shape_manual(values = c(16, 4)) +
    
    labs(
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

ggsave(file.path(plots_dir, "plot4a_cpu_efficiency_exp1.pdf"), plot = p_efficiency1, width = 8, height = 5)
ggsave(file.path(plots_dir, "plot4b_cpu_efficiency_exp2.pdf"), plot = p_efficiency2, width = 8, height = 5)

# =============================================================================================== #
# Overhead analysis
# =============================================================================================== #

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

    facet_grid( ~ num_iterations, scales = "free_x", labeller = 
        labeller(num_iterations = function(x) paste0("Iterações: ", x))
    ) +     
    geom_hline(yintercept = c(10, 25, 50), linetype = "dotted", color = "gray50", alpha = 0.8) +
    annotate("text", x = min(data$problem_size), y = 52, label = "50% Overhead", 
             color = "gray50", size = 3, hjust = 0, vjust = 0) +
    
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    
    labs(
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

ggsave(file.path(plots_dir, "plot5a_overhead_percent_exp1.pdf"), plot = p_overhead1, width = 10, height = 6)
ggsave(file.path(plots_dir, "plot5b_overhead_percent_exp2.pdf"), plot = p_overhead2, width = 10, height = 6)


# =============================================================================================== #
# Linear Models for Computation time
# =============================================================================================== #


create_linear_model <- function(data, formula) {
    lm(formula, data = data)
    model <- lm(formula, data = data)
    print(summary(model))
    return (model)
}

plot_lm <- function(data, model, faceting, n_pred = 100) {
    pred_grid <- data |>
        distinct(across(c(-problem_size, -value))) |>
        crossing(problem_size = seq(min(data$problem_size), 
                                   max(data$problem_size), 
                                   length.out = n_pred))
    
    plot_data <- bind_rows(
        data |> mutate(type = "actual"),
        pred_grid |> mutate(type = "predicted", 
                           value = predict(model, newdata = pred_grid))
    )
    
    p_lm1 <- ggplot(plot_data, aes(x = problem_size, y = value, color = type)) +
        geom_line(data = filter(plot_data, type == "predicted"), linewidth = 1) +
        geom_point(data = filter(plot_data, type == "actual"), alpha = 0.6, size = 2) +
        faceting + 
        labs(y = "Computation Time (s)", x = "Problem Size", color = "Type") + 
        scale_color_manual(values = c("actual" = "#E41A1C", "predicted" = "#377EB8")) +
        my_style()
    
    return(p_lm1)
}

cpu_draco_data <- experiment_results_clean |>
    filter(device == "cpu") |>
    filter(machine == "draco2") |>
    filter(metric_name == "computation_time_s")

gpu_draco_data <- experiment_results_clean |>
    filter(device == "gpu") |>
    filter(machine == "draco1") |>
    filter(metric_name == "computation_time_s")

gpu_beagle_data <- experiment_results_clean |>
    filter(device == "gpu") |>
    filter(machine == "beagle") |>
    filter(metric_name == "computation_time_s")    

cp_time_cpu_model <- create_linear_model(
    cpu_draco_data, 
    value ~ I(problem_size^3) * num_iterations * I(1/num_threads)
)
                          
cp_time_draco_gpu_model <- create_linear_model(
    gpu_draco_data, 
    value ~ I(problem_size^3) * num_iterations
)

cp_time_beagle_gpu_model <- create_linear_model(
    gpu_beagle_data, 
    value ~ I(problem_size^3)
)

p_draco_cpu_lm <- plot_lm(
    cpu_draco_data,
    cp_time_cpu_model,
    facet_wrap(~ num_iterations + num_threads, scales = "free_y", labeller = labeller(
        num_iterations = function(x) paste0("Iterações: ", x),
        num_threads = function(x) paste0("Cores: ", x)
    ))
)

p_draco_gpu_lm <- plot_lm(
    gpu_draco_data, 
    cp_time_draco_gpu_model,
    facet_wrap(~ num_iterations, scales = "free_y", labeller = labeller(
        num_iterations = function(x) paste0("Iterações: ", x)
    ))
)

p_beagle_gpu_lm <- plot_lm(
    gpu_beagle_data, 
    cp_time_beagle_gpu_model,
    facet_grid(~ num_iterations, scales = "free_y", labeller = labeller(
        num_iterations = function(x) paste0("Iterações: ", x)
    ))
)

ggsave(file.path(plots_dir, "lm_cpu_diagnostics.pdf"), plot = p_draco_cpu_lm, width = 10, height = 8)
ggsave(file.path(plots_dir, "lm_draco_gpu_diagnostics.pdf"), plot = p_draco_gpu_lm, width = 8, height = 5)
ggsave(file.path(plots_dir, "lm_beagle_gpu_diagnostics.pdf"), plot = p_beagle_gpu_lm, width = 8, height = 5)