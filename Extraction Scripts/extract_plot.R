# Script to extract the accuracy, precision, bias and correlation of behavioral estimates
# from simulated focal follows and group scans to true values & generate plots

# set up workspace -------------------------------------------------------------
library(tidyverse)
library(future)
library(furrr)
library(lme4)
library(rpart)
library(rpart.plot)
library(vip)
library(scales)
library(caret)
library(ggridges)
library(ggpubr)
library(patchwork)
library(sjPlot)
library(effectsize)
library(fastmatch)
options(dplyr.summarise.inform = FALSE) # suppress summarise info

setwd('GitHub/Observation_Methods_Comparison/')
source("Simulation Scripts/simulation_functions.r")

# ## The different performance measures of the simulations (accuracy, precision, correlation, bias)
# ## are stored in different objects, as are the parameters underlying each simulation.
# ## We call them all up and then attach the simulation parameter values
# load("Simulation Outputs/all_accuracy1.RData")
# load("Simulation Outputs/all_accuracy2.RData")
# all_accuracy <- rbind(all_accuracy1, all_accuracy2)
# 
# load("Simulation Outputs/all_precision1.RData")
# load("Simulation Outputs/all_precision2.RData")
# all_precision <- rbind(all_precision1, all_precision2)
# 
# load("Simulation Outputs/all_bias1.RData")
# load("Simulation Outputs/all_bias2.RData")
# all_bias <- rbind(all_bias1, all_bias2)
# 
# load("Simulation Outputs/all_correlation.RData")
# load("Simulation Outputs/all_parameters.RData")
# 
# 
# # Add simulation parameters to all frames
# 
# # Remove 'SimulationsOutput_' and '.Rdata' from all_parameters$Run_ID
# all_parameters <- all_parameters %>%
#   mutate(Run_ID = str_remove_all(Run_ID, "Simulations?Output_|\\.Rdata"))
# 
# all_correlation <- all_correlation %>%
#   mutate(Run_ID = str_remove_all(Run_ID, "Simulations?Output_|\\.Rdata"))
# 
# # Split Run_ID by '_'
# 
# all_accuracy <- all_accuracy %>%
#   separate(Run_ID,
#            into = c('A','B','Iteration'),
#            sep = '_',
#            remove = TRUE) %>% 
#   unite(Run_ID,
#         c('A','B'),
#         sep = '_',
#         remove = TRUE)
#   
# all_precision <- all_precision %>%
#   separate(Run_ID,
#            into = c('A','B','Iteration'),
#            sep = '_',
#            remove = TRUE) %>% 
#   unite(Run_ID,
#         c('A','B'),
#         sep = '_',
#         remove = TRUE)
# 
# all_bias <- all_bias %>%
#   separate(Run_ID,
#            into = c('A','B','Iteration'),
#            sep = '_',
#            remove = TRUE) %>% 
#   unite(Run_ID,
#         c('A','B'),
#         sep = '_',
#         remove = TRUE)
# 
# 
#   
# # Join with parameters
# 
# all_accuracy <- all_accuracy %>%
#   left_join(all_parameters, by = 'Run_ID')
# 
# all_precision <- all_precision %>%
#   left_join(all_parameters, by = 'Run_ID')
# 
# all_bias <- all_bias %>%
#   left_join(all_parameters, by = 'Run_ID')
# 
# all_correlation <- all_correlation %>%
#   left_join(all_parameters, by = 'Run_ID')


load(file = 'G:/Other computers/My Computer/rethinking-obs-methods/all_accuracy.Rdata')
load(file = 'G:/Other computers/My Computer/rethinking-obs-methods/all_precision.Rdata')
load(file = 'G:/Other computers/My Computer/rethinking-obs-methods/all_bias.Rdata')
load(file = 'G:/Other computers/My Computer/rethinking-obs-methods/all_correlation.Rdata')

# Plot densities of each performance measure ----------------------------------------


# Accuracy

accuracy_density <- all_accuracy %>%  
  select(Accuracy_focal, Accuracy_scan) %>%  
  # rename columns to 'Focal' and 'Scan'
  rename(Focal = Accuracy_focal,
         Scan = Accuracy_scan) %>%
  pivot_longer(cols = everything(),
               names_to = "Category",
               values_to = "Value") %>%  
  ggplot(aes(x = Value, fill = Category, color = Category)) +  
  geom_density(alpha = 0.3, linewidth = 1, adjust = 1.5) +  
  geom_vline(
    data = all_accuracy %>%
      select(Accuracy_focal, Accuracy_scan) %>%
      rename(Focal = Accuracy_focal,
             Scan = Accuracy_scan) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Category",
        values_to = "Value"
      ) %>%
      group_by(Category) %>%
      summarize(Median = median(Value)),
    aes(xintercept = Median, color = Category),
    linetype = "dashed",
    linewidth = 1.2
  ) +  
  # Add corner labels
  annotate("text", 
           x = 0, y = 2.5, 
           label = "more accurate", 
           hjust = -0.1, vjust = 1.1, #adjust these values to position
           size = 14/.pt) +
  annotate("text", 
           x = 1000, y = 2.5, 
           label = "less accurate", 
           hjust = 0, vjust = 1.1, 
           size = 14/.pt) +
  labs(title = "A: Accuracy",
       x = "Standarized RMSE",
       y = "Density") +  
  theme_minimal() +  
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18),
    legend.position = "none"  # Remove legend
  ) +  
  scale_fill_manual(
    values = c("Focal" = "#5DA8E4",
               "Scan" = "#EF9401")
  ) +  
  scale_color_manual(
    values = c("Focal" = "#5DA8E4",
               "Scan" = "#EF9401")
  ) +  
  scale_x_log10(labels = label_comma()) +
  scale_y_continuous(breaks = function(x) {
    pretty(x, n = 5)  # Generate exactly 5 pretty breaks
  })


# Precision plot
precision_density <- all_precision %>%
  filter(scan_break_time_min <= 60) %>%
  filter(Precision_focal != 0 & Precision_scan != 0) %>%
  filter(!is.na(Precision_focal) & !is.na(Precision_scan)) %>%
  select(Precision_focal, Precision_scan) %>%
  rename(Focal = Precision_focal,
         Scan = Precision_scan) %>%
  pivot_longer(cols = everything(),
               names_to = "Category",
               values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Category, color = Category)) +
  geom_density(alpha = 0.3, linewidth = 1, adjust = 1.5) +
  geom_vline(
    data = all_precision %>%
      filter(Precision_focal != 0 & Precision_scan != 0) %>%
      filter(!is.na(Precision_focal) & !is.na(Precision_scan)) %>%
      select(Precision_focal, Precision_scan) %>%
      rename(Focal = Precision_focal,
             Scan = Precision_scan) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Category",
        values_to = "Value"
      ) %>%
      group_by(Category) %>%
      summarize(Median = median(Value)),
    aes(xintercept = Median, color = Category),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  # Add corner labels
  annotate("text", 
           x = 0, y = 1.3, 
           label = "more precise", 
           hjust = -0.1, vjust = 1.1, #adjust these values for position
           size = 14/.pt) +
  annotate("text", 
           x = 90, y = 1.3, 
           label = "less precise", 
           hjust = 0, vjust = 1.1, #adjust these values for position
           size = 14/.pt) +
  labs(title = "B: Precision", x = "CV", y = "Density") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18),
    legend.position = "none"  # Remove legend
  ) +
  scale_fill_manual(
    values = c("Focal" = "#5DA8E4",
               "Scan" = "#EF9401")
  ) +
  scale_color_manual(
    values = c("Focal" = "#5DA8E4",
               "Scan" = "#EF9401")
  ) +
  scale_x_log10(labels = label_comma()) +
  scale_y_continuous(breaks = function(x) {
    pretty(c(0, x[2]), n = 5)  # Force 5 breaks from 0 to the max
  })

# Bias plot
bias_density <- all_bias %>%
  filter(scan_break_time_min <= 60) %>%
  select(Bias_focal, Bias_scan) %>%
  rename(Focal = Bias_focal,
         Scan = Bias_scan) %>%
  pivot_longer(cols = everything(),
               names_to = "Category",
               values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Category, color = Category)) +
  geom_density(alpha = 0.3, linewidth = 1, adjust = 1.5) +
  geom_vline(
    data = all_bias %>%
      select(Bias_focal, Bias_scan) %>%
      rename(Focal = Bias_focal,
             Scan = Bias_scan) %>% 
      pivot_longer(
        cols = everything(),
        names_to = "Category",
        values_to = "Value"
      ) %>%
      group_by(Category) %>%
      summarize(Median = median(Value)),
    aes(xintercept = Median, color = Category),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  labs(title = "C: Bias", x = "Standarized Mean Error", y = "Density") +
  theme_minimal() +
  xlim(-1.1, 3) +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18),
    legend.position = "none"  # Remove legend
  ) +
  scale_fill_manual(
    values = c("Focal" = "#5DA8E4",
               "Scan" = "#EF9401")
  ) +
  scale_color_manual(
    values = c("Focal" = "#5DA8E4",
               "Scan" = "#EF9401")
  ) +
  scale_y_continuous(breaks = function(x) {
    pretty(c(0, x[2]), n = 5)  # Force 5 breaks from 0 to the max
  })



# Correlation plot
correlation_density <- all_correlation %>%
  filter(scan_break_time_min <= 60) %>%
  select(Correlation_focal, Correlation_scan) %>%
  rename(Focal = Correlation_focal,
         Scan = Correlation_scan) %>%
  pivot_longer(cols = everything(),
               names_to = "Category",
               values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Category, color = Category)) +
  geom_density(alpha = 0.3, linewidth = 1, adjust = 1.5) +
  geom_vline(
    data = all_correlation %>%
      select(Correlation_focal, Correlation_scan) %>%
      rename(Focal = Correlation_focal,
             Scan = Correlation_scan) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Category",
        values_to = "Value"
      ) %>%
      group_by(Category) %>%
      summarize(Median = median(Value)),
    aes(xintercept = Median, color = Category),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  labs(title = "D: Correlation with True Values", x = "Correlation Coefficient", y = "Density") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18),
    legend.position = "none"  # Remove legend
  ) +
  scale_fill_manual(
    values = c("Focal" = "#5DA8E4",
               "Scan" = "#EF9401")
  ) +
  scale_color_manual(
    values = c("Focal" = "#5DA8E4",
               "Scan" = "#EF9401")
  ) +
  scale_y_continuous(breaks = function(x) {
    pretty(c(0, x[2]), n = 5)  # Force 5 breaks from 0 to the max
  })



# # Save plots
# 
correlation_difference <- all_correlation %>%
  ggplot(aes(x = Correlation_approaches)) +
  geom_histogram(
    fill = "#584B53",
    color = "#584B53",
    alpha = 0.2,
    bins = 50,
    position = 'identity'
  ) +
  geom_vline(
    data = all_correlation %>%
      summarize(Median = median(Correlation_approaches)),
    aes(xintercept = 0),
    linetype = "dashed",
    size = 0.8
  ) +
  labs(title = "Density of correlation values between focal follows and group scans", x = "Correlation Coefficient", y = "Density") +
  theme_minimal() +
  scale_x_continuous(labels = label_comma(), limits = c(-1, 1))
# 
# Save plots

p_density_plot <- ggarrange(
  accuracy_density,
  precision_density,
  bias_density,
  correlation_density,
  ncol = 2,
  nrow = 2,
  legend = 'bottom',
  common.legend = T
)

ggsave(
  "Density Plot.jpg",
  p_density_plot,
  dpi = 300,
  width = 14,
  height = 10
)

ggsave(
  "Correlation Methods.jpg",
  correlation_difference,
  dpi = 300,
  width = 10,
  height = 5
)


# Impact of parameters for each approach ----------------------------------

## ---- Setup ----


scale_vars <- c(
  "n_days",
  "group_size",
  "p_terrain_visibility",
  "p_behavior_visibility",
  "mean_events",
  "behavior_duration",
  "focal_duration_min",
  "focal_break_time_min",
  "scan_obsTime_perID",
  "scan_break_time_min"
)

parameter_labels <- c(
  "group_size" = "Larger group size",
  "p_terrain_visibility" = "Better terrain visibility",
  "mean_events" = "Higher behaviour frequency",
  "behavior_duration" = "Longer behaviour duration",
  "p_behavior_visibility" = "Better behaviour visibility",
  "n_days" = "Longer study duration",
  "focal_duration_min" = "Longer focal duration",
  "focal_break_time_min" = "Longer focal break time",
  "scan_break_time_min" = "Longer scan interval time",
  "scan_obsTime_perID" = "Longer scan time per subject"
)

performance_colors <- c(
  "Group Scan" = "#EF9401",
  "Focal Follows" = "#5DA8E4"
)

common_theme <- theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, margin = margin(0, 0, 5, 0)),
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "none"
  )

zscore_predictors <- function(df) {
  df %>%
    mutate_at(scale_vars, ~ (scale(.) %>% as.vector))
}

rename_parameters <- function(df, label_vector) {
  df$Parameter <- sapply(df$Parameter, function(param) {
    if (param %in% names(label_vector)) {
      label_vector[[param]]
    } else {
      param
    }
  })
  return(df)
}

make_perf_df <- function(mdl_scan, mdl_focal) {
  rbind(
    standardize_parameters(mdl_scan) %>%
      data.frame() %>%
      mutate(performance = "Group Scan"),
    standardize_parameters(mdl_focal) %>%
      data.frame() %>%
      mutate(performance = "Focal Follows")
  )
}


## ---- Models ----


# Accuracy data + models
z_data_accuracy <- zscore_predictors(all_accuracy)

mdl_accuracy_focal <-
  lmer(
    log(Accuracy_focal) ~
      n_days +
      group_size +
      p_terrain_visibility +
      p_behavior_visibility +
      mean_events +
      behavior_duration +
      focal_duration_min +
      focal_break_time_min +
      (1 | Run_ID),
    data = z_data_accuracy
  )

mdl_accuracy_scan <-
  lmer(
    log(Accuracy_scan) ~
      n_days +
      group_size +
      p_terrain_visibility +
      p_behavior_visibility +
      mean_events +
      behavior_duration +
      scan_obsTime_perID +
      scan_break_time_min +
      (1 | Run_ID),
    data = z_data_accuracy
  )

accuracy_df <-
  make_perf_df(mdl_accuracy_scan, mdl_accuracy_focal) %>%
  filter(Parameter != "(Intercept)") %>%
  arrange(Parameter)

accuracy_df <- rename_parameters(accuracy_df, parameter_labels) %>%
  arrange(Parameter)

# Precision data + models
z_data_precision <- all_precision %>%
  filter(Precision_focal != 0 & Precision_scan != 0) %>%
  zscore_predictors()

mdl_precision_focal <-
  lmer(
    log(Precision_focal) ~
      n_days +
      group_size +
      p_terrain_visibility +
      p_behavior_visibility +
      mean_events +
      behavior_duration +
      focal_duration_min +
      focal_break_time_min +
      (1 | Run_ID),
    data = z_data_precision
  )

mdl_precision_scan <-
  lmer(
    log(Precision_scan) ~
      n_days +
      group_size +
      p_terrain_visibility +
      p_behavior_visibility +
      mean_events +
      behavior_duration +
      scan_obsTime_perID +
      scan_break_time_min +
      (1 | Run_ID),
    data = z_data_precision
  )

precision_df <-
  make_perf_df(mdl_precision_scan, mdl_precision_focal) %>%
  arrange(Parameter)

precision_df <- rename_parameters(precision_df, parameter_labels)


# Bias data + models
z_data_bias <- zscore_predictors(all_bias)

mdl_bias_focal <-
  lmer(
    Bias_focal ~
      n_days +
      group_size +
      p_terrain_visibility +
      p_behavior_visibility +
      mean_events +
      behavior_duration +
      focal_duration_min +
      focal_break_time_min +
      (1 | Run_ID),
    data = z_data_bias
  )

mdl_bias_scan <-
  lmer(
    Bias_scan ~
      n_days +
      group_size +
      p_terrain_visibility +
      p_behavior_visibility +
      mean_events +
      behavior_duration +
      scan_obsTime_perID +
      scan_break_time_min +
      (1 | Run_ID),
    data = z_data_bias
  )

# Correlation data + models
z_data_correlation <- all_correlation %>%
  filter(!is.na(Correlation_focal) & !is.na(Correlation_scan) & !is.na(Correlation_approaches)) %>%
  zscore_predictors()

mdl_correlation_focal <-
  lmer(
    Correlation_focal ~
      n_days +
      group_size +
      p_terrain_visibility +
      p_behavior_visibility +
      mean_events +
      behavior_duration +
      focal_duration_min +
      focal_break_time_min +
      (1 | Run_ID),
    data = z_data_correlation
  )

mdl_correlation_scan <-
  lmer(
    Correlation_scan ~
      n_days +
      group_size +
      p_terrain_visibility +
      p_behavior_visibility +
      mean_events +
      behavior_duration +
      scan_obsTime_perID +
      scan_break_time_min +
      (1 | Run_ID),
    data = z_data_correlation
  )

correlation_df <-
  make_perf_df(mdl_correlation_scan, mdl_correlation_focal) %>%
  arrange(Parameter)

correlation_df <- rename_parameters(correlation_df, parameter_labels)


## ---- Plots ----

### ---- Accuracy plot ----

accuracy_plot <-
  accuracy_df %>%
  ggplot(aes(
    y = Parameter,
    x = Std_Coefficient,
    color = performance,
    fill = performance
  )) +
  geom_errorbarh(
    aes(xmin = 0, xmax = Std_Coefficient),
    height = 0,
    linewidth = 1,
    position = position_dodge(width = .7)
  ) +
  geom_point(size = 3, position = position_dodge(width = .7)) +
  geom_text(
    aes(label = round(Std_Coefficient, 2)),
    position = position_dodge(width = 0.7),
    vjust = -0.6,
    color = "black",
    size = 3
  ) +
  geom_vline(aes(xintercept = 0), lty = 2) +
  xlim(
    -max(abs(accuracy_df$Std_Coefficient)) * 1.5,
    max(abs(accuracy_df$Std_Coefficient)) * 1.5
  ) +
  common_theme +
  ggtitle("A: Accuracy") +
  scale_color_manual(values = performance_colors) +
  scale_fill_manual(values = performance_colors) +
  labs(
    color = "Observation Method",
    fill = "Observation Method",
    x = "Estimated Effect",
    y = "Parameters"
  ) +
  annotate(
    "text",
    x = -0.3,
    y = length(parameter_labels) + 0.5,
    label = "more accurate",
    size = 3
  ) +
  annotate(
    "text",
    x = 0.2,
    y = length(parameter_labels) + 0.5,
    label = "less accurate",
    size = 3
  )

### ---- Precision plot ----

precision_plot <-
  precision_df %>%
  filter(Parameter != "(Intercept)") %>%
  ggplot(aes(
    y = Parameter,
    x = Std_Coefficient,
    color = performance,
    fill = performance
  )) +
  geom_errorbarh(
    aes(xmin = 0, xmax = Std_Coefficient),
    height = 0,
    linewidth = 1,
    position = position_dodge(width = .7)
  ) +
  geom_point(size = 3, position = position_dodge(width = .7)) +
  geom_text(
    aes(label = round(Std_Coefficient, 2)),
    position = position_dodge(width = 0.7),
    vjust = -0.6,
    color = "black",
    size = 3
  ) +
  geom_vline(aes(xintercept = 0), lty = 2) +
  common_theme +
  xlim(
    -max(abs(precision_df$Std_Coefficient)) * 1.1,
    max(abs(precision_df$Std_Coefficient)) * 1.1
  ) +
  ggtitle("B: Precision") +
  scale_color_manual(values = performance_colors) +
  scale_fill_manual(values = performance_colors) +
  labs(
    color = "Observation Method",
    fill = "Observation Method",
    x = "Estimated Effect",
    y = NULL
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  annotate(
    "text",
    x = -0.3,
    y = length(parameter_labels) + 0.5,
    label = "more precise",
    size = 3
  ) +
  annotate(
    "text",
    x = 0.2,
    y = length(parameter_labels) + 0.5,
    label = "less precise",
    size = 3
  )

### ---- Correlation plot ----

correlation_plot <-
  correlation_df %>%
  filter(Parameter != "(Intercept)") %>%
  ggplot(aes(
    y = Parameter,
    x = Std_Coefficient,
    color = performance,
    fill = performance
  )) +
  geom_errorbarh(
    aes(xmin = 0, xmax = Std_Coefficient),
    height = 0,
    linewidth = 1,
    position = position_dodge(width = .7)
  ) +
  geom_point(size = 3, position = position_dodge(width = .7)) +
  geom_text(
    aes(label = round(Std_Coefficient, 2)),
    position = position_dodge(width = 0.7),
    vjust = -0.6,
    color = "black",
    size = 3
  ) +
  geom_vline(aes(xintercept = 0), lty = 2) +
  common_theme +
  xlim(
    -max(abs(correlation_df$Std_Coefficient)) * 1.1,
    max(abs(correlation_df$Std_Coefficient)) * 1.1
  ) +
  ggtitle("C: Correlation") +
  scale_color_manual(values = performance_colors) +
  scale_fill_manual(values = performance_colors) +
  labs(
    color = "Observation Method",
    fill = "Observation Method",
    x = "Estimated Effect",
    y = NULL
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  annotate(
    "text",
    x = - 0.3,
    y = length(parameter_labels) + 0.5,
    label = "less correlated",
    size = 3
  ) +
  annotate(
    "text",
    x = 0.3,
    y = length(parameter_labels) + 0.5,
    label = "more correlated",
    size = 3
  )

# Combined plot
p_model_plot <- ggarrange(
  accuracy_plot,
  precision_plot,
  correlation_plot,
  ncol = 3,
  nrow = 1,
  legend = "bottom",
  common.legend = TRUE,
  widths = c(0.9, 0.6, 0.6)
)

## ---- Save plots ----

ggsave(
  "Accuracy Impact.jpg",
  accuracy_plot,
  dpi = 300,
  width = 10,
  height = 10
)

ggsave(
  "Precision Impact.jpg",
  precision_plot,
  dpi = 300,
  width = 10,
  height = 10
)

ggsave(
  "Bias Impact.jpg",
  bias_plot,
  dpi = 300,
  width = 10,
  height = 10
)

ggsave(
  "Correlation Impact.jpg",
  correlation_plot,
  dpi = 300,
  width = 10,
  height = 10
)

ggsave(
  "Combined Impact.jpg",
  p_model_plot,
  dpi = 300,
  width = 12,
  height = 8
)


# Decision Tree -----------------------------------------------------------



## Accuracy ----------------------------------------------------------------


# Add a new column 'scan_better' to the accuracy_aggregate dataframe
all_accuracy_sum <-
  all_accuracy %>%
  group_by(Run_ID) %>%
  summarise(
    Accuracy_focal = mean(Accuracy_focal),
    Accuracy_scan = mean(Accuracy_scan),
    n_days = mean(n_days),
    group_size = mean(group_size),
    p_terrain_visibility = mean(p_terrain_visibility),
    p_behavior_visibility = mean(p_behavior_visibility),
    mean_events = mean(mean_events),
    behavior_duration = mean(behavior_duration),
    focal_duration_min = mean(focal_duration_min),
    focal_break_time_min = mean(focal_break_time_min),
    scan_obsTime_perID = mean(scan_obsTime_perID),
    scan_break_time_min = mean(scan_break_time_min)
  ) %>%
  ungroup() %>%
  mutate(scan_better = if_else(Accuracy_scan < Accuracy_focal, 'ScanBetter', 'FocalBetter')) %>%
  mutate(scan_better = if_else(abs(Accuracy_scan - Accuracy_focal) <= 1, 'Same', scan_better))

# Fit a decision tree to predict Outcome based on the predictors
accuracy_tree <- rpart(
  scan_better ~
    n_days +
    group_size +
    p_behavior_visibility +
    p_terrain_visibility +
    mean_events +
    focal_duration_min +
    focal_break_time_min +
    behavior_duration +
    scan_obsTime_perID +
    scan_break_time_min,
  data = all_accuracy_sum,
  method = "class"
)

# Plot the decision tree for interpretation

png(file = "Accuracy Tree.png",
    width = 1000,
    height = 1000)
rpart.plot(
  accuracy_tree,
  type = 2,
  extra = 104,
  clip.facs = TRUE,
  box.palette = list(
    performance_colors["Focal Follows"],
    'grey',
    performance_colors["Group Scan"]
  ),
  fallen.leaves = FALSE
)
dev.off()

# Print the tree model's summary
png(file = "Accuracy VIP.png",
    width = 500,
    height = 500)
vip(
  accuracy_tree,
  num_features = 40,
  horizontal = TRUE,
  geom = 'point'
) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma(), transform = 'log10')
dev.off()

# Predict the outcome on the training data
predicted <- predict(accuracy_tree, all_accuracy_sum, type = "class")

# Generate a confusion matrix
accuracy_conf_matrix <- confusionMatrix(as.factor(predicted),
                                        as.factor(all_accuracy_sum$scan_better))


## Correlation ---------------------------------------------------------------

# Add a new column 'focal_better' to the correlation_aggregate dataframe

all_correlation_sum <-
  all_correlation %>%
  group_by(Run_ID) %>%
  summarise(
    Correlation_focal = mean(Correlation_focal),
    Correlation_scan = mean(Correlation_scan),
    Correlation_approaches = mean(Correlation_approaches),
    n_days = mean(n_days),
    group_size = mean(group_size),
    p_terrain_visibility = mean(p_terrain_visibility),
    p_behavior_visibility = mean(p_behavior_visibility),
    mean_events = mean(mean_events),
    behavior_duration = mean(behavior_duration),
    focal_duration_min = mean(focal_duration_min),
    focal_break_time_min = mean(focal_break_time_min),
    scan_obsTime_perID = mean(scan_obsTime_perID),
    scan_break_time_min = mean(scan_break_time_min)
  ) %>%
  ungroup() %>%
  mutate(focal_better = if_else(Correlation_focal > Correlation_scan, 'FocalBetter', 'ScanBetter'))# %>%
#mutate(focal_better = if_else((Correlation_approaches > 0.9)|(abs(Correlation_focal - Correlation_scan) <= 0.05), 'Same', focal_better))

# Fit a decision tree to predict Outcome based on the predictors

correlation_tree <- rpart(
  focal_better ~
    n_days +
    group_size +
    p_behavior_visibility +
    p_terrain_visibility +
    mean_events +
    focal_duration_min +
    focal_break_time_min +
    behavior_duration +
    scan_obsTime_perID +
    scan_break_time_min,
  data = all_correlation_sum,
  method = "class"
)

# Plot the decision tree for interpretation

png(file = "Correlation Tree.png",
    width = 1000,
    height = 1000)
rpart.plot(
  correlation_tree,
  type = 2,
  extra = 104,
  clip.facs = TRUE,
  fallen.leaves = FALSE
)
dev.off()

# Print the tree model's summary

png(file = "Correlation VIP.png",
    width = 500,
    height = 500)
vip(
  correlation_tree,
  num_features = 40,
  horizontal = TRUE,
  geom = 'point'
) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma(), transform = 'log10')
dev.off()

# Predict the outcome on the training data
predicted <- predict(correlation_tree, all_correlation_sum, type = "class")

# Generate a confusion matrix
correlation_conf_matrix <- confusionMatrix(as.factor(predicted),
                                           as.factor(all_correlation_sum$focal_better))



# Case studies ------------------------------------------------------------

# Comparison behaviours ---------------------------------------------------

# Case 1: Different behaviours in a very large monkey group ------------------------------------------------------------------
### Group of 50 monkeys, high visibility, visible/short/rare, visible/long/common, poorly visible/short/rare

aggression <- c(5, 3, 0.9)
threat <- c(5, 3, 0.3)
grooming <- c(10, 60, 0.9)

# create individual events
n_aggression <- round(abs(rnorm(50, aggression[1], 3)))
n_grooming <- round(abs(rnorm(50, grooming[1], 3)))
n_threat <- round(abs(rnorm(50, threat[1], 3)))
n_aggression[n_aggression == 0] = 1
n_grooming[n_grooming == 0] = 1
n_threat[n_threat == 0] = 1


plan(multicore, workers = 10)

iterate_simulations <-
  function(n_events,
           mean_events,
           behavior_duration,
           behaviour_visibility) {
    future_map(
      .options = furrr_options(seed = 1234),
      1:20,
      ~ degree_simulation(
        n_days = 200,
        n_hours = 7,
        # set at 7
        group_size = 50,
        p_behavior_visibility = behaviour_visibility,
        p_terrain_visibility = 0.5,
        mean_events = mean_events,
        sd_events = 3,
        n_events = n_events,
        # as calculated above
        behavior_duration = behavior_duration,
        focal_duration_min = 15,
        focal_break_time_min = 1,
        scan_obsTime_perID = 3,
        scan_break_time_min = 5
      )
    )
  }

precision_and_accuracy <- function(simulation_iteration) {
  # calculate precision and accuracy for scans and focal follows (functions specified in simulation_functions.R)
  precision_focal_prop <-
    precision_perID(simulation_runs = simulation_iteration, observed_data = 'focal_prop_perID')
  precision_scan_prop <-
    precision_perID(simulation_runs = simulation_iteration, observed_data = 'scan_prop_perID')
  
  accuracy_focal_prop <-
    accuracy_perID(
      simulation_runs = simulation_iteration,
      true_data = 'true_prop_behav_perID',
      observed_data = 'focal_prop_perID'
    )
  accuracy_scan_prop <-
    accuracy_perID(
      simulation_runs = simulation_iteration,
      true_data = 'true_prop_behav_perID',
      observed_data = 'scan_prop_perID'
    )
  
  # put all the precisions together with the parameter information for subsequent plotting
  
  precision_frame <- data.frame(
    CV = c(precision_focal_prop, precision_scan_prop),
    observed_data = c(
      # whether focal continuous or group time sampling
      rep(
        'focal continuous sampling proportion',
        length(precision_focal_prop)
      ),
      rep(
        'group time sampling proportion',
        length(precision_scan_prop)
      )
    )
  )
  # add the simulation parameters to every row
  precision_frame <- cbind(precision_frame, data.frame(simulation_iteration[[1]][1:13][-7]))
  
  # put all the accuracies together with the parameter information for subsequent plotting
  accuracy_frame <- data.frame(
    mean_squared_error = c(# mean squared errors
      accuracy_focal_prop, accuracy_scan_prop),
    observed_data = c(
      # focal continuous or group time sampling
      rep(
        'focal continuous sampling proportion',
        length(accuracy_focal_prop)
      ),
      rep('group time sampling proportion', length(accuracy_scan_prop))
    )
  )
  # add the simulation parameters to every row
  accuracy_frame <- cbind(accuracy_frame, data.frame(simulation_iteration[[1]][1:13][-7]))
  
  cor_frame <-
    data.frame(cor_true_scan = sapply(simulation_iteration, function(x) cor(x$scan_prop_results, x$true_prop_behav_perID)),
               cor_true_focal = sapply(simulation_iteration, function(x) cor(x$focal_prop_results, x$true_prop_behav_perID)),
               cor_scan_focal = sapply(simulation_iteration, function(x) cor(x$focal_prop_results, x$scan_prop_results)))
  
  cor_frame <- cbind(cor_frame,
                     data.frame(simulation_iteration[[1]][1:13][-7]))
  
  
  return(list(accuracy_frame = accuracy_frame, precision_frame = precision_frame, cor_frame = cor_frame))
}


simulation_iteration_grooming <-
  iterate_simulations(
    n_events = n_grooming,
    mean_events = grooming[1],
    behavior_duration = grooming[2],
    behaviour_visibility = grooming[3]
  )
simulation_iteration_aggression <-
  iterate_simulations(
    n_events = n_aggression,
    mean_events = aggression[1],
    behavior_duration = aggression[2],
    behaviour_visibility = aggression[3]
  )
simulation_iteration_threat <-
  iterate_simulations(
    n_events = n_threat,
    mean_events = threat[1],
    behavior_duration = threat[2],
    behaviour_visibility = threat[3]
  )


pa_grooming <- precision_and_accuracy(simulation_iteration_grooming)
pa_aggression <- precision_and_accuracy(simulation_iteration_aggression)
pa_threat <- precision_and_accuracy(simulation_iteration_threat)

pa_grooming$accuracy_frame$condition = 'long, common, visible'
pa_aggression$accuracy_frame$condition = 'short, less common, visible'
pa_threat$accuracy_frame$condition = 'short, less common, less visible'

pa_grooming$precision_frame$condition = 'long, common, visible'
pa_aggression$precision_frame$condition = 'short, less common, visible'
pa_threat$precision_frame$condition = 'short, less common, less visible'

pa_grooming$cor_frame$condition = 'long, common, visible'
pa_aggression$cor_frame$condition = 'short, less common, visible'
pa_threat$cor_frame$condition = 'short, less common, less visible'

accuracies <- rbind(
  pa_grooming$accuracy_frame,
  pa_aggression$accuracy_frame,
  pa_threat$accuracy_frame
)


precisions <- rbind(
  pa_grooming$precision_frame,
  pa_aggression$precision_frame,
  pa_threat$precision_frame
)


corr <- rbind(
  pa_grooming$cor_frame,
  pa_aggression$cor_frame,
  pa_threat$cor_frame
) %>% pivot_longer(cols =
                     c(cor_true_scan, cor_true_focal),
                   names_to = 'observed_data',
                   values_to = 'correlation') %>%
  # rename values in 'observed_data' column to fit the other datasets
  mutate(observed_data = if_else(observed_data == 'cor_true_scan', 'group time sampling proportion', 'focal continuous sampling proportion'))


p_accuracies_large <-
  ggplot(accuracies,
         aes(x = mean_squared_error, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  common_theme +
  xlim(0, max(accuracies$mean_squared_error) + 0.1) +
  ggtitle('Accuracies: Large group') +
  xlab('Mean Squared Error') +
  ylab('Behaviour') +
  scale_fill_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  )

p_precisions_large <-
  ggplot(precisions, aes(x = CV, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  common_theme +
  xlim(0, max(precisions$CV) + 0.1) +
  ggtitle('Precisions: Large group') +
  xlab('CV') +
  ylab(NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_fill_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  )

p_correlation_large <-
  ggplot(corr, aes(x = correlation, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  common_theme +
  xlim(0, 1) +
  ggtitle('Correlation: Large group') +
  xlab('Correlation') +
  ylab(NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_fill_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  )


ggsave(
  "Case 1 Accuracies.jpg",
  p_accuracies_large,
  dpi = 300,
  width = 10,
  height = 5
)

ggsave(
  "Case 1 Precisions.jpg",
  p_precisions_large,
  dpi = 300,
  width = 10,
  height = 5
)

ggsave(
  "Case 1 Correlation.jpg",
  p_correlation_large,
  dpi = 300,
  width = 10,
  height = 5
)

ggsave(
  "Case 1 All.jpg",
  ggarrange(p_accuracies_large +
              ggtitle('A: Accuracy'),
            p_precisions_large +
              ggtitle('B: Precision'),
            p_correlation_large +
              ggtitle('C: Correlation'), 
            ncol=3, 
            nrow=1,
            widths = c(1.1, 0.6, 0.6), 
            common.legend = TRUE, legend="bottom"),
  dpi = 300,
  width = 12,
  height = 8
)


# Case 2: Grooming, scan by group size ------------------------------------

large_frequent <- c(90, 10)
medium_frequent <- c(50, 10)
small_frequent <- c(15, 10)

large_rare <- c(90, 5)
medium_rare <- c(50, 5)
small_rare <- c(15, 5)

# create individual events (frequent only)
n_large_frequent <- round(abs(rnorm(large_frequent[1], large_frequent[2], 3)))
n_medium_frequent <- round(abs(rnorm(medium_frequent[1], medium_frequent[2], 3)))
n_small_frequent <- round(abs(rnorm(small_frequent[1], small_frequent[2], 3)))

n_large_rare <- round(abs(rnorm(large_rare[1], large_rare[2], 3)))
n_medium_rare <- round(abs(rnorm(medium_rare[1], medium_rare[2], 3)))
n_small_rare <- round(abs(rnorm(small_rare[1], small_rare[2], 3)))


plan(multicore, workers = 10)

iterate_simulations <-
  function(group_size,
           behaviour_visibility,
           behaviour_duration,
           mean_events,
           n_events) {
    future_map(
      .options = furrr_options(seed = 1234),
      1:20,
      ~ degree_simulation(
        n_days = 200,
        n_hours = 7,
        # set at 7
        group_size = group_size,
        p_behavior_visibility = behaviour_visibility,
        p_terrain_visibility = 0.5,
        mean_events = mean_events,
        sd_events = 3,
        n_events = n_events,
        # as calculated above
        behavior_duration = behaviour_duration,
        focal_duration_min = 15,
        focal_break_time_min = 1,
        scan_obsTime_perID = 3,
        scan_break_time_min = 5
      )
    )
  }

# simulations (frequent only)
simulation_iteration_large_frequent <-
  iterate_simulations(group_size = 90, 
                      behaviour_visibility = 0.9, 
                      behaviour_duration = 60, 
                      mean_events =  10,
                      n_events = n_large_frequent)
simulation_iteration_medium_frequent <-
  iterate_simulations(group_size = 50,
                      behaviour_visibility = 0.9, 
                      behaviour_duration = 60, 
                      mean_events =  10,
                      n_events = n_medium_frequent)
simulation_iteration_small_frequent <-
  iterate_simulations(group_size = 15,
                      behaviour_visibility = 0.9,
                      behaviour_duration = 60,
                      mean_events =  10,
                      n_events = n_small_frequent)
simulation_iteration_large_rare <-
  iterate_simulations(group_size = 90, 
                      behaviour_visibility = 0.9, 
                      behaviour_duration = 3, 
                      mean_events =  5,
                      n_events = n_large_rare)
simulation_iteration_medium_rare <-
  iterate_simulations(group_size = 50,
                      behaviour_visibility = 0.9,
                      behaviour_duration = 3,
                      mean_events =  5,
                      n_events = n_medium_rare)
simulation_iteration_small_rare <-
  iterate_simulations(group_size = 15,
                      behaviour_visibility = 0.9,
                      behaviour_duration = 3,
                      mean_events =  5,
                      n_events = n_small_rare)


# precision/accuracy/correlation (frequent only)
pa_large_frequent <- precision_and_accuracy(simulation_iteration_large_frequent)
pa_medium_frequent <- precision_and_accuracy(simulation_iteration_medium_frequent)
pa_small_frequent <- precision_and_accuracy(simulation_iteration_small_frequent)
pa_large_rare <- precision_and_accuracy(simulation_iteration_large_rare)
pa_medium_rare <- precision_and_accuracy(simulation_iteration_medium_rare)
pa_small_rare <- precision_and_accuracy(simulation_iteration_small_rare)


# condition labels (frequent only)
pa_large_frequent$accuracy_frame$condition = "90 Individuals, Grooming"
pa_medium_frequent$accuracy_frame$condition = "50 Individuals, Grooming"
pa_small_frequent$accuracy_frame$condition = "15 Individuals, Grooming"
pa_large_rare$accuracy_frame$condition = "90 Individuals, Aggression"
pa_medium_rare$accuracy_frame$condition = "50 Individuals, Aggression"
pa_small_rare$accuracy_frame$condition = "15 Individuals, Aggression"

pa_large_frequent$precision_frame$condition = "90 Individuals, Grooming"
pa_medium_frequent$precision_frame$condition = "50 Individuals, Grooming"
pa_small_frequent$precision_frame$condition = "15 Individuals, Grooming"
pa_large_rare$precision_frame$condition = "90 Individuals, Aggression"
pa_medium_rare$precision_frame$condition = "50 Individuals, Aggression"
pa_small_rare$precision_frame$condition = "15 Individuals, Aggression"


pa_large_frequent$cor_frame$condition = "90 Individuals, Grooming"
pa_medium_frequent$cor_frame$condition = "50 Individuals, Grooming"
pa_small_frequent$cor_frame$condition = "15 Individuals, Grooming"
pa_large_rare$cor_frame$condition = "90 Individuals, Aggression"
pa_medium_rare$cor_frame$condition = "50 Individuals, Aggression"
pa_small_rare$cor_frame$condition = "15 Individuals, Aggression"


accuracies_grooming <- rbind(
  pa_large_frequent$accuracy_frame,
  pa_medium_frequent$accuracy_frame,
  pa_small_frequent$accuracy_frame
)

accuracies_aggression <- rbind(
  pa_large_rare$accuracy_frame,
  pa_medium_rare$accuracy_frame,
  pa_small_rare$accuracy_frame
)

precisions_grooming <- rbind(
  pa_large_frequent$precision_frame,
  pa_medium_frequent$precision_frame,
  pa_small_frequent$precision_frame
)

precisions_aggression <- rbind(
  pa_large_rare$precision_frame,
  pa_medium_rare$precision_frame,
  pa_small_rare$precision_frame
)

corrs_grooming <- rbind(
  pa_large_frequent$cor_frame,
  pa_medium_frequent$cor_frame,
  pa_small_frequent$cor_frame
) %>%
  pivot_longer(
    cols = c(cor_true_scan, cor_true_focal),
    names_to = "observed_data",
    values_to = "correlation"
  ) %>%
  # rename values in 'observed_data' column to fit the other datasets
  mutate(
    observed_data = if_else(
      observed_data == "cor_true_scan",
      "group time sampling proportion",
      "focal continuous sampling proportion"
    )
  )

corrs_aggression <- rbind(
  pa_large_rare$cor_frame,
  pa_medium_rare$cor_frame,
  pa_small_rare$cor_frame
) %>%
  pivot_longer(
    cols = c(cor_true_scan, cor_true_focal),
    names_to = "observed_data",
    values_to = "correlation"
  ) %>%
  # rename values in 'observed_data' column to fit the other datasets
  mutate(
    observed_data = if_else(
      observed_data == "cor_true_scan",
      "group time sampling proportion",
      "focal continuous sampling proportion"
    )
  )


# Grooming
p_accuracies_group_size_grooming <-
  ggplot(
    accuracies_grooming,
    aes(x = mean_squared_error, y = condition, fill = observed_data)
  ) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  common_theme +
  xlim(0, max(accuracies_grooming$mean_squared_error) + 0.1) +
  ggtitle("Accuracies") +
  xlab("Mean Squared Error") +
  ylab("Condition") +
  scale_fill_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  )

p_precisions_group_size_grooming <-
  ggplot(precisions_grooming, aes(x = CV, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  common_theme +
  xlim(0, max(precisions_grooming$CV) + 0.1) +
  ggtitle("Precisions") +
  xlab("CV") +
  ylab(NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_fill_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  )

p_correlation_group_size_grooming <-
  ggplot(corrs_grooming, aes(x = correlation, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  common_theme +
  xlim(0, 1) +
  ggtitle("Correlation") +
  xlab("Correlation") +
  ylab(NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_fill_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  )

# Aggression
p_accuracies_group_size_aggression <-
  ggplot(
    accuracies_aggression,
    aes(x = mean_squared_error, y = condition, fill = observed_data)
  ) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  common_theme +
  xlim(0, max(accuracies_aggression$mean_squared_error) + 0.1) +
  ggtitle("Accuracies") +
  xlab("Mean Squared Error") +
  ylab("Condition") +
  scale_fill_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  )

p_precisions_group_size_aggression <-
  ggplot(precisions_aggression, aes(x = CV, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  common_theme +
  xlim(0, max(precisions_aggression$CV) + 0.1) +
  ggtitle("Precisions") +
  xlab("CV") +
  ylab(NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_fill_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  )

p_correlation_group_size_aggression <-
  ggplot(corrs_aggression, aes(x = correlation, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  common_theme +
  xlim(0, 1) +
  ggtitle("Correlation") +
  xlab("Correlation") +
  ylab(NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_fill_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  )

ggsave(
  "Case 2 All Grooming.jpg",
  ggarrange(p_accuracies_group_size_grooming +
              ggtitle('A: Accuracy'),
            p_precisions_group_size_grooming +
              ggtitle('B: Precision'),
            p_correlation_group_size_grooming +
              ggtitle('C: Correlation'), 
            ncol=3, 
            nrow=1,
            widths = c(1.1, 0.6, 0.6),  
            common.legend = TRUE, legend="bottom"),
  dpi = 300,
  width = 12,
  height = 8
)

ggsave(
  "Case 2 All.jpg",
  ggarrange(p_accuracies_group_size_grooming +
              ggtitle('A: Accuracy'),
            p_precisions_group_size_grooming +
              ggtitle('B: Precision'),
            p_correlation_group_size_grooming +
              ggtitle('C: Correlation'),
            p_accuracies_group_size_aggression +
              ggtitle('D: Accuracy'),
            p_precisions_group_size_aggression +
              ggtitle('E: Precision'),
            p_correlation_group_size_aggression +
              ggtitle('F: Correlation'), 
            ncol=3, 
            nrow=2,
            widths = c(1.1, 0.6, 0.6,1.1, 0.6, 0.6),  
            common.legend = TRUE, legend="bottom"),
  dpi = 300,
  width = 12,
  height = 12
)

ggsave(
  "Case 2 All Aggression.jpg",
  ggarrange(p_accuracies_group_size_aggression +
              ggtitle('D: Accuracy'),
            p_precisions_group_size_aggression +
              ggtitle('E: Precision'),
            p_correlation_group_size_aggression +
              ggtitle('F: Correlation'), 
            ncol=3, 
            nrow=1,
            widths = c(1.1, 0.6, 0.6),  
            common.legend = TRUE, legend="bottom"),
  dpi = 300,
  width = 12,
  height = 8
)

# Case 3: Study Duration --------------------------------------------------

grooming_30 <- c(30, 10, 60, 0.9)
grooming_90 <- c(90, 10, 60, 0.9)
grooming_180 <- c(180, 10, 60, 0.9)
grooming_730 <- c(730, 10, 60, 0.9)


# create individual events
n_grooming_30 <- round(abs(rnorm(50, grooming_30[2], 3)))
n_grooming_90 <- round(abs(rnorm(50, grooming_90[2], 3)))
n_grooming_180 <- round(abs(rnorm(50, grooming_180[2], 3)))
n_grooming_730 <- round(abs(rnorm(50, grooming_730[2], 3)))

plan(multicore, workers = 10)

iterate_simulations <-
  function(days,
           n_events,
           mean_events,
           behavior_duration,
           behaviour_visibility) {
    future_map(
      .options = furrr_options(seed = 1234),
      1:20,
      ~ degree_simulation(
        n_days = days,
        n_hours = 7,
        # set at 7
        group_size = 50,
        p_behavior_visibility = behaviour_visibility,
        p_terrain_visibility = 0.5,
        mean_events = mean_events,
        sd_events = 3,
        n_events = n_events,
        # as calculated above
        behavior_duration = behavior_duration,
        focal_duration_min = 15,
        focal_break_time_min = 1,
        scan_obsTime_perID = 3,
        scan_break_time_min = 5
      )
    )
  }


simulation_iteration_grooming_30 <-
  iterate_simulations(grooming_30[1], n_grooming_30, grooming_30[2], grooming_30[3], grooming_30[4])
simulation_iteration_grooming_90 <-
  iterate_simulations(grooming_90[1], n_grooming_90, grooming_90[2], grooming_90[3], grooming_90[4])
simulation_iteration_grooming_180 <-
  iterate_simulations(grooming_180[1], n_grooming_180, grooming_180[2], grooming_180[3], grooming_180[4])
simulation_iteration_grooming_730 <-
  iterate_simulations(grooming_730[1], n_grooming_730, grooming_730[2], grooming_730[3], grooming_730[4])


pa_grooming_30 <- precision_and_accuracy(simulation_iteration_grooming_30)
pa_grooming_90 <- precision_and_accuracy(simulation_iteration_grooming_90)
pa_grooming_180 <- precision_and_accuracy(simulation_iteration_grooming_180)
pa_grooming_730 <- precision_and_accuracy(simulation_iteration_grooming_730)

pa_grooming_30$accuracy_frame$condition = '30 Days'
pa_grooming_90$accuracy_frame$condition = '90 Days'
pa_grooming_180$accuracy_frame$condition = '180 Days'
pa_grooming_730$accuracy_frame$condition = '730 Days'

pa_grooming_30$precision_frame$condition = '30 Days'
pa_grooming_90$precision_frame$condition = '90 Days'
pa_grooming_180$precision_frame$condition = '180 Days'
pa_grooming_730$precision_frame$condition = '730 Days'

pa_grooming_30$cor_frame$condition = '30 Days'
pa_grooming_90$cor_frame$condition = '90 Days'
pa_grooming_180$cor_frame$condition = '180 Days'
pa_grooming_730$cor_frame$condition = '730 Days'


accuracies_grooming <- rbind(
  pa_grooming_30$accuracy_frame,
  pa_grooming_90$accuracy_frame,
  pa_grooming_180$accuracy_frame,
  pa_grooming_730$accuracy_frame
)

precisions_grooming <- rbind(
  pa_grooming_30$precision_frame,
  pa_grooming_90$precision_frame,
  pa_grooming_180$precision_frame,
  pa_grooming_730$precision_frame
)


corr_grooming <- rbind(
  pa_grooming_30$cor_frame,
  pa_grooming_90$cor_frame,
  pa_grooming_180$cor_frame,
  pa_grooming_730$cor_frame
) %>% pivot_longer(cols =
                     c(cor_true_scan, cor_true_focal),
                   names_to = 'observed_data',
                   values_to = 'correlation') %>%
  # rename values in 'observed_data' column to fit the other datasets
  mutate(observed_data = if_else(observed_data == 'cor_true_scan',
                                 'group time sampling proportion',
                                 'focal continuous sampling proportion'))


p_accuracies_duration_grooming <-
  ggplot(accuracies_grooming,
         aes(x = mean_squared_error, y = as.factor(n_days), fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  common_theme +
  xlim(0, max(accuracies_grooming$mean_squared_error) + 0.1) +
  ggtitle('Accuracies') +
  xlab('Mean Squared Error') +
  ylab('Days') +
  scale_fill_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_y_discrete(limits = rev(levels(as.factor(accuracies_grooming$n_days))))

p_precisions_duration_grooming <-
  ggplot(precisions_grooming, aes(x = CV, y = as.factor(n_days), fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  common_theme +
  xlim(0, max(precisions_grooming$CV) + 0.1) +
  ggtitle('Precisions') +
  xlab('CV') +
  ylab(NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_fill_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_y_discrete(limits = rev(levels(as.factor(precisions_grooming$n_days))))


p_correlation_duration_grooming <-
  ggplot(corr_grooming, aes(x = correlation, y = as.factor(n_days), fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  common_theme +
  xlim(0, 1) +
  ggtitle('Correlation') +
  xlab('Correlation') +
  ylab(NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_fill_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "focal continuous sampling proportion" = "#5DA8E4",
      "group time sampling proportion" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_y_discrete(limits = rev(levels(as.factor(corr_grooming$n_days))))




ggsave(
  "Case 3 All Grooming.jpg",
  ggarrange(p_accuracies_duration_grooming +
              ggtitle('A: Accuracy'),
            p_precisions_duration_grooming +
              ggtitle('B: Precision'),
            p_correlation_duration_grooming +
              ggtitle('C: Correlation'), 
            ncol=3, 
            nrow=1, 
            widths = c(1.1, 0.6, 0.6), common.legend = TRUE, legend="bottom"),
  dpi = 300,
  width = 12,
  height = 8
)

