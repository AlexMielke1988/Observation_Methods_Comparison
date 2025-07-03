


# Libraries ---------------------------------------------------------------

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# # load packages

library(tidyverse)
library(purrr)
library(future)
library(furrr)
library(lme4)
library(ggplot2)
library(sjPlot)
library(gridExtra)
library(rpart)
library(rpart.plot)
library(vip)
library(scales)
library(caret)
library(ggridges)
library(brms)
library(loo)
library(ggpubr)
library(scales)
library(effectsize)
setwd('GitHub/ReviewBurner/Observation_Methods_Comparisons/')
source("Simulation Scripts/simulation_functions.r")
# Load data ---------------------------------------------------------------

## The different aspects of the simulations (accuracy, precision, correlation, bias) 
## are stored in different objects, as are the parameters underlying each simulation.
## We call them all up and then attach the simulation parameter values
load("~/all_accuracy.RData")
load("~/all_precision.RData")
load("~/all_correlation.RData")
load("~/all_bias.RData")
load("~/all_parameters.RData")

all_accuracy <- all_accuracy %>% 
  left_join(all_parameters, by = 'Run_ID')

all_precision <- all_precision %>% 
  left_join(all_parameters, by = 'Run_ID')

all_bias <- all_bias %>% 
  left_join(all_parameters, by = 'Run_ID')

all_correlation <- all_correlation %>% 
  left_join(all_parameters, by = 'Run_ID')


# Plot densities of each approach -----------------------------------------

# Accuracy

accuracy_density <- all_accuracy %>%
  filter(scan_break_time_min <= 60) %>% 
  select(Accuracy_focal, Accuracy_scan) %>%
  pivot_longer(cols = everything(),
               names_to = "Category",
               values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Category, color = Category)) +
  geom_histogram(alpha = 0.3,
                 bins = 200,
                 position = 'identity') +
  geom_vline(
    data = all_accuracy %>%
      select(Accuracy_focal, Accuracy_scan) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Category",
        values_to = "Value"
      ) %>%
      group_by(Category) %>%
      summarize(Median = median(Value)),
    aes(xintercept = Median, color = Category),
    linetype = "dashed",
    size = 1
  ) +
  labs(title = "A: Accuracy", 
       x = "RMSE", 
       y = "Frequency") +
  theme_minimal() +
  # scale x-axis as log
  #add legend for fills
  theme(legend.position = "right") +
  scale_fill_manual(
    values = c(
      "Accuracy_focal" = "#5DA8E4",
      "Accuracy_scan" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "Accuracy_focal" = "#5DA8E4",
      "Accuracy_scan" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_x_continuous(labels = label_comma(), 
                     transform = 'log10') +
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )


# Precision

precision_density <- all_precision %>%
  filter(scan_break_time_min <= 60) %>% 
  filter(Precision_focal != 0 & Precision_scan != 0) %>%
  filter(!is.na(Precision_focal) & !is.na(Precision_scan)) %>%
  select(Precision_focal, Precision_scan) %>%
  pivot_longer(cols = everything(),
               names_to = "Category",
               values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Category, color = Category)) +
  geom_histogram(alpha = 0.3,
                 bins = 200,
                 position = 'identity') +
  geom_vline(
    data = all_precision %>%
      filter(Precision_focal != 0 &
               Precision_scan != 0) %>%
      filter(!is.na(Precision_focal) &
               !is.na(Precision_scan)) %>%
      select(Precision_focal, Precision_scan) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Category",
        values_to = "Value"
      ) %>%
      group_by(Category) %>%
      summarize(Median = median(Value)),
    aes(xintercept = Median, color = Category),
    linetype = "dashed",
    size = 0.8
  ) +
  labs(title = "B: Precision", x = "CV", y = "Frequency") +
  theme_minimal() +
  #add legend for fills
  theme(legend.position = "right") +
  scale_fill_manual(
    values = c(
      "Precision_focal" = "#5DA8E4",
      "Precision_scan" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c(
      "Precision_focal" = "#5DA8E4",
      "Precision_scan" = "#EF9401"
    ),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_x_continuous(labels = label_comma(), 
                     transform = 'log10')+
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )

# Bias

bias_density <- all_bias %>%
  filter(scan_break_time_min <= 60) %>% 
  select(Bias_focal, Bias_scan) %>%
  pivot_longer(cols = everything(),
               names_to = "Category",
               values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Category, color = Category)) +
  geom_histogram(alpha = 0.3,
                 bins = 200,
                 position = 'identity') +
  geom_vline(
    data = all_bias %>%
      select(Bias_focal, Bias_scan) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Category",
        values_to = "Value"
      ) %>%
      group_by(Category) %>%
      summarize(Median = median(Value)),
    aes(xintercept = Median, color = Category),
    linetype = "dashed",
    size = 0.8
  ) +
  labs(title = "C: Bias", x = "Bias", y = "Frequency") +
  theme_minimal() +
  xlim(-1.1, 3) +
  #add legend for fills
  theme(legend.position = "right") +
  scale_fill_manual(
    values = c("Bias_focal" = "#5DA8E4", 
               "Bias_scan" = "#EF9401"),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c("Bias_focal" = "#5DA8E4", 
               "Bias_scan" = "#EF9401"),
    labels = c("Focal Follows", "Group Scans")
  )+
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )


# Correlation

correlation_density <- all_correlation %>%
  filter(scan_break_time_min <= 60) %>% 
  select(Correlation_focal, Correlation_scan) %>%
  pivot_longer(cols = everything(),
               names_to = "Category",
               values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Category, color = Category)) +
  geom_histogram(alpha = 0.3,
                 bins = 200,
                 position = 'identity') +
  geom_vline(
    data = all_correlation %>%
      select(Correlation_focal, Correlation_scan) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Category",
        values_to = "Value"
      ) %>%
      group_by(Category) %>%
      summarize(Median = median(Value)),
    aes(xintercept = Median, color = Category),
    linetype = "dashed",
    size = 0.8
  ) +
  labs(title = "C: Correlation with True", x = "Correlation Coefficient", y = "Frequency") +
  theme_minimal() +
  #add legend for fills
  theme(legend.position = "right") +
  scale_fill_manual(
    values = c("Correlation_focal" = "#5DA8E4", 
               "Correlation_scan" = "#EF9401"),
    labels = c("Focal Follows", "Group Scans")
  ) +
  scale_color_manual(
    values = c("Correlation_focal" = "#5DA8E4", 
               "Correlation_scan" = "#EF9401"),
    labels = c("Focal Follows", "Group Scans")
  )+
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )

# Save plots

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
  width = 10,
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

## Accuracy ---------------------------------------------------------------

# Accuracy - Focal

## Accuracy can be analysed with a log-normal distribution

### prepare data for analysis
z_data_accuracy <- all_accuracy %>%
  mutate_at(
    c(
      "n_days",
      "group_size",
      "p_terrain_visibility",
      "p_behavior_visibility",
      "mean_events",
      "behavior_duration",
      "focal_duration_min",
      'focal_break_time_min',
      "scan_obsTime_perID",
      "scan_break_time_min"
    ),
    ~ (scale(.) %>% as.vector)
  )

### fit a model

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

# plot model output
accuracy_focal_plot <- plot_model(
  mdl_accuracy_focal,
  sort.est = FALSE,
  # NEED TO THINK HOW TO INTERPRET THIS EXACTLY
  vline.color = "red",
  transform = NULL,
  show.values = TRUE,
  value.offset = .3
) +
  theme_minimal() +
  ggtitle('Accuracy Focal Follows')

# Accuracy - Scan

### fit a model

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

# plot model output
accuracy_scan_plot <- plot_model(
  mdl_accuracy_scan,
  sort.est = FALSE,
  # NEED TO THINK HOW TO INTERPRET THIS EXACTLY
  vline.color = "red",
  transform = NULL,
  show.values = TRUE,
  value.offset = .3
) +
  theme_minimal() +
  ggtitle('Accuracy Group Scan')


ggsave(
  "Accuracy Impact.jpg",
  grid.arrange(accuracy_focal_plot, accuracy_scan_plot, ncol = 2),
  dpi = 300,
  width = 10,
  height = 5
)


performance_colors <- c(
  "Group Scan" = "#EF9401",
  "Focal Follows" = "#5DA8E4"
)

accuracy_plot <-
  rbind(
    standardize_parameters(mdl_accuracy_scan) %>% 
      data.frame() %>% 
      mutate(performance = 'Group Scan'),
    standardize_parameters(mdl_accuracy_focal) %>% 
      data.frame() %>% 
      mutate(performance = 'Focal Follows')) %>% 
  filter(Parameter != '(Intercept)') %>% 
  ggplot(aes(y = Parameter, 
             x = Std_Coefficient, 
             color = performance, 
             fill = performance)) + 
  geom_errorbarh(aes(xmin = 0, 
                     xmax = Std_Coefficient), 
                 height = 0,
                 linewidth = 1,
                 position = position_dodge(width = .7)) +
  geom_point(size = 3,
             position = position_dodge(width = .7)) + 
  geom_text(aes(label = round(Std_Coefficient, 2)),  # Add labels
            position = position_dodge(width = 0.7),  # Align with points
            vjust = -0.6,
            color = 'black',
            size = 3) +    
  geom_vline(aes(xintercept = 0), lty = 2) +
  theme_minimal() + 
  ggtitle('Accuracy - Parameter Impact') +
  scale_color_manual(values = performance_colors) +  # Set custom colors for points
  scale_fill_manual(values = performance_colors) +   # Set custom colors for fills (if applicable) +
  labs(
    color = "Observation Method",  # Legend title for colors
    fill = "Observation Method",   # Legend title for fills (if applicable)
    x = "Standardised Estimate",
    y = "Parameter"
  )


ggsave(
  "Accuracy Impact2.jpg",
  accuracy_plot,
  dpi = 300,
  width = 10,
  height = 10
)
  
## Precision ---------------------------------------------------------------

# Precision - Focal

### prepare data for analysis

z_data_precision <- all_precision %>%
  filter(Precision_focal != 0 & Precision_scan != 0) %>%
  mutate_at(
    c(
      "n_days",
      "group_size",
      "p_terrain_visibility",
      "p_behavior_visibility",
      "mean_events",
      "behavior_duration",
      "focal_duration_min",
      'focal_break_time_min',
      "scan_obsTime_perID",
      "scan_break_time_min"
    ),
    ~ (scale(.) %>% as.vector)
  )

### fit a model

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

# plot model output
precision_focal_plot <- plot_model(
  mdl_precision_focal,
  sort.est = FALSE,
  # NEED TO THINK HOW TO INTERPRET THIS EXACTLY
  vline.color = "red",
  transform = NULL,
  show.values = TRUE,
  value.offset = .3
) +
  theme_minimal() +
  ggtitle('Precision Focal Follows')

# Precision - Scan

### fit a model

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


# plot model output
precision_scan_plot <- plot_model(
  mdl_precision_scan,
  sort.est = FALSE,
  # NEED TO THINK HOW TO INTERPRET THIS EXACTLY
  vline.color = "red",
  transform = NULL,
  show.values = TRUE,
  value.offset = .3
) +
  theme_minimal() +
  ggtitle('Precision Group Scan')

# save grid arranged plots

ggsave(
  "Precision Impact.jpg",
  grid.arrange(precision_focal_plot, precision_scan_plot, ncol = 2),
  dpi = 300,
  width = 10,
  height = 5
)


performance_colors <- c(
  "Group Scan" = "#EF9401",
  "Focal Follows" = "#5DA8E4"
)

precision_plot <-
  rbind(
    standardize_parameters(mdl_precision_scan) %>% 
      data.frame() %>% 
      mutate(performance = 'Group Scan'),
    standardize_parameters(mdl_precision_focal) %>% 
      data.frame() %>% 
      mutate(performance = 'Focal Follows')) %>% 
  filter(Parameter != '(Intercept)') %>% 
  ggplot(aes(y = Parameter, 
             x = Std_Coefficient, 
             color = performance, 
             fill = performance)) + 
  geom_errorbarh(aes(xmin = 0, 
                     xmax = Std_Coefficient), 
                 height = 0,
                 linewidth = 1,
                 position = position_dodge(width = .7)) +
  geom_point(size = 3,
             position = position_dodge(width = .7)) + 
  geom_text(aes(label = round(Std_Coefficient, 2)),  # Add labels
            position = position_dodge(width = 0.7),  # Align with points
            vjust = -0.6,
            color = 'black',
            size = 3) +    
  geom_vline(aes(xintercept = 0), lty = 2) +
  theme_minimal() + 
  ggtitle('Precision - Parameter Impact') +
  scale_color_manual(values = performance_colors) +  # Set custom colors for points
  scale_fill_manual(values = performance_colors) +   # Set custom colors for fills (if applicable) +
  labs(
    color = "Observation Method",  # Legend title for colors
    fill = "Observation Method",   # Legend title for fills (if applicable)
    x = "Standardised Estimate",
    y = "Parameter"
  )


ggsave(
  "Precision Impact2.jpg",
  precision_plot,
  dpi = 300,
  width = 10,
  height = 10
)

## Bias --------------------------------------------------------------------

# Bias - Focal

### prepare data for analysis

z_data_bias <- all_bias %>%
  mutate_at(
    c(
      "n_days",
      "group_size",
      "p_terrain_visibility",
      "p_behavior_visibility",
      "mean_events",
      "behavior_duration",
      "focal_duration_min",
      'focal_break_time_min',
      "scan_obsTime_perID",
      "scan_break_time_min"
    ),
    ~ (scale(.) %>% as.vector)
  )

### fit a model

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

# plot model output
bias_focal_plot <- plot_model(
  mdl_bias_focal,
  sort.est = FALSE,
  # NEED TO THINK HOW TO INTERPRET THIS EXACTLY
  vline.color = "red",
  transform = NULL,
  show.values = TRUE,
  value.offset = .3
) +
  theme_minimal() +
  ggtitle('Bias Focal Follows')

# Bias - Scan

### fit a model

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

# plot model output
bias_scan_plot <- plot_model(
  mdl_bias_scan,
  sort.est = FALSE,
  # NEED TO THINK HOW TO INTERPRET THIS EXACTLY
  vline.color = "red",
  transform = NULL,
  show.values = TRUE,
  value.offset = .3
) +
  theme_minimal() +
  ggtitle('Bias Group Scan')

# save grid arranged plots

ggsave(
  "Bias Impact.jpg",
  grid.arrange(bias_focal_plot, bias_scan_plot, ncol = 2),
  dpi = 300,
  width = 10,
  height = 5
)



performance_colors <- c(
  "Group Scan" = "#EF9401",
  "Focal Follows" = "#5DA8E4"
)

bias_plot <-
  rbind(
    standardize_parameters(mdl_bias_scan) %>% 
      data.frame() %>% 
      mutate(performance = 'Group Scan'),
    standardize_parameters(mdl_bias_focal) %>% 
      data.frame() %>% 
      mutate(performance = 'Focal Follows')) %>% 
  filter(Parameter != '(Intercept)') %>% 
  ggplot(aes(y = Parameter, 
             x = Std_Coefficient, 
             color = performance, 
             fill = performance)) + 
  geom_errorbarh(aes(xmin = 0, 
                     xmax = Std_Coefficient), 
                 height = 0,
                 linewidth = 1,
                 position = position_dodge(width = .7)) +
  geom_point(size = 3,
             position = position_dodge(width = .7)) + 
  geom_text(aes(label = round(Std_Coefficient, 2)),  # Add labels
            position = position_dodge(width = 0.7),  # Align with points
            vjust = -0.6,
            color = 'black',
            size = 3) +    
  geom_vline(aes(xintercept = 0), lty = 2) +
  theme_minimal() + 
  ggtitle('Bias - Parameter Impact') +
  scale_color_manual(values = performance_colors) +  # Set custom colors for points
  scale_fill_manual(values = performance_colors) +   # Set custom colors for fills (if applicable) +
  labs(
    color = "Observation Method",  # Legend title for colors
    fill = "Observation Method",   # Legend title for fills (if applicable)
    x = "Standardised Estimate",
    y = "Parameter"
  )


ggsave(
  "Bias Impact2.jpg",
  bias_plot,
  dpi = 300,
  width = 10,
  height = 10
)
## Correlation -------------------------------------------------------------

# Correlation - Focal

### prepare data for analysis

z_data_correlation <- all_correlation %>%
  filter(!is.na(Correlation_focal) & !is.na(Correlation_scan) & !is.na(Correlation_approaches)) %>%
  mutate_at(
    c(
      "n_days",
      "group_size",
      "p_terrain_visibility",
      "p_behavior_visibility",
      "mean_events",
      "behavior_duration",
      "focal_duration_min",
      'focal_break_time_min',
      "scan_obsTime_perID",
      "scan_break_time_min"
    ),
    ~ (scale(.) %>% as.vector)
  )


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

correlation_focal_plot <- plot_model(
  mdl_correlation_focal,
  sort.est = FALSE,
  # NEED TO THINK HOW TO INTERPRET THIS EXACTLY
  vline.color = "red",
  transform = NULL,
  show.values = TRUE,
  value.offset = .3
) +
  theme_minimal() +
  ggtitle('Correlation Focal Follows')


# Correlation - Scan

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

correlation_scan_plot <- plot_model(
  mdl_correlation_scan,
  sort.est = FALSE,
  # NEED TO THINK HOW TO INTERPRET THIS EXACTLY
  vline.color = "red",
  transform = NULL,
  show.values = TRUE,
  value.offset = .3
) +
  theme_minimal() +
  ggtitle('Correlation Group Scan')

# save grid arranged plots
ggsave(
  "Correlation Impact.jpg",
  grid.arrange(correlation_focal_plot, correlation_scan_plot, ncol = 2),
  dpi = 300,
  width = 10,
  height = 5
)


performance_colors <- c(
  "Group Scan" = "#EF9401",
  "Focal Follows" = "#5DA8E4"
)

correlation_plot <-
  rbind(
    standardize_parameters(mdl_correlation_scan) %>% 
      data.frame() %>% 
      mutate(performance = 'Group Scan'),
    standardize_parameters(mdl_correlation_focal) %>% 
      data.frame() %>% 
      mutate(performance = 'Focal Follows')) %>% 
  filter(Parameter != '(Intercept)') %>% 
  ggplot(aes(y = Parameter, 
             x = Std_Coefficient, 
             color = performance, 
             fill = performance)) + 
  geom_errorbarh(aes(xmin = 0, 
                     xmax = Std_Coefficient), 
                 height = 0,
                 linewidth = 1,
                 position = position_dodge(width = .7)) +
  geom_point(size = 3,
             position = position_dodge(width = .7)) + 
  geom_text(aes(label = round(Std_Coefficient, 2)),  # Add labels
            position = position_dodge(width = 0.7),  # Align with points
            vjust = -0.6,
            color = 'black',
            size = 3) +    
  geom_vline(aes(xintercept = 0), lty = 2) +
  theme_minimal() + 
  ggtitle('Correlation - Parameter Impact') +
  scale_color_manual(values = performance_colors) +  # Set custom colors for points
  scale_fill_manual(values = performance_colors) +   # Set custom colors for fills (if applicable) +
  labs(
    color = "Observation Method",  # Legend title for colors
    fill = "Observation Method",   # Legend title for fills (if applicable)
    x = "Standardised Estimate",
    y = "Parameter"
  )


ggsave(
  "Correlation Impact2.jpg",
  correlation_plot,
  dpi = 300,
  width = 10,
  height = 10
)

# Linear Model ------------------------------------------------------------

## Accuracy ---------------------------------------------------------------

# Accuracy - Focal

## Accuracy can be analysed with a log-normal distribution

### prepare data for analysis
z_data_accuracy_comparison <-  all_accuracy %>%
  group_by(Run_ID) %>%
  summarise(
    Accuracy_focal = median(Accuracy_focal),
    Accuracy_scan = median(Accuracy_scan),
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
  mutate_at(
    c(
      "n_days",
      "group_size",
      "p_terrain_visibility",
      "p_behavior_visibility",
      "mean_events",
      "behavior_duration",
      "focal_duration_min",
      'focal_break_time_min',
      "scan_obsTime_perID",
      "scan_break_time_min"
    ),
    ~ (scale(.) %>% as.vector)
  ) %>% 
  mutate(Focal_benefit = Accuracy_focal - Accuracy_scan) %>% 
  mutate(Focal_ratio = Accuracy_focal/Accuracy_scan)



# Fit a GLM with student t (heavy-tailed approximation)
# Full model formula
full_formula <- Focal_benefit ~ 
  n_days +
  group_size +
  p_terrain_visibility +
  p_behavior_visibility +
  mean_events +
  behavior_duration +
  focal_duration_min +
  focal_break_time_min +
  scan_obsTime_perID +
  scan_break_time_min

z_data_accuracy_comparison$Focal_benefit[z_data_accuracy_comparison$Focal_benefit > 1000] <- 1000
z_data_accuracy_comparison$Focal_benefit[z_data_accuracy_comparison$Focal_benefit < -1000] <- -1000

# Fit the full model
mdl_accuracy_full <- lm(
  full_formula,
  data = z_data_accuracy_comparison)

variable_importance_accuracy <- vip::vi(mdl_accuracy_full, scale = T)


# plot model output
accuracy_comparison_effects <- plot_model(
  mdl_accuracy_full,
  sort.est = FALSE,
  # NEED TO THINK HOW TO INTERPRET THIS EXACTLY
  vline.color = "red",
  transform = NULL,
  show.values = TRUE,
  value.offset = .3
) +
  theme_minimal() +
  ggtitle('Accuracy Comparison')


ggsave(
  "Accuracy Comparison.jpg",
  accuracy_comparison_effects,
  dpi = 300,
  width = 10,
  height = 5
)


## Precision ---------------------------------------------------------------


### prepare data for analysis
z_data_precision_comparison <-  all_precision %>%
  filter(!is.infinite(Precision_focal) & !is.infinite(Precision_scan)) %>% 
  group_by(Run_ID) %>%
  summarise(
    Precision_focal = median(Precision_focal),
    Precision_scan = median(Precision_scan),
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
  mutate_at(
    c(
      "n_days",
      "group_size",
      "p_terrain_visibility",
      "p_behavior_visibility",
      "mean_events",
      "behavior_duration",
      "focal_duration_min",
      'focal_break_time_min',
      "scan_obsTime_perID",
      "scan_break_time_min"
    ),
    ~ (scale(.) %>% as.vector)
  ) %>% 
  mutate(Focal_benefit = Precision_focal - Precision_scan) %>% 
  mutate(Focal_ratio = Precision_focal/Precision_scan)



# Fit a GLM with student t (heavy-tailed approximation)
# Full model formula
full_formula <- Focal_benefit ~ 
  n_days +
  group_size +
  p_terrain_visibility +
  p_behavior_visibility +
  mean_events +
  behavior_duration +
  focal_duration_min +
  focal_break_time_min +
  scan_obsTime_perID +
  scan_break_time_min

# Fit the full model
mdl_precision_full <- lm(
  full_formula,
  data = z_data_precision_comparison)

variable_importance_precision <- vip::vi(mdl_precision_full, scale = T)


# plot model output
precision_comparison_effects <- plot_model(
  mdl_precision_full,
  sort.est = FALSE,
  # NEED TO THINK HOW TO INTERPRET THIS EXACTLY
  vline.color = "red",
  transform = NULL,
  show.values = TRUE,
  value.offset = .3
) +
  theme_minimal() +
  ggtitle('Precision Comparison')


ggsave(
  "Precision Comparison.jpg",
  precision_comparison_effects,
  dpi = 300,
  width = 10,
  height = 5
)



## Correlation -------------------------------------------------------------

### prepare data for analysis
z_data_correlation_comparison <-  all_correlation %>%
  group_by(Run_ID) %>%
  summarise(
    Correlation_focal = median(Correlation_focal),
    Correlation_scan = median(Correlation_scan),
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
  mutate_at(
    c(
      "n_days",
      "group_size",
      "p_terrain_visibility",
      "p_behavior_visibility",
      "mean_events",
      "behavior_duration",
      "focal_duration_min",
      'focal_break_time_min',
      "scan_obsTime_perID",
      "scan_break_time_min"
    ),
    ~ (scale(.) %>% as.vector)
  ) %>% 
  mutate(Focal_benefit = Correlation_focal - Correlation_scan)


# Fit a GLM with student t (heavy-tailed approximation)
# Full model formula
full_formula <- Focal_benefit ~ 
  n_days +
  group_size +
  p_terrain_visibility +
  p_behavior_visibility +
  mean_events +
  behavior_duration +
  focal_duration_min +
  focal_break_time_min +
  scan_obsTime_perID +
  scan_break_time_min


# Fit the full model
mdl_correlation_full <- lm(
  full_formula,
  data = z_data_correlation_comparison
)


variable_importance_correlation <- vip::vi(mdl_correlation_full, scale = T)


# plot model output
correlation_comparison_effects <- plot_model(
  mdl_correlation_full,
  sort.est = FALSE,
  # NEED TO THINK HOW TO INTERPRET THIS EXACTLY
  vline.color = "red",
  transform = NULL,
  show.values = TRUE,
  value.offset = .3
) +
  theme_minimal() +
  ggtitle('Correlation Comparison')


ggsave(
  "Correlation Comparison.jpg",
  correlation_comparison_effects,
  dpi = 300,
  width = 10,
  height = 5
)


### Plots

performance_colors <- c(
  "Accuracy" = "blue",
  "Precision" = "gold",
  "Correlation" = "red"
)

variable_importance_plot <-
  rbind(
    variable_importance_accuracy %>% 
      mutate(performance = 'Accuracy'),
    variable_importance_precision %>% 
      mutate(performance = 'Precision'),
    variable_importance_correlation %>% 
      mutate(performance = 'Correlation')) %>% 
  ggplot(aes(y = Variable, 
             x = Importance, 
             color = performance, 
             fill = performance)) + 
  geom_errorbarh(aes(xmin=0, 
                   xmax=Importance), 
                 height = 0,
                 linewidth = 1,
                position = position_dodge(width = .7)) +
  geom_point(size = 3,
             position = position_dodge(width = .7)) + 
  geom_text(aes(label = round(Importance, 0)),  # Add labels
            position = position_dodge(width = 0.7),  # Align with points
            vjust = -0.6,
            color = 'black',
            size = 3) +   
  theme_minimal() + 
  scale_color_manual(values = performance_colors) +  # Set custom colors for points
  scale_fill_manual(values = performance_colors) +   # Set custom colors for fills (if applicable) +
  labs(
    color = "Performance Criterion",  # Legend title for colors
    fill = "Performance Criterion",   # Legend title for fills (if applicable)
    x = "Scaled Importance",
    y = "Parameter"
  )

ggsave(
  "Variable Importance Comparison.jpg",
  variable_importance_plot,
  dpi = 300,
  width = 10,
  height = 5
)

## Standardised Estimates

estimate_plot <-
  rbind(
    standardize_parameters(mdl_accuracy_full) %>% 
      data.frame() %>% 
      mutate(performance = 'Accuracy'),
    standardize_parameters(mdl_precision_full) %>% 
      data.frame() %>% 
      mutate(performance = 'Precision'),
    standardize_parameters(mdl_correlation_full) %>% 
      data.frame() %>% 
      mutate(Std_Coefficient = Std_Coefficient * (-1)) %>% 
      mutate(performance = 'Correlation')) %>% 
  filter(Parameter != '(Intercept)') %>% 
  ggplot(aes(y = Parameter, 
             x = Std_Coefficient, 
             color = performance, 
             fill = performance)) + 
  geom_errorbarh(aes(xmin=0, 
                     xmax=Std_Coefficient), 
                 height = 0,
                 linewidth = 1,
                 position = position_dodge(width = .7)) +
  geom_point(size = 3,
             position = position_dodge(width = .7)) + 
  geom_text(aes(label = round(Std_Coefficient, 2)),  # Add labels
            position = position_dodge(width = 0.7),  # Align with points
            vjust = -0.6,
            color = 'black',
            size = 3) +    
  geom_vline(aes(xintercept = 0), lty = 2) +
  theme_minimal() + 
  scale_color_manual(values = performance_colors) +  # Set custom colors for points
  scale_fill_manual(values = performance_colors) +   # Set custom colors for fills (if applicable) +
  labs(
    color = "Performance Criterion",  # Legend title for colors
    fill = "Performance Criterion",   # Legend title for fills (if applicable)
    x = "Standardised Estimate",
    y = "Parameter"
  )

ggsave(
  "Estimate Comparison.jpg",
  estimate_plot,
  dpi = 300,
  width = 10,
  height = 5
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
### Group of 90 monkeys, high visibility, visible/short/rare, visible/long/common, poorly visible/short/rare

aggression <- c(5, 3, 0.9)
threat <- c(5, 3, 0.3)
grooming <- c(10, 60, 0.9)

# create individual events
n_aggression <- round(abs(rnorm(90, aggression[1], 3)))
n_grooming <- round(abs(rnorm(90, grooming[1], 3)))
n_threat <- round(abs(rnorm(90, threat[1], 3)))
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
        group_size = 90,
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
  theme_bw() +
  xlim(0, max(accuracies$mean_squared_error) + 0.1) +
  ggtitle('Accuracies: Large group') +
  xlab('Mean Squared Error') +
  ylab('Condition') +
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
  theme_bw() +
  xlim(0, max(precisions$CV) + 0.1) +
  ggtitle('Precisions: Large group') +
  xlab('CV') +
  ylab('Condition') +
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
  theme_bw() +
  xlim(0, 1) +
  ggtitle('Correlation: Large group') +
  xlab('Correlation') +
  ylab('Condition') +
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
              ggtitle('C: Correlation'), ncol=1, nrow=3, common.legend = TRUE, legend="bottom"),
  dpi = 300,
  width = 6,
  height = 12
)

# Case 2: Grooming, scan by group size ------------------------------------

large_frequent <- c(90, 5)
large_medium <- c(90, 20)
large_rare <- c(90, 40)
medium_frequent <- c(50, 5)
medium_medium <- c(50, 20)
medium_rare <- c(50, 40)
small_frequent <- c(15, 5)
small_medium <- c(15, 20)
small_rare <- c(15, 40)

# create individual events
n_large_frequent <- round(abs(rnorm(large_frequent[1], 10, 3)))
n_large_medium <- round(abs(rnorm(large_medium[1], 10, 3)))
n_large_rare <- round(abs(rnorm(large_rare[1], 10, 3)))
n_medium_frequent <- round(abs(rnorm(medium_frequent[1], 10, 3)))
n_medium_medium <- round(abs(rnorm(medium_medium[1], 10, 3)))
n_medium_rare <- round(abs(rnorm(medium_rare[1], 10, 3)))
n_small_frequent <- round(abs(rnorm(small_frequent[1], 10, 3)))
n_small_medium <- round(abs(rnorm(small_medium[1], 10, 3)))
n_small_rare <- round(abs(rnorm(small_rare[1], 10, 3)))

plan(multicore, workers = 10)

iterate_simulations <-
  function(group_size,
           scan_frequency,
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
        p_behavior_visibility = 0.95,
        p_terrain_visibility = 0.5,
        mean_events = mean_events,
        sd_events = 3,
        n_events = n_events,
        # as calculated above
        behavior_duration = 90,
        focal_duration_min = 15,
        focal_break_time_min = 1,
        scan_obsTime_perID = 3,
        scan_break_time_min = scan_frequency
      )
    )
  }

simulation_iteration_large_frequent <-
  iterate_simulations(90, 5, 10, n_large_frequent)
simulation_iteration_large_medium <-
  iterate_simulations(90, 20, 10, n_large_medium)
simulation_iteration_large_rare <-
  iterate_simulations(90, 40, 10, n_large_rare)
simulation_iteration_medium_frequent <-
  iterate_simulations(50, 5, 10, n_medium_frequent)
simulation_iteration_medium_medium <-
  iterate_simulations(50, 20, 10, n_medium_medium)
simulation_iteration_medium_rare <-
  iterate_simulations(50, 40, 10, n_medium_rare)
simulation_iteration_small_frequent <-
  iterate_simulations(15, 5, 10, n_small_frequent)
simulation_iteration_small_medium <-
  iterate_simulations(15, 20, 10, n_small_medium)
simulation_iteration_small_rare <-
  iterate_simulations(15, 40, 10, n_small_rare)


pa_large_frequent <- precision_and_accuracy(simulation_iteration_large_frequent)
pa_large_medium <- precision_and_accuracy(simulation_iteration_large_medium)
pa_large_rare <- precision_and_accuracy(simulation_iteration_large_rare)
pa_medium_frequent <- precision_and_accuracy(simulation_iteration_medium_frequent)
pa_medium_medium <- precision_and_accuracy(simulation_iteration_medium_medium)
pa_medium_rare <- precision_and_accuracy(simulation_iteration_medium_rare)
pa_small_frequent <- precision_and_accuracy(simulation_iteration_small_frequent)
pa_small_medium <- precision_and_accuracy(simulation_iteration_small_medium)
pa_small_rare <- precision_and_accuracy(simulation_iteration_small_rare)

pa_large_frequent$accuracy_frame$condition = '90 Individuals'
pa_large_medium$accuracy_frame$condition = '90 Individuals, 20min'
pa_large_rare$accuracy_frame$condition = '90 Individuals, 40min'
pa_medium_frequent$accuracy_frame$condition = '50 Individuals'
pa_medium_medium$accuracy_frame$condition = '50 Individuals, 20min'
pa_medium_rare$accuracy_frame$condition = '50 Individuals, 40min'
pa_small_frequent$accuracy_frame$condition = '15 Individuals'
pa_small_medium$accuracy_frame$condition = '15 Individuals, 20min'
pa_small_rare$accuracy_frame$condition = '15 Individuals, 40min'

pa_large_frequent$precision_frame$condition = '90 Individuals'
pa_large_medium$precision_frame$condition = '90 Individuals, 20min'
pa_large_rare$precision_frame$condition = '90 Individuals, 40min'
pa_medium_frequent$precision_frame$condition = '50 Individuals'
pa_medium_medium$precision_frame$condition = '50 Individuals, 20min'
pa_medium_rare$precision_frame$condition = '50 Individuals, 40min'
pa_small_frequent$precision_frame$condition = '15 Individuals'
pa_small_medium$precision_frame$condition = '15 Individuals, 20min'
pa_small_rare$precision_frame$condition = '15 Individuals, 40min'

pa_large_frequent$cor_frame$condition = '90 Individuals'
pa_large_medium$cor_frame$condition = '90 Individuals, 20min'
pa_large_rare$cor_frame$condition = '90 Individuals, 40min'
pa_medium_frequent$cor_frame$condition = '50 Individuals'
pa_medium_medium$cor_frame$condition = '50 Individuals, 20min'
pa_medium_rare$cor_frame$condition = '50 Individuals, 40min'
pa_small_frequent$cor_frame$condition = '15 Individuals'
pa_small_medium$cor_frame$condition = '15 Individuals, 20min'
pa_small_rare$cor_frame$condition = '15 Individuals, 40min'


accuracies <- rbind(
  pa_large_frequent$accuracy_frame,
  # pa_large_medium$accuracy_frame,
  # pa_large_rare$accuracy_frame,
  pa_medium_frequent$accuracy_frame,
  # pa_medium_medium$accuracy_frame,
  # pa_medium_rare$accuracy_frame,
  pa_small_frequent$accuracy_frame
  # pa_small_medium$accuracy_frame,
  # pa_small_rare$accuracy_frame
)


precisions <- rbind(
  pa_large_frequent$precision_frame,
  # pa_large_medium$precision_frame,
  # pa_large_rare$precision_frame,
  pa_medium_frequent$precision_frame,
  # pa_medium_medium$precision_frame,
  # pa_medium_rare$precision_frame,
  pa_small_frequent$precision_frame
  # pa_small_medium$precision_frame,
  # pa_small_rare$precision_frame
)

corrs <- rbind(
  pa_large_frequent$cor_frame,
  # pa_large_medium$cor_frame,
  # pa_large_rare$cor_frame,
  pa_medium_frequent$cor_frame,
  # pa_medium_medium$cor_frame,
  # pa_medium_rare$cor_frame,
  pa_small_frequent$cor_frame
  # pa_small_medium$cor_frame,
  # pa_small_rare$cor_frame
) %>% pivot_longer(cols = 
                     c(cor_true_scan, cor_true_focal), 
                   names_to = 'observed_data', 
                   values_to = 'correlation') %>% 
  # rename values in 'observed_data' column to fit the other datasets
  mutate(observed_data = if_else(observed_data == 'cor_true_scan', 'group time sampling proportion', 'focal continuous sampling proportion'))


p_accuracies_group_size <-
  ggplot(accuracies,
         aes(x = mean_squared_error, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  theme_bw() +
  xlim(0, max(accuracies$mean_squared_error) + 0.1) +
  ggtitle('Accuracies: Group Size') +
  xlab('Mean Squared Error') +
  ylab('Condition') +
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

p_precisions_group_size <-
  ggplot(precisions, aes(x = CV, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  theme_bw() +
  xlim(0, max(precisions$CV) + 0.1) +
  ggtitle('Precisions: Group Size') +
  xlab('CV') +
  ylab('Condition') +
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

p_correlation_group_size <-
  ggplot(corrs, aes(x = correlation, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  theme_bw() +
  xlim(0, 1) +
  ggtitle('Correlation: Group Size') +
  xlab('Correlation') +
  ylab('Condition') +
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
  "Case 2 Accuracies.jpg",
  p_accuracies_group_size,
  dpi = 300,
  width = 10,
  height = 5
)

ggsave(
  "Case 2 Precisions.jpg",
  p_precisions_group_size,
  dpi = 300,
  width = 10,
  height = 5
)

ggsave(
  "Case 2 Correlation.jpg",
  p_correlation_group_size,
  dpi = 300,
  width = 10,
  height = 5
)

ggsave(
  "Case 2 All.jpg",
  ggarrange(p_accuracies_group_size + 
              ggtitle('A: Accuracy'),
            p_precisions_group_size + 
              ggtitle('B: Precision'),
            p_correlation_group_size + 
              ggtitle('C: Correlation'), ncol=1, nrow=3, common.legend = TRUE, legend="bottom"),
  dpi = 300,
  width = 6,
  height = 12
)

# Case 3: Study Duration --------------------------------------------------

aggression_30 <- c(30, 5, 3, 0.9)
aggression_90 <- c(90, 5, 3, 0.9)
aggression_180 <- c(180, 5, 3, 0.9)
aggression_730 <- c(730, 5, 3, 0.9)
grooming_30 <- c(30, 10, 60, 0.9)
grooming_90 <- c(90, 10, 60, 0.9)
grooming_180 <- c(180, 10, 60, 0.9)
grooming_730 <- c(730, 10, 60, 0.9)


# create individual events
n_aggression_30 <- round(abs(rnorm(40, aggression_30[2], 3)))
n_aggression_90 <- round(abs(rnorm(40, aggression_90[2], 3)))
n_aggression_180 <- round(abs(rnorm(40, aggression_180[2], 3)))
n_aggression_730 <- round(abs(rnorm(40, aggression_730[2], 3)))
n_grooming_30 <- round(abs(rnorm(40, grooming_30[2], 3)))
n_grooming_90 <- round(abs(rnorm(40, grooming_90[2], 3)))
n_grooming_180 <- round(abs(rnorm(40, grooming_180[2], 3)))
n_grooming_730 <- round(abs(rnorm(40, grooming_730[2], 3)))

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
        group_size = 40,
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

simulation_iteration_aggression_30 <-
  iterate_simulations(aggression_30[1], n_aggression_30, aggression_30[2], aggression_30[3], aggression_30[4])
simulation_iteration_aggression_90 <-
  iterate_simulations(aggression_90[1], n_aggression_90, aggression_90[2], aggression_90[3], aggression_90[4])
simulation_iteration_aggression_180 <-
  iterate_simulations(aggression_180[1], n_aggression_180, aggression_180[2], aggression_180[3], aggression_180[4])
simulation_iteration_aggression_730 <-
  iterate_simulations(aggression_730[1], n_aggression_730, aggression_730[2], aggression_730[3], aggression_730[4])
simulation_iteration_grooming_30 <-
  iterate_simulations(grooming_30[1], n_grooming_30, grooming_30[2], grooming_30[3], grooming_30[4])
simulation_iteration_grooming_90 <-
  iterate_simulations(grooming_90[1], n_grooming_90, grooming_90[2], grooming_90[3], grooming_90[4])
simulation_iteration_grooming_180 <-
  iterate_simulations(grooming_180[1], n_grooming_180, grooming_180[2], grooming_180[3], grooming_180[4])
simulation_iteration_grooming_730 <-
  iterate_simulations(grooming_730[1], n_grooming_730, grooming_730[2], grooming_730[3], grooming_730[4])


pa_aggression_30 <- precision_and_accuracy(simulation_iteration_aggression_30)
pa_aggression_90 <- precision_and_accuracy(simulation_iteration_aggression_90)
pa_aggression_180 <- precision_and_accuracy(simulation_iteration_aggression_180)
pa_aggression_730 <- precision_and_accuracy(simulation_iteration_aggression_730)
pa_grooming_30 <- precision_and_accuracy(simulation_iteration_grooming_30)
pa_grooming_90 <- precision_and_accuracy(simulation_iteration_grooming_90)
pa_grooming_180 <- precision_and_accuracy(simulation_iteration_grooming_180)
pa_grooming_730 <- precision_and_accuracy(simulation_iteration_grooming_730)

pa_aggression_30$accuracy_frame$condition = '30 Days'
pa_aggression_90$accuracy_frame$condition = '90 Days'
pa_aggression_180$accuracy_frame$condition = '180 Days'
pa_aggression_730$accuracy_frame$condition = '730 Days'
pa_grooming_30$accuracy_frame$condition = '30 Days'
pa_grooming_90$accuracy_frame$condition = '90 Days'
pa_grooming_180$accuracy_frame$condition = '180 Days'
pa_grooming_730$accuracy_frame$condition = '730 Days'

pa_aggression_30$precision_frame$condition = '30 Days'
pa_aggression_90$precision_frame$condition = '90 Days'
pa_aggression_180$precision_frame$condition = '180 Days'
pa_aggression_730$precision_frame$condition = '730 Days'
pa_grooming_30$precision_frame$condition = '30 Days'
pa_grooming_90$precision_frame$condition = '90 Days'
pa_grooming_180$precision_frame$condition = '180 Days'
pa_grooming_730$precision_frame$condition = '730 Days'

pa_aggression_30$cor_frame$condition = '30 Days'
pa_aggression_90$cor_frame$condition = '90 Days'
pa_aggression_180$cor_frame$condition = '180 Days'
pa_aggression_730$cor_frame$condition = '730 Days'
pa_grooming_30$cor_frame$condition = '30 Days'
pa_grooming_90$cor_frame$condition = '90 Days'
pa_grooming_180$cor_frame$condition = '180 Days'
pa_grooming_730$cor_frame$condition = '730 Days'


accuracies_aggression <- rbind(
  pa_aggression_30$accuracy_frame,
  pa_aggression_90$accuracy_frame,
  pa_aggression_180$accuracy_frame,
  pa_aggression_730$accuracy_frame
)

accuracies_grooming <- rbind(
  pa_grooming_30$accuracy_frame,
  pa_grooming_90$accuracy_frame,
  pa_grooming_180$accuracy_frame,
  pa_grooming_730$accuracy_frame
)

precisions_aggression <- rbind(
  pa_aggression_30$precision_frame,
  pa_aggression_90$precision_frame,
  pa_aggression_180$precision_frame,
  pa_aggression_730$precision_frame
)

precisions_grooming <- rbind(
  pa_grooming_30$precision_frame,
  pa_grooming_90$precision_frame,
  pa_grooming_180$precision_frame,
  pa_grooming_730$precision_frame
)

corr_aggression <- rbind(
  pa_aggression_30$cor_frame,
  pa_aggression_90$cor_frame,
  pa_aggression_180$cor_frame,
  pa_aggression_730$cor_frame
) %>% pivot_longer(cols = 
                     c(cor_true_scan, cor_true_focal), 
                   names_to = 'observed_data', 
                   values_to = 'correlation') %>% 
  # rename values in 'observed_data' column to fit the other datasets
  mutate(observed_data = if_else(observed_data == 'cor_true_scan', 
                                 'group time sampling proportion', 
                                 'focal continuous sampling proportion'))

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

p_accuracies_duration_aggression <-
  ggplot(accuracies_aggression,
         aes(x = mean_squared_error, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  theme_bw() +
  xlim(0, max(accuracies_aggression$mean_squared_error) + 0.1) +
  ggtitle('Accuracies: Aggression Duration') +
  xlab('Mean Squared Error') +
  ylab('Approach') +
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

p_precisions_duration_aggression <-
  ggplot(precisions_aggression, aes(x = CV, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  theme_bw() +
  xlim(0, max(precisions_aggression$CV) + 0.1) +
  ggtitle('Precisions: Aggression Duration') +
  xlab('CV') +
  ylab('Approach') +
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

p_accuracies_duration_grooming <-
  ggplot(accuracies_grooming,
         aes(x = mean_squared_error, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  theme_bw() +
  xlim(0, max(accuracies_grooming$mean_squared_error) + 0.1) +
  ggtitle('Accuracies: Grooming Duration') +
  xlab('Mean Squared Error') +
  ylab('Approach') +
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

p_precisions_duration_grooming <-
  ggplot(precisions_grooming, aes(x = CV, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  theme_bw() +
  xlim(0, max(precisions_grooming$CV) + 0.1) +
  ggtitle('Precisions: Grooming Duration') +
  xlab('CV') +
  ylab('Approach') +
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

p_correlation_duration_aggression <-
  ggplot(corr_aggression, aes(x = correlation, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  theme_bw() +
  xlim(0, 1) +
  ggtitle('Correlation: Aggression Duration') +
  xlab('Correlation') +
  ylab('Approach') +
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

p_correlation_duration_grooming <-
  ggplot(corr_grooming, aes(x = correlation, y = condition, fill = observed_data)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3) +
  theme_ridges() +
  theme(legend.position = "none") +
  theme_bw() +
  xlim(0, 1) +
  ggtitle('Correlation: Grooming Duration') +
  xlab('Correlation') +
  ylab('Approach') +
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
  "Case 3 Accuracies Aggression.jpg",
  p_accuracies_duration_aggression,
  dpi = 300,
  width = 10,
  height = 5
)

ggsave(
  "Case 3 Precisions Aggression.jpg",
  p_precisions_duration_aggression,
  dpi = 300,
  width = 10,
  height = 5
)

ggsave(
  "Case 3 Accuracies Grooming.jpg",
  p_accuracies_duration_grooming,
  dpi = 300,
  width = 10,
  height = 5
)

ggsave(
  "Case 3 Precisions Grooming.jpg",
  p_precisions_duration_grooming,
  dpi = 300,
  width = 10,
  height = 5
)

ggsave(
  "Case 3 Correlation Aggression.jpg",
  p_correlation_duration_aggression,
  dpi = 300,
  width = 10,
  height = 5
)

ggsave(
  "Case 3 Correlation Grooming.jpg",
  p_correlation_duration_grooming,
  dpi = 300,
  width = 10,
  height = 5
)

ggsave(
  "Case 3 All Aggression.jpg",
  ggarrange(p_accuracies_duration_aggression + 
              ggtitle('A: Accuracy'), 
            p_precisions_duration_aggression + 
              ggtitle('B: Precision'), 
            p_correlation_duration_aggression + 
              ggtitle('C: Correlation'), ncol=1, nrow=3, common.legend = TRUE, legend="bottom"),
  dpi = 300,
  width = 6,
  height = 12
)

ggsave(
  "Case 3 All Grooming.jpg",
  ggarrange(p_accuracies_duration_grooming + 
              ggtitle('A: Accuracy'), 
            p_accuracies_duration_grooming + 
              ggtitle('B: Precision'),
            p_correlation_duration_grooming + 
              ggtitle('C: Correlation'), ncol=1, nrow=3, common.legend = TRUE, legend="bottom"),
  dpi = 300,
  width = 6,
  height = 12
)

