# set up workspace
library(tidyverse)
library(dplyr)
library(purrr)
library(qs2)
library(stringr)

# load Shiny simulations (saved per chunks for space)
load("Shiny_simus_group1.RData")
sim1 <- simulations
load("Shiny_simus_group2.RData")
sim2 <- simulations
load("Shiny_simus_group3.RData")
sim3 <- simulations
load("Shiny_simus_group4.RData")
sim4 <- simulations

# merge all simulations together & save as a compressed .qs file
sim <- c(sim1, sim2, sim3, sim4)

# extract parameters
param_names <- map_chr(sim, ~ paste(
  .x$parameters$n_days,
  .x$parameters$group_size,
  .x$parameters$p_terrain_visibility,
  .x$parameters$p_behavior_visibility,
  .x$parameters$mean_events,
  .x$parameters$behavior_duration,
  .x$parameters$focal_duration_min,
  .x$parameters$focal_break_time_min,
  .x$parameters$scan_obsTime_perID,
  .x$parameters$scan_break_time_min,
  sep = "_"))

# set desired total rows per parameter set
TARGET_TOTAL_ROWS <- 90

set.seed(42)

# for each parameter set, select rows across replicates to meet the target
below_target_count <- 0

sim_filtered <- split(sim, param_names) %>%
  map(function(replicates) {
    # combine accuracy, precision and cor frames from all replicates
    accuracy_combined  <- bind_rows(map(replicates, ~ .x$accuracy_frame),  .id = "replicate")
    precision_combined <- bind_rows(map(replicates, ~ .x$precision_frame), .id = "replicate")
    cor_combined       <- bind_rows(map(replicates, ~ .x$cor_frame),       .id = "replicate")
    
    # check if fewer rows than target
    if (min(nrow(accuracy_combined), nrow(precision_combined)) < TARGET_TOTAL_ROWS) {
      below_target_count <<- below_target_count + 1
    }
    
    # randomly sample rows to reach TARGET_TOTAL_ROWS
    list(
      accuracy_frame  = accuracy_combined  %>% slice_sample(n = min(nrow(accuracy_combined),  TARGET_TOTAL_ROWS)),
      precision_frame = precision_combined %>% slice_sample(n = min(nrow(precision_combined), TARGET_TOTAL_ROWS)),
      cor_frame       = cor_combined
    )
  })

cat("Number of unique parameter sets retained:", length(sim_filtered), "\n")
cat("Parameter sets with fewer rows than target:", below_target_count, "\n")

# save compressed data
qs_save(sim_filtered, "Shiny_simulations_filtered.qs")
cat("Compressed file size (MB):", file.info("Shiny_simulations_filtered.qs")$size / 1e6, "\n")
