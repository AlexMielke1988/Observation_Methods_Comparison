# set up workspace
setwd("Shiny App")
library(tidyverse)
library(dplyr)
library(purrr)
library(qs)
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

# function to extract parameter sets from simulation names
only_params <- function(name) {
  name %>%
    str_remove("_[0-9]+$")} # remove the suffix "_<iteration_number>"

# create a dataframe with the full name (incl. iteration nr) and just the parameter set name
param_df <- data.frame(
  original_name = names(sim),
  param_set = sapply(names(sim), only_params),
  stringsAsFactors = FALSE)

# set desired total rows per parameter set
TARGET_TOTAL_ROWS <- 90

set.seed(42)

# for each parameter set, select rows across replicates to meet the target
grouped_param <- param_df %>%
  group_by(param_set) %>%
  group_split()

# track parameter sets with fewer rows than target
below_target_count <- 0

sim_filtered <- map(grouped_param, function(group) {
  replicates <- sim[group$original_name]

  # combine accuracy and precision frames from all replicates
  accuracy_combined <- bind_rows(map(replicates, ~ .x$accuracy_frame), .id = "replicate")
  precision_combined <- bind_rows(map(replicates, ~ .x$precision_frame), .id = "replicate")
  cor_combined <- bind_rows(map(replicates, ~ .x$cor_frame), .id = "replicate")

  total_rows_available <- min(nrow(accuracy_combined), nrow(precision_combined))

  # check if fewer rows than target
  if (total_rows_available < TARGET_TOTAL_ROWS) {
    below_target_count <<- below_target_count + 1
  }

  # randomly sample rows to reach TARGET_TOTAL_ROWS
  accuracy_sampled <- accuracy_combined %>% slice_sample(n = min(nrow(accuracy_combined), TARGET_TOTAL_ROWS))
  precision_sampled <- precision_combined %>% slice_sample(n = min(nrow(precision_combined), TARGET_TOTAL_ROWS))
  
  # store sampled data back into a single entry per parameter set
  list(
    accuracy_frame = accuracy_sampled,
    precision_frame = precision_sampled,
    cor_frame = cor_combined
  )
})

# rename as just the parameter set names
names(sim_filtered) <- map_chr(grouped_param, ~ .x$param_set[1])
cat("Number of unique parameter sets retained:", length(sim_filtered), "\n")
cat("Parameter sets with fewer rows than target:", below_target_count, "\n")

# save compressed data
qsave(sim_filtered, "Shiny_simulations_filtered.qs", preset = "high")
cat("Compressed file size (MB):", file.info("Shiny_simulations_filtered.qs")$size / 1e6, "\n")
