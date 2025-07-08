library(ggplot2)
library(furrr)
source("Simulation Scripts/simulation_functions.r")
set.seed(1234)

# decide on reasonable values for each parameter we are testing
simulation_parameters <-
  expand.grid(
    n_days = c(30, 90, 180),
    group_size = c(20, 50, 100),
    p_terrain_visibility = c(0.2, 0.5, 0.8),
    p_behavior_visibility = c(0.2, 0.5, 0.8),
    mean_events = c(1, 7, 20, 50),
    behavior_duration = c(3, 30, 120, 600),
    focal_duration_min = c(15, 60),
    focal_break_time_min = 5,
    scan_obsTime_perID = c(1, 5),
    scan_break_time_min = c(1, 15, 60)
  ) 

simulation_parameters <- 
  bind_rows(simulation_parameters, 
            simulation_parameters)

plan(multisession, workers = 16, gc = TRUE)

# run simulations (can be parallelised)

for(j in 1:nrow(simulation_parameters)){
  
  i = 1
  sim_values <- as.vector(simulation_parameters[j,])
  
  n_events <-
    round(# if not given, calculate average number of daily interactions per individual for this simulation run
      abs(
        rnorm(
          # assuming normal distribution of events per individual
          sim_values$group_size[i], # for each individual in the group pick a number of daily interactions from a normal distribution with
          sim_values$mean_events[i], # mean_events as set in simulations_parameters and
          2 # sd of 2 around the mean
        )
      )) + 1 # add 1 to make sure each individual has at least one behavioral event per day
  
  print(paste(c(sim_values, sample(1:1000, 1)), collapse = '_'))
  
  # run simulations to generate individual true values and behavioral estimates; the more, the better, but obviously adds time
  simulation_iteration <-
    future_map(.options = furrr_options(seed = 1234),
               1:10,
               ~ behaviour_simulation( # run the simulation function specified in simulation_functions.R
                 n_days = sim_values$n_days[i],
                 n_hours = 7, # set at 7
                 group_size = sim_values$group_size[i],
                 p_behavior_visibility = sim_values$p_behavior_visibility[i],
                 p_terrain_visibility = sim_values$p_terrain_visibility[i],
                 mean_events = sim_values$mean_events[i],
                 sd_events = 2,
                 n_events = n_events, # as calculated above
                 behavior_duration = sim_values$behavior_duration[i],
                 focal_duration_min = sim_values$focal_duration_min[i],
                 focal_break_time_min = sim_values$focal_break_time_min[i],
                 scan_obsTime_perID = sim_values$scan_obsTime_perID[i],
                 scan_break_time_min = sim_values$scan_break_time_min[i]
               )
    )
  
  # calculate accuracy and precision for focal follows and group scans (using functions specified in simulation_functions.R)
  precision_focal <-
    precision_perID(simulation_runs = simulation_iteration,
                    observed_data = 'focal_prop_perID')
  
  precision_scan <-
    precision_perID(simulation_runs = simulation_iteration,
                    observed_data = 'scan_prop_perID')
  
  accuracy_focal <-
    accuracy_perID(
      simulation_runs = simulation_iteration,
      true_data = 'true_prop_behav_perID',
      observed_data = 'focal_prop_perID'
    )
  
  accuracy_scan <-
    accuracy_perID(
      simulation_runs = simulation_iteration,
      true_data = 'true_prop_behav_perID',
      observed_data = 'scan_prop_perID'
    )
  
  # put all the precisions together with the parameter information for subsequent plotting
  precision_frame <- data.frame(
    CV = c(precision_focal,
           precision_scan),
    observed_data = c(
      rep('focal follow', length(precision_focal)),
      rep('group scan', length(precision_scan))
    )
  )
  # add the simulation parameters to every row
  precision_frame <- cbind(precision_frame,
                           data.frame(sim_values)[rep(seq_len(nrow(data.frame(sim_values))), each = nrow(precision_frame)),])
  
  # put all the accuracies together with the parameter information for subsequent plotting
  accuracy_frame <- data.frame(
    mean_squared_error = c(
      accuracy_focal,
      accuracy_scan),
    observed_data = c(
      # focal continuous or group time sampling
      rep('focal follow', length(accuracy_focal)),
      rep('group scan', length(accuracy_scan))
    )
  )
  # add the simulation parameters to every row
  accuracy_frame <- cbind(accuracy_frame,
                          data.frame(sim_values)[rep(seq_len(nrow(data.frame(sim_values))), 
                                                     each = nrow(accuracy_frame)),])
  
  
  # calculate correlations between true and estimated values for focal follows and group scans
  cor_frame <- 
    data.frame(cor_true_scan = sapply(simulation_iteration, function(x) cor(x$scan_prop_results, x$true_prop_behav_perID)),
               cor_true_focal = sapply(simulation_iteration, function(x) cor(x$focal_prop_results, x$true_prop_behav_perID)),
               cor_scan_focal = sapply(simulation_iteration, function(x) cor(x$focal_prop_results, x$scan_prop_results)))
  
  cor_frame <- cbind(cor_frame,
                     data.frame(sim_values))
  
  
  results <- list(
    simulation_iteration = simulation_iteration,
    parameters = data.frame(sim_values),
    accuracy_frame = accuracy_frame,
    precision_frame = precision_frame,
    cor_frame = cor_frame
  )
  
  save(results, file = paste0("Shiny App/", paste(c(sim_values, sample(1:10010000, 1)), collapse = "_"), ".RData"))
  # return the whole thing
}
future:::ClusterRegistry("stop")

# load all the Shiny simulations
file_list <-
  list.files(path = 'Shiny App',
             pattern = "\\.RData$",
             full.names = T)

# initialize an empty list to store the results
simulations <- list()

# loop through each file in the list
for (file in file_list) {
  # Load the .RData file into the current R session
  load(file)
  
  # add true and observed values for plotting
  # get summary of true values and observed
  
  raw_summary <- data.frame(
    true_proportion = colMeans(do.call(rbind, lapply(results$simulation_iteration, function(x) x$true_prop_results))),
    focal_proportion = colMeans(do.call(rbind, lapply(results$simulation_iteration, function(x) x$focal_prop_results))),
    scan_proportion = colMeans(do.call(rbind, lapply(results$simulation_iteration, function(x) x$scan_prop_results))),
    results$parameters
  )
  
  results$raw_summary <- raw_summary
  
  # Save the 'results' object to the list
  simulations[[file]] <- results[-1]
  gc()
}

save(simulations, file = "Shiny App/fixed_simus.RData")