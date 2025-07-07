library(furrr)
source("Simulation Scripts/simulation_functions.r")
set.seed(12345)

# decide on reasonable values for each parameter we are testing
simulation_parameters <-
  list(
    n_days = seq(30, 180, by = 5),  # number of observation days to simulate (assume 7h per day, see below)
    group_size = seq(10, 100, by = 5),
    p_terrain_visibility = seq(0.1, 1, by = 0.1), # probability that an individual is observed
    p_behavior_visibility = seq(0.1, 1, by = 0.1), # probability that a behaviour is observed
    mean_events = c(seq(1, 19, by = 1), # mean number of behavioral events per day, per individual
                    seq(20, 50, by = 5)), 
    behavior_duration = c(seq(1, 9, by = 1), # behavior duration in seconds
                          seq(10, 55, by = 5),
                          seq(60, 110, by = 10),
                          seq(120, 600, by = 60)),
    focal_duration_min = c(seq(5, 25, by = 5), # time of focal observation in minutes
                           seq(30, 60, by = 10)),
    focal_break_time_min = 5, # minimum break time between focals in minutes
    scan_obsTime_perID = seq(1, 11, by = 2), # scan time needed per individual in seconds
    scan_break_time_min = c(seq(5, 25, by = 5), # minimum break time between end of scan and start of new one in seconds
                            seq(30, 60, by = 10))
  ) 

plan(multisession, workers = 1, gc = TRUE)

for(j in 1:1000000){
  
  i = 1
  sim_values <- lapply(simulation_parameters, sample, 1)
  
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
  
  save(results, file = paste0("Simulation Outputs/simulations_output_", paste(c(sim_values, sample(1:10010000, 1)), collapse = "_"), ".RData"))
  # return the whole thing
}
future:::ClusterRegistry("stop")
