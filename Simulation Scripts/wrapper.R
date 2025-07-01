library(ggplot2)
library(furrr)
setwd("~/Documents/GitHub/rethinking-obs-methods/")
source("Simulation Scripts/simulation_functions.r")
#source("~/Documents/Github/rethinking-obs-methods/Simulation Scripts/simulation_functions.r")
set.seed(12345)
# Author: Alex Mielke
# 
# decide on a bunch of reasonable values for each parameter we are testing
simulation_parameters <-
  expand.grid(
    n_days = c(30, 90, 180, 360),
    # number of observation days to simulate (assume 7h per day, see below)
    group_size = c(20, 50, 100),
    p_terrain_visibility = c(0.2, 0.8),
    p_behavior_visibility = c(0.2, 0.8),
    mean_events = c(1, 7, 20, 50),
    # mean number of behavioral events per day, per individual (sd set as mean/3, see below)
    behavior_duration = c(3, 30, 120),
    # behavior duration in sec
    focal_duration_min = c(15, 60),
    # time of focal observation in minutes
    focal_break_time_min = 5,
    # minimum break time between focals in minutes
    scan_obsTime_perID = c(1, 5),
    # scan time needed per individual in minutes
    scan_break_time_min = c(60, 300, 900)
  ) # minimum break time between end of scan and start of new one in seconds
# 
# simulation_parameters <-
#   list(
#     n_days = seq(30, 180, by = 5),
#     # number of observation days to simulate (assume 7h per day, see below)
#     group_size = seq(10, 100, by = 5),
#     p_terrain_visibility = seq(0.1, 1, by = 0.1),
#     p_behavior_visibility = seq(0.1, 1, by = 0.1),
#     mean_events = c(seq(1, 19, by = 1), 
#                     seq(20, 50, by = 5)),
#     # mean number of behavioral events per day, per individual (sd set as mean/3, see below)
#     behavior_duration = c(seq(1, 9, by = 1), 
#                           seq(10, 55, by = 5), 
#                           seq(60, 110, by = 10), 
#                           seq(120, 600, by = 60)),
#     # behavior duration in sec
#     focal_duration_min = c(seq(5, 25, by = 5),
#                            seq(30, 60, by = 10)),
#     # time of focal observation in minutes
#     focal_break_time_min = 5,
#     # minimum break time between focals in minutes
#     scan_obsTime_perID = seq(1, 11, by = 2),
#     # scan time needed per individual in seconds
#     scan_break_time_min = c(seq(5, 25, by = 5),
#                           seq(30, 60, by = 10))
#   ) # minimum break time between end of scan and start of new one in seconds



plan(multisession, workers = 10, gc = TRUE)
# 
# unused_parameters <- which(!(
#   sapply(1:nrow(simulation_parameters), 
#          function(x){
#            paste(c(paste(simulation_parameters[x, ], collapse = '_'),'.RData'), collapse = '')}) %in% 
#     list.files('~/GitHub/rethinking-obs-methods/runs/')))

# run simulations (can be parallelised)

for(j in 1:1000000){
  
  i = 1
  sim_values <- lapply(simulation_parameters, sample, 1)
  
  n_events <-
    round(# if not given, calculate average number of daily interactions per individual for this simulation run
      abs(
        rnorm(
          # assuming normal distribution of events per individual
          sim_values$group_size[i],
          # for each individual in the group pick a number of daily interactions from a normal distribution with
          sim_values$mean_events[i],
          # mean_events as set in simulations_parameters and
          2
        )
      )) + 1 # sd set as mean_events/3, a reasonable variation that will keep a similar distribution for different means
  # add 1 because sometimes there would be 0s and then it cracks
  print(paste(c(sim_values, sample(1:1000, 1)), collapse = '_'))
  # run simulations; the more, the better, but obviously adds time
  simulation_iteration <-
    future_map(.options = furrr_options(seed = 1234),
               1:10,
               ~ degree_simulation(
                 n_days = sim_values$n_days[i],
                 n_hours = 7,
                 # set at 7
                 group_size = sim_values$group_size[i],
                 p_behavior_visibility = sim_values$p_behavior_visibility[i],
                 p_terrain_visibility = sim_values$p_terrain_visibility[i],
                 mean_events = sim_values$mean_events[i],
                 sd_events = 2,
                 n_events = n_events,
                 # as calculated above
                 behavior_duration = sim_values$behavior_duration[i],
                 focal_duration_min = sim_values$focal_duration_min[i],
                 focal_break_time_min = sim_values$focal_break_time_min[i],
                 scan_obsTime_perID = sim_values$scan_obsTime_perID[i],
                 scan_break_time_min = sim_values$scan_break_time_min[i]
               )
    )
  
  # calculate precision and accuracy for scans and focal follows (functions specified in simulation_functions.R)
  precision_focal_prop <-
    precision_perID(simulation_runs = simulation_iteration,
                    observed_data = 'focal_prop_perID')
  precision_focal_rate <-
    precision_perID(simulation_runs = simulation_iteration,
                    observed_data = 'focal_rate_perID')
  precision_scan_prop <-
    precision_perID(simulation_runs = simulation_iteration,
                    observed_data = 'scan_prop_perID')
  
  accuracy_focal_prop <-
    accuracy_perID(
      simulation_runs = simulation_iteration,
      true_data = 'true_prop_behav_perID',
      observed_data = 'focal_prop_perID'
    )
  accuracy_focal_rate <-
    accuracy_perID(
      simulation_runs = simulation_iteration,
      true_data = 'true_rate_behav_perID',
      observed_data = 'focal_rate_perID'
    )
  accuracy_scan_prop <-
    accuracy_perID(
      simulation_runs = simulation_iteration,
      true_data = 'true_prop_behav_perID',
      observed_data = 'scan_prop_perID'
    )
  
  # put all the precisions together with the parameter information for subsequent plotting
  
  precision_frame <- data.frame(
    CV = c(precision_focal_prop,
           precision_focal_rate,
           precision_scan_prop),
    observed_data = c(
      # whether focal continuous or group time sampling
      rep('focal continuous sampling proportion', length(precision_focal_prop)),
      rep('focal continuous sampling rate', length(precision_focal_rate)),
      rep('group time sampling proportion', length(precision_scan_prop))
    )
  )
  # add the simulation parameters to every row
  precision_frame <- cbind(precision_frame,
                           data.frame(sim_values)[rep(seq_len(nrow(data.frame(sim_values))), each = nrow(precision_frame)),])
  
  # put all the accuracies together with the parameter information for subsequent plotting
  accuracy_frame <- data.frame(
    mean_squared_error = c(# mean squared errors
      accuracy_focal_prop,
      accuracy_focal_rate,
      accuracy_scan_prop),
    observed_data = c(
      # focal continuous or group time sampling
      rep('focal continuous sampling proportion', length(accuracy_focal_prop)),
      rep('focal continuous sampling rate', length(accuracy_focal_rate)),
      rep('group time sampling proportion', length(accuracy_scan_prop))
    )
  )
  # add the simulation parameters to every row
  accuracy_frame <- cbind(accuracy_frame,
                          data.frame(sim_values)[rep(seq_len(nrow(data.frame(sim_values))), 
                                                     each = nrow(accuracy_frame)),])
  
  
  # add correlations per iteration
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

  save(results, file = paste(c('~/GitHub/rethinking-obs-methods/runs_shiny_new//',paste(c(sim_values, sample(size = 1, ceiling(runif(100, min=1, max=10010000)))), collapse = '_'),'.RData'), collapse = ''))
  # return the whole thing
}
future:::ClusterRegistry("stop")


############################################
### EXTRACT RESULTS FROM SIMULATION RUNS ###
############################################

#CT July 5th 2024: Now moved to a separate script: extract_results.R 

# setwd("~/Documents/GitHub/rethinking-obs-methods/")
# file_list <-
#   list.files(path = 'runs/',
#              pattern = "\\.RData$",
#              full.names = T)
# 
# simulations <- list()
# 
# # Loop through each file in the list
# file = file_list[1]
# 
# for (file in file_list) {
#   
#   # Load the .RData file into the current R session
#   load(file)
#   
#   # add true and observed values for plotting
#   # get summary of true values and observed
#   # 
#   # raw_summary <- data.frame(
#   #   true_proportion = colMeans(do.call(rbind, lapply(results$simulation_iteration, function(x) x$true_prop_results))),
#   #   focal_proportion = colMeans(do.call(rbind, lapply(results$simulation_iteration, function(x) x$focal_prop_results))),
#   #   scan_proportion = colMeans(do.call(rbind, lapply(results$simulation_iteration, function(x) x$scan_prop_results)))
#   # )
#   # 
#   # results$raw_summary <- dplyr::as_tibble(raw_summary)
#   # 
#   simulation_iteration <- results$simulation_iteration
#   # calculate precision and accuracy for scans and focal follows (functions specified in simulation_functions.R)
# 
#   sim_values <- unlist(results$parameters)
#   
#   accuracy_focal_prop <-
#     accuracy_perID(
#       simulation_runs = simulation_iteration,
#       true_data = 'true_prop_behav_perID',
#       observed_data = 'focal_prop_perID'
#     )
#   accuracy_focal_rate <-
#     accuracy_perID(
#       simulation_runs = simulation_iteration,
#       true_data = 'true_rate_behav_perID',
#       observed_data = 'focal_rate_perID'
#     )
#   accuracy_scan_prop <-
#     accuracy_perID(
#       simulation_runs = simulation_iteration,
#       true_data = 'true_prop_behav_perID',
#       observed_data = 'scan_prop_perID'
#     )
#   bias_focal_prop <-
#     bias_perID(
#       simulation_runs = simulation_iteration,
#       true_data = 'true_prop_behav_perID',
#       observed_data = 'focal_prop_perID'
#     )
#   bias_scan_prop <-
#     bias_perID(
#       simulation_runs = simulation_iteration,
#       true_data = 'true_prop_behav_perID',
#       observed_data = 'scan_prop_perID'
#     )
#   
#   # put all the accuracies together with the parameter information for subsequent plotting
#   accuracy_frame <- data.frame(
#     mean_squared_error = c(# mean squared errors
#       accuracy_focal_prop,
#       accuracy_focal_rate,
#       accuracy_scan_prop
#       ),
#     observed_data = c(
#       # focal continuous or group time sampling
#       rep('focal continuous sampling proportion', length(accuracy_focal_prop)),
#       rep('focal continuous sampling rate', length(accuracy_focal_rate)),
#       rep('group time sampling proportion', length(accuracy_scan_prop))
#     )
#   )
#   # add the simulation parameters to every row
#   accuracy_frame <- cbind(accuracy_frame,
#                           data.frame(sim_values)[rep(seq_len(nrow(data.frame(sim_values))), 
#                                                      each = nrow(accuracy_frame)),])
#   
#   
#   results$accuracy_frame <- accuracy_frame
#   
#   # save as smaller object
#   results$accuracy_frame = dplyr::as_tibble(results$accuracy_frame[,setdiff(colnames(results$accuracy_frame), 
#                                                            colnames(results$parameters))])
#   results$precision_frame = dplyr::as_tibble(results$precision_frame[,setdiff(colnames(results$precision_frame), 
#                                                              colnames(results$parameters))])
#   results$cor_frame = dplyr::as_tibble(results$cor_frame[,setdiff(colnames(results$cor_frame), 
#                                                  colnames(results$parameters))])
#   # Save the 'results' object to the list
#   simulations[[file]] <- results[-1]
#   gc()
# }
# 
# # extract precision for each iteration
# all_precision <-
#   do.call(rbind,
#           lapply(simulations, function(x)
#             x$precision_frame))
# 
# # and plot
# precision_plot <- ggplot(
#   all_precision %>% filter(observed_data != 'focal continuous sampling rate')
#   ,
#   aes(
#     x = mean_events,
#     y = CV,
#     color = observed_data,
#     fill = observed_data
#   )
# ) +
#   geom_smooth(method = 'lm') +
#   theme_classic() +
#   ggtitle('Precision as Coefficient of Variance')
# 
# # extract accuracy for each iteration
# all_accuracy <-
#   do.call(rbind,
#           lapply(simulations, function(x)
#             x$accuracy_frame))
# 
# # and plot
# accuracy_plot <- ggplot(
#   accuracies %>% filter(observed_data != 'focal continuous sampling rate'),
#   aes(
#     x = mean_events,
#     y = mean_squared_error,
#     color = observed_data,
#     fill = observed_data
#   )
# ) +
#   geom_jitter(alpha = 0.1) +
#   theme_classic() + ylim(0,2)+ facet_wrap(~observed_data)+
#   ggtitle('Accuracy as Mean Squared Error from True Value')
