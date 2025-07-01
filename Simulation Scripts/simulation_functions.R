#' Simulating ability to detect behavioral bouts in group scans vs. continuous focals
#'
#' The function assigns each focal individuals an interaction probability and
#'    assigns a number of parameters that determine group structure (# of individuals),
#'    observation conditions (probability of observing behaviour, observation time,
#'    breaks, number of days), and focal and scan conditions (focal length, scan frequency etc).
#'    Then, for x number of days, the observed probabilities/rates of behaviour of
#'    each are compared with the 'true' individual values
#'
#' @param n_days integer. number of days the simulated study presumably runs; days are assumed independent from each other.
#' @param group_size integer. number of individuals in the study population. It is assumed that all of these will be sampled as focals at the same rate
#' @param p_behavior_visibility 0-1. probability that a behaviour is observed during a focal or scan (common or cryptic). This is implemented by making a proportion of behavior *bouts* non-visible (*not* by making certain seconds of the behavior bout non-visible)
#' @param p_terrain_visibility 0-1. probability that an individual is observed during a focal or scan  (forest vs. open field). This is implemented by making a proportion of the focal follows or group scans non-visible (random). Not the full focal follow.
#' @param mean_events numeric. mean number of interactions each individual has in a day
#' @param sd_events numeric. standard deviation of number of interactions each individual has in a day.
#' @param n_events numeric vector of the same length as group size. lets user reuse same individual distributions across simulations; if NULL, mean and sd are used to calculate new; if vector of length group_size is provided, those are used, to allow for same distribution across simulation runs
#' @param behavior_duration integer. duration of behaviour in seconds. longer behaviours are easier to detect.
#' @param n_hours integer. number of observation hours in each day (determines the number of focals and scans possible)
#' @param focal_duration_min integer. duration of each focal period
#' @param focal_break_time_min integer. duration of break between two focal periods; restricts number of focals possible in a day
#' @param scan_obsTime_perID integer. number of seconds it takes to observe each available individual in a scan.
#' @param scan_break_time_min integer. number of minutes between the end of one scan and the beginning of the new scan.
#'
#' @return Function returns a list of all input parameters and matrices with individual-level focal and scan rates (both observed and 'true' rates/probabilities) for further analysis/plotting
#'
#'
#' @author Camille Testard, reviewed by Alex Mielke; 2023
#' @export
#'


#' Calculate accuracy per Individual from the list returned by the degree_simulation function when it is run in a loop
#'
#' For each individual, for each simulation run, the difference between observed rate/probability and the expected true value is calculated; function calculates the mean squared error of all individual values for each simulation iteration as a measure of accuracy for that simulation iteration.
#'
#' @param simulation_runs list produced by degree_simulation function
#' @param true_data name of the list object that holds the true, expected individual rates for each individual.
#' @param observed_data name of the list object that holds the observed individual rates for each individual.
#'
#' @return Function returns a vector with mean squared errors for each simulation
#'
#'
#' @author Alex Mielke reviewed by Camille Testard
#' @export

degree_simulation <-
  function(n_days,
           # number of days that the simulation goes on
           group_size,
           # group size to be looked at
           p_behavior_visibility,
           # 'observability' of behavior (common or cryptic)
           p_terrain_visibility,
           # visibility of individuals due to terrain (forest vs. open field)
           mean_events,
           # mean number of behavioral events per day, per individual
           sd_events,
           # sd number of behavioral events per day, per individual
           n_events = NULL,
           # individual distributions; if NULL, mean and sd are used to calculate new; if vector of length group_size is provided, those are used, to allow for same distribution across simulation runs
           behavior_duration,
           # behavior duration in sec
           n_hours,
           # number of hours of observation in the day
           focal_duration_min,
           # time of focal in hours (i.e. 5min)
           focal_break_time_min,
           # minimum break time between focals in min
           scan_obsTime_perID,
           # scan time needed per individual in sec
           scan_break_time_min)
# minimum break time between scans in min)
# CT note 2024-07-12: What is the break between scans periods? No because the scan length = obsTime * group_size
  {
    
    ### Study population ###
    group_size <- group_size # group size
    p_terrain_visibility <-
      p_terrain_visibility # visibility of individuals due to terrain (forest vs. open field)
    # Note: group size and visibility could interact ?
    
    ### Behavior studied ###
    mean <- mean_events
    sd <-
      sd_events # mean and sd number of behavioral events per day, per individual
    # extend by number of days
    if (!is.null(n_events)) {
      n_events <- n_events * n_days
    } else {
      n_events <- (round(abs(rnorm(group_size, mean, sd))) + 1) * n_days
    }
    
    # Number of behavioral events in the day per individual, or frequency of behavior
    # Assuming each individual engages in the behavior at normally distributed frequencies
    # NB: We could sample from another distribution (e.g. more skewed)
    behavior_duration <-
      behavior_duration # behavior duration in sec
    # Note: Not have a unique time per behavior? draw from a distribution around a mean?
    p_behavior_visibility <-
      p_behavior_visibility # 'observability' of behavior (visible or cryptic)
    
    # retiring the combined visibility parameter
    # p_visibility <-
    #   p_behavior_visibility * p_terrain_visibility # Final visibility, which is a combination of the sources of occlusions.
    # Note: Visibility at behavior and habitat level. Do we also need one at the individual level?
    # Currently visibility only affects scan observations
    
    
    ### Observation method ###
    # total observation time - number of hours times number of days
    n_hours <- n_hours * n_days # number of hours of observation in the day, times the number of days
    time_in_s <-
      n_hours * 60 * 60 # number of seconds of observations in a study period
    
    # continuous focal
    focal_duration_min <-
      focal_duration_min # time of focal in minutes (i.e. 5min)
    focal_duration_s <-
      focal_duration_min * 60 # focal duration in seconds
    focal_break_time_min <-
      focal_break_time_min # minimum break time between focals in min
    focal_break_time_s <-
      focal_break_time_min * 60 # minimum break time between focals in sec
    n_focals <-
      round(time_in_s / (focal_duration_s + focal_break_time_s), 0) # number of focals in study period
    total_focal_s <-
      n_focals * focal_duration_s # focal observation time in seconds
    
    # group scans
    scan_obsTime_perID <- scan_obsTime_perID
    # scan time needed per individual. Assuming 1sec
    scan_duration <-
      scan_obsTime_perID * (round(p_terrain_visibility * group_size)) # duration of a scan observation in sec, scales with group size
    scan_break_time_s <-
      scan_break_time_min * 60 # minimum break time between scans in seconds
    num_scans <-
      round(time_in_s / (scan_duration + scan_break_time_s)) # equivalent # scans than the focal hours observed
    
    
    ### Initiate outcomes of simulation ###
    focaltime_perID <-
      rep(NA, group_size) # seconds of focal observation per ID
    focalsamples_perID <-
      rep(NA, group_size) # number of focal observations per ID
    behav_timeObserved_focal_perID <-
      rep(NA, group_size) # seconds of behavior observed per ID
    behav_boutsObserved_focal_perID <-
      rep(NA, group_size) # number of bouts observed per ID
    focal_prop_perID <-
      rep(NA, group_size) # observed proportion of time engaged in behavior X per ID
    # = [time in behavior X / total observation time]
    focal_rate_perID <-
      rep(NA, group_size) # observed rate of behavior per ID
    # = [#Events in behavior X / total observation time]
    
    scansamples_perID <-
      rep(NA, group_size) # number of scan samples per ID
    behav_boutsObserved_scan_perID <-
      rep(NA, group_size) # number of bouts observed per ID
    scan_prop_perID <-
      rep(NA, group_size) # observed rate of behavior per ID
    # = [#Events in behavior X / total observation time]
    
    true_prop_behav_perID <-
      rep(NA, group_size) # TRUE proportion of time engaged in behavior X per individual
    true_rate_behav_perID <-
      rep(NA, group_size) # TRUE rate at which behavior X occurs
    
    
    # create time sequences for behaviour duration, focal time, and scan time; each iteration picks from these sequences
    beh_time_seq <- seq.int(behavior_duration, floor(time_in_s - behavior_duration),
                            by = behavior_duration)
    
    # Initialize behaviors
    # = times at which each individual engaged in behavior X (unique identifier for each bout)
    
    #######################
    
    # Across individuals, randomly assign time for every event in study period
    event_times <- sample(
      beh_time_seq,
      sum(unlist(n_events)), replace = T
    ) 
    # sample behavioral events times (mid-point of behavior)
    # Sample behavioral event times during the day with a minimum
    # time lapse between behavioral events ('by' time).
    # Currently min time lapse = length of the behavior
    
    # for all events, set the seconds when it is observable
    event_range <- 
      lapply(event_times, function(x){
        seq.int(from = (x - round(behavior_duration/2) + 1), # sequence from minimum to maximum based on behaviour duration around mid point
                to = (x + round(behavior_duration/2)))})
    
    # Assign each event to one individual - each individual gets their value from n_events
    ids_events <- sample(unlist(sapply(1:group_size, 
                                       function(y) rep(y, n_events[y]))))
    
    # Assign each bout a unique bout identifier
    bout_range <- lapply(seq_along(event_range), function(x){
      rep(x, behavior_duration + 1)
    })
    
    bouts_events <- seq_along(event_range)
    
    # True number of bouts and seconds per individual
    
    # count the number of seconds that each individual is engaged in action for whole dataset
    true_seconds <- lapply(1:group_size, function(x){
      length(unlist(event_range[ids_events == x]))
    }) 
    # count the number of bouts that each individual is engaged in action for whole dataset
    true_bouts <- lapply(1:group_size, function(x){
      length(unique(bouts_events[ids_events == x]))
    })
    
    ##### randomly remove a subset equal to p_behavior_visibility 
    
    vis_include <- sample(bouts_events, 
                          size = length(bouts_events) * p_behavior_visibility)
    
    bout_range <- bout_range[vis_include]
    ids_events <- ids_events[vis_include]
    event_range <- event_range[vis_include]
    
    ########################################################
    # Find observed behaviors using continuous focal sampling
    
    # Set focal list for the full time period
    if (group_size < n_focals) {
      # if there are fewer individuals than #focals in a day
      # make sure that each individual is selected at least once, by selecting the first focals without replacement, after with replacement
      focal_id_list <-
        sample(
          c(sample(1:group_size, group_size, replace = F), # first x focal follows, give at least on per individual
            sample(1:group_size, (n_focals-group_size), replace = T)) # after, just hand them out as they come
        )
    } else {
      # If there are more or equal #individuals in a day
      focal_id_list <- sample(1:group_size, n_focals)
    } # Loop through all focal individuals in the day (no repeats)
    
    # split the total observation time into n_focals equally sized chunks of length focal_duration_s plus the break after
    focal_times <- split(1:time_in_s, # take full time
                         rep(1:n_focals,  # cut into n_focal chunks
                             each = (focal_duration_s + focal_break_time_s), # each encompassing the focal duration plus break
                             length.out = length(1:time_in_s)))
    
    # remove the break by only selecting the first focal_duration_s for each focal chunk
    focal_times <- lapply(focal_times, 
                          function(x) head(x, focal_duration_s))
    
    # for each focal, collect all the seconds in which they were the focal in a list
    focals <- lapply(1:group_size, 
                     function(x) unlist(focal_times[focal_id_list == x]))
    
    # for each focal, randomly remove time when focal was not observed, based on p_terrain_visibility
    focals <- lapply(focals,
                     function(x) sample(x, size = length(x) * p_terrain_visibility))
    
    # calculate the number of seconds in which focal behaviour was observed, 
    # by comparing the seconds in which each individual was observed with those in which they were active
    
    overlap_focal <- lapply(seq_along(focals), function(x){ # for each focal
      unlist(event_range[ids_events == x] # select all events where they were they focal and unlist the seconds
      ) %in% focals[[x]]})
    
    focal_seconds <- 
      lapply(seq_along(focals), function(x){ # for each focal
        sum(overlap_focal[[x]])# count how many of those seconds overlap with their focal seconds
      })
    
    # same for bouts: check in how many unique bouts at least one second overlapped
    focal_bouts <- lapply(seq_along(focals), function(x){
      length(unique(unlist(bout_range[ids_events == x])[overlap_focal[[x]]]))
    })
    
    ##############################################
    # Find observed behaviors using group scans
    
    # for the number of scans, assign all observed group members based on group size and visibility
    scan_id_list <- 
      lapply(1:num_scans, # all scans
             function(x) sample(1:group_size, size = round(p_terrain_visibility * group_size))) # assign group members
    
    # split the total observation time into num_scans equally sized chunks of length scan_duration plus the break after
    scan_times <- split(1:time_in_s, # take full time
                        rep(1:num_scans, # take number of scans
                            each = (scan_duration + scan_break_time_s), # full duration is scan duration plus break
                            length.out = length(1:time_in_s)))
    
    # remove the break by only selecting the first scan_duration for each scan chunk
    scan_times <- lapply(scan_times, function(x) head(x, scan_duration))
    
    # for each chunk, split it into x equally sized chunks of length scan_obsTime_perID, where x is the number of visible group members
    scan_times <- lapply(seq_along(scan_times), function(x) split(
      scan_times[[x]], #take each scan
      rep(1:length(unique(scan_id_list[[x]])), # take the number of individuals for that scan
          each = scan_obsTime_perID, # assign them their seconds based on the scan_obsTime_perID
          length.out = length(scan_times[[x]]))))
    
    # unlist both the times and the IDs to know which individual was scanned at which seconds
    scans_times_unlist <- scan_times %>% unlist(recursive = F) 
    scan_id_list_unlist <- scan_id_list %>% unlist(recursive = F)
    
    # aggregate by individual - for each individual, select all seconds where they were scanned
    scans <- lapply(1:group_size, 
                    function(x) unlist(scans_times_unlist[scan_id_list_unlist == x]))
    
    # check in how many unique bouts at least one second overlapped with an individual being scanned
    overlap_scan <- lapply(seq_along(scans), function(x){ # for each focal
      unlist(event_range[ids_events == x] # select all events where they were they focal and unlist the seconds
      ) %in% scans[[x]]})
    
    
    scan_bouts <- lapply(seq_along(scans), function(x){
      length(unique(unlist(bout_range[ids_events == x])[overlap_scan[[x]]]))
    })
    
    ##############################################
    # Evaluate scan vs. continuous sampling -based behavior observed
    
    # True rates/proportion
    true_prop_behav_perID <-
      n_events * behavior_duration / time_in_s # true proportion of time engaged in behavior X per ID
    true_rate_behav_perID <-
      n_events / time_in_s # true rate of behavior X per ID
    
    # Continuous-sampling-based estimates
    focaltime_perID <-
      sapply(focals, length) # seconds of continuous observation per ID
    focalsamples_perID <-
      sapply(focals, length) / focal_duration_s # number of focal observations per ID
    behav_timeObserved_focal_perID <-
      unlist(focal_seconds) # seconds of behavior observed per ID
    behav_boutsObserved_focal_perID <-
      unlist(focal_bouts) # number of bouts observed per ID
    focal_prop_perID <-
      unlist(focal_seconds) / sapply(focals, length) # observed proportion of time of behavior per ID
    focal_rate_perID <-
      unlist(focal_bouts) / sapply(focals, length) # observed rate of behavior per ID
    
    # Scan-sampling-based estimates
    scansamples_perID <-
      sapply(scans, length)/scan_obsTime_perID # number of scan samples per ID
    behav_boutsObserved_scan_perID <-
      unlist(scan_bouts) # number of bouts observed per ID
    scan_prop_perID <- behav_boutsObserved_scan_perID / scansamples_perID
    
    # Remove NAs
    if (any(is.nan(focal_prop_perID))){ focal_prop_perID[is.nan(focal_prop_perID)]=0 }
    if (any(is.nan(focal_rate_perID))){ focal_rate_perID[is.nan(focal_rate_perID)]=0 }
    if (any(is.nan(scan_prop_perID))){ scan_prop_perID[is.nan(scan_prop_perID)]=0 }
    
    # Pool results for later plotting
    true_prop_results <- c(true_prop_behav_perID)
    true_rate_results <- c(true_rate_behav_perID)
    scan_prop_results <- c(scan_prop_perID)
    focal_rate_results <- c(focal_rate_perID)
    focal_prop_results <- c(focal_prop_perID)
    
    gc()
    # return the used parameters and all variables so we can use them later
    return(
      list(
        n_days = n_days,
        group_size = group_size,
        p_behavior_visibility = p_behavior_visibility,
        p_terrain_visibility = p_behavior_visibility,
        mean_events = mean_events,
        sd_events = sd_events,
        n_events = n_events,
        behavior_duration = behavior_duration,
        n_hours = n_hours,
        focal_duration_min = focal_duration_min,
        focal_break_time_min = focal_break_time_min,
        scan_obsTime_perID = scan_obsTime_perID,
        scan_break_time_min = scan_break_time_min,
        focaltime_perID = focaltime_perID,
        focalsamples_perID = focalsamples_perID,
        behav_timeObserved_focal_perID = behav_timeObserved_focal_perID,
        behav_boutsObserved_focal_perID = behav_boutsObserved_focal_perID,
        focal_prop_perID = focal_prop_perID,
        focal_rate_perID = focal_rate_perID,
        scansamples_perID = scansamples_perID,
        behav_boutsObserved_scan_perID = behav_boutsObserved_scan_perID,
        scan_prop_perID = scan_prop_perID,
        true_prop_behav_perID = true_prop_behav_perID,
        true_rate_behav_perID = true_rate_behav_perID,
        true_prop_results = true_prop_results,
        true_rate_results = true_rate_results,
        scan_prop_results = scan_prop_results,
        focal_rate_results = focal_rate_results,
        focal_prop_results = focal_prop_results
      )
    )
  }



#' For each individual, for each simulation run, the difference between observed rate/probability and the expected 
#' true value is calculated; function calculates the root mean squared error of all individual values for each simulation
#' divided by the true value as a measure of accuracy for that simulation iteration.
#'
#' @param simulation_runs list produced by degree_simulation function
#' @param true_data name of the list object that holds the true, expected individual rates for each individual.
#' @param observed_data name of the list object that holds the observed individual rates for each individual.
#'
#' @return Function returns a vector with standardised root mean squared errors for each ID
#'
#'
#' @author Alex Mielke reviewed by Camille Testard
#' @export

# standardised root mean squared error
accuracy_perID <-
  function(simulation_runs,
           true_data = "true_prop_behav_perID",
           observed_data = "focal_prop_perID") {
    # Calculate the difference between observed and true data
    d <-
      sapply(simulation_runs, function(x) {
        (x[[observed_data]] - x[[true_data]])
      })
    
    # Calculate the RMSE (root mean squared error) for each ID
    RMSE <- apply(d, 1, function(row) {
      sqrt(mean(row^2))  # RMSE
    })
    
    # Get the range of the true values for normalization
    true_values <- sapply(simulation_runs, function(x) x[[true_data]])
    range_true_values <- apply(true_values, 1, function(row) {
      max(row)
    })
    
    # Calculate the NRMSE by normalizing RMSE with the range of true values
    NRMSE <- (RMSE / range_true_values)*100
    
    return(NRMSE)
  }

#' For each individual, for each simulation run, the difference between observed rate/probability and the expected 
#' true value is calculated; function calculates the mean error (*not squared*) of all individual values for each simulation 
#' iteration as a measure of bias for that simulation iteration.
#'
#' @param simulation_runs list produced by degree_simulation function
#' @param true_data name of the list object that holds the true, expected individual rates for each individual.
#' @param observed_data name of the list object that holds the observed individual rates for each individual.
#'
#' @return Function returns a vector with mean difference between true and observed values for each ID
#'
#'
#' @author Alex Mielke reviewed by Camille Testard
#' @export

bias_perID <-
  function(simulation_runs,
           true_data = "true_prop_behav_perID",
           observed_data = "focal_prop_perID") {
    bias <-
      sapply(simulation_runs, function(x) {
        (x[[observed_data]] - x[[true_data]])/x[[true_data]]
      })
    mean_bias <- apply(bias, 1, mean)
    return(mean_bias)
  }

#' Calculates precision of each individual's rate calculation from the list returned by the degree_simulation function when it is run in a loop
#'
#' For each individual, the mean and standard deviation of rates/probabilities across all iterations are calculated, and reported as Coefficient of Variation ((sd/mean) * 100).
#'
#' @param simulation_runs list produced by degree_simulation function
#' @param observed_data name of the list object that holds the observed individual rates for each individual.
#'
#' @return Function returns a vector with Coefficient of Variation for each Individual
#'
#'
#' @author Alex Mielke
#' @export


precision_perID <-
  function(simulation_runs,
           observed_data = "focal_prop_perID") {
    d <-
      do.call(cbind, sapply(simulation_runs, function(x) {
        x[observed_data]
      }))
    sd.d <- apply(d, 1, sd)
    mean.d <- apply(d, 1, mean)
    CV <- (sd.d / mean.d) * 100
    CV[is.na(CV)] <- max(CV, na.rm = TRUE)
    return(CV)
  }


