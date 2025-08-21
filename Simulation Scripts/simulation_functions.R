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
      n_events_total <- n_events * n_days
    } else {
      n_events_total <- (round(abs(rnorm(group_size, mean, sd))) + 1) * n_days
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
    n_hours_total <- n_hours * n_days # number of hours of observation in the day, times the number of days
    time_in_s <-
      n_hours_total * 60 * 60 # number of seconds of observations in a study period
    
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
    
    
    # --- Initiate outcomes of simulation ---
    focaltime_perID                 <- rep(NA_real_, group_size)  # seconds of focal observation per ID
    focalsamples_perID              <- focaltime_perID  # number of focal observations per ID
    Observed_focal_time_perID       <- focaltime_perID  # seconds of behavior observed per ID
    Observed_focal_bouts_perID      <- focaltime_perID  # number of bouts observed per ID
    focal_prop_perID                <- focaltime_perID  # [time in behavior X / total observation time]
    focal_rate_perID                <- focaltime_perID  # [#Events in behavior X / total observation time]
    scansamples_perID               <- focaltime_perID  # number of scan samples per ID
    Observed_scans_perID            <- focaltime_perID  # number of bouts observed per ID
    scan_prop_perID                 <- focaltime_perID  # [#Events in behavior X / total observation time]
    true_prop_behav_perID           <- focaltime_perID  # TRUE proportion of time engaged in behavior X
    true_rate_behav_perID           <- focaltime_perID  # TRUE rate at which behavior X occurs
    
    # create time sequences for behaviour duration, focal time, and scan time; each iteration picks from these sequences
    beh_time_seq <- seq.int(behavior_duration, floor(time_in_s - behavior_duration),
                            by = behavior_duration)
    
    # Initialize behaviors
    # = times at which each individual engaged in behavior X (unique identifier for each bout)
    
    #######################
    
    # Across individuals, randomly assign time for every event in study period
    event_times <- sample(
      beh_time_seq,
      sum(unlist(n_events_total)), replace = T
    ) 
    # sample behavioral events times (mid-point of behavior)
    # Sample behavioral event times during the day with a minimum
    # time lapse between behavioral events ('by' time).
    # Currently min time lapse = length of the behavior
    
    # Assign each event to one individual - each individual gets their value from n_events
    ids_events <- sample(unlist(sapply(1:group_size, 
                                       function(y) rep(y, n_events_total[y]))))
    
    # ensure that no individual has more than one event per second
    dup_idx <- !duplicated(cbind(event_times, ids_events))
    event_times <- event_times[dup_idx]
    ids_events <- ids_events[dup_idx]
    
    # for all events, set the seconds when it is observable
    event_range <- 
      lapply(event_times, function(x){
        seq.int(from = (x - round(behavior_duration/2) + 1), # sequence from minimum to maximum based on behaviour duration around mid point
                to = (x + round(behavior_duration/2)))})
    
        # Assign each bout a unique bout identifier
    bout_range <- split(rep(seq_along(event_range), each = behavior_duration + 1),
                        seq_along(event_range))
    
    bouts_events <- seq_along(event_range)
    
    # True number of bouts and seconds per individual
    
    # count the number of seconds that each individual is engaged in action for whole dataset
    all_events <- unlist(event_range, use.names = FALSE)
    # Map each element of all_events to its ID
    id_vec <- rep(ids_events, lengths(event_range))
    # Count the number of events per ID
    true_seconds <- as.list(tabulate(id_vec, nbins = group_size))
    
    # count the number of bouts that each individual is engaged in action for whole dataset
    bouts_split <- split(bouts_events, ids_events)
    true_bouts <- vapply(bouts_split, function(x) length(unique(x)), integer(1))
    
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
      # First: one focal follow per individual (random order)
      first_round <- sample.int(group_size)
      # Then: remaining focals assigned randomly with replacement
      remaining <- sample.int(group_size, n_focals - group_size, replace = TRUE)
      
      # Combine and shuffle
      focal_id_list <- sample.int(n_focals)[order(seq_len(n_focals))] # ❌ no, wrong
      focal_id_list <- sample(c(first_round, remaining))
    } else {
      # If there are more or equal #individuals in a day
      focal_id_list <- sample(1:group_size, n_focals)
    } # Loop through all focal individuals in the day (no repeats)

    # split the total observation time into n_focals equally sized chunks of length focal_duration_s plus the break after
    focal_times <- split(
      seq_len(time_in_s),                               # take full time
      rep(seq_len(n_focals),                            # cut into n_focal chunks
          each = focal_duration_s + focal_break_time_s, # focal duration plus break
          length.out = time_in_s)
    )
    # remove the break by only selecting the first focal_duration_s for each focal chunk
    focal_times <- lapply(focal_times, 
                          function(x) head(x, focal_duration_s))
    
    # for each focal, collect all the seconds in which they were the focal in a list
    # Group focal_times by focal_id_list in one go
    focals <- split(focal_times, focal_id_list)
    focals <- lapply(focals, unlist)# Unlist each group
    
    # Ensure every ID from 1:group_size exists (fill missing with integer(0))
    focals <- focals[as.character(seq_len(group_size))]
    focals[vapply(focals, is.null, logical(1))] <- list(integer(0))
    
    # for each focal, randomly remove time when focal was not observed, based on p_terrain_visibility
    focals <- lapply(focals,
                     function(x) sample(x, size = length(x) * p_terrain_visibility))
    
    # calculate the number of seconds in which focal behaviour was observed, 
    # by comparing the seconds in which each individual was observed with those in which they were active
    
    
    focal_seconds <- numeric(length(focals)) # preallocate vector for focal seconds
    focal_bouts <- numeric(length(focals))   # preallocate vector for focal bouts
    
    for (i in seq_along(focals)) {
      idx <- ids_events == i
      ev  <- unlist(event_range[idx])                  # seconds where focal = i
      overlap <- ev %in% focals[[i]]                   # overlap check
      
      focal_seconds[i] <- sum(overlap)                 # seconds overlap
      
      # bouts overlap
      focal_bouts[i] <- length(unique(unlist(bout_range[idx])[overlap]))
    }
    
    ##############################################
    # Find observed behaviors using group scans
    
    # for the number of scans, assign all observed group members based on group size and visibility
    scan_id_list <- replicate(
      num_scans,
      sample.int(group_size, size = round(p_terrain_visibility * group_size)),
      simplify = FALSE
    ) # assign group members
    
    # split the total observation time into num_scans equally sized chunks of length scan_duration plus the break after
    scan_times <- split(
      seq_len(time_in_s),
      rep(seq_len(num_scans),
          each = scan_duration + scan_break_time_s,
          length.out = time_in_s)
    )
    
    # remove the break by only selecting the first scan_duration for each scan chunk
    scan_times <- lapply(scan_times, function(x) head(x, scan_duration))
    
    # for each chunk, split it into x equally sized chunks of length scan_obsTime_perID, where x is the number of visible group members
    # Preallocate list for split scan times
    n_scans <- length(scan_times)
    split_scan_times <- vector("list", n_scans)
    
    for (i in seq_len(n_scans)) {
      ids <- unique(scan_id_list[[i]])                     # unique individuals for scan i
      n_ids <- length(ids)                                 # number of individuals
      split_scan_times[[i]] <- split(
        scan_times[[i]], 
        rep(seq_len(n_ids), each = scan_obsTime_perID, length.out = length(scan_times[[i]]))
      )
    }
    
    # unlist both the times and the IDs to know which individual was scanned at which seconds
    scans_times_unlist <- unlist(split_scan_times, recursive = FALSE)
    scan_id_list_unlist <- unlist(scan_id_list, recursive = FALSE)
    
    # aggregate by individual - for each individual, select all seconds where they were scanned
    # Precompute per-individual event seconds
    event_seconds_perID <- lapply(seq_len(group_size), function(x) unlist(event_range[ids_events == x]))
    
    # Aggregate by individual: count scans where at least one second overlaps with events
    scan_corrects <- integer(group_size)
    scans <- vector("list", group_size)
    
    for (i in seq_len(group_size)) {
      scan_list <- scans_times_unlist[scan_id_list_unlist == i]
      event_list <- event_seconds_perID[[i]]
      
      # Overlap check for each scan (fastmatch is used for vectorised matching)
      scan_corrects[i] <- sum(vapply(scan_list, function(y) any(!is.na(fmatch(y, event_list))),
                                     logical(1L)))
      
      # Store all scanned seconds
      scans[[i]] <- unlist(scan_list, use.names = FALSE)
    }
    
    # # check in how many unique bouts at least one second overlapped with an individual being scanned
    # overlap_scan <- lapply(seq_along(scans), function(x){ # for each focal
    #   unlist( # select all events where they were they focal and unlist the seconds
    #   ) %in% scans[[x]]})
    # 
    # # calculate the number of seconds in which focal behaviour was observed
    # scan_seconds <- 
    #   lapply(seq_along(scans), function(x){ # for each focal
    #     sum(overlap_scan[[x]])# count how many of those seconds overlap with their focal seconds
    #   })
    # 
    # scan_bouts <- lapply(seq_along(scans), function(x){
    #   length(unique(unlist(bout_range[ids_events == x])[overlap_scan[[x]]]))
    # })
    
    ##############################################
    # Evaluate scan vs. continuous sampling -based behavior observed
    
    # True rates/proportion
    true_prop_behav_perID <-
      unlist(true_seconds) / time_in_s # true proportion of time engaged in behavior X per ID
    true_rate_behav_perID <-
      unlist(true_bouts) / time_in_s # true rate of behavior X per ID
    
    # Continuous-sampling-based estimates
    focaltime_perID <-
      sapply(focals, length) # seconds of continuous observation per ID
    focalsamples_perID <-
      sapply(focals, length) / focal_duration_s # number of focal observations per ID
    Observed_focal_time_perID <-
      unlist(focal_seconds) # seconds of behavior observed per ID
    Observed_focal_bouts_perID <-
      unlist(focal_bouts) # number of bouts observed per ID
    focal_prop_perID <-
      unlist(focal_seconds) / sapply(focals, length) # observed proportion of time of behavior per ID
    focal_rate_perID <-
      unlist(focal_bouts) / sapply(focals, length) # observed rate of behavior per ID
    
    # Scan-sampling-based estimates
    scansamples_perID <-
      sapply(scans, length)/scan_obsTime_perID # number of scan samples per ID
    Observed_scans_perID <-
      unlist(scan_corrects) # number of bouts observed per ID
    scan_prop_perID <- Observed_scans_perID / scansamples_perID
    
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
        p_terrain_visibility = p_terrain_visibility,
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
        Observed_focal_time_perID = Observed_focal_time_perID,
        Observed_focal_bouts_perID = Observed_focal_bouts_perID,
        focal_prop_perID = focal_prop_perID,
        focal_rate_perID = focal_rate_perID,
        scansamples_perID = scansamples_perID,
        Observed_scans_perID = Observed_scans_perID,
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


