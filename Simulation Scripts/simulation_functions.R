#' Simulation to evaluate the performance of focal follows and group scans in capturing behavioural data accurately and precisely
#'
#' The function assigns each focal individuals a probability to engage in a behaviour and 
#'    sets specific system, behaviour, and observation parameters that can influence the performance of the two methods.
#'    It then simulates the occurrence of the behaviour across a study period for each individual (the true values), 
#'    and simulates observing the behaviour using focal follows and group scans, generating the behavioral estimates for each individual.
#'
#' @param group_size integer. number of individuals in the study population; it is assumed that all of these will be sampled as focal subjects at the same rate
#' @param n_days integer. number of days the simulated study runs; days are assumed independent from each other.
#' @param p_behavior_visibility 0-1. probability that a behaviour is observed during a focal or scan. This is implemented by making a proportion of behavior *bouts* non-visible (*not* by making certain seconds of the behavior bout non-visible)
#' @param p_terrain_visibility 0-1. probability that an individual is observed during a focal or scan. This is implemented by making a proportion of the focal follows or group scans non-visible (random).
#' @param mean_events numeric. mean number of bouts of the behaviour each individual engages in per day.
#' @param sd_events numeric. standard deviation of the mean number of bouts of the behaviour each individual engages in per day.
#' @param n_events numeric. vector of the same length as group size. lets user reuse same individual distributions across simulations; if NULL, mean and sd are used to calculate new; if vector of length group_size is provided, those are used to allow for same distribution across simulation runs
#' @param behavior_duration integer. duration of behaviour in seconds.
#' @param n_hours integer. number of observation hours in each day (determines the number of focals and scans possible)
#' @param focal_duration_min integer. duration of each focal follow (in minutes).
#' @param focal_break_time_min integer. duration of break between two focal follows (in minutes); restricts number of focals possible in a day
#' @param scan_obsTime_perID integer. number of seconds it takes to observe each available individual in a scan.
#' @param scan_break_time_min integer. number of minutes between the end of one scan and the beginning of the new scan.
#'
#' @return function returns a list of all input parameters and matrices with individual-level behavioural estimates and true values from the simulated focal follows and group scans for further analysis
#'
#' @param simulation_runs list produced by behaviour_simulation function
#' @param true_data name of the list object that holds the true occurences of simulated behaviour for each individual.
#' @param observed_data name of the list object that holds the behavioural estimates for each individual.
#'
#' @export

behaviour_simulation <-
  function(n_days,
           group_size,
           p_behavior_visibility,
           p_terrain_visibility,
           mean_events,
           sd_events,
           n_events = NULL,
           behavior_duration,
           n_hours,
           focal_duration_min,
           focal_break_time_min,
           scan_obsTime_perID,
           scan_break_time_min) {
    
    ## 1. parameterize study population
    group_size <- group_size
    p_terrain_visibility <- p_terrain_visibility
    
    ## 2. parameterize behaviour
    mean <- mean_events
    sd <- sd_events
    if (!is.null(n_events)) { # if n_events is provided, scale it by the number of days
      n_events <- n_events * n_days
    } else { # otherwise, generate random daily number of behaviour bouts for each individual (minimum 1 per day) and then scale by the number of days
      n_events <- (round(abs(rnorm(group_size, mean, sd))) + 1) * n_days
    }
    
    behavior_duration <- behavior_duration
    p_behavior_visibility <- p_behavior_visibility
    
    ## 3. parameterize observation methods
    n_hours <- n_hours * n_days # total observation time = number of hours of observation per day, times the number of days
    time_in_s <- n_hours * 60 * 60 # number of seconds of observations in a study period
    
    # focal follows
    focal_duration_min <- focal_duration_min
    focal_duration_s <- focal_duration_min * 60 # focal duration in seconds
    focal_break_time_min <- focal_break_time_min 
    focal_break_time_s <- focal_break_time_min * 60 # minimum break time between focal follows in seconds
    n_focals <- round(time_in_s / (focal_duration_s + focal_break_time_s), 0) # number of focal follows in study period
    total_focal_s <- n_focals * focal_duration_s # focal follow observation time in seconds
    
    # group scans
    scan_obsTime_perID <- scan_obsTime_perID
    scan_duration <- scan_obsTime_perID * (round(p_terrain_visibility * group_size)) # duration of a scan observation in sec, scales with group size &terrain visibility
    scan_break_time_s <- scan_break_time_min * 60 # minimum break time between scans in seconds
    num_scans <- round(time_in_s / (scan_duration + scan_break_time_s)) # number of scans that can be done in the study period
    
    ## 4. initiate outcomes of simulation
    focaltime_perID <- rep(NA, group_size) # seconds of focal observation per ID
    focalsamples_perID <- rep(NA, group_size) # number of focal follows per ID
    behav_timeObserved_focal_perID <- rep(NA, group_size) # seconds of behaviour observed using focal follows per ID
    behav_boutsObserved_focal_perID <- rep(NA, group_size) # number of behaviour bouts observed using focal follows per ID
    focal_prop_perID <- rep(NA, group_size) # observed proportion of time engaged in behavior using focal follows per ID [time in behaviour / total observation time]
    focal_rate_perID <- rep(NA, group_size) # observed frequency of behaviour per ID [bouts of behaviour / total observation time]
    
    scansamples_perID <- rep(NA, group_size) # number of scans per ID
    behav_boutsObserved_scan_perID <- rep(NA, group_size) # number of scans with the behaviour occuring per ID
    scan_prop_perID <- rep(NA, group_size) # observed proportion of scans in which the behaviour occured per ID [scans with behaviour / total number of scans]
    
    true_prop_behav_perID <- rep(NA, group_size) # TRUE proportion of time engaged in behaviour per ID
    true_rate_behav_perID <- rep(NA, group_size) # TRUE frequency of behaviour per ID
    
    ## 5. simulate true behavioural occurences
    
    # create time points when a behavior bout could start, spaced by behavior_duration
    beh_time_seq <- seq.int(behavior_duration, floor(time_in_s - behavior_duration),
                            by = behavior_duration)
    
    # across individuals, randomly assign time at which each behaviour bouts occur throughout the study period
    event_times <- sample(beh_time_seq, sum(unlist(n_events)), replace = T) 
    
    # for each behaviour bout, set the seconds when it is occuring as the behaviour duration evenly spread around the event_time
    event_range <- 
      lapply(event_times, function(x){
        seq.int(from = (x - round(behavior_duration/2) + 1),
                to = (x + round(behavior_duration/2)))})
    
    # assign each bout to one individual (each individual gets a given number of random events assigned based on their value of n_events)
    ids_events <- sample(unlist(sapply(1:group_size, 
                                       function(y) rep(y, n_events[y]))))
    
    # assign each bout a unique identifier
    bout_range <- lapply(seq_along(event_range), function(x){
      rep(x, behavior_duration + 1)
    })
    bouts_events <- seq_along(event_range)
    
    # 6. calculate the true proportion of time spent engaged in the behaviour and the frequency of the behaviour for each individual
    
    # count the true number of seconds that each individual is engaged in the behaviour across the study period
    true_seconds <- lapply(1:group_size, function(x){
      length(unlist(event_range[ids_events == x]))
    }) 
    
    # count the true number of bouts that each individual is engaged in the behaviour across the study period
    true_bouts <- lapply(1:group_size, function(x){
      length(unique(bouts_events[ids_events == x]))
    })
    
    # 7. simulate the observation of behavioural occurences for each individual
    
    # randomly remove a subset of behaviours that are missed based on p_behavior_visibility 
    
    vis_include <- sample(bouts_events, size = length(bouts_events) * p_behavior_visibility)
    
    bout_range <- bout_range[vis_include]
    ids_events <- ids_events[vis_include]
    event_range <- event_range[vis_include]
    
    # FOCAL FOLLOWS
    
    # set list of focal subjects across the full study period
    if (group_size < n_focals) {
      # if there are fewer individuals than the number of focal follows in a day
      # make sure that each individual is selected at least once, by selecting the first focals without replacement, after with replacement
      focal_id_list <-
        sample(
          c(sample(1:group_size, group_size, replace = F), # first make sure that each individual is selected at least once
            sample(1:group_size, (n_focals-group_size), replace = T))) # after, assign individuals randomly as focal subjects
    } else {
      # if there are more (or equal number of) individuals than the number of focal follows in a day
      focal_id_list <- sample(1:group_size, n_focals)
    } # randomly select individuals as folcol follow subjects (no repeats)
    
    # split the total observation time into n_focals equally sized chunks of length focal_duration_s + focal_break_time_s
    focal_times <- split(1:time_in_s, # take full time
                         rep(1:n_focals, # cut into n_focal chunks
                             each = (focal_duration_s + focal_break_time_s), # each encompassing the focal duration plus break
                             length.out = length(1:time_in_s)))
    
    # remove the break time by keeping only the first focal_duration_s seconds from each focal follow chunk
    focal_times <- lapply(focal_times, 
                          function(x) head(x, focal_duration_s))
    
    # for each focal subject, collect all the seconds in which they were the focal throughout the study period in a list
    focals <- lapply(1:group_size, 
                     function(x) unlist(focal_times[focal_id_list == x]))
    
    # for each focal, randomly remove time when focal was not observed, based on p_terrain_visibility
    focals <- lapply(focals,
                     function(x) sample(x, size = length(x) * p_terrain_visibility))
    
    # calculate the number of seconds in which the focal subject was observed engaging in the behaviour by finding the seconds in which they were engaged in the behaviour & were the focal subject
    overlap_focal <- lapply(seq_along(focals), function(x){ # for each focal subject
      unlist(event_range[ids_events == x] # select all events that belong it (ie seconds where the focal is engaged in the behaviour) and unlist the seconds
      ) %in% focals[[x]]})
    
    focal_seconds <- 
      lapply(seq_along(focals), function(x){ # for each focal
        sum(overlap_focal[[x]]) # count how many of those seconds overlap with the seconds during which they are the focal subject
      })
    
    # for bouts: check in how many unique bouts at least one second overlapped with the focal being observed
    focal_bouts <- lapply(seq_along(focals), function(x){
      length(unique(unlist(bout_range[ids_events == x])[overlap_focal[[x]]]))
    })
    
    # GROUP SCANS
    
    # for each scan, randomly select visible individuals based on p_terrain_visibility
    scan_id_list <- 
      lapply(1:num_scans, # all scans
             function(x) sample(1:group_size, size = round(p_terrain_visibility * group_size)))
    
    # split the total observation time into num_scans equally sized chunks of length scan_duration plus the break after
    scan_times <- split(1:time_in_s, # take full time
                        rep(1:num_scans, # take number of scans
                            each = (scan_duration + scan_break_time_s), # full duration is scan duration plus break
                            length.out = length(1:time_in_s)))
    
    # remove the break time by keeping only the first scan_duration seconds from each scan chunk
    scan_times <- lapply(scan_times, function(x) head(x, scan_duration))
    
    # split each scan into chunks of scan_obsTime_perID per visible individual
    scan_times <- lapply(seq_along(scan_times), function(x) split(
      scan_times[[x]], #take each scan
      rep(1:length(unique(scan_id_list[[x]])), # take the number of individuals for that scan
          each = scan_obsTime_perID, # assign them their seconds based on the scan_obsTime_perID
          length.out = length(scan_times[[x]]))))
    
    # unlist both the times and the IDs to know which individual was scanned at which seconds
    scans_times_unlist <- unlist(scan_times, recursive = F) 
    scan_id_list_unlist <- unlist(scan_id_list, recursive = F)
    
    # for each individual, select all seconds where they were scanned
    scans <- lapply(1:group_size, 
                    function(x) unlist(scans_times_unlist[scan_id_list_unlist == x]))
    
    # calculate the number of scans in which the individual was observed engaging in the behaviour by finding the seconds in which they were engaged in the behaviour & were they were scanned
    overlap_scan <- lapply(seq_along(scans), function(x){ # for each individual
      unlist(event_range[ids_events == x] # select all events that belong it (ie seconds where the focal is engaged in the behaviour) and unlist the seconds
      ) %in% scans[[x]]})
    
    scan_bouts <- lapply(seq_along(scans), function(x){ # for each individual
      length(unique(unlist(bout_range[ids_events == x])[overlap_scan[[x]]])) # count how many of those seconds overlap with the seconds during which they are scanned
    })
    
    # 8. calculate the the true and observed proportions of time spent engaged in the behaviour for each individual
    
    # true values
    true_prop_behav_perID <- n_events * behavior_duration / time_in_s
    true_rate_behav_perID <- n_events / time_in_s
    
    # behaviour estimates from focal follows
    focaltime_perID <- sapply(focals, length) # seconds of focal follow observation per ID
    focalsamples_perID <- sapply(focals, length) / focal_duration_s # number of focal observations per ID
    behav_timeObserved_focal_perID <- unlist(focal_seconds) # seconds of behavior observed per ID
    behav_boutsObserved_focal_perID <-  unlist(focal_bouts) # number of bouts observed per ID
    focal_prop_perID <- unlist(focal_seconds) / sapply(focals, length) # observed proportion of time of behavior per ID
    focal_rate_perID <- unlist(focal_bouts) / sapply(focals, length) # observed rate of behavior per ID
    
    # behaviour estimates from group scans
    scansamples_perID <- sapply(scans, length)/scan_obsTime_perID # number of scans per ID
    behav_boutsObserved_scan_perID <- unlist(scan_bouts) # number of scans during which behaviour occured per ID
    scan_prop_perID <- behav_boutsObserved_scan_perID / scansamples_perID # proportion of scans with behaviour per ID
    
    # remove NAs
    if (any(is.nan(focal_prop_perID))){ focal_prop_perID[is.nan(focal_prop_perID)]=0 }
    if (any(is.nan(focal_rate_perID))){ focal_rate_perID[is.nan(focal_rate_perID)]=0 }
    if (any(is.nan(scan_prop_perID))){ scan_prop_perID[is.nan(scan_prop_perID)]=0 }
    
    # pool results for plotting
    true_prop_results <- c(true_prop_behav_perID)
    true_rate_results <- c(true_rate_behav_perID)
    scan_prop_results <- c(scan_prop_perID)
    focal_rate_results <- c(focal_rate_perID)
    focal_prop_results <- c(focal_prop_perID)
    
    gc() # run garbage collection to free up memory
    
    # 9. return the used parameters and all variables to use for plotting and analyses
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


#' For each individual, for each simulation run, the behavioral estimates obtained from the simulated focal follows and group scans 
#'    are compared to the true values - accuracy, precision, bias and correlation to the true values is calculated to estimate performance of the two methods.
#'
#' ACCURACY
#' The mean root squared error across individuals for each simulation iteration is calculated as a measure of accuracy for that simulation iteration.
#'
#' @param simulation_runs list produced by behaviour_simulation function
#' @param true_data name of the list object that holds the true behavioral values for each individual.
#' @param observed_data name of the list object that holds the behavioral estimates for each individual.
#'
#' @return Function returns a vector with standardised root mean squared errors for each ID
#'
#' @export

accuracy_perID <-
  function(simulation_runs,
           true_data = "true_prop_behav_perID",
           observed_data = "focal_prop_perID") {
    # calculate the difference between observed and true data
    d <- 
      sapply(simulation_runs, function(x) {
        (x[[observed_data]] - x[[true_data]])
      })
    
    # calculate the RMSE (root mean squared error) for each ID
    RMSE <- apply(d, 1, function(row) {
      sqrt(mean(row^2))  # RMSE
    })
    
    # get the range of the true values for standardization
    true_values <- sapply(simulation_runs, function(x) x[[true_data]])
    range_true_values <- apply(true_values, 1, function(row) {
      max(row)
    })
    
    # calculate the standardized RMSE by standardising RMSE with the range of true values
    SRMSE <- (RMSE / range_true_values)*100
    
    return(SRMSE)
  }

#' BIAS
#' The mean error of all individual values for each simulation iteration is calculated as a measure of bias for that simulation iteration.
#'
#' @param simulation_runs list produced by behaviour_simulation function
#' @param true_data name of the list object that holds the true behavioral values for each individual.
#' @param observed_data name of the list object that holds the behavioral estimates for each individual.
#'
#' @return Function returns a vector with mean difference between true and observed values for each ID

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

#' PRECISION
#' For each individual, the coefficient of variation of behavioral estimates across all iterations are calculated as a measure of precision for that individual.
#'
#' @param simulation_runs list produced by behaviour_simulation function
#' @param observed_data name of the list object that holds the behavioral estimates for each individual.
#'
#' @return Function returns a vector with coefficient of variation for each ID
#'
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