library('move2')
library('lubridate')
library('magrittr')
library('dplyr')
library('ggplot2')
library('data.table')
library('sf')
library('units')
library('stringr')
library('tidyr') 
library('MRSea')
library("purrr")
library("zoo")
library("spatstat.utils")

`%!in%` <- Negate(`%in%`)


# Main RFunction -------------------------------------------------------------------------------------

rFunction = function(data, travelcut,
                     create_plots = TRUE,
                     sunrise_leeway = 0,
                     sunset_leeway = 0,
                     altbound = 25,
                     keepAllCols = FALSE
                     # second_stage_model = NULL, fit_speed_time  # Will be reincorporated later
) {
  

  # Validate Input Data ----------------------------------------------------------------------------------------------
  
  logger.trace(paste0(
    "Input data provided:  \n", 
    "travelcut: ", toString(travelcut), "\n",
    "sunrise_leeway: ", toString(sunrise_leeway), "\n", 
    "sunset_leeway: ", toString(sunset_leeway), "\n",
    "altbound: ", toString(altbound), "\n",
    "input data dimensions: ", toString(dim(data))
  ))
  
  logger.info("Starting input validation")
  
  if(nrow(data) == 0) {
    logger.warn("Input data is empty. Returning input.")
    return(data)}
  
  
  if(travelcut <=  0 | !is.numeric(travelcut)) {
    logger.fatal("Speed cut-off for travelling behavour is not a valid speed. Returning input - please use valid settings")
    stop("Speed cut-off for travelling behavour (`travelcut`) is not a valid speed. Returning input - please use valid settings")
  }
  
  
  ## altbound ----
  ## (only if column named "altitude" is in input dataset)
  
  if("altitude" %in% colnames(data)){
    
    if(!is.numeric(altbound)) {
      logger.warn("`altbound` is non-numeric. Stopping computation - please check inputs")
      stop("Altitude change threshold (`altbound`) is non-numeric. Please check input settings")
      
    } else if(altbound == 0){
      
      logger.warn(
        paste0(" |- Beware that altitude threshold (`altbound`) is set to 0m, which ",
               "means that ANY change is altitude will be considered as ascencing/descending ",
               "movement.")
      )
    }  
  } 
  
  ## 'leeway' inputs ----
  if(is.null(sunrise_leeway)) {
    logger.warn(" |- No sunrise leeway provided as input. Defaulting to no leeway.")
    sunrise_leeway <- 0
  }
  if(is.null(sunset_leeway)) {
    logger.warn(" |- No sunset leeway provided as input. Defaulting to no leeway.")
    sunset_leeway <- 0
  }
  
  
  ## Input data: time-related columns -----
  if("altitude" %!in% colnames(data)){
    logger.warn(" |- Column `altitude` is absent from input data. Unable to calculate altitude changes.")
  } else{
    logger.info(" |- `altitude` column identified. Able to detect altitude changes.")
  }
  
  
  if ("timestamp_local" %!in% colnames(data)) {
    logger.warn(
      paste0(
        " |- `timestamp_local` is not a column within the data. Classification process ", 
        "may be flawed if timezones of time-related columns are not consistent. ",
        "Please use 'Add Local and Solar Time' MoveApp to generate this column.")
    )
  }
  
  
  if ("sunrise_timestamp" %!in% colnames(data) | "sunset_timestamp" %!in% colnames(data)) {
    logger.fatal("`sunrise_timestamp` or `sunset timestamp` is not a column in the input data. Sunrise-sunset classification cannot be performed. Terminating - please use the 'Add Local and Solar Time' MoveApp in this workflow BEFORE this MoveApp")
    stop(
      paste0(
        "Either `sunrise_timestamp` or `sunset timestamp` is not a column in the ",
        "input data. Identification of night-time points is fundamental for the ",
        "classification process. Please use the 'Add Local and Solar Time' MoveApp in ",
        "the workflow BEFORE this MoveApp to add the sought columns.")
    )
  } else {
    logger.info(" |- Sunrise and sunset columns identified. Able to perform night-time identification.")
  }
  
  
  
  logger.info(" |- Input is in correct format. Proceeding with data preparation.")
  
  
  # Data Preparation --------------------------------------------------------------------------------------
  logger.info("Initiate Data Preparation Steps")
  
  ## Generate overarching variables  ------
  logger.info("Data Preparation - generate overarching variables")
  
  data %<>% dplyr::mutate(
    ID = mt_track_id(.),
    timestamp = mt_time(.)
  )
  
  if ("timestamp_local" %in% colnames(data)) {
    data %<>% 
      mutate(
        yearmonthday = stringr::str_replace_all(stringr::str_sub(timestamp_local, 1, 10), "-", ""),
        hourmin = lubridate::hour(timestamp_local) + 
          lubridate::minute(timestamp_local)/60 + 
          lubridate::second(timestamp_local)/3600
      ) 
  } else {
    data %<>% 
      mutate(
        yearmonthday = stringr::str_replace_all(stringr::str_sub(timestamp, 1, 10), "-", ""),
        hourmin = lubridate::hour(timestamp) + 
          lubridate::minute(timestamp)/60 + 
          lubridate::second(timestamp)/3600
      )                     
  }

  
  data %<>% 
    arrange(mt_track_id(data), mt_time(data)) %>%
    # distinct(timestamp, .keep_all = TRUE) %>%
    mutate(
      timediff_hrs = as.vector(mt_time_lags(., units = "hours")),
      dist_m = as.vector(mt_distance(., units = "m")),
      kmph = as.vector(mt_speed(., units = "km/h"))
    ) 
  

  
  ## Identify stationary points ------------------------------------------------
  
  logger.info("Data Preparation - identify stationary points")
  
  #' Identify stationary points
  #' - events with speed <= travelcut == stationary (1),
  #' - events where speed > travelcut == non-stationary (0),
  #' - events with NA speed == stationary (1)

  data %<>%
    mutate(
      stationary = case_when(
        kmph <= travelcut ~ 1,
        kmph > travelcut ~ 0,
        is.na(kmph) | is.nan(kmph) ~ 1 # assume stationary if no data
      )
    )
  
  
  
  ## Detect Altitude Changes ----------------------------------------
  
  #' Categorize vertical movement based on altitude change
  #'  (i) change in altitude to next location > threshold: altchange == "ascent"
  #'  (ii) change in altitude to next location < -threshold: altchange == "descent"
  #'  (iii) else (including NAs): altchange == "flatline"
  
  logger.info(" |- Categorize altitude change between consecutive locations")
  
  if ("altitude" %in% colnames(data)) {
      
    # Classify altitude changes
    data %<>% 
      # Reset altitude change each day:
      group_by(ID, yearmonthday) %>%
      dplyr::mutate(
        altitude = as.numeric(altitude), # fix when input is character vector
        
        altdiff = dplyr::lead(altitude) - altitude,
        
        altchange = case_when(
          altdiff < -altbound ~ "descent",
          altdiff > altbound ~ "ascent",
          .default = "flatline"
        )) %>% 
      ungroup()
    
  } else {
    logger.warn(" |- Unable to calculate altitude changes. Vertical movement will not be considered for classification.")
  }
  
  
  
  ## Night-time point identification ----------------------------

  #' (i) nightpoint == 0 if sunrise_timestamp < timestamp < sunrise_timestamp (+/- leeway), 
  #' (ii) otherwise nightpoint == 1
  
  logger.info(" |- Identify night-time locations")

  data %<>% mutate(
    nightpoint = ifelse(
      between(
        lubridate::with_tz(timestamp, lubridate::tz(sunrise_timestamp)), # with_tz() ensures TZs consistency
        sunrise_timestamp + lubridate::minutes(sunrise_leeway), 
        sunset_timestamp + lubridate::minutes(sunset_leeway)
      ), 
      0, 1)
    )
  
  
  
  ## Calculate ACC variance ---------------
  
  #' If ACC data is available, calculate variance in acceleration bursts
  #' till subsequent location event - expect one variance statistic for each enabled ACC axes
  
  if ("acc_dt" %in% colnames(data)) {
    
    # logical flag for all-NULL 'acc_dt' column 
    acc_null <- all(purrr::map_lgl(data$acc_dt, is.null))
    
    if(!acc_null){
      logger.info(" |- ACC data identified: calculating variance in acceleration between locations")
      
      # Unnest ACC data and compute ACC variance between consecutive locations
      data <- acc_var(data, interpolate = FALSE) |> 
        dplyr::select(-acc_dt)
      
      ACCclassify <- TRUE
      
    } else {
      
      data <- dplyr::select(data, -acc_dt)
      ACCclassify <- FALSE
      
    }
  } else{
    
    ACCclassify <- FALSE
    
  }  
  
  if(!ACCclassify) logger.info("  |- No accelerometer data detected: skipping ACC preparation")
  
  
  ## 5. Roosting Reclassification Prep -----------------------------------------------------------------------------------------------
  
  
  logger.info("[5] Preparing overnight-roost data")
  
  data %<>% 
    mutate(temptime = mt_time(.)) %>%
    group_by(ID, yearmonthday) %>%
    mutate(
      # Mark the first night point in the evening, and final in the morning:
      endofday = case_when(
        nightpoint == 1 & lag(nightpoint) == 0 ~ "FINAL",
        nightpoint == 1 & lead(nightpoint) == 0 ~ "FIRST", 
        TRUE ~ NA
      ) ) %>% ungroup() %>%
    
    # Calculate time difference from each timestamp to sunrise/sunset (and their leeways):
    mutate(
      sunrise_difference = difftime(.$temptime, .$sunrise_timestamp + minutes(sunrise_leeway), units = "mins") %>% abs(),
      sunset_difference = difftime(.$temptime, .$sunset_timestamp + minutes(sunset_leeway), units = "mins") %>% abs()
    ) %>%
    group_by(ID, yearmonthday) %>%
    
    mutate(closest = case_when(
      # Mark the closest timestamps to sunrise/sunset, which will proxy for the absence of night points:
      sunrise_difference == min(sunrise_difference, na.rm = T) ~ "SUNRISE",
      sunset_difference == min(sunset_difference, na.rm = T) ~ "SUNSET", 
      TRUE ~ NA
    ))
  
  logger.trace("    Identifying locations for overnight roosting checks")
  # Identify which days don't have night points in the morning/at night:
  missing_nightpoints <- data %>%
    as.data.frame() %>%
    group_by(ID, yearmonthday) %>%
    summarise(
      morning = ifelse(any(endofday == "FIRST"), 1, 0),
      evening = ifelse(any(endofday == "FINAL"), 1, 0),
      .groups = "keep"
    )
  
  # Join to data:
  data %<>% left_join(missing_nightpoints, by = c("ID", "yearmonthday"))
  
  # And 'patch' these days up using the sunrise/sunset time-difference proxies:
  data %<>% mutate(
    endofday = case_when(
      # If there is no morning point, but this is the nearest timestamp to sunset, 
      # use it as a proxy:
      is.na(endofday) & is.na(morning) & closest == "SUNRISE" ~ "FIRST",
      # Same applies to evening:
      is.na(endofday) & is.na(evening) & closest == "SUNSET" ~ "FINAL",
      TRUE ~ endofday
    )
  ) %>% dplyr::select(-c("morning", "evening", "closest", "sunrise_difference", "sunset_difference"))
  
  # Shortcut for calculating night-distances:
  # Filter dataset to only the marked final/first point
  # Bind distance using mt_distance and keep only overnight distances
  # then merge back into main dataset
  logger.trace("    Generating overnight roosting distances")
  nightdists <- data %>% 
    filter(!is.na(endofday)) %>% 
    ungroup() %>%
    mutate(endofday_dist = mt_distance(.),
           endofday_dist = ifelse(
             endofday == "FINAL", endofday_dist, NA
           )) %>%
    as.data.frame() %>%
    dplyr::select(c("ID", mt_time_column(.), "endofday_dist"))
  data %<>% left_join(nightdists, by = c("ID", mt_time_column(.)))
  # This gives us one overnight-distance measure at the end of each bird's day
  data %<>% mutate(
    roostsite = ifelse(
      !is.na(endofday_dist) & endofday_dist < 15,
      1, 0
    )
  ) 
  
  logger.trace("    Generating roost-group data")
  #  Calculate cumulative travel and reverse cumulative travel per day
  data %<>%
    group_by(ID, yearmonthday) %>%
    mutate(travel01 = ifelse(stationary == T, 0, 1)) %>%
    mutate(cum_trav = cumsum(travel01),
           revcum_trav = spatstat.utils::revcumsum(travel01)) %>%
    ungroup() %>%
    mutate(
      # Generate runs of stationary behaviour before/after final/first location:
      roostgroup = ifelse(cum_trav == 0 | revcum_trav == 0, 1, 0),
      roostgroup = rleid(roostgroup),
      roostgroup = ifelse(cum_trav != 0 & revcum_trav != 0, NA, roostgroup)
    ) 
  
  
  ## 6. Cumulative Stationary Time Reassignment ------------------------------------------
  
  logger.info("[6] Preparing cumulative-time-spent-stationary data")
  
  # This will be generated in the reclassification step
  # as it needs to filter out any roostgroup locations
  
 
  ## 7. Speed-Time Reclassification --------------------------------------------
  
  #logger.trace("[7] Preparing speed-time model data")
  # This is where the second-stage models will go, once reworked
  #'
  #'
  
  # *** move this section to the standardising app???** ##
  # Probably more efficient ways to do this #
  
  store <- NULL
  for (bird in unique(data$ID)){
    #print(bird)
    newdat <- data %>%
      filter(ID == bird) %>%
      mutate(month = month(timestamp),
             response = kmph + 0.00001) %>%
      filter(!is.na(response), 
             response < travelcut,
             timestamp > (max(timestamp) - days(30))) 
    
    # ** add if statement or similar to ensure that if not enough data, 
    # this modelling is not done (e.g. need 10 days??) ** #
    # ALSO what happens if more than 30 days data provided??
    
    # predict to full dataset (we'll ignore the travelling points in the classification later)
    preddat <- filter(data, ID == bird)
    
    initialModel <- glm(response  ~ 1 , family = Gamma(link="log"), data = newdat)
    
    salsa1dlist <- list(fitnessMeasure = 'BIC', 
                        minKnots_1d = c(1), 
                        maxKnots_1d = c(5), 
                        startKnots_1d = c(1), 
                        degree = c(2), 
                        maxIterations = 10,
                        gaps = c(0))
    
    # run SALSA
    fit<-runSALSA1D(initialModel, 
                    salsa1dlist, 
                    varlist=c("hourmin"), 
                    splineParams=NULL, 
                    datain=newdat, 
                    predictionData = filter(preddat, !is.na(kmph)),
                    panelid = newdat$yearmonthday, 
                    suppress.printout = TRUE)$bestModel
    

    
    preddat$kmphpreds <- predict(object = fit, newdata = preddat)
    
    boots <- do.bootstrap.cress.robust(model.obj = fit, 
                                       predictionGrid = preddat, 
                                       B = 1000, robust = TRUE, 
                                       cat.message = FALSE)
    cis <- makeBootCIs(boots)
    
    # -- Construct prediction intervals
    d = summary(fit)$dispersion
    predint <- apply(boots, 2, function(x){rgamma(n = length(x), shape = 1/d, scale= x*d)})
    pis <- t(apply(predint, 1, FUN = quantile,probs = c(0.025, 0.975)))
    
    preddat <- preddat %>% 
      mutate("kmphCI2.5" = cis[,1],
             "kmphCI97.5" = cis[,2],
             "kmphPI2.5" = pis[,1], 
             "kmphPI97.5" = pis[,2]) 
    
    # ultimately probably only need to keep last column (kmphPI97.5)
    
    store <- rbind(store, preddat)
    # store the model objects too??
  }
  
  # join stored thresholds back to main data
  data <- store
  rm(store)
  
  
  # PERFORM CLASSIFICATION STEPS 1-7 -------------------------------------------------------
  
  logger.info("All data prepared. Performing all classification steps")
  
  
  #### 1. Speed Classification ----
  logger.info("[1] Performing speed classification")
  data %<>% mutate(
    # Add column to explain classification:
    RULE = ifelse(stationary == 1, "[1] Low speed","[1] High speed"), 
    behav = ifelse(stationary == 1, "SResting", "STravelling")
  )
  
  # Log results
  logger.trace(paste0("   ", sum(data$behav == "SResting", na.rm = T), " locations classified as SResting"))
  logger.trace(paste0("   ", sum(data$behav == "STravelling", na.rm = T), " locations classified as STravelling"))
  
  
  #### 2. Altitude Classification ----
  logger.info("[2] Performing altitude classification")
  data %<>%
    mutate(
      RULE = case_when(
        (behav == "SResting") & (altchange == "ascent") ~ "[2] Altitude increasing",
        (behav == "SResting") & (altchange == "descent") & (lead(altchange) %in% c("descent", "ascent")) ~ "[2] Altitude decreasing",
        TRUE ~ RULE
      ),
      behav = case_when(
        (behav == "SResting") & (altchange == "ascent") ~ "STravelling",
        (behav == "SResting") & (altchange == "descent") & (lead(altchange) %in% c("descent", "ascent")) ~ "STravelling",
        TRUE ~ behav
      )
    )
  # Log results
  logger.trace(paste0("   ", sum(data$RULE == "[2] Altitude increasing" | data$RULE == "[2] Altitude decreasing", na.rm = T), " locations re-classified as STravelling"))
  
  
  #### 3. Sunrise-Sunset Classification -----
  logger.info("[3] Performing night-time classification")
  data %<>%
    mutate(
      RULE = case_when(
        (behav == "SResting") & (nightpoint == 1) ~ "[3] Stationary at night",
        TRUE ~ RULE
      ),
      behav = case_when(
        (behav == "SResting") & (nightpoint == 1) ~ "SRoosting",
        TRUE ~ behav
      )
    )
  logger.trace(paste0("   ", sum(data$RULE == "[3] Stationary at night", na.rm = T), " locations re-classified as SRoosting"))
  
  # use night time roosting to estimate ACC thresholds if ACC available
  if (ACCclassify == TRUE) {
    roostpoints <- data %>%
      filter(behav == "SRoosting") %>%
      as.data.frame() %>%      
      group_by(ID) %>%
      dplyr::summarise(
        thresx = quantile(var_acc_x, probs = 0.95, na.rm = T) %>% as.vector(),
        thresy = quantile(var_acc_y, probs = 0.95, na.rm = T) %>% as.vector(),
        thresz = quantile(var_acc_z, probs = 0.95, na.rm = T) %>% as.vector()
      )
  }
  
  

  #### 4. Roosting Classification -----
  logger.info("[4] Performing roosting classification")
  data %<>%
    group_by(ID, roostgroup) %>%
    mutate(
      # Reclassify any stationary runs that involve an overnight roost to SRoosting
      RULE = ifelse(!is.na(roostgroup) & any(roostsite == 1) & (behav != "STravelling"), "[4] Stationary at roost site", RULE),
      behav = ifelse(!is.na(roostgroup) & any(roostsite == 1) & (behav != "STravelling"), "SRoosting", behav)
    ) %>%
    ungroup()
  
  # Log results
  logger.trace(paste0("   ", sum(data$RULE == "[4] Stationary at roost site", na.rm = T), " locations re-classified as SRoosting"))

  logger.trace(paste0("   ", sum(data$behav == "SResting", na.rm = T), " locations classified as SResting"))
  logger.trace(paste0("   ", sum(data$behav == "STravelling", na.rm = T), " locations classified as STravelling"))
  logger.trace(paste0("   ", sum(data$behav == "SRoosting", na.rm = T), " locations classified as SRoosting"))
  

  
  #### 5. Cumulative-Time Reclassification -----
  logger.info("[5] Performing stationary-time classification")
  
  # Generate stationary data
  data %<>%
    group_by(ID) %>%
    mutate(
      stationaryNotRoost = ifelse(stationary == 1 & behav != "SRoosting", 1, 0),
      stationary_runLts = data.table::rleid(stationaryNotRoost == 1)     # id runs of stationary & non-stationary entries
    ) %>%
    group_by(ID, stationary_runLts) %>%
    mutate(
      cumtimestat = cumsum(as.numeric(timediff_hrs)), # compute cumulative time (hrs) spent stationary & non-stationary
      cumtimestat = ifelse(stationaryNotRoost == 0 | cumtimestat < 0, 0, cumtimestat)
    ) %>%
    group_by(ID) %>%
    mutate(
      # cumtimestat_pctl = 1 - (match(cumtimestat, sort(cumtimestat))/(length(which(cumtimestat!="NA")) + 1)), # From original code, which perhaps is not doing what's suppposed to do
      cumtimestat_pctl = 1 - ecdf(cumtimestat)(cumtimestat) 
    )       
  
  # find the length (in time) of every stationary run
  eventtimes <- data %>% data.frame() %>%
    group_by(ID, stationary_runLts) %>%
    summarise(runtime = suppressWarnings(max(cumtimestat, na.rm = TRUE)),
              runtime = ifelse(is.infinite(runtime), 0, runtime)) %>%
    mutate(dayRunThresh = quantile(runtime, probs = 0.95))
  
# add run time back to main data
  data %<>% 
    left_join(., eventtimes, by = c("ID", "stationary_runLts"))
  
  
  # Reasssign SResting => SFeeding
  # The longest 5 percentiles of runs of stationary behaviours will be reclassified as SFeeding
  data %<>% 
    mutate(
      RULE = ifelse(cumtimestat > dayRunThresh & behav == "SResting", "[5] Extended stationary behaviour", RULE),
      behav = ifelse(cumtimestat > dayRunThresh & behav == "SResting", "SFeeding", behav),
      #RULE = ifelse(cumtimestat_pctl < 0.05 & behav == "SResting", "[5] Extended stationary behaviour", RULE),
      #behav = ifelse(cumtimestat_pctl < 0.05 & behav == "SResting", "SFeeding", behav),
    ) %>%
    ungroup()
  
  # Log results
  logger.trace(paste0("   ", sum(data$RULE == "[5] Extended stationary behaviour", na.rm = T), " locations re-classified as SFeeding"))
  
  
  #### 6. Speed-Time Reclassification -----
  logger.info("[6] Performing speed-time classification")

  data %<>% 
    mutate(
      RULE = ifelse(kmph > kmphPI97.5 & behav == "SResting", "[6] Exceed Speed-Time threshold", RULE),
      behav = ifelse(kmph > kmphPI97.5 & behav == "SResting", "SFeeding", behav),
      #RULE = ifelse(cumtimestat_pctl < 0.05 & behav == "SResting", "[5] Extended stationary behaviour", RULE),
      #behav = ifelse(cumtimestat_pctl < 0.05 & behav == "SResting", "SFeeding", behav),
    ) %>%
    ungroup()
  
  # Log results
  logger.trace(paste0("   ", sum(data$RULE == "[6] Exceed Speed-Time threshold", na.rm = T), " locations re-classified as SFeeding"))
  
  #### 7. Accelerometer Classification -----
  
  logger.info("[7] Performing accelerometer classification")
  if (ACCclassify == TRUE) {
    data %<>% 
      left_join(roostpoints, by = "ID") %>%
      mutate(
        RULE = case_when(
          # For ACC values that exceed their threshold, reclassify to feeding
          (behav == "SResting") & (var_acc_x > thresx) ~ "[7] ACC not similar to roosting",
          (behav == "SResting") & (var_acc_y > thresy) ~ "[7] ACC not similar to roosting",
          (behav == "SResting") & (var_acc_z > thresz) ~ "[7] ACC not similar to roosting",
          #behav == "SFeeding" ~ "[7] ACC not similar to roosting",
          TRUE ~ RULE),
        behav = case_when(
        
          # For ACC values that exceed their threshold, reclassify to feeding
          (behav == "SResting") & (var_acc_x > thresx) ~ "SFeeding",
          (behav == "SResting") & (var_acc_y > thresy) ~ "SFeeding",
          (behav == "SResting") & (var_acc_z > thresz) ~ "SFeeding",
          TRUE ~ behav
        ))%>%
      
      # Move these attributes to track data:
      mt_as_track_attribute(c("thresx", "thresy", "thresz"))
  }
  
  # Log results
  logger.trace(paste0("   ", sum(data$RULE == "[7] ACC not similar to roosting", na.rm = T), " locations re-classified as SFeeding"))
  
  
  
  # Create plots, if selected ------------------------------------------------------
  
  logger.info("Classification complete. Generating output plots")
  if(create_plots == TRUE) {
    
    # create simple plot
    for (bird in unique(mt_track_id(data))) {
      
      # Create artefact
      png(appArtifactPath(
        paste0("birdtrack_", toString(bird), ".png")
      ))
      
      birddat <- data %>% 
        filter_track_data(
          .track_id = bird
        )
      
      birdplot <- ggplot(data = birddat, aes(x = sf::st_coordinates(birddat)[, 1], 
                                             y = sf::st_coordinates(birddat)[, 2])) +
        geom_path() +
        geom_point(data = birddat, 
                   aes(x = sf::st_coordinates(birddat)[, 1], 
                       y = sf::st_coordinates(birddat)[, 2],
                       colour = behav)) +
        ggtitle(paste0("Behaviour classification of ID ", bird)) +
        xlab("Easting") +
        ylab("Northing")
      print(birdplot)
      
      #Save as artefact
      dev.off()
      
    }
    
  }
  
  # Generate summary table
  behavsummary <- table(mt_track_id(data), data$behav)
  write.csv(behavsummary, file = appArtifactPath("behavsummary.csv"))
  
  # Remove nonessential behavioural columns
  if (keepAllCols == FALSE) {
    logger.trace("Removing all nonessential columns")
    data %<>% dplyr::select(-any_of(
      c(
        "sunrise_timestamp", "sunset_timestamp", "timestamp_local", "ID", "altdiff", "temptime", "endofday", "endofday_dist", "roostsite", "travel01", "cum_trav", "revcumtrav", "roostgroup", "stationaryNotRoost", "stationary_runLts", "cumtimestat", "cumtimestat_pctl", "cumtimestat_pctl_BC",
        "kmphCI2.5", "kmphPI2.5", "kmphpreds"
      ) 
    ))
    
  } else {
    # Just get rid of the unuseful columns
    logger.trace("Removing select nonessential columns")
    data %<>% dplyr::select(-any_of(
      c(
        "ID", "temptime", "endofday_dist", "roostsite", "travel01", "cum_trav", "revcum_trav", "stationaryNotRoost", "cumtimestat", "kmphCI2.5", "kmphPI2.5", "kmphpreds"
      ) 
    ))
  }
  
  # Return final result
  return(data)
  
}


# helper to compute acceleration variance till next location ---------------------
acc_var <- function(data, interpolate = FALSE) {
  
  # Store track data for later recall
  tracklevel <- mt_track_data(data)
  tm_col <- mt_time_column(data)
  trk_col <- mt_track_id_column(data)
  
  # Unnest into variances
  data <- data |> 
    dplyr::mutate(
      var = purrr::map(acc_dt, \(acc_events){
        if(!is.null(acc_events$acc_burst)){
          # bind list of matrices (one per ACC event) by row
          all_bursts <- do.call(rbind, acc_events$acc_burst)
          # variance in each active ACC axis
          apply(all_bursts, 2, var)  
        } else{
          NULL
        }
      }, 
      .progress = "Summarising ACC")
    ) %>% 
    tidyr::unnest_wider(var, names_sep = "_") |> 
    # convert back to move2 object (property lost in the last unnest step)
    mt_as_move2(time_column = tm_col, track_id_column = trk_col) |> 
    # re-append track data
    mt_set_track_data(tracklevel)
  
  if (interpolate == TRUE) {
    
    #' Interpolate missing ACCs using the nearest two values on the condition that
    #' both values are within 30mins of the location's timestamp
    
    data <- data |> 
      dplyr::group_by(.data[[trk_col]]) |> 
      dplyr::mutate(
        # add temp columns
        null_acc = purrr::map_lgl(acc_dt, is.null),
        # max of lag to next and previous locations
        acc_max_lag = pmax(
          # time till next location
          difftime(dplyr::lead(.data[[tm_col]]), .data[[tm_col]], units = "mins"),
          # time since previous location
          difftime(.data[[tm_col]], dplyr::lag(.data[[tm_col]]), units = "mins"), 
          na.rm = TRUE)
      ) |> 
      dplyr::mutate(
        # interpolate, keeping leading/trailing NAs and not interpolating more than 4 consecutive NAs
        dplyr::across(dplyr::matches("var_acc_[xyz]"), ~ zoo::na.approx(.x, na.rm = FALSE, maxgap = 4)),
        # nullify interpolated values if lag to previous or next location > 30mins
        dplyr::across(dplyr::matches("var_acc_[xyz]"), ~ ifelse(null_acc & acc_max_lag > 30, NA, .x))
      ) |> 
      # identify locations with interpolated ACC
      dplyr::mutate(interpACC = null_acc & !is.na(var_acc_x)) |> 
      # remove temp columns
      dplyr::select(-null_acc, -acc_max_lag) |> 
      dplyr::ungroup()
  }
  
  return(data)
}

  
  




