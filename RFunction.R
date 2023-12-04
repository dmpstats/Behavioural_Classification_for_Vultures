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

`%!in%` <- Negate(`%in%`)

# Accelerometer Interpolation Function --------------------------------------------

prep_ACC <- function(data, interpolate = FALSE) {

  #' This will interpolate ACC using the nearest two values on the condition that
  #' both values are within 30mins of the location's timestamp
  
  # Unnest into variances ------------------------------
  
  # Store track data for later recall
  tracklevel <- mt_track_data(data)
  
  # Split between no ACC and ACC:
  missrows <- filter(data, is.na(acc_dt)) 
  accrows <- filter(data, !is.na(acc_dt))  

  # Operate on ACC rows
  loc_acc_var <- accrows %>% 
    mutate(
      var = purrr::map(acc_dt, \(acc_events){
        if(!is.null(acc_events$acc_burst)){
          # bind list of matrices (one per ACC event) by row
          all_bursts <- do.call(rbind, acc_events$acc_burst)
          # variance in each active ACC axis
          apply(all_bursts, 2, var)  
        } else{
          NULL
        }
      })
    ) %>% 
    unnest_wider(var, names_sep = "_")
  
  # Add empties to non-ACC rows 
  missrows %<>% mutate(
    var_acc_x = NULL,
    var_acc_y = NULL,
    var_acc_z = NULL
  )
  
  alldat <- mt_stack(mt_as_move2(loc_acc_var, 
                                 track_id_column = mt_track_id_column(missrows),
                                 time_column = mt_time_column(missrows)
  ), missrows,
  .track_combine = "merge") %>%
    arrange(mt_track_id(.), mt_time(.))
  
  # Restore track data (lost in above steps)
  alldat <- mt_set_track_data(alldat, tracklevel)
  
  # Missing ACC Interpolation -----------------------------------------
  # Complete this only if selected
  
  if (interpolate == TRUE) {
    
    acc_tmp <- alldat %>%
      mutate(track = mt_track_id(.),
             timestamp = mt_time(.),
             interpACC = ifelse(is.na(acc_dt), TRUE, FALSE)) %>%
      group_by(track) %>%
      mutate(acc_ts = ifelse(is.na(acc_dt), NA, timestamp),
             acc_ts = lubridate::as_datetime(acc_ts)
      ) %>%
      as.data.frame() %>%
      dplyr::select(c("track", "timestamp", "acc_dt", "var_acc_x", "var_acc_y", "var_acc_z", "acc_ts", "interpACC")) %>%
      group_by(track) %>%
      mutate(acc_ts_next = acc_ts,
             acc_ts_prev = acc_ts) %>%
      fill(acc_ts_next, .direction = "up") %>%
      fill(acc_ts_prev, .direction = "down") %>%
      mutate(timetonext = ifelse(is.na(acc_dt), difftime(acc_ts_next, timestamp, units = "mins"), NA),
             timetoprev = ifelse(is.na(acc_dt), difftime(timestamp, acc_ts_prev, units = "mins"), NA)
      ) %>%
      mutate(across(c(var_acc_x, var_acc_y, var_acc_z), ~ zoo::na.approx(.x, na.rm = FALSE, maxgap=4)))
    
    # Nullify cases which require interpolation of >30 minutes in either direction:
    acc_tmp %<>% mutate(across(c(var_acc_x, var_acc_y, var_acc_z), ~ case_when(
      (timetonext > 30) | (timetoprev > 30) ~ NA,
      TRUE ~ .))) %>%
      arrange(track, timestamp)
    
    # Return to mandata
    alldat %<>%
      arrange(mt_track_id(.), mt_time(.)) %>%
      mutate(
        var_acc_x = acc_tmp$var_acc_x,
        var_acc_y = acc_tmp$var_acc_y,
        var_acc_z = acc_tmp$var_acc_z,
        interpACC = acc_tmp$interpACC
      ) %>%
      dplyr::select(-acc_dt)
  } else {
    alldat %<>% dplyr::select(-acc_dt)
  }
  
  return(alldat)
}


# Main RFunction -------------------------------------------------------------------------------------

rFunction = function(data, travelcut,
                     create_plots = TRUE,
                     sunrise_leeway = 0,
                     sunset_leeway = 0,
                     altbound = 25,
                     keepAllCols = FALSE
                     # second_stage_model = NULL, fit_speed_time  # Will be reincorporated later
                     ) {
  
  
  # Prepare Second-Stage Models --------------------------------------------------------------------------------------------

      #' This section will be re-incorporated at a later date when the MoveApp required to
      #' generate the speed-time models is published. For now, this stage of classification
      #' will be skipped
    
  # Call model file, if provided:
  # modelPath <- paste0(getAppFilePath("second_stage_model", fallbackToProvidedFiles = TRUE), "modelfit.rds")
  # providedModels <- readRDS(modelPath)
  # 
  # if(is.character(providedModels)) {
  #   nomodels <- TRUE
  #   logger.warn(providedModels)
  # } else {
  #   nomodels <- FALSE
  #   logger.info(paste0("Models provided for tags ", toString(names(providedModels)), ". Proceeding with classification"))
  # }
  # 
  # # Call standard classification model file, for all IDs not included in the above file:
  # standardModelPath <- paste0(getAppFilePath("standard_model", fallbackToProvidedFiles = TRUE), "standard_model.rds")
  # standardModel <- readRDS(standardModelPath)
  # 

  
  # Check Input Data ----------------------------------------------------------------------------------------------
  
  logger.trace(paste0(
    "Input data provided:  \n", 
    "travelcut: ", toString(travelcut), "\n",
    "sunrise_leeway: ", toString(sunrise_leeway), "\n", 
    "sunset_leeway: ", toString(sunset_leeway), "\n",
    "altbound: ", toString(altbound), "\n",
    "input data dimensions: ", toString(dim(data))
  ))

  if(nrow(data) == 0) {
    logger.warn("Input data is empty. Returning input.")
    return(data)}

  
  
  logger.trace("Input is in correct format. Proceeding with classification for all IDs")
  
  
  # CLASSIFICATION STEPS --------------------------------------------------------------------------------------
  
  data %<>% dplyr::mutate(
    ID = mt_track_id(.),
    timestamp = mt_time(.),
    #hourmin = hour(mt_time(.)) + minute(mt_time(.))/60 + second(mt_time(.))/3600,
    yearmonthday = ifelse(
      
      # IMPORTANT: Cut this 'ifelse' statement when "timestamp_local" is added to preprocessing's essential columns 
      # and use only the top line (i.e. a dependency on "timestamp_local")
      
      # Use local time if available
      # Revert to UTC timestamp if not
      "timestamp_local" %in% colnames(data),
      stringr::str_replace_all(stringr::str_sub(timestamp_local, 1, 10), "-", ""),
      stringr::str_replace_all(stringr::str_sub(timestamp, 1, 10), "-", "")
    ) 
  )
  
  # Generate necessary data
  data$dist_m <- as.vector(move2::mt_distance(data))
  data$kmph <- move2::mt_speed(data) %>% units::set_units("km/h") %>% as.vector()
  #data$timediff_hrs <- lubridate::mt_time_lags(data)
  data %<>% 
    arrange(mt_track_id(data), mt_time(data)) %>%
    # distinct(timestamp, .keep_all = TRUE) %>%
    mutate(
      #dist_m = sqrt((x - lag(x, default = NA))^2 + (y - lag(y, default = NA))^2),
      timediff_hrs =   mt_time_lags(.) %>%
        units::set_units("hours") %>%
        as.vector(),
      #kmph = move2::mt_speed(data),
      hour = lubridate::hour(mt_time(data))
    ) 
  
  
  ## 1. Speed Classification Prep -----------------------------------------------------
  
  logger.info("[1] Preparing speed-classification data")
  
     #' Speed Classification
     #' Anything above travelcut is STravelling
     #' Anything below is initially SResting
     
  if(travelcut <=  0 | !is.numeric(travelcut)) {
    logger.fatal("Speed cut-off for travelling behavour is not a valid speed. Returning input - please use valid settings")
    stop("Speed cut-off for travelling behavour is not a valid speed. Returning input - please use valid settings")
  }

  data %<>%
    mutate(
      stationary = case_when(
        kmph < travelcut ~ 1,
        kmph > travelcut ~ 0,
        is.na(kmph) | is.nan(kmph) ~ 1, # assume stationary if no data 
        TRUE ~ NA
      )
    )
  
  
  
  ## 2. Altitude Reclassification Prep -------------------------------------------------

      #' If a bird is ascending, it is STravelling
      #' If a bird is flatlining, it remains SResting
      #' If a bird is descending, 
      #'      Next location is ascending/descending ==> STravelling
      #'      Next location is flatlining ==> remains SResting
  
  logger.info("[2] Preparing altitude-classification data")

  if ("altitude" %in% colnames(data)) {
    logger.trace("   Altitude column identified. Beginning altitude classification")
    
    if(!is.numeric(altbound)) {
      logger.warn("    altbound is non-numeric. Stopping computation - please check inputs")
      stop("    altbound (altitude threshold) is non-numeric. Please check input settings")
    }

    # Classify altitude changes
    data %<>% 
      # Reset altitude change each day:
      group_by(ID, yearmonthday) %>%
      dplyr::mutate(
      
      altitude = as.numeric(unlist(altitude)), # fix when input is character vector
      
      altdiff = ifelse(!is.na(altitude) & !is.na(dplyr::lead(altitude)), dplyr::lead(altitude) - altitude, NA),
      
      altchange = case_when(
                       altdiff < -altbound ~ "descent",
                       altdiff > altbound ~ "ascent",
                       is.na(altdiff) ~ "flatline",
                       TRUE ~ "flatline"
                     )) %>% 
      ungroup()

    

    
  } else {
    
    logger.warn("    No column named 'altitude' present. Skipping altitude classification")
    
  }
  
  
  ## 3. Day-Night Reclassification Prep --------------------------------------------------
  
  logger.info("[3] Preparing day-night classification")
  
  # Check all columns present & inputs valid
  
  if ("timestamp_local" %!in% colnames(data)) {
    logger.warn("   timestamp_local is not a column within the data. Overnight roosting classification may be flawed. Please use 'Add Local and Solar Time' MoveApp to generate this column.")
  }
  if(is.null(sunrise_leeway)) {
    logger.warn("    No sunrise leeway provided as input. Defaulting to no leeway")
    sunrise_leeway <- 0
  }
  if(is.null(sunset_leeway)) {
    logger.warn("    No sunset leeway provided as input. Defaulting to no leeway")
    sunset_leeway <- 0
  }
  
  if ("sunrise_timestamp" %!in% colnames(data) | "sunset_timestamp" %!in% colnames(data)) {
    logger.warn("    'sunrise_timestamp' or 'sunset timestamp' is not a column in the input data. Sunrise-sunset classification cannot be performed. Terminating - please use the 'Add Local and Solar Time' MoveApp in this workflow BEFORE this MoveApp")
    stop()
  } else {
    logger.trace("    Sunrise and sunset columns identified. Able to perform sunrise-sunset classification")
  }
  
  ### Add necessary columns
  # Add 'nightpoint' column determining what is day/night:
  data %<>% mutate(
    nightpoint = ifelse(!between(mt_time(.), sunrise_timestamp + lubridate::minutes(sunrise_leeway), sunset_timestamp + lubridate::minutes(sunset_leeway)), 1, 0)
  )
  



  
  
  ## 4. Accelerometer Relassification Prep -----------------------------------------------------------------------------------------------
  
  
  if ("acc_dt" %in% colnames(data)) {
    
    logger.info("[4] Accelerometer data identified: preparing and unnesting")
    
    # Unnest ACC data
    data <- prep_ACC(data, interpolate = FALSE)
    
    
    # Set up threshold table
    thresh <- data.frame(
      track = unique(mt_track_id(data)),
      thresx = NA, thresy = NA, thresz = NA
    )
    

    
    # Clean track data (remove NA columns)
    #not_all_na <- function(x) any(!is.na(x))
    #data <- mt_set_track_data(data, mt_track_data(data) %>% select(where(not_all_na))) 
    
    ACCclassify <- TRUE
    
  } else {
    logger.info("[4] No accelerometer data detected: skipping ACC preparation")
  }
  
  
  
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
      !is.na(endofday_dist) & endofday_dist < 25,
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
  
  # data %<>%
  #   group_by(ID) %>%
  #   mutate(
  #     stationaryNotRoost = ifelse(stationary == 1 & behav != "SRoosting", 1, 0),
  #     stationary_runLts = data.table::rleid(stationaryNotRoost == 1)     # id runs of stationary & non-stationary entries
  #   ) %>%
  #   group_by(ID, stationary_runLts) %>%
  #   mutate(
  #     cumtimestat = cumsum(as.numeric(timediff_hrs)), # compute cumulative time (hrs) spent stationary & non-stationary
  #     cumtimestat = ifelse(stationaryNotRoost == 0 | cumtimestat < 0, 0, cumtimestat)
  #   ) %>%
  #   group_by(ID) %>%
  #   mutate(
  #     cumtimestat_pctl = 1 - (match(cumtimestat, sort(cumtimestat))/(length(which(cumtimestat!="NA")) + 1)), # From original code, which perhaps is not doing what's suppposed to do
  #     cumtimestat_pctl_BC = 1 - ecdf(cumtimestat)(cumtimestat)                                               # Correct calculation?
  #   )
  
  
  ## 7. Speed-Time Reclassification --------------------------------------------
  
  #logger.trace("[7] Preparing speed-time model data")
  # This is where the second-stage models will go, once reworked
  #'
  #'
  #'
  #'
  #'
  #'
  
  
  
  # PERFORM CLASSIFICATION STEPS 1-7 -------------------------------------------------------
  
  logger.info("All data prepared. Performing all classification steps")
  
  
  #### 1. Speed Classification ----
  logger.info("[1] Performing speed classification")
  data %<>% mutate(
    # Add column to explain classification:
    RULE = case_when(
      kmph < travelcut ~ "[1] Low speed",
      kmph > travelcut ~ "[1] High speed"
    ), 
    behav = case_when(
      kmph < travelcut  ~ "SResting",
      kmph > travelcut  ~ "STravelling",
      TRUE ~ "Unknown"
    )
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
  
  
  
  
  #### 4. Accelerometer Classification -----
  logger.info("[4] Performing accelerometer classification")
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
    
    data %<>% 
      left_join(roostpoints, by = "ID") %>%
    mutate(behav = case_when(

      # If any ACC values exceed their threshold, reclassify to feeding
      (behav == "SResting") & (var_acc_x > thresx) ~ "SFeeding",
      (behav == "SResting") & (var_acc_y > thresy) ~ "SFeeding",
      (behav == "SResting") & (var_acc_z > thresz) ~ "SFeeding",
      TRUE ~ behav
    ),
      RULE = case_when(
        # because this is the only feeding classification so far:
        behav == "SFeeding" ~ "[4] Abnormally high ACC",
        TRUE ~ RULE)) %>%
      
      # Move these attributes to track data:
      mt_as_track_attribute(c("thresx", "thresy", "thresz"))
  }
  
  # Log results
  logger.trace(paste0("   ", sum(data$RULE == "[4] Abnormally high ACC", na.rm = T), " locations re-classified as SFeeding"))
  

    
  #### 5. Roosting Classification -----
  logger.info("[5] Performing roosting classification")
  data %<>%
    group_by(ID, roostgroup) %>%
    #dplyr::select(-c("travel01", "cum_trav", "revcum_trav", "endofday_dist")) %>%
    mutate(
      # Reclassify any stationary runs that involve an overnight roost to SRoosting
      RULE = ifelse(!is.na(roostgroup) & any(roostsite == 1) & (behav != "STravelling"), "[5] Stationary at roost site", RULE),
      behav = ifelse(!is.na(roostgroup) & any(roostsite == 1) & (behav != "STravelling"), "SRoosting", behav)
    ) %>%
    #dplyr::select(-c("endofday", "roostsite", "mt_track_id(.)", "date(mt_time(.))")) %>%
    ungroup()
  
  # Log results
  logger.trace(paste0("   ", sum(data$RULE == "[5] Stationary at roost site", na.rm = T), " locations re-classified as SRoosting"))
                                       # Correct calculation?

  
  #### 6. Cumulative-Time Reclassification -----
  logger.info("[6] Performing stationary-time classification")

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
      cumtimestat_pctl = 1 - (match(cumtimestat, sort(cumtimestat))/(length(which(cumtimestat!="NA")) + 1)), # From original code, which perhaps is not doing what's suppposed to do
      cumtimestat_pctl_BC = 1 - ecdf(cumtimestat)(cumtimestat) 
      )       

    
  # Reasssign SResting => SFeeding
  # The longest 5 percentiles of runs of stationary behaviours will be reclassified as SFeeding
  data %<>% 
    mutate(
      RULE = ifelse(cumtimestat_pctl < 0.05 & behav == "SResting", "[6] Extended stationary behaviour", RULE),
      behav = ifelse(cumtimestat_pctl < 0.05 & behav == "SResting", "SFeeding", behav),
    ) %>%
    ungroup()
  
  # Log results
  logger.trace(paste0("   ", sum(data$RULE == "[6] Extended stationary behaviour", na.rm = T), " locations re-classified as SFeeding"))
  
  
  #### 7. Speed-Time Reclassification -----
  #logger.info("[7] Performing speed-time classification")
  #' This is where the reclassification step will go, once
  #' models are reworked
  
  
  
  
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
        "sunrise_timestamp", "sunset_timestamp", "timestamp_local", "ID", "altdiff", "temptime", "endofday", "endofday_dist", "roostsite", "travel01", "cum_trav", "revcumtrav", "roostgroup", "stationaryNotRoost", "stationary_runLts", "cumtimestat", "cumtimestat_pctl", "cumtimestat_pctl_BC"
      ) 
    ))
    
  } else {
    # Just get rid of the unuseful columns
    logger.trace("Removing select nonessential columns")
    data %<>% dplyr::select(-any_of(
      c(
        "ID", "temptime", "endofday_dist", "roostsite", "travel01", "cum_trav", "revcum_trav", "stationaryNotRoost", "cumtimestat"
      ) 
    ))
  }
  
  # Return final result
  return(data)
  
}
