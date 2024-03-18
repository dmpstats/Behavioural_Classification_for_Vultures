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
library("furrr")
library("future")
library("progressr")

`%!in%` <- Negate(`%in%`)
not_null <- Negate(is.null)


# Main RFunction ====================================================================

rFunction_roost_reloc = function(data, 
                     travelcut,
                     create_plots = TRUE,
                     sunrise_leeway = 0,
                     sunset_leeway = 0,
                     altbound = 25,
                     keepAllCols = FALSE
                     # second_stage_model = NULL, fit_speed_time  # Will be reincorporated later
) {
  
  
  #' TODO
  #' 
  #' > High Priority:
  #'  - Check if projected Vs non-projected input has an effect on results  
  #' 
  #' > Medium Priority:
  #'   - Consolidate summary plots
  #' 
  #' > Low priority:
  #'   - drop "ID" and "timestamp" redefinition and use "mt_" functions instead
  #'   - improve error messages with {rlang}
  #'   - add timestamps to logger
  
  
  ## Validate Input Data --------------------------------------------
  
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
  
  
  ### travelcut ----
  if(travelcut <= 0 | !is.numeric(travelcut)) {
    logger.fatal("Speed cut-off for travelling behavour is not a valid speed. Returning input - please use valid settings")
    stop("Speed cut-off for travelling behavour (`travelcut`) is not a valid speed. Returning input - please use valid settings")
  }
  
  
  ### altbound ----
  ### (only if column named "altitude" is in input dataset)
  
  if("altitude" %in% colnames(data)){
    
    if(!is.numeric(altbound)) {
      logger.warn("`altbound` is non-numeric. Stopping computation - please check inputs.")
      stop("Altitude change threshold (`altbound`) is non-numeric. Please check input settings.")
      
    } else if(altbound == 0){
      
      logger.warn(
        paste0(" |- Altitude threshold (`altbound`) is set to 0m, and thus ",
               "ANY change is altitude will be considered as ascencing/descending ",
               "movement.")
      )
    }  
  } 
  
  ### 'leeway' inputs ----
  if(is.null(sunrise_leeway)) {
    logger.warn(" |- No sunrise leeway provided as input. Defaulting to no leeway.")
    sunrise_leeway <- 0
  }
  if(is.null(sunset_leeway)) {
    logger.warn(" |- No sunset leeway provided as input. Defaulting to no leeway.")
    sunset_leeway <- 0
  }
  
  
  ### Input data: time-related columns -----
  if("altitude" %!in% colnames(data)){
    logger.warn(" |- Column `altitude` is absent from input data.")
  } else{
    logger.info(" |- `altitude` column identified. Able to detect altitude changes.")
  }
  
  
  if ("timestamp_local" %!in% colnames(data)) {
    stop(
      paste0(
        "Column `timestamp_local` is not comprised in input data. Local time is ",
        "a fundamental requirement for the classification process. Please deploy ",
        "the App 'Add Local and Solar Time' earlier in the Workflow to bind local ",
        "time to the input dataset."),
      call. = FALSE
    )
  }else{
    logger.info(" |- Local Time column identified")
  }
  
  
  if ("sunrise_timestamp" %!in% colnames(data) | "sunset_timestamp" %!in% colnames(data)) {
    logger.fatal("`sunrise_timestamp` or `sunset timestamp` is not a column in the input data. Sunrise-sunset classification cannot be performed. Terminating - please use the 'Add Local and Solar Time' MoveApp in this workflow BEFORE this MoveApp")
    stop(
      paste0(
        "`sunrise_timestamp` and/or `sunset timestamp` are not a columns in the ",
        "input data. Identification of night-time points is fundamental for the ",
        "classification process. Please deploy the App 'Add Local and Solar Time' ",
        "earlier in the workflow to add the required columns."), 
      call. = FALSE
    )
  } else {
    logger.info(" |- Sunrise and sunset columns identified. Able to perform night-time identification.")
  }
  
  
  
  logger.info(" |- Input is in correct format. Proceeding with data preparation.")
  
  
  ## Data Preparation ===========================================================
  
  logger.info("Initiate Data Preparation Steps")
  
  
  ### Generate overarching variables  ------------------------------
  
  logger.info(" |- Generate overarching variables")
  
  data %<>% dplyr::mutate(
    ID = mt_track_id(.),
    timestamp = mt_time(.)
  )
  
  # Add date label and day-hours (i.e. decimal hours since start of day)
  data %<>% 
    mutate(
      yearmonthday = stringr::str_replace_all(stringr::str_sub(timestamp_local, 1, 10), "-", ""),
      hourmin = lubridate::hour(timestamp_local) + 
        lubridate::minute(timestamp_local)/60 + 
        lubridate::second(timestamp_local)/3600
    ) 
  
  
  #' NOTE:`timediff_hrs`, `dist_m` & `kmph` are variables expected to provide
  #' information between consecutive locations. If the Standardizing App (or
  #' other) has been used earlier in the WF, these cols could already be present
  #' in the input. However, there is no guarantee that data coming as input has
  #' been thinned by other in-between App. Therefore, to ensure accuracy, we
  #' always (re)generate these columns here.
  data %<>% 
    arrange(mt_track_id(data), mt_time(data)) %>%
    # distinct(timestamp, .keep_all = TRUE) %>%
    mutate(
      timediff_hrs = as.vector(mt_time_lags(., units = "hours")),
      kmph = as.vector(mt_speed(., units = "km/h")),
      dist_m = as.vector(mt_distance(., units = "m"))
    ) 
  
  
  ### Identify stationary points -----------------------------------
  
  logger.info(" |- Identify stationary points")
  
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
  
  
  
  ### Detect Altitude Changes -------------------------------------
  
  #' Categorize vertical movement based on altitude change
  #'  (i) change in altitude to next location > threshold: altchange == "ascent"
  #'  (ii) change in altitude to next location < -threshold: altchange == "descent"
  #'  (iii) else (including NAs): altchange == "flatline"
  
  if ("altitude" %in% colnames(data)) {
    
    if(!all(is.na(data$altitude))){
      
      logger.info(" |- Categorize altitude change between consecutive locations")
      
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
      
      alt_classify <- TRUE
      
    } else{
      alt_classify <- FALSE  
    }
  } else {
    alt_classify <- FALSE  
  }
  
  if(!alt_classify){
    logger.warn(
      paste0(" |- Column `altitude` is either not present in input data or is entirely ",
             "filled with NAs - skipping altitude change calculations."))
  }
  
  
  
  ### Night-time identification ----------------------------
  
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
  
  
  
  ### Calculate ACC variance ----------------------------
  
  #' If ACC data is available, calculate variance in acceleration bursts
  #' till subsequent location event - expect one variance statistic for each enabled ACC axes
  
  if ("acc_dt" %in% colnames(data)) {
    
    # logical flag for all-NULL 'acc_dt' column 
    acc_null <- all(purrr::map_lgl(data$acc_dt, is.null))
    
    if(!acc_null){
      logger.info(" |- ACC data identified: calculating variance in acceleration between locations.")
      
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
  
  if(!ACCclassify) logger.info(" |- No accelerometer data detected in any of the tracks: skipping ACC preparation.")
  
  
  
  ### Stationary Speed Vs day-hours model  --------------------------------------------
  
  logger.info(" |- Deriving thresholds for stationary-speed given hour-of-day.")
  
  #progressr::handlers(global = TRUE)
  progressr::handlers("cli")
  
  #' setting parallel processing using availableCores() to set # workers.
  #' {future} imports that function from {parallelly}, which  is safe to use in
  #' container environments (e.g. Docker)
  future::plan("multisession", workers = future::availableCores(omit = 2))
  
  progressr::with_progress({
    
    # initiate progress signaler
    p <- progressr::progressor(steps = mt_n_tracks(data))
    
    data <- data |> 
      group_by(ID) |>
      dplyr::group_split() |> 
      furrr::future_map(
        .f = speed_time_model, travelcut = travelcut, p = p,
        .options = furrr_options(
          seed = TRUE,
          packages = c("move2", "sf", "MRSea", "dplyr", "lubridate")
        )
      ) |>
      mt_stack()
    
  })
  
  
  future::plan("sequential")
  
  
  # Behaviour Classification Steps [1 -7] ========================================================
  
  logger.info("All data prepared. Performing all classification steps")
  
  
  ### [1] Speed Classification -------------------
  
  logger.info("[1] Performing speed classification")
  data %<>% mutate(
    # Add column to explain classification:
    RULE = ifelse(stationary == 1, "[1] Low speed","[1] High speed"), 
    behav = ifelse(stationary == 1, "SResting", "STravelling")
  )
  
  # Log results
  logger.info(paste0("   |> ", sum(data$behav == "SResting", na.rm = T), " locations classified as SResting"))
  logger.info(paste0("   |> ", sum(data$behav == "STravelling", na.rm = T), " locations classified as STravelling"))
  
  
  ### [2] Altitude Classification --------------------
  
  #' Remaining resting locations reclassified as travelling according to the following rules:
  #' (i) If a bird is ascending ==> STravelling
  #' (ii) If a bird is descending AND:
  #'      Next location is ascending/descending ==> STravelling
  #'      Next location is flatlining ==> remains SResting
  #' (iii) If a bird is flatlining, it remains SResting
  
  if(alt_classify){
    
    logger.info("[2] Performing altitude classification")
    
    data %<>%
      # QUESTION (BC): shouldn't this step be grouped by bird given we're using `lead()`?
      # group_by(ID) %>%
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
    
    #### <!> Update stationary status -------------
    data <- data |> mutate(stationary = ifelse(behav == "STravelling", 0, stationary))
    
    # Log results
    logger.info(paste0("   |> ", sum(data$RULE == "[2] Altitude increasing" | data$RULE == "[2] Altitude decreasing", na.rm = T), " locations re-classified as STravelling"))  
    
  } else {
    logger.warn("[2] Skipping altitude classification due to absence of altitude data")
  }
  
  
  
  
  ### [3] Night-time Classification ---------------
  
  #' Remaining resting locations re-classified as (night-time) roosting if they've 
  #' been identified as a night point (i.e. occurred between sunset and sunrise)
  #' 
  #' NOTE: STravelling locations are kept unchanged, i.e. night-time travelling 
  #' treated as a valid behaviour
  
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
  
  logger.trace(paste0("   |> ", sum(data$RULE == "[3] Stationary at night", na.rm = T), " locations re-classified as SRoosting"))
  
  
  
  #### <!> Estimate ACC thresholds at night-time roosting locations -----
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
  
  
  
  
  ### [4] Roosting-site Classification -------------
  
  #' Remaining (daytime) resting locations re-classified as roosting if
  #' identified as part of a roosting-site, which is defined as:
  #' 
  #' Consecutive stationary locations (`roostgroup`) encompassing night-time
  #' locations with total overnight distance traveled less than 15 meters
  #' (`roostsite`)
  #' 
  #' NOTE: STravelling locations not affected by this step, even if they were
  #' tagged as part of a roost-site
  
  logger.info("[4] Performing roosting-site classification")
  
  #### [4.1] Identify overnight roosting sites ------
  data <- add_roost_cols(data, sunrise_leeway, sunset_leeway)
  
  #### [4.2] Apply roosting-site rule ---------
  data %<>%
    group_by(ID, roostgroup) %>%
    mutate(
      # Reclassify any stationary runs that involve an overnight roost to SRoosting
      RULE = ifelse(!is.na(roostgroup) & any(roostsite == 1) & (behav != "STravelling"), "[4] Stationary at roost site", RULE),
      behav = ifelse(!is.na(roostgroup) & any(roostsite == 1) & (behav != "STravelling"), "SRoosting", behav)
    ) %>%
    ungroup()
  
  # Log results
  logger.info(paste0("   |> ", sum(data$RULE == "[4] Stationary at roost site", na.rm = T), " locations re-classified as SRoosting"))
  
  
  
  
  ### [5] Non-roosting Stationary Cumulative-time Classification ------------
  
  #' Remaining Resting locations re-classified as Feeding if they are part of a
  #' sequence of non-roosting time-points that remain stationary for an
  #' unusually long period of time
  
  logger.info("[5] Performing non-roosting stationary cumulative-time classification")
  
  #### [5.1] Derive non-roosting stationary runs  -----
  data <- add_nonroost_stationary_cols(data)
  
  #### [5.2] Apply non-roosting stationary Rule  --------- 
  
  #' Re-classify Resting locations assigned with cumulative stationary times
  #' that exceed the 95th percentile of stationary run durations. Percentile
  #' thresholds are individual-based and calculated from the input data
  data %<>% 
    mutate(
      RULE = ifelse(!is.na(cumtimestat) & cumtimestat > dayRunThresh & behav == "SResting", "[5] Extended stationary behaviour", RULE),
      behav = ifelse(!is.na(cumtimestat) & cumtimestat > dayRunThresh & behav == "SResting", "SFeeding", behav),
      #RULE = ifelse(cumtimestat_pctl < 0.05 & behav == "SResting", "[5] Extended stationary behaviour", RULE),
      #behav = ifelse(cumtimestat_pctl < 0.05 & behav == "SResting", "SFeeding", behav),
    ) %>%
    ungroup()
  
  # Log results
  logger.trace(paste0("  |> ", sum(data$RULE == "[5] Extended stationary behaviour", na.rm = T), " locations re-classified as SFeeding"))
  
  
  
  
  ### [6] Speed-Time Classification --------------
  
  #' Remaining Resting locations re-classified as Feeding if the speed to next
  #' location is greater the 97.5th percentile of the predicted stationary
  #' speeds at that time of the day (day-hours)
  
  logger.info("[6] Performing speed-time classification")
  
  data %<>% 
    ungroup() %>%
    mutate(
      RULE = ifelse(!is.na(kmphCI97.5) & !is.na(kmph) & kmph > kmphCI97.5 & behav == "SResting", "[6] Exceed Speed-Time threshold", RULE),
      behav = ifelse(!is.na(kmphCI97.5) & !is.na(kmph) & kmph > kmphCI97.5 & behav == "SResting", "SFeeding", behav)
    )
  
  # Log results
  logger.trace(paste0("  |> ", sum(data$RULE == "[6] Exceed Speed-Time threshold", na.rm = T), " locations re-classified as SFeeding"))
  
  
  
  ### [7] Accelerometer Classification -----
  
  #' Remaining Resting locations re-classified as Feeding if the variance in
  #' acceleration to the next location exceeds the 95th percentile of
  #' acceleration variation values during night-time roosting, in any of the
  #' active accelerometer axis. Percentile thresholds are calculated for each
  #' individual from input data.
  
  if (ACCclassify == TRUE) {
    
    logger.info("[7] Performing accelerometer classification")
    
    data %<>% 
      left_join(roostpoints, by = "ID") %>%
      mutate(
        RULE = case_when(
          # For ACC values that exceed their threshold, reclassify to feeding
          (behav == "SResting") & (var_acc_x > thresx) ~ "[7] ACC not similar to roosting",
          (behav == "SResting") & (var_acc_y > thresy) ~ "[7] ACC not similar to roosting",
          (behav == "SResting") & (var_acc_z > thresz) ~ "[7] ACC not similar to roosting", 
          TRUE ~ RULE
        ),
        behav = case_when(
          # For ACC values that exceed their threshold, reclassify to feeding
          (behav == "SResting") & (var_acc_x > thresx) ~ "SFeeding",
          (behav == "SResting") & (var_acc_y > thresy) ~ "SFeeding",
          (behav == "SResting") & (var_acc_z > thresz) ~ "SFeeding",
          TRUE ~ behav
        )
      ) %>%
      # Move these attributes to track data:
      mt_as_track_attribute(c("thresx", "thresy", "thresz"))
    
    # Log results
    logger.trace(paste0("   ", sum(data$RULE == "[7] ACC not similar to roosting", na.rm = T), " locations re-classified as SFeeding"))
    
  }else{
    logger.warn("[7] Skipping accelerometer classification due to absence of ACC data in all tracks.")
  }
  
  
  
  # Summarise classified behaviour 
  logger.info(" |- Behaviour Classification Summary")
  logger.info(paste0("   |> ", sum(data$behav == "SResting", na.rm = T), " locations classified as SResting"))
  logger.info(paste0("   |> ", sum(data$behav == "STravelling", na.rm = T), " locations classified as STravelling"))
  logger.info(paste0("   |> ", sum(data$behav == "SRoosting", na.rm = T), " locations classified as SRoosting"))
  logger.info(paste0("   |> ", sum(data$behav == "SFeeding", na.rm = T), " locations classified as SFeeding"))
  
  
  
  ## Create plots, if selected ------------------------------------------------------
  
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
  
  
  ## Remove nonessential behavioural columns -----------------------------------
  if (keepAllCols == FALSE) {
    logger.trace("Removing all nonessential columns")
    data %<>% dplyr::select(-any_of(
      c(
        "sunrise_timestamp", "sunset_timestamp", "timestamp_local", "ID", "altdiff", "endofday", "endofday_dist_m", "roostsite", "travel01", "cum_trav", "revcumtrav", "roostgroup", "stationaryNotRoost", "stationary_runLts", "cumtimestat", "cumtimestat_pctl", "cumtimestat_pctl_BC",
        "kmphCI2.5", "kmphPI2.5", "kmphpreds"
      ) 
    ))
    
  } else {
    # Just get rid of the unuseful columns
    logger.trace("Removing select nonessential columns")
    data %<>% dplyr::select(-any_of(
      c(
        "ID", "endofday_dist_m", "roostsite", "travel01", "cum_trav", "revcumtrav", "stationaryNotRoost", "cumtimestat", "kmphCI2.5", "kmphPI2.5", "kmphpreds"
      ) 
    ))
  }
  
  # Return final result
  return(data)
  
}



# Helper Functions ====================================================================

#' //////////////////////////////////////////////////////////////////////////////
#' Compute acceleration variance till next location
#' 
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



#' /////////////////////////////////////////////////////////////////////////////////////////////  
#' Derive and add roosting columns to data 
#' 
#' New columns relevant for classification:
#'  - `roostsite`: identifies overnight roosting sites (based on overnight 
#'  traveled distance < 15m)
#'  - `roostgroup`: identifies groups of locations with roost-like behaviour 
#'  (consecutive non-travelling locations)
#'  
add_roost_cols <- function(data, sunrise_leeway, sunset_leeway){
  
  data %<>% 
    group_by(ID, yearmonthday) %>%
    mutate(
      temptime = lubridate::with_tz(timestamp, lubridate::tz(sunrise_timestamp)), # ensuring all timestamps are in same tz
      # Mark the final and first daytime points in each day 
      endofday = case_when(
        nightpoint == 1 & lag(nightpoint) == 0 ~ "FINAL",
        nightpoint == 1 & lead(nightpoint) == 0 ~ "FIRST", 
        TRUE ~ NA
      ) ) %>% 
    # Calculate time difference from each timestamp to sunrise/sunset (and their leeways):
    mutate(
      sunrise_difference = difftime(temptime, sunrise_timestamp + minutes(sunrise_leeway), units = "mins") %>% abs(),
      sunset_difference = difftime(temptime, sunset_timestamp + minutes(sunset_leeway), units = "mins") %>% abs()
    ) %>%
    mutate(closest = case_when(
      # Mark the closest timestamps to sunrise/sunset, which will proxy for the absence of night points:
      sunrise_difference == min(sunrise_difference, na.rm = T) ~ "SUNRISE",
      sunset_difference == min(sunset_difference, na.rm = T) ~ "SUNSET", 
      TRUE ~ NA
    ))
  
  logger.trace("  |> Identifying locations for overnight roosting checks")
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
  ) %>% dplyr::select(-c("temptime", "morning", "evening", "closest", "sunrise_difference", "sunset_difference"))
  
  # Shortcut for calculating night-distances:
  # Filter dataset to only the marked final/first point
  # Bind distance using mt_distance and keep only overnight distances
  # then merge back into main dataset
  logger.trace("  |> Generating overnight roosting distances")
  nightdists <- data %>% 
    filter(!is.na(endofday)) %>% 
    ungroup() %>%
    mutate(
      endofday_dist_m = mt_distance(., units = "m"),
      endofday_dist_m = ifelse(endofday == "FINAL", endofday_dist_m, NA)
    ) %>%
    as.data.frame() %>%
    dplyr::select(c(ID, mt_time_column(.), endofday_dist_m))
  
  data %<>% left_join(nightdists, by = c("ID", mt_time_column(.)))
  
  # This gives us one overnight-distance measure at the end of each bird's day
  data %<>% mutate(
    roostsite = ifelse(
      !is.na(endofday_dist_m) & endofday_dist_m < 15,
      1, 0
    )
  ) 
  
  logger.trace("  |> Generating roost-group data")
  #  Calculate cumulative travel and reverse cumulative travel per day
  data %<>%
    group_by(ID, yearmonthday) %>%
    mutate(travel01 = ifelse(stationary == 1, 0, 1)) %>%
    mutate(cum_trav = cumsum(travel01),
           revcum_trav = spatstat.utils::revcumsum(travel01)) %>%
    ungroup() %>%
    mutate(
      # Generate runs of stationary behaviour before/after final/first location:
      roostgroup = ifelse(cum_trav == 0 | revcum_trav == 0, 1, 0),
      roostgroup = data.table::rleid(roostgroup),
      roostgroup = ifelse(cum_trav != 0 & revcum_trav != 0, NA, roostgroup)
    ) 
}



#' /////////////////////////////////////////////////////////////////////////////////////////////  
#' Derive columns required for the non-roosting stationary cumulative time  
#' 
#' Relevant added columns 
#'  - `cumtimestat`: cumulative time spent, up to each location, in a run of
#'  non-roosting stationary time-points. 0's attributed to locations that are
#'  not part of a stationary run#'  
#'  - `dayRunThresh`: 95th percentile of stationary run durations, per bird  
add_nonroost_stationary_cols <- function(data){
  
  # Generate non-roosting stationary run-length data
  data %<>%
    # QUESTION (BC): Shouldn't it be grouped by yearmonthday too? If not, the
    # same run-length could link locations separated by gaps larger than a day
    # (e.g. due to lack of GPS signal). Note: yearmonthday would have to be
    # included in the subsequent group_by steps accordingly
    group_by(ID) %>%
    mutate(
      stationaryNotRoost = ifelse(stationary == 1 & behav %!in% c("SRoosting"), 1, 0),
      stationary_runLts = data.table::rleid(stationaryNotRoost == 1),     # id runs of stationary & non-stationary entries
      stationary_runLts = ifelse(stationaryNotRoost == 0, NA, stationary_runLts)
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
  
  
  # find the duration of every stationary run
  eventtimes <- data %>% data.frame() %>%
    group_by(ID, stationary_runLts) %>%
    summarise(runtime = suppressWarnings(max(cumtimestat, na.rm = TRUE)),
              runtime = ifelse(is.infinite(runtime), 0, runtime)) %>%
    # making grouping explicit, for clarity
    group_by(ID) %>% 
    mutate(dayRunThresh = quantile(runtime, probs = 0.95))
  
  # add run time back to main data
  data %<>% left_join(., eventtimes, by = c("ID", "stationary_runLts"))
  
  data
}



#' /////////////////////////////////////////////////////////////////////////////////////////////
speed_time_model <- function(dt, travelcut, p){
  
  id <- unique(dt$ID)
  
  logger.info(paste0("  |> Fitting model for track ", id, " @ ", lubridate::now()))
  
  newdat <- dt %>%
    dplyr::mutate(
      month = month(timestamp),
      response = kmph + 0.00001
    ) %>%
    dplyr::filter(
      !is.na(response),
      response < travelcut,
      timestamp > (max(timestamp) - days(30))
    )
  
  # ** add if statement or similar to ensure that if not enough data,
  # this modelling is not done (e.g. need 10 days??) ** #
  # ALSO what happens if more than 30 days data provided??
  #
  # NOTE: Predicting to full dataset (we'll ignore the travelling points in the classification later)
  
  initialModel <- glm(response  ~ 1 , family = Gamma(link="log"), data = newdat)
  
  salsa1dlist <- list(fitnessMeasure = 'BIC',
                      minKnots_1d = c(1),
                      maxKnots_1d = c(5),
                      startKnots_1d = c(1),
                      degree = c(2),
                      maxIterations = 10,
                      gaps = c(0))
  
  # run SALSA
  fit <- tryCatch(
    
    suppressPackageStartupMessages( # prevent dependency loading msgs on workers' launch
      
      runSALSA1D(
        initialModel,
        salsa1dlist,
        varlist=c("hourmin"),
        splineParams=NULL,
        datain=newdat,
        predictionData = filter(dt, !is.na(kmph)),
        panelid = newdat$yearmonthday,
        suppress.printout = TRUE)$bestModel
    ),
    
    error = \(cnd){
      # needed to handle apparent unclosed connection in some error cases of runSALSA1D
      sink()
      
      logger.warn(
        paste0(
          "    |x Ouch!! Something went wrong while fitting the model for subject ", id, ".\n",
          "           |x `runSALSA1D()` returned the following error message:\n",
          "           |x \"", conditionMessage(cnd), "\"\n",
          "           |x Speed thresholds WON'T be considered in the behaviour classification of subject ", id, "."
        ))
      
      return(NULL)
    }
  )
  
  if(!is_null(fit)){
    
    dt$kmphpreds <- predict(object = fit, newdata = dt)
    
    
    boots <- suppressPackageStartupMessages( # prevent dependency loading msgs on workers' launch
      MRSea::do.bootstrap.cress.robust(
        model.obj = fit,
        predictionGrid = dt,
        B = 1000, robust = TRUE,
        cat.message = FALSE)
    )
    
    cis <- MRSea::makeBootCIs(boots)
    
    # # -- Construct prediction intervals
    # d = summary(fit)$dispersion
    # predint <- apply(boots, 2, function(x){rgamma(n = length(x), shape = 1/d, scale= x*d)})
    # pis <- t(apply(predint, 1, FUN = quantile,probs = c(0.025, 0.975)))
    
    dt <- dt %>%
      mutate("kmphCI2.5" = cis[,1],
             "kmphCI97.5" = cis[,2],
             #"kmphPI2.5" = pis[,1],
             #"kmphPI97.5" = pis[,2]
      )
    
    # ultimately probably only need to keep last column (kmphPI97.5)
  } else{
    
    dt <- dt |>
      mutate(
        kmphpreds = NA,
        `kmphCI2.5` = NA,
        `kmphCI97.5` = NA,
        #`kmphPI2.5` = NA,
        #`kmphPI97.5` = NA
      )
  }
  
  p()
  
  return(dt)
}

